
#' Käivitusmoodul appi jaoks
#' 
#' Selle mooduli abil käivitatakse simulatsioonimudel. Mudeli võib käivitada
#' ilma parameetriteta (sellisel juhul küsitakse sisendeid jooksvalt),
#' või siis käsurealt, andes vajalikud parameetrid teksti kujul ette.
#' @param andmed Sisendandmeid sisaldava faili asukoht (vaata näidiseid 
#' kataloogis '4.user_input_examples')
#' @param intervall Sisendandmete ajaarvestuse periood. Võib olla: c("kuu", "aasta")
#' @param elastsused Simulatsiooni kasutatavad nõudluse hinnaelastsused. 
#' On kolm valikut c("null", "miinusyks", "ekspert"). Esimesel juhul eeldatakse
#' et kõigi tegevusalade toodete/teenuste hinnaelastsused on nullid, teisel 
#' juhul miinus ühed ning kolmandal juhul võrdsed kirjanduse analüüsi
#' põhjal kokku koondatud väärtustega. Elastsused ise on andmete kataloogis, 
#' failis "elasticities.xlsx"   
#' @param multiplikaatorid Leontiefi tootmismultiplikaatorid, mida simulatsioonis 
#' kasutatakse. Valida saab kahte tüüpi multiplikaatoreid c("I", "II")
#' @param label_raha Telje nimi rahas mõõdetavatele näitajatele
#' @param label_inimest Telje nimi hõivenäitajatele
#' @param murra_y Y telje andmesiltide maksimaalne pikkus tähemärkides
#' @param murra_f Fassetil esitatava teksti maksimaalne pikkus tähemärkides
#' @param f_axes_y Kas fassetid peaksid paiknema y teljel c(TRUE, FALSE) 
#' @param vaikselt Kas mudel peaks mudeli kasutamise käigus kuvatavaid
#' @param hoive Hõive näitajte arvutamise arvutamise baasandmestik. Valida saab c("ETU", "EMTA") 
#' tekste mitte näitama (kasulik Rmd raportite koostamisel) c(TRUE, FALSE)
#' @returns List, mille element "joonised" sisaldab simulatsiooni tulemusi 
#' esitatuna joonistel ning element "andmed" tulemusi andmefailina.
run_model = function(input_data, 
                     intervall = 'aasta', 
                     elastsused = 'null', 
                     multiplikaatorid = 'I', 
                     saastumaar = 0, 
                     label_raha = "eurot", 
                     label_inimesed = "inimest", 
                     murra_y=200, murra_f=200, 
                     f_axes_y = T,  
                     hoive = "ETU") {
  

  
  # # Arvutame tulemused
  # input_data = input_data_f(file_path = andmed) # Loeme andmed sisse
  
  impact_of_taxes_on_prices = input_data %>% # Maksude mõju hindadele (kui maksumäärades plaanitakse muutusi)
    impact_of_taxes_on_prices_f()
  
  
  # Salvestame simulatsiooni tulemused listi
  result = list()
  result[["Joonised"]][["Koondtulemused"]] = NA
  result[["Andmed"]][["Toored"]] = NA
  
  # Andmed - pakkumise pool
  supply_side_impact_TP = supply_side_impact_on_TP_f(data_i = input_data, time_interval = intervall, source_emp = hoive)
  
  # Andmed - nõudluse pool (NB! Kui pakkumise poole andmed on olemas, siis asendatakse nõudluse poole efektid moodulis 
  # impact_on_VA_LC_PROF_f pakkumise poole omadega).
  
  result_data = impact_of_taxes_on_prices %>% 
    impact_of_prices_on_FD_f(time_interval = intervall, elasticity = elastsused) %>% # Hinnamuutuste mõju lõpptarbimisele
    demand_side_impact_on_TP_f(data_i = input_data, mult_type_i = multiplikaatorid, hh_savings_rate_i = saastumaar, change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused) %>% # Lõpptarbimise muutuse mõju kogutoodangule
    impact_on_VA_LC_PROF_f(data_i_supply = supply_side_impact_TP) %>% # Kogutoodangu muutuse mõju lisandväärtusele, tööjõukulule ja kasumitele
    impact_on_tax_revenue_f(change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused, time_interval = intervall) %>% # Mõju maksutulule
    demand_side_impact_on_employment(time_interval = intervall, source_emp = hoive) # Mõju hõivele
  
  
  # Mitu stsenaariumi on? Sellest lähtuvalt teeme joonised
  n_sts <- result_data$total_production$d_household_FD %>% ncol()
  
  result[["Andmed"]][["Toored"]] = result_data
  
  result[["Andmed"]][["Koond pikk"]] = aggregate_stuff(data_i = result_data, 
                                                       format = "long", 
                                                       results = "totals")
  
  result[["Andmed"]][["Koond pikk sektor"]] = aggregate_stuff(data_i = result_data, 
                                                              format = "long", 
                                                              results = "by_sectors")
  
  
  # Euromodi sisend
  dict = read_excel(file.path(dict_loc, "dictionaries.xlsx"), sheet = "IO-COICOP") %>%
    .[, c("Tegevusala tekst IO tabel", "COICOP kood", "COICOP nimetus")]
  
  result[["Andmed"]][["Euromodi sisend"]] = result$Andmed$`Koond pikk sektor`$totals %>%
    as.data.table() %>%
    .[`Näitaja` == "Hõive"] %>%
    merge(., dict, by.x = "Tegevusala", by.y = "Tegevusala tekst IO tabel", all.x = T) %>%
    .[, Tegevusala :=NULL] %>%
    .[, .(Value = sum(Value)), .( `COICOP kood`, `COICOP nimetus`, `Näitaja`, `Stsenaarium`)] %>%
    .[order(`Stsenaarium`, `COICOP kood`)]
  
  
  
  
  # Koondtulemuste joonis
  ####################################
  if(n_sts == 1){
    result[["Joonised"]][["Koondtulemused"]] = result_data %>%
      aggregate_stuff(results = "totals", format = "long") %>%
      `[[`("totals") %>%
      as.data.table() %>% 
      .[Näitaja != 'Hõive'] %>% # Jätame hõive jooniselt välja
      .[, Näitaja := factor(Näitaja, levels = c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Maksutulu"))] %>% 
      ggplot(aes(x=Näitaja, y=Value))+
      scale_x_discrete(labels = scales::label_wrap(murra_y))+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      geom_bar(stat = "identity")+
      ylab(paste0(label_raha))+
      xlab("")+
      theme_pander() +
      scale_fill_pander()
  } else {
    result[["Joonised"]][["Koondtulemused"]] = result_data %>%
      aggregate_stuff(results = "totals", format = "long") %>%
      `[[`("totals") %>%
      as.data.table() %>% 
      .[Näitaja != 'Hõive'] %>% # Jätame hõive jooniselt välja
      .[, Näitaja := factor(Näitaja, levels = c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Maksutulu"))] %>% 
      ggplot(aes(x=Stsenaarium, y=Value))+
      scale_x_discrete(labels = scales::label_wrap(murra_y))+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      geom_bar(stat = "identity")+
      ylab(paste0(label_raha, "/", label_inimesed))+
      xlab("")+
      facet_grid(.~Näitaja, scales="free_x", labeller = label_wrap_gen(width = murra_f))+
      coord_flip()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_pander() +
      scale_fill_pander()
  }
  
  
  
  # Detailne maksutulu
  ####################################
  if(n_sts == 1){
    result[["Joonised"]][["Maksutulu-detailne"]] = result_data %>%
      aggregate_stuff(results = "totals", format = "long") %>%
      `[[`("taxes_det") %>%
      as.data.table() %>%
      #.[Stsenaarium == 'Kodumajapidamised'] %>% 
      .[, Näitaja:=factor(Näitaja, levels = rev(names(result_data$taxes)))] %>%
      ggplot(aes(x=Näitaja, y=Value))+
      scale_x_discrete(labels = scales::label_wrap(murra_y))+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      geom_bar(stat = "identity")+
      coord_flip()+
      ylab(label_raha)+
      xlab("")+
      theme_pander() +
      scale_fill_pander()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0))
  } else {
    result[["Joonised"]][["Maksutulu-detailne"]] = result_data %>%
      aggregate_stuff(results = "totals", format = "long") %>%
      `[[`("taxes_det") %>%
      as.data.table() %>%
      .[, Näitaja:=factor(Näitaja, levels = rev(names(result_data$taxes)))] %>%
      ggplot(aes(x=Stsenaarium, y=Value, group = 1))+
      facet_grid(Näitaja~., scales="free_x",  labeller = label_wrap_gen(width = murra_f))+
      scale_x_discrete(labels = scales::label_wrap(murra_y))+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      geom_bar(stat = "identity")+
      coord_flip()+
      ylab(label_raha)+
      xlab("")+
      theme_pander() +
      scale_fill_pander()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0))
  }
  
    
  
  # Väljundid eraldi tegevusalade kaupa
  tegevusala_dict <- read_xlsx(file.path(dict_loc, 'dictionaries.xlsx'), sheet = 'EMTAK2008') %>% 
    as.data.table() %>% 
    .[, .(Tegevusala = `Tegevusala tekst IO tabel`, tegevusala_lyh = IO_lühike)] %>% 
    unique()
  
  p_data <- result_data %>%
    aggregate_stuff(results = "by_sectors", format = "long") %>%
    `[[`("totals") %>%
    as.data.table() %>% 
    merge(tegevusala_dict, all.x = T, by = 'Tegevusala', sort = F) %>% 
    .[, tegevusala_lyh := factor(tegevusala_lyh, levels = rev(unique(tegevusala_lyh)))]
  
  # Kõik väljundid eraldi joonistel
  for(i in c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Hõive", "Maksutulu")) {
    if(i == "Hõive") {
      label_y = label_inimesed      
    } else {
      label_y = label_raha
    }
    
    result[["Joonised"]][[i]] = p_data %>%
      .[Näitaja == i]%>%
      ggplot(aes(x=tegevusala_lyh, y=Value))+
      geom_bar(stat = "identity")+
      facet_grid(.~Stsenaarium)+
      coord_flip()+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      ylab(label_y)+
      xlab("")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_pander()+
      scale_fill_pander()
    
  }
  
  
  return (result)
}





#' Andmesisestusmoodul
#' 
#' Moodul küsib sisendandmeid MS Exceli formaadis,
#' sisendandmete näidiste kaustas ("4.user_input_examples") tood kujul. Kõigi
#' lehtede tulpade päised peavad olema ühesugused, see mis tulpade 
#' päistesse kirjutatud on ei ole enamasti oluline (kuupäeva formaadis esitatud
#' andmete puhul formaat säilitatakse). Tühje lahtreid käsitletakse
#' lõpptarbimise elemente sisaldavate lehte puhul nullidena,
#' maksumäärasid sisaldavate lehtede puhul kehtivate maksumääradena.    
#' @param file_path Sisendandmeid sisaldava faili asukoht. 
#' @returns Üheksast data.frame formaadis andmestikust koosneva listi, millest
#' esimesed neli sisaldavad absoluutmahus muutusi õpptarbimise 
#' komponentides, viies muutust hõivatute arvus ning ülejäänud neli uusi
#' käibemaksu, sotsiaalmaksu ja töötuskindlustusmaksu määrasid.
input_data_f = function(file_path) {
  # Üldine lähenemine
  # - Loeme sisse sisendandmeid sisaldava faili
  # - Asendame lõpptarbimise puhul tühjad lahtrid nullidega
  # - Asendame maksumäärade puhul tühjad lahtrid viimaste teadaolevate maksumääradega
  # - Säilitame vajadusel tulpades päises oleva kuupäevaformaadi
  # - Salvestame andmed 9-elemendise listina.
  
  # Lõpptarbimine
  d_household_FD = openxlsx::readWorkbook(file_path, sheet = enc2utf8("kodumajapidamised")) %>% as.data.frame()
  d_investments_FD = openxlsx::readWorkbook(file_path, sheet = enc2utf8("investeeringud")) %>% as.data.frame()
  d_govexp_FD = openxlsx::readWorkbook(file_path, sheet = enc2utf8("valitsussektor")) %>% as.data.frame()
  d_exports_FD = openxlsx::readWorkbook(file_path, sheet = enc2native("eksport")) %>% as.data.frame()
  
  # Hõive
  d_empl = openxlsx::readWorkbook(file_path, sheet = enc2utf8("töötajate arv")) %>% as.data.frame()
  
  # Maksumäärad
  d_VAT_20 = openxlsx::readWorkbook(file_path, sheet = enc2utf8("uus käibemaks 20")) %>% as.data.frame()
  d_VAT_9 = openxlsx::readWorkbook(file_path, sheet = enc2utf8("uus käibemaks 9")) %>% as.data.frame()
  d_ST = openxlsx::readWorkbook(file_path, sheet = enc2utf8("uus sotsiaalmaks")) %>% as.data.frame()
  d_UIC = openxlsx::readWorkbook(file_path, sheet = enc2utf8("uus TKM")) %>% as.data.frame()
  
  # Tühjaks jäetud väljad maksumuudatuste failis täidetakse hetkel kehtivate maksumääradega
  # NB! Kontrolli, millised on mudeli viimased andmed maksumäärade kohta. 
  old_tax_rates = read_xlsx(file.path(data_loc, "nominal_tax_rates_by_sector.xlsx"))
  d_VAT_20[is.na(d_VAT_20)] <- old_tax_rates$`Käibemaks - tavamäär`[1]
  d_VAT_9[is.na(d_VAT_9)] = old_tax_rates$`Käibemaks - vähendatud määr`[1]
  d_ST[is.na(d_ST)] = old_tax_rates$Sotsiaalmaks[1]
  d_UIC[is.na(d_UIC)] = old_tax_rates$Töötuskindlustusmaksed[1]
  
  
  # Funktsioon, mille abil viime sisendandmete lehtedel (nii tulpade päistes kui tulpades) olevad andmed õigesse formaati
  format_input_data = function(data, tax_rate = F) {
    # Viime kõik tulbad (peale esimese) numbrilisse formaati.
    
    for(i in colnames(data)[2:length(colnames(data))]) {
      eval(parse(text = paste0("data$`", i, "` = as.numeric(data$`", i,"`)")))
    }
    
    if(tax_rate == F) {
      data[is.na(data)] = 0 # Lõpptarbimise sisendandmetes on tühjad lahtrid sisuliselt nullid
    } 
    
    data = as.data.frame(data)
    rownames(data) = data$Tegevusala # Maatriksarvutuste lihtsustamiseks viime tegevusalade nimed eraldi muutujast reanimeks. Selle miinuseks on, et kohati on ebamugav kasutada data.table formaati.
    data$Tegevusala = NULL
    
    # Sisestatavate andmete tulpade nimed võivad olla kuupäeva formaadis. Sellisel juhul soovime selle formaadi säilitada
    test = "date"
    
    for(i in 1:length(names(data))){
      test_abi = names(data)[i] %>%
        as.numeric() %>%
        suppressWarnings()
      
      if(is.na(test_abi) == T){
        test = "not_date"
        break
      }
    }
    
    if(test == "date") {
      date_names = names(data) %>% 
        as.numeric() %>% 
        as.Date(origin = "1899-12-30") %>%  
        substr(1,7)
      names(data) = date_names
    } 
    
    data = data %>%
      as.data.frame()
    
    return(data)
  }
  
  result = list(d_household_FD = format_input_data(d_household_FD),
                d_investments_FD = format_input_data(d_investments_FD),
                d_govexp_FD = format_input_data(d_govexp_FD),
                d_exports_FD = format_input_data(d_exports_FD),
                d_empl = format_input_data(d_empl),
                d_VAT_20 = format_input_data(d_VAT_20, tax_rate = T),
                d_VAT_9 = format_input_data(d_VAT_9, tax_rate = T),
                d_ST = format_input_data(d_ST, tax_rate = T),
                d_UIC = format_input_data(d_UIC, tax_rate = T))
  return(result)
}      





# Sisestatud parameetrite alusel sisendandmestike loomine
################################################################################

gen_sim_stsenaarium <- function(inp_list = input_list,
                                tegevusala = c("Haridus", "Tervishoid"),
                                kategooria = 'kodumajapidamised',
                                kategooria_value = 100,
                                km = 0.2,
                                kmv = 0.09,
                                sm = 0.33,
                                tkm = 0.024,
                                hoive = 1000){
  
  # Kui tegevusala on valitud, siis muudame komponentide ja/või hõive väärtuseid
  if(any(tegevusala != '')){
    
    # Kui komponent on valitud, siis muudame vastava komponendi väärtuseid
    if(kategooria == 'kodumajapidamised'){
      inp_list$d_household_FD[rownames(inp_list$d_household_FD) %in% tegevusala, 'Stsenaarium'] <- kategooria_value
    }
    if(kategooria == 'investeeringud'){
      inp_list$d_investments_FD[rownames(inp_list$d_investments_FD) %in% tegevusala, 'Stsenaarium'] <- kategooria_value
    }
    if(kategooria == 'valitsussektor'){
      inp_list$d_govexp_FD[rownames(inp_list$d_govexp_FD) %in% tegevusala, 'Stsenaarium'] <- kategooria_value
    }
    if(kategooria == 'eksport'){
      inp_list$d_exports_FD[rownames(inp_list$d_exports_FD) %in% tegevusala, 'Stsenaarium'] <- kategooria_value
    }
    
    # Hõive
    if(hoive != 0){
      inp_list$d_empl[rownames(inp_list$d_exports_FD) %in% tegevusala, 'Stsenaarium'] <- hoive
    }
  }
  
  # Maksud
  inp_list$d_VAT_20[, 'Stsenaarium'] <- km
  inp_list$d_VAT_9[, 'Stsenaarium'] <- kmv
  inp_list$d_ST[, 'Stsenaarium'] <- sm
  inp_list$d_UIC[, 'Stsenaarium'] <- tkm
  
  
  return(inp_list)
  
}



# Muudame mudeli sisendite listi nimesid ja formaati, et saaks selle excelina salvestada
################################################################################
format_input_list <- function(inp_list){
  new_list <- list()
  new_list[['kodumajapidamised']] <- as.data.table(inp_list$d_household_FD, keep.rownames = 'Tegevusala')
  new_list[['investeeringud']] <- as.data.table(inp_list$d_investments_FD, keep.rownames = 'Tegevusala')
  new_list[['valitsussektor']] <- as.data.table(inp_list$d_govexp_FD, keep.rownames = 'Tegevusala')
  new_list[['eksport']] <- as.data.table(inp_list$d_exports_FD, keep.rownames = 'Tegevusala')
  new_list[['töötajate arv']] <- as.data.table(inp_list$d_empl, keep.rownames = 'Tegevusala')
  new_list[['uus käibemaks 20']] <- as.data.table(inp_list$d_VAT_20, keep.rownames = 'Tegevusala')
  new_list[['uus käibemaks 9']] <- as.data.table(inp_list$d_VAT_9, keep.rownames = 'Tegevusala')
  new_list[['uus sotsiaalmaks']] <- as.data.table(inp_list$d_ST, keep.rownames = 'Tegevusala')
  new_list[['uus TKM']] <- as.data.table(inp_list$d_UIC, keep.rownames = 'Tegevusala')
  return(new_list)
}





# Ekspordimooduli sisendist lähtuva mudelisisendi genereerimine
################################################################################

# Ekspordi muutus tegevusalade lõikes
####################################

gen_eksport_stsenaarium_tegevusala <- function(sisend = eksport_sisend_c,
                                               input_template = input_list_templ,
                                               goods_services = "goods_and_services",
                                               input_type = "relative_change",
                                               eksport_tegevusala_tgala = 'Haridus',
                                               eksport_tegevusala_riik = "FIN",
                                               eksport_muutus = 0.1){
  
  sisend$topK[['Stsenaarium']] <- as.numeric(sisend$topK[['Stsenaarium']])
  sisend$topK[origin_industry_text == eksport_tegevusala_tgala & 
                destination_country == eksport_tegevusala_riik, 
              'Stsenaarium'] <- eksport_muutus
  
  export_change_long <- calculate_export_demand_change_long_app(input_type = input_type, 
                                                                input_agr = 'c', 
                                                                topK = sisend$topK, 
                                                                sihtriik = sisend$countries, 
                                                                intervall = 'aasta', 
                                                                export_baseline = export_baseline, 
                                                                goods_services = goods_services)
  export_change <- calculate_export_demand_change_app(export_change_long)
  export_change <- rbind(export_change,
                         data.table(industry_text = "Kodumajapidamised  tööandjana; oma tarbeks mõeldud kaupade tootmine ja teenuste osutamine",
                                    Stsenaarium = 0),
                         fill = T)
  export_change_mat <- as.data.frame(export_change[, 'Stsenaarium'])
  rownames(export_change_mat) <- export_change$industry_text
  
  out <- input_template
  out$d_exports_FD <- export_change_mat
  
  return(out)
  
  
}




# Ekspordi muutus sihtriikide lõikes
####################################

gen_eksport_stsenaarium_riik <- function(sisend_ci = eksport_sisend_ci,
                                         sisend_c = eksport_sisend_c,
                                         input_template = input_list_templ,
                                         eksport_riik_agr = 'ci',
                                         goods_services = "goods_and_services",
                                         input_type = "relative_change",
                                         eksport_riik_tgala = 'Haridus',
                                         eksport_riik_riik = "FIN",
                                         eksport_muutus = 0.1){
  
  if(eksport_riik_agr == 'c'){
    
    sisend_c$countries[['Stsenaarium']] <- as.numeric(sisend_c$countries[['Stsenaarium']])
    sisend_c$countries[destination_country == eksport_riik_riik, 'Stsenaarium'] <- eksport_muutus
    
    topK_dat <- sisend_c$topK
    sihtriik_dat <- sisend_c$countries
    
  }
  
  if(eksport_riik_agr == 'ci'){
    
    sisend_ci[[eksport_riik_riik]][['Stsenaarium']] <- as.numeric(sisend_ci[[eksport_riik_riik]][['Stsenaarium']])
    sisend_ci[[eksport_riik_riik]][destination_industry_text == eksport_riik_tgala, 'Stsenaarium'] <- eksport_muutus
    
    topK_dat <- sisend_ci$topK
    sihtriik_dat <- sisend_ci[-1]
  }
  
  
  export_change_long <- calculate_export_demand_change_long_app(input_type = input_type, 
                                                                input_agr = eksport_riik_agr, 
                                                                topK = topK_dat, 
                                                                sihtriik = sihtriik_dat, 
                                                                intervall = 'aasta', 
                                                                export_baseline = export_baseline, 
                                                                goods_services = goods_services)
  export_change <- calculate_export_demand_change_app(export_change_long)
  export_change <- rbind(export_change,
                         data.table(industry_text = "Kodumajapidamised  tööandjana; oma tarbeks mõeldud kaupade tootmine ja teenuste osutamine",
                                    Stsenaarium = 0),
                         fill = T)
  export_change_mat <- as.data.frame(export_change[, 'Stsenaarium'])
  rownames(export_change_mat) <- export_change$industry_text
  
  out <- input_template
  out$d_exports_FD <- export_change_mat
  
  return(out)
  
  
}
