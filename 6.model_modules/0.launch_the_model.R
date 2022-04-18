#' Käivitusmoodul
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
#' @param saastumaar  Säästumäär ehk see osa kodumajapidamiste sissetulekust, mille 
#' majapidamised säästavad. Säästumäära kasutatakse tingitud efektide arvutamisel. 
#' Kui säästumäär on 10,5%, siis tuleks see sisestada kujul 10.5
#' @param hoive Hõive näitajate arvutamise baasandmestik. Valida saab c("ETU", "EMTA") 
#' @param label_raha Telje nimi rahas mõõdetavatele näitajatele
#' @param label_inimest Telje nimi hõivenäitajatele
#' @param murra_y Y telje andmesiltide maksimaalne pikkus tähemärkides
#' @param murra_f Fassetil esitatava teksti maksimaalne pikkus tähemärkides
#' @param f_axes_y Kas fassetid peaksid paiknema y teljel c(TRUE, FALSE) 
#' @param vaikselt Kas mudel peaks mudeli kasutamise käigus kuvatavaid
#' tekste mitte näitama (kasulik Rmd raportite koostamisel) c(TRUE, FALSE)
#' @returns List, mille element "joonised" sisaldab simulatsiooni tulemusi 
#' esitatuna joonistel ning element "andmed" tulemusi andmefailina.
run = function(andmed, intervall, elastsused, multiplikaatorid, saastumaar, label_raha = "eurot", label_inimesed = "inimest", murra_y=200, murra_f=200, f_axes_y = T, vaikselt = F, hoive = "EMTA") {
  
  if(missing("andmed") == T) {
    # Palume valida andmete asukoht
    cat(enc2utf8("Vali fail, kus on simulatsiooni sisendandmed
        (kodumajapidamiste, valitsuse, investeeringute ja ekspordi
        lõpptarbimine ning töötajate arvu muutused. Kui mõnes sektoris/perioodis
        muutust poole, siis jätke see tühjaks."))
    
    andmed = file.choose(new = T) %>% enc2native()
  }
  
  if(missing("intervall") == T) {
    intervall  = prompt_options(text_promt = "Kas sisestatavad andmed on kuised või aastased? (vali kuu/aasta): ", 
                                text_error = "Sellist valikut pole. Vali uuesti: ",
                                options = c("kuu", "aasta"),
                                case_sensitive = "No")
  }
  
  if(missing("elastsused") == T) {
    elastsused  = prompt_options(text_promt = "Milliseid elastsusi kasutame? (vali nullid/miinusühed/ekspert): ", 
                                 text_error = "Sellist valikut pole. Vali uuesti: ",
                                 options = c("nullid", "miinusühed", "ekspert"),
                                 case_sensitive = "No")
  }
  
  elastsused = switch (elastsused,
                       "ekspert" = "ekspert",
                       "nullid" = "null",
                       "miinusühed" = "miinusyks")
  
  if(missing("multiplikaatorid")==T) {
    multiplikaatorid  = prompt_options(text_promt = "Kas kasutame I või II tüüpi multiplikaatoreid? (vali I/II): ", 
                                       text_error = "Sellist valikut pole. Vali uuesti: ",
                                       options = c("I", "II"),
                                       case_sensitive = "Yes")
  }
  
  if(vaikselt == F) {
    cat("Sisend-väljund tabelid on koostatud 2017. aasta pakkumise ja kasutamise tabeli põhjal,\ntulemused esitatakse, 2020. aasta nominaalses vääringus\n")
  }
  
  
  
  if(intervall=="kuu" & vaikselt == F){
    cat("NB! Mudel võta kuiste andmete puhul arvesse sessoonsust,\n iga kuu näitajad on lihtsalt 1/12 aastastest väärtustest.\n")
  }
  
  
  if(multiplikaatorid == "II" & missing("saastumaar")) {
    saastumaar =  prompt_for_num(text_promt = "Millist säästumäära kasutame? (kui nt 10.5%, siis sisesta 10.5): ", 
                                      text_error = "Sa kas sisestasid arvu asemel teksti või ei kasutanud koma kohal punkti (või siis vastupidi). Sisesta uuesti: ")
  } 
    
  if(multiplikaatorid == "I") {
    saastumaar = 0
  }
  
  # Arvutame tulemused
  input_data = input_data_f(file_path = andmed) # Loeme andmed sisse
  
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
  result[["Joonised"]][["Koondtulemused"]] = result_data %>%
    aggregate_stuff(results = "totals", format = "long") %>%
    `[[`("totals") %>%
    as.data.table() %>%
    .[, Näitaja := factor(Näitaja, levels = c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Hõive", "Maksutulu"))]%>%
    ggplot(aes(x=Stsenaarium, y=Value, group = 1))+
    scale_x_discrete(labels = scales::label_wrap(murra_y))+
    scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
    geom_bar(stat = "identity")+
    ylab(paste0(label_raha, "/", label_inimesed))+
    xlab("")
  
  if(f_axes_y == TRUE) {
    result[["Joonised"]][["Koondtulemused"]] = result[["Joonised"]][["Koondtulemused"]] + 
      facet_grid(.~Näitaja, scales="free_x", labeller = label_wrap_gen(width = murra_f))+
      coord_flip()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
  } else {
    result[["Joonised"]][["Koondtulemused"]] = result[["Joonised"]][["Koondtulemused"]] + 
      facet_grid(Näitaja~., scales="free_y", labeller = label_wrap_gen(width = murra_f))+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0))
  }
  
  
  # Kõik väljundid eraldi joonistel
  for(i in c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Hõive", "Maksutulu")) {
    if(i == "Hõive") {
      label_y = label_inimesed      
    } else {
      label_y = label_raha
    }
    
    result[["Joonised"]][[i]] = result_data %>%
      aggregate_stuff(results = "totals", format = "long") %>%
      `[[`("totals") %>%
      as.data.table() %>%
      .[Näitaja == i]%>%
      ggplot(aes(x=Stsenaarium, y=Value, group = 1))+
      scale_x_discrete(labels = scales::label_wrap(murra_y))+
      scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
      geom_bar(stat = "identity")+
      ylab(label_y)+
      xlab("")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
    if(f_axes_y == TRUE) {
      result[["Joonised"]][[i]] = result[["Joonised"]][[i]] +
        coord_flip()  
    }
  }
  
  # Detailne maksutulu
  result[["Joonised"]][["Maksutulu-detailne"]] = result_data %>%
    aggregate_stuff(results = "totals", format = "long") %>%
    `[[`("taxes_det") %>%
    as.data.table() %>%
    .[, Näitaja:=factor(Näitaja, levels = names(result_data$taxes))] %>%
    ggplot(aes(x=Stsenaarium, y=Value, group = 1))+
    facet_grid(Näitaja~., scales="free_x",  labeller = label_wrap_gen(width = murra_f))+
    scale_x_discrete(labels = scales::label_wrap(murra_y))+
    scale_y_continuous(labels = scales::label_comma(big.mark = " "))+
    geom_bar(stat = "identity")+
    ylab(label_raha)+
    xlab("")
  
  if(f_axes_y == TRUE) {
    result[["Joonised"]][["Maksutulu-detailne"]] = result[["Joonised"]][["Maksutulu-detailne"]] + 
      coord_flip()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0))
    
  } else {
    result[["Joonised"]][["Maksutulu-detailne"]] = result[["Joonised"]][["Maksutulu-detailne"]] + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            strip.text.y = element_text(angle = 0, hjust = 0))
  }
  
  
  
  return (result)
}
