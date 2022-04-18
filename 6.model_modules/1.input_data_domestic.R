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
  d_household_FD = read_excel(file_path, sheet = enc2utf8("kodumajapidamised")) %>% as.data.frame()
  d_investments_FD = read_excel(file_path, sheet = enc2utf8("investeeringud")) %>% as.data.frame()
  d_govexp_FD = read_excel(file_path, sheet = enc2utf8("valitsussektor")) %>% as.data.frame()
  d_exports_FD = read_excel(file_path, sheet = enc2native("eksport")) %>% as.data.frame()
  
  # Hõive
  d_empl = read_excel(file_path, sheet = enc2utf8("töötajate arv")) %>% as.data.frame()
  
  # Maksumäärad
  d_VAT_20 = read_excel(file_path, sheet = enc2utf8("uus käibemaks 20")) %>% as.data.frame()
  d_VAT_9 = read_excel(file_path, sheet = enc2utf8("uus käibemaks 9")) %>% as.data.frame()
  d_ST = read_excel(file_path, sheet = enc2utf8("uus sotsiaalmaks")) %>% as.data.frame()
  d_UIC = read_excel(file_path, sheet = enc2utf8("uus TKM")) %>% as.data.frame()
  
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
# input_data = input_data_f(file_path = andmed)
