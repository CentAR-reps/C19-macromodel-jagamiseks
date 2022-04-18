#' Küsi kasutaja sisendit 
#' 
#' Funktsioon küsib kasutajalt sisendit ja takistab
#' mittesobiva sisendi andmist.
#' 
#' @param text_promt Küsimus, mida soovitakse kasutajale 
#' kuvada: nt "Kas soovite liikuda edasi (Jah/Ei)?: "
#' @param text_error Veateade, kui kasutaja valik ei vasta 
#' parameetritele, nt "Sellist valikuvõimalust pole. Valige uuesti: "
#' @param options Lubatud vastusevariandid, nt c("jah", "ei")
#' @param case_sensitive Kas vastuseid käsitletakse 
#' tõstutundlikult? Võimalikud valikud: c("Yes", "No")
#' @returns Kasutajapoolse valiku teksti vormis
prompt_options <- function(text_promt = "Yes/No?: ", 
                           text_error = "This option is not available. Choose again: ",
                           options = c("Yes", "No"),
                           case_sensitive = "No") {
  
  if(tolower(case_sensitive) == "no") {
    options = options %>%
      tolower()
    
    result =  readline(prompt=text_promt) %>%
      tolower()
  } else {
    options = options
    
    result =  readline(prompt=text_promt)
  }
  
  while(!(result %in% options)){
    
    result = readline(text_error) %>%
      tolower()
    
    if (result %in% options) {
      break
    } else {
      result<- -1
    }
  }   
  
  return(result)
}

#' Küsi kasutaja arvulist sisendit 
#' 
#' Funktsioon küsib kasutajalt arvulist sisendit ja takistab
#' mittesobiva sisendi andmist.
#' 
#' @param text_promt Küsimus, mida soovitakse kasutajale 
#' kuvada: nt "Sisesta säästumäär (kui 10%, siis 10): "
#' @param text_error Veateade, kui kasutaja valik ei vasta 
#' parameetritele
#' @returns asutajapoolse valiku arvuna
prompt_for_num = function(text_promt = "Millist säästumäära kasutame? (kui nt 10.5%, siis sisesta 10.5): ", 
                          text_error = "Sa kas sisestasid arvu asemel teksti või ei kasutanud koma asemel punkti (või siis vastupidi). Sisesta uuesti:") {
  
  result = readline(prompt=text_promt) 
  result = result %>% as.numeric() %>% suppressWarnings()
  
  while(is.na(result)){
    
    result = readline(prompt = text_error)
    result = result %>% as.numeric() %>% suppressWarnings()
      
    if (!is.na(result)) {
      break
    } else {
      result<- NA
    }
  }
  return(result)
}


#' Funktsioon, mis agregeerib simulatsiooni tulemused ilusamale kujule
#' 
#' Funktsioon agregeerib tulemused, mis mudelist väljuvad
#' lõpptarbimise komponentide lõikes, kokku ning võimaldab neid vaadata
#' kas majandustegevusalade lõikes või siis summeerituna ka üle
#' majandustegevusalade. Ühtlasi on võimalik viia andmed nii pikale
#' kui laiale kujule.
#' 
#' @param results Tulemuste agregatsiooniaste c("totals", "by_sectors")
#' @param format Andmete esitamise formaat c("long", "wide)
#' @returns List, millel on kaks elementi - "totals" sisaldab andmeid 
#' kõigi simulatsiooni väljundite kohta, kogu maksutulu on agregeeritud,
#' "taxes_det" esitab maksutulu maksuliikide lõikes.   

aggregate_stuff = function(data_i, results = "totals", format = "wide"){
  
  total_production = data_i$total_production$d_household_FD+
    data_i$total_production$d_investments_FD+
    data_i$total_production$d_govexp_FD+
    data_i$total_production$d_exports_FD
  
  total_production$Tegevusala = rownames(total_production)
  total_production$Näitaja = "Kogutoodang"
  
  value_added = data_i$value_added$d_household_FD+
    data_i$value_added$d_investments_FD+
    data_i$value_added$d_govexp_FD+
    data_i$value_added$d_exports_FD
  
  value_added$Tegevusala = rownames(value_added)
  value_added$Näitaja = "Lisandväärtus"
  
  labour_cost = data_i$labour_cost$d_household_FD+
    data_i$labour_cost$d_investments_FD+
    data_i$labour_cost$d_govexp_FD+
    data_i$labour_cost$d_exports_FD
  
  labour_cost$Tegevusala = rownames(labour_cost)
  labour_cost$Näitaja = "Tööjõukulu"
  
  profits = data_i$profits$d_household_FD+
    data_i$profits$d_investments_FD+
    data_i$profits$d_govexp_FD+
    data_i$profits$d_exports_FD
  
  profits$Tegevusala = rownames(profits)
  profits$Näitaja = "Kasum"
  
  taxes = data_i$taxes$`Füüsilise isiku tulumaks`+
    data_i$taxes$`Ettevõtte tulumaks`+
    data_i$taxes$Sotsiaalmaks+
    data_i$taxes$Töötuskindlustusmaksed+
    data_i$taxes$Käibemaks+
    data_i$taxes$Aktsiisid+
    data_i$taxes$`Muu (kõik ülejäänud kokku agregeeritud)`
  
  taxes$Tegevusala = rownames(taxes)
  taxes$Näitaja = "Maksutulu"
  
  employment = data_i$employment
  employment$Tegevusala = rownames(employment)
  employment$Näitaja = "Hõive"
  
  results_by_sector = list()
  results_by_sector[["totals"]] = rbind(total_production,value_added,labour_cost, profits, employment, taxes)
  
  
  # Maksud detailselt
  for(j in names(data_i$taxes)) {
    eval(parse(text = paste0("tegevusalad_abi = rownames(data_i$taxes$`", j,"`)")))
    eval(parse(text = paste0("abi = data_i$taxes$`", j,"`")))
    abi$Tegevusala = tegevusalad_abi
    abi$Näitaja = j
    if(j == "Füüsilise isiku tulumaks" ) {
      abi2 = abi
    } else {
      abi2 = rbind(abi2, abi)
    }
  }
  
  results_by_sector[["taxes_det"]] = abi2
  
  # Vormistus ilusamaks
  # Järjestame muutujad ringi ja edasiste operatsioonide jaoks ajutiselt data.table formaati
  
  abi_names = names(results_by_sector[["totals"]])
  abi_names1 = rev(abi_names)[1:2]
  abi_names2 = abi_names[! (abi_names %in% abi_names1)]
  order_new = c(abi_names1, abi_names2)
  
  results_by_sector[["totals"]] = results_by_sector[["totals"]][, order_new] %>%
    as.data.table()
  results_by_sector[["taxes_det"]] = results_by_sector[["taxes_det"]][, order_new] %>%
    as.data.table()
  
  # Kustutame reanimed 
  rownames(results_by_sector[["totals"]]) = NULL
  rownames(results_by_sector[["taxes_det"]]) = NULL
  
  # Defineerime funktsiooni, mis aitab andmed pikale kujule viia
  to_long = function(data, idvars) {
    start_var = length(idvars)+1
    result = data %>% 
      melt(id.vars = idvars, 
           measure.vars = names(data)[start_var:length(names(data))],
           variable.name = "Stsenaarium",
           value.name = "Value") %>%
      as.data.frame()
    return(result)
  }
  
  if(results == "by_sectors") {
    if(format == "long") {
      results_by_sector_long = list()
      results_by_sector_long[["totals"]] = to_long(results_by_sector[["totals"]], idvars=c("Näitaja", "Tegevusala"))  
      results_by_sector_long[["taxes_det"]] = to_long(results_by_sector[["taxes_det"]], idvars = c("Näitaja", "Tegevusala"))
      
      result = results_by_sector_long
    } else {
      result = results_by_sector
    }
  }
  
  if(results == "totals") {
    
    results_as_totals = list()
    
    abi = results_by_sector[["totals"]]
    results_as_totals[["totals"]] = abi%>%
      .[, lapply(.SD, sum, na.rm=T), .SDcols = 3:length(names(abi)), by = "Näitaja"]
    
    abi = results_by_sector[["taxes_det"]]
    results_as_totals[["taxes_det"]] = abi%>%
      .[, lapply(.SD, sum, na.rm=T), .SDcols = 3:length(names(abi)), by = "Näitaja"]
    
    if(format == "long") {
      results_as_totals_long = list()
      results_as_totals_long[["totals"]] = to_long(results_as_totals[["totals"]], idvars=c("Näitaja"))  
      results_as_totals_long[["taxes_det"]] = to_long(results_as_totals[["taxes_det"]], idvars = c("Näitaja"))
      
      result = results_as_totals_long
    } else {
      result = results_as_totals
    }
  }
  
  # Väljundid data.frame formaati
  result[["totals"]] = result[["totals"]] %>% as.data.frame()
  result[["taxes_det"]] = result[["taxes_det"]] %>% as.data.frame()
  
  return(result)
}

