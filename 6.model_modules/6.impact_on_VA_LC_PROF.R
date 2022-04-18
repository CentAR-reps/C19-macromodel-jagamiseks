#' Lisandväärtuse moodul
#' 
#' Moodul võtab sisendiks muutused kogutoodangus ning 
#' arvutab selle põhjal mõju lisandväärtusele ning 
#' osadele selle komponentidele (tööjõukulu ja kasumi).
#' NB! Hinnata saab kas nõudluse või pakkumise 
#' poolseid mõjusid, mõlemaid korraga hinnata ei saa 
#' (sellisel juhul lähtutakse pakkumise poolsest muutusest).
#' @param data_i Nõudluspoolse kogutoodangu muutuse
#' mooduli väljundiks olev viiest elemendist koosnev list, mille 
#' neli esimest elementi sisaldavad infot kogutoodangu,
#' muutuste kohta lõpptarbimise elementide lõikes ning 
#' viimane element on muutus impordi lõpptarbimises. 
#' Andmed on data.frame formaadis.
#' @param data_i_supply Pakkumispoolse kogutoodangu muutusese 
#' mooduli väljundiks olev andmestik (data.frame formaadis), mis
#' sisaldab kogutoodangu muutust tegevusalade lõikes.    
#' @returns Kaheastmeline list, kus esimene tase on 
#' liigendatud meid huvitavate näitajate järgi 
#' (kogutoodang, lisandväärtus, tööjõukulu, kasum), 
#' millele on lisatud lõpptarbimise impordi osakaal 
#' (kasutame seda maksumoodulis). Iga komponendi all on 
#' omakorda neljast elemendist koosnev list, elemendid 
#' sisaldavad erienvate lõppkasutamise komponentide 
#' (kodumajapidamiste lõpptarbimine, investeeringud, 
#' valitsuse kulutused ning eksport) muutuse 
#' mõju aste kõrgemal olevale näitajale tegevusalade 
#' (read) ja stsenaariumide (tulbad) lõikes. 
#' Listi elemendid on data.frame formaadis. 
impact_on_VA_LC_PROF_f = function(data_i, data_i_supply) {
  # Üldine lähenemine
  # - Kui sisendandmetes on pakkumise poolsed mudatused, siis lähtume nendest
  # - Loeme sisse lisandväärtuse ja selle komponentide osakaalud kogutoodangus (tegevusalade lõikes)
  # - Korrutame muutused kogutoodangus eelmises punktis mainitud osakaaludega.
  
  # Kui pakkumise poolsed andmed on olemas, siis lähtume nendest (võimalik, et muudame seda veel)
  if((abs(max(data_i_supply$d_household_FD, na.rm =T)) + abs(min(data_i_supply$d_household_FD, na.rm =T))) > 0) {
    data_i = data_i_supply
  }
  
  # Kirjutame impordi lõpptarbimise andmed eraldi objekti (lisame selle hiljem väljunditele)
  FD_imports = data_i$FD_of_imports
  
  load(file.path(data_loc, "ibi_IO_elements.Rda"))
  
  # Osakaalud kogutoodangus
    # Lisandvärätuse osakaalud kogutoodangus
    VA_share = L[-2,] %>%
      colSums() %>%
      as.numeric()
    
    # Tööjõukulude osakaalud kogutoodangus
    LC_share = L["Hüvitised töötajatele",]%>%
      as.numeric()
    
    # Kasumite osakaalud kogutoodangus
    PROF_share = L["Tegevuse ülejääk ja segatulu, neto",]%>%
      as.numeric()
    
    comp_abi = c("VA_share", "LC_share", "PROF_share")
  
  # Muutused lisandväärtuses ja selle elementides
    for(i in comp_abi) {
      abi = data_i
      abi$FD_of_imports = NULL # Neid elemente me ei vaja
      abi$FD_imports = NULL # Neid elemente me ei vaja
      
      abi$d_household_FD = data_i$d_household_FD*get(i)
      abi$d_investments_FD = data_i$d_investments_FD*get(i)
      abi$d_govexp_FD = data_i$d_govexp_FD*get(i)
      abi$d_exports_FD = data_i$d_exports_FD*get(i)
      
      eval(parse(text = paste0(substr(i, 1,2), "<-abi")))
    }
    
    # Kogutoodangu lehelt ka ebavajalik info maha
    data_i$FD_of_imports = NULL
    data_i$FD_imports = NULL

  # Vormistame väljundid
  result = list(total_production = data_i,
                value_added = VA,
                labour_cost = LC,
                profits = PR,
                "FD_of_imports" = FD_imports)
  
  
  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
# impact_of_prices_on_FD = impact_of_prices_on_FD_f(data_i = impact_of_taxes_on_prices, year = 2020, IO_year = 2017, time_interval = "kuu", elasticity = "miinusyks")
# demand_side_TP = demand_side_impact_on_TP_f(data_prices_i = impact_of_prices_on_FD, data_i = input_data, mult_type_i = "I", hh_savings_rate_i = 0,change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused)
# supply_side_impact_TP = supply_side_impact_on_TP_f(data_i = input_data, time_interval = "aasta")
# impact_on_VA_LC_PROF_TP = impact_on_VA_LC_PROF_f(data_i = demand_side_TP, data_i_supply = supply_side_impact_TP)
