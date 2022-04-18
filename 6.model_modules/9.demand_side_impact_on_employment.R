#' Hõivemoodul  
#' 
#' Moodul võtab sisendiks muutuse kogutoodangus (vahetu väljund kogutoodangumoodulist,
#' mida kantakse edasi läbi teiste moodulite kuni maksumoodulini) ning arvutab
#' selle põhjal välja mõju hõivele. Arvutustes lähtutakse sellest, kui palju
#' töötajaid on vaja, et toota ühe aasta jagu kogutoodangud (tegevusalade lõikes).
#' Kui sisestatavate andmete ajaintervall on kuudes, siis lähtutakse eeldusest,
#' tekkiv kogutoodang tuleb valmis teha ühe kuu jooksul, mis tähendab, et vaja
#' on 12 korda rohkem töötajaid kui olukorras, kus tekkiv kogutoodang tuleb toota
#' aasta jooksul.  
#' @param data_i Maksumooduli väljundiks olevad andmed   
#' @param time_interval Sisendandmete ajaarvestuse periood. Võib olla: c("kuu", "aasta")
#' @param source_emp Millisest allikast pärit töötajate arvu kasutame (suhestatuna kogutoondagusse) c("EMTA", "ETU")
#' @param year Milline on viimane aasta, mille kohta on töötajate arvu ja kogutoondagu andmed 
#' @returns Sisendandmed, millele on lisatud täiendav, simulatsioonistsenaariumi mõju töötajate arvule 
#' sisaldav andmestik (data.frame formaadis.   
demand_side_impact_on_employment = function(data_i = impact_on_tax_revenue, time_interval, source_emp = "EMTA", year = 2020) {
  # Üldine lähenemine:
  # - Loeme sisse sisendandmed, mis sisaldavad muuhulgas kogutoodangu muutust
  # - Loeme sisse töötajate arvu ja kogutoodangu suhte (tegevusalade lõikes) 
  # - Korrutame kogutoodangu muutuse töötajate arvu ja kogutoodangu suhtega
  # - Kui sisendandmed on aegrida ning ajaarvestus on kuine, siis realiseerub igas tulbas toodud muudatuste
  # mõju lõpptarbimisele ühe kuu (mitte aasta) jooksul. Seega on vaja 12 korda rohkem töötajaid.
  # Seetõttu korrutatakse kuise arvestusperioodi puhul loodavate töökohtade arv 12-ga
  
  
  # Saadame sisendandmed väljundisse
  result = data_i
  
  # Anname arvestusperioodi pikkuse koefitsiendile õige väärtuse 
  if(time_interval=="kuu") {
    time_interval_coef = 12
  } else {
    time_interval_coef = 1
  } 
  
  # Loeme sisse töötajate arvu kogutoodangu ühiku kohta (see on väljendatud kujul: ühe töötaja tööaastat kogutoodangu ühiku kohta )
  fname= paste0("IO_extensions_", source_emp, ".Rda")
  load(file.path(data_loc, fname))
  employment_to_TP = get(paste0("employment_to_TP_", year))
  
  
  # Summeerime kõik kogutoodangu muutused
  total_production = data_i$total_production$d_household_FD+
    data_i$total_production$d_investments_FD+
    data_i$total_production$d_govexp_FD+
    data_i$total_production$d_exports_FD
  
  # Korrutame kogutoodangu muutuse läbi töötajate arvu suhtega kogutoodangusse ning arvestusperioodi koefitsiendiga
  result[["employment"]] = total_production*employment_to_TP*time_interval_coef
  
  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
# impact_of_prices_on_FD = impact_of_prices_on_FD_f(data_i = impact_of_taxes_on_prices, year = 2020, IO_year = 2017, time_interval = "kuu", elasticity = "miinusyks")
# demand_side_TP = demand_side_impact_on_TP_f(data_prices_i = impact_of_prices_on_FD, data_i = input_data, change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused, mult_type_i = "I", hh_savings_rate_i = 0)
# supply_side_impact_TP = supply_side_impact_on_TP_f(data_i = input_data, time_interval = "aasta")
# impact_on_VA_LC_PROF = impact_on_VA_LC_PROF_f(data_i = demand_side_TP, data_i_supply = supply_side_impact_TP)
# impact_on_tax_revenue = impact_on_tax_revenue_f(data_i=impact_on_VA_LC_PROF, year=2020, change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused, time_interval = "aasta")

