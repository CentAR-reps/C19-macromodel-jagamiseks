#' Pakkumispoolse kogutoodangu muutuse moodul 
#' 
#' Moodul arvutab pakkimispoolsete muutuse (mis käesolevas mudelis on 
#' töötajate arvu muutus) mõju kogutoodangule, korrutades tegevusalade lõikes 
#' töötajate arvu muutuse läbi ühe töötaja poolt loodud kogutoodanguga.
#' @param data_i Andmesisestusmooduli väljund  
#' @param time_interval Sisendandmete ajaarvestuse periood. Võib olla: c("kuu", "aasta")
#' @param source_emp Millisest allikast pärit töötajate arvu kasutame (suhestatuna kogutoondagusse) c("EMTA", "ETU")
#' @param year Milline on viimane aasta, mille kohta on töötajate arvu ja kogutoondagu andmed 
#' @returns Andmestik data.frame formaadis, mis sisaldab kogutoodangu muutust tegevusalade lõikes
supply_side_impact_on_TP_f = function(data_i, time_interval, source_emp = "EMTA", year = 2020) {
  # Üldine lähenemine:
  # - Loeme  andmesisestusmooduli väljundist sisse töötajate arvu mooduli 
  # - Loeme sisse töötajate arvu ja kogutoodangu suhte (tegevusalade lõikes)
  # - Jagame töötajate arvu muutuse läbi töötajate arvu ja kogutoodangu suhtega
  # - Kui töötajate arvu muutus on muutus kuises töötajate arvus (time_interval == "kuu"),
  # siis jagame kogutoodangu kasvu 12ga, sest töötajate arvu ja kogutoodangu suhe on leitud
  # aastasel baasil, ehk väljendab seda, kui palju 1 töötaja tööaastaid ühe ühiku
  # kogutoodangu tootmiseks kulutatakse.
  
  # Muutus töötajate arvus
  change_in_empl= data_i$d_empl 
  
  # Anname arvestusperioodi pikkuse koefitsiendile õige väärtuse 
  if(time_interval=="kuu") {
    time_interval_coef = 12
  } else {
    time_interval_coef = 1
  } 
  
  # Muutus kogutoodangus
  fname= paste0("IO_extensions_", source_emp, ".Rda")
  load(file.path(data_loc, fname))
  employment_to_TP = get(paste0("employment_to_TP_", year))
  change_in_TP = (change_in_empl / as.numeric(employment_to_TP)) / time_interval_coef   
  change_in_TP[is.na(change_in_TP)] <- 0 # Nad tekivad nulliga jagamise tagajärjel, antud juhul on õige need nulliga asendada.
  
  # Kirjutame töötajate arvu muutusest tuleneva lõpptarbimise muutuse kodumajapidamiste reale. Kuna multiplikatiivseid mõjusid
  # pakkumisepoolsete efektide arvutamisel arvesse ei võeta, siis pole vahet, millise lõpptarbimise komponendiga tegemist on, 
  # sama hästi võiks see kogutoodangu muutus olla tingitud ka nt valitsussektori lõpptarbimise muutusest.
  
  result = data_i
  result$d_household_FD = change_in_TP
  
  # Kui mõnele teisele sisendi lehele on eksikombel midagi kirjutautd, siis kirjutame selle nulliks - ei luba modelleerida samaaegselt
  # nii töötajate arvu langust (ilma multiplikatiivsete efektideta) kui 
  
  result$d_investments_FD = result$d_investments_FD*0
  result$d_govexp_FD = result$d_govexp_FD*0
  result$d_exports_FD = result$d_exports_FD*0
  
  # Ebavajalikud lehed kaotame ära 
  result$d_empl = NULL
  result$d_VAT_20 = NULL
  result$d_VAT_9 = NULL
  result$d_ST = NULL
  result$d_UIC = NULL
  
  # Kirjutame üles ka lõpptarbimise impordisisalduse, mis antud juhul on null 
  result[["FD_of_imports"]] = result
  result$FD_imports$d_household_FD = result$FD_imports$d_household_FD*0

  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# supply_side_impact_TP = supply_side_impact_on_TP_f(data_i = input_data, time_interval = "aasta", source_emp = "ETU", year = 2020)
