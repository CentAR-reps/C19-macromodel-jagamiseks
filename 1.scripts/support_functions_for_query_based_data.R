#' Prepare custom query data from Statistics Estonia on total production and value added 
#'
#' The data comes from custom  query  to Rober Müürsepp (Statistics Estonia), file name is  
#' 'Toodang ja lisandväärtus_2017-2020.xlsx'. Function shapes it into format used by 
#' model. It is likely that this function needs to be modified each time the model 
#' is updated as there is no guarantee that data will arrive
#' in the same format. This script is only useful for UPDATING the model!
#' @export tax_base.Rda A file that includes both old data + added new years
#' on total production and components of value added, in matrix form
#' @export tax_base_new.Rda A file that includes new data in matrix form on  
#' total production and components of value added
prep_raw_data_TP_VA =  function() {
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  test_loc <<- file.path(wd, "9.testground")
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")

  # Choose file
  cat("Choose the file that includes most recient information on total production
      and value added. It should be in the same format as 
      file 'Toodang ja lisandväärtus_2017-2020.xlsx', see '2.data/custom_query_raw_data':")
  data_path = file.choose() %>%
    enc2native()
    
  years = readline(prompt = "List the years (see sheet names in 'Toodang ja lisandväärtus_2017-2020.xlsx') the data should be prepared for, eg 'c(2017, 2018, 2019, 2020)'. NB! Sheet names must be named by year!:") %>%
    parse(text = .) %>%
    eval()
    
  # Source dictionary that will be used as a bridge
  dict = read_excel(file.path(dict_loc, "dictionaries.xlsx")) %>%
    select(`Kood EMTAK2008`, `Kood IO tabel`, `Tegevusala tekst IO tabel`)
    
  # Test if all sheets are there
  for (i in years) {
    test = try(read_excel(data_path, sheet = paste0(i)))
    if("try-error" %in% class(test)) {
      cat("Sheet names do not mach the years you entered!")
    } 
  }
  
  # NB! All years must be formatted  
  for(i in years){
    data_abi = read_excel(data_path, sheet = paste0(i)) %>%
      merge(., dict, by.x = "...3", by.y ="Kood EMTAK2008")
    
    # To get the name encoding right
    names(data_abi) = names(data_abi) %>%
      enc2native()

    data_abi = data_abi %>%
      as.data.table() %>%
      .[, .(Toodang = sum(as.numeric(Toodang), na.rm = T), 
            `Lisandväärtus` = sum(as.numeric(`Lisandväärtus`), na.rm = T),
            `Hüvitised töötajatele`= sum(as.numeric(Palk), na.rm = T) + sum(as.numeric(`Sotsiaal- maks`), na.rm = T),               
            `..palk` = sum(as.numeric(Palk), na.rm = T),
            `Muud neto-tootmismaksud` = sum(as.numeric(`Muud neto-tootmismaksud`), na.rm = T),
            `Põhivara kulum` = sum(as.numeric(`Põhivara kulum`), na.rm = T),
            `Tegevuse ülejääk ja segatulu, neto` = sum(as.numeric(`Tegevuse ülejääk, segatulu`), na.rm = T)), .(`Tegevusala tekst IO tabel`)] %>%
      as.data.frame() # data.table does not support long row names
    
    rownames(data_abi) = data_abi$`Tegevusala tekst IO tabel`
    data_abi$`Tegevusala tekst IO tabel`=NULL
    data_abi = data_abi %>%
      t() %>%
      `*`(1000000) # Source data was in millions
    
    eval(parse(text = paste0("tax_base_", i, " <- data_abi")))  
  }
  
  filenames_abi = paste0("tax_base_", as.character(years))
  filenames = filenames_abi %>%   
    paste(., collapse = ",")
  
  # Save a copy of new data - for testing purposes
  eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'tax_base_new.Rda'))")))
  
  # Check the content of old 'tax_base.Rda'
  # Get data names from 'tax_base.Rda'
  env1 = new.env()
  old_names = load(file = file.path(data_loc, 'tax_base.Rda'), envir = env1)
  rm(env1)
  
  load(file = file.path(data_loc, 'tax_base.Rda')) # load old data and overwrite the new
  new_names = filenames_abi[!(filenames_abi %in% old_names)]
  if(length(new_names)!=0) {
    filenames_abi1 = paste(old_names, collapse= ",") 
    filenames_abi2 = paste(new_names, collapse = ",")
    filenames = paste0(filenames_abi1,",", filenames_abi2)
    
    # Save new version of combined old and new data
    eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'tax_base.Rda'))")))
  }
  
  cat("Data is ready. Usually two files are created ('tax_base_new.Rda', 'tax_base.Rda') 
      and saved in folder '9.testground'. The first one contains only the data 
      in the .xlsx files you used with this script.
      The second is created only if there were some additional years the new data (when comapred to old 'tax_base.Rda').
      If there were some new years, then new 'tax_base.Rda' is a combinatsion of old 'tax_base.Rda' and
      data from the xlsx files used with this script. ONLY NEW YEARS are added, nothing is 
      overwritten. If, of example, you wanted to add only one additional year of data, then 
      recheck the content of 'tax_base.Rda' in the folder '9. testground' and if 
      everything is ok, move the file into folder '2.data'.
          
      If you wanted to do something else (e.g.overwrite some years in the old 'tax_base.Rda') 
      you have to it manually by combinding the old 'tax_base.Rda' and 'tax_base_new.Rda' 
      as you see fit. NB! In the end, by moving 'tax_base.Rda' into '2.data' 
      YOU WILL OVERWRITE THE OLD DATA. Make sure that you have all the 
      data you need from the old file!")
}

#prep_raw_data_TP_VA()


#' Prepare custom query data from Statistics Estonia on Labour Force Survey employment by sector 
#'
#' The comes from custom query (Statistics Estonia), file name is
#' 'Toodang ja lisandväärtus_2017-2020.xlsx'. Function shapes it into format used by 
#' the model. The following modifications are made - missing data (by sector) is filled. 
#' As total employment is known the difference between total employment and sum over sector
#' is divided by number of sectors with missing data (except household own production) and 
#' this is used as substitute for missing values. Secondly there are some persons working in
#' sectors that are not known. We find the share of these people in overall employment
#' and inclrease by this fraction the employment in each industry.  
#' It is likely that this function needs to be modified each time the model 
#' is updated as there is no guarantee that data will arrive
#' in the same format. This script is only useful for UPDATING the model!
#' @export employment_ETU.Rda A file that includes both old data + added new years
#' on Labour force survey employment by sector, in matrix form
#' @export employment_ETUnew.Rda A file that includes new data in matrix form on  
#' Labour force survey employment by sector.
prep_raw_data_ETU_emp =  function() {
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  test_loc <<- file.path(wd, "9.testground")
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
  
  # Choose file
  cat("Choose the file that includes most recient information on LFS  employment. It should be in the same format as 
      file 'Hõivatute arv - Centari tellimus.xlsx', see '2.data/custom_query_raw_data':")
  data_path = file.choose() %>%
    enc2native()
  
  years = readline(prompt = "List the years (see col names in 'Hõivatute arv - Centari tellimus.xlsx') the data should be prepared for, eg 'c(2015, 2017)':") %>%
    parse(text = .) %>%
    eval()
  
  # Source dictionary that will be used as a bridge
  dict = read_excel(file.path(dict_loc, "dictionaries.xlsx")) %>%
    select(`Kood EMTAK2008`, `Kood IO tabel`, `Tegevusala tekst IO tabel`)
  
  data_abi = read_excel(data_path, skip = 2, na = "..", trim_ws = T) %>%
    as.data.table()
  
  # Test if all sheets are there
  for (i in years) {
    test = try(data_abi[, .(`Tegevusala grupp`, get(as.character(i)))])
    if("try-error" %in% class(test)) {
      cat("No such year in xlsx file you selected!")
    } 
  }

  
  for(i in years) {
    data_abi2 =  data_abi %>%
      .[, .(`Tegevusala grupp`, get(as.character(i)))]
    
    # Missing data
      # We will find the total employment in sectors with missing data (Grand total - sum of all sectors) and 
      # Divide it by number of sectors with missing data - 1 (will exclude households as producers)
      # The result is substituted for all sectors with missing data.   
    
      cases_missing_data = data_abi2 %>%
        .[`Tegevusala grupp` != "Tegevusala teadmata" & `Tegevusala grupp` != "Grand Total"] %>%
        .[is.na(V2), .N]-1 # Will not extrapolate to households
      
      sum_grand_total = data_abi2 %>%
        .[`Tegevusala grupp` == "Grand Total", V2]
      
      sum_total = data_abi2 %>%
        .[`Tegevusala grupp` != "Grand Total", sum(V2, na.rm = T)]
      
      substitution_for_missing = (sum_grand_total - sum_total) / cases_missing_data
      
      data_abi2 = data_abi2 %>%
        .[is.na(V2), V2:=substitution_for_missing] %>%
        .[`Tegevusala grupp` == "Kodumajapidamised  tööandjana; oma tarbeks mõeldud kaupade tootmine ja teenuste osutamine", V2:=0]
      
    # Sector not known
    # We will find the ratio of persons whose employment sector is unknown in Grand total and multiply the employment of 
    # each sector with this coefficient+1
    # Finally - all employment figures are multiplied by thousand (as source data is in thousands)
      
      sum_not_kown = data_abi2 %>%
        .[`Tegevusala grupp` == "Tegevusala teadmata", V2]
      
      correction_coef = sum_not_kown/sum_grand_total + 1 
      
      data_abi2 = data_abi2 %>%
        .[`Tegevusala grupp` != "Tegevusala teadmata" & `Tegevusala grupp` != "Grand Total", V2:=V2*correction_coef]
      
      data_abi2 = data_abi2 %>%
        .[`Tegevusala grupp` != "Tegevusala teadmata" & `Tegevusala grupp` != "Grand Total"] %>%
        .[, V2:=round(V2*1000, 0)]
        
      names_col = data_abi2[, `Tegevusala grupp`]
      
      data_abi2 = data_abi2 %>%
        .[, .(V2)] %>%
        t()
      
      colnames(data_abi2) = names_col
      rownames(data_abi2) = "Tööhõive - ETU"
      
      eval(parse(text = paste0("employment_ETU_", i, " <- data_abi2")))  
  }
    

  filenames_abi = paste0("employment_ETU_", as.character(years))
  filenames = filenames_abi %>%   
    paste(., collapse = ",")
  
  # Save a copy of new data - for testing purposes
  eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'employment_ETU_new.Rda'))")))
  
  # Check the content of old 'employment_ETU.Rda'
  # Get data obj. names
  env1 = new.env()
  old_names = load(file = file.path(data_loc, 'employment_ETU.Rda'), envir = env1)
  rm(env1)
  
  load(file = file.path(data_loc, 'employment_ETU.Rda')) # load old data and overwrite the new
  new_names = filenames_abi[!(filenames_abi %in% old_names)]
  if(length(new_names)!=0) {
    filenames_abi1 = paste(old_names, collapse= ",") 
    filenames_abi2 = paste(new_names, collapse = ",")
    filenames = paste0(filenames_abi1,",", filenames_abi2)
    
    # Save new version of combined old and new data
    eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'employment_ETU.Rda'))")))
  }
  
  cat("Andmed on valmis. Tavaliselt luuakse kaks andmestikku ('employment_ETU_new.Rda', 'employment_ETU.Rda'), 
      mis salvestatakse '9.testground' kataloogi. Esimene fail sisaldab infot nende aastate kohta
      mis skripti sisendiks antud csv/zip failis olemas on. 
      
      Teine tekitatakse vaid siis, kui võrreldes juba kasutuses olevate andmetega oli sisestatud 
      andmetes mõni uus aasta juures. Kui oli, siis on fail 'employment_ETU.Rda' kombinatsioon vanast 
      'employment_ETU.Rda' failist ja uutest andmetest. NB! Uued andmed lisati vaid nende aastate kohta
      mille kohta andmed varem puudusid  - juba olemasolevate aastate andmeid üle ei kirjutatud.
      
      Kui tahtsidki lisada nt ühe aasta andmed juba olemasolevatele andmetele, siis kontrolli igaks juhuks  
      'employment_ETU.Rda' sisu üle ja kui kõik on sobiv, siis kopeeri  fail '2.data' kataloogi.
          
      Kui tahtsid teha midagi muud (nt kirjutada üle varasemate aastate andmed), siis tuleb seda teha käsitsi.
      Saad selleks kombineerida vana 'employment_ETU.Rda' ja 'employment_ETU_new.Rda' failide sisu nii,
      nagu ise vajalikuks pead. NB! Kui kopeerid lõpuks fail 'employment_ETU.Rda' '2.data' kataloogi, 
      siis KIRJUTAD SELLEGA VANAD ANDMED ÜLE. Tee kindlasti enne ka mingi tagavarakoopia!")
  
}

# prep_raw_data_ETU_emp()



#' EMTA töötajate arvu andmete ettevalmistus
#' 
#' Skript arvutab EMTA TöRi järgi kokku tegevusala aasta keskmsied töötajate arvud.
#' Selleks summeritakse iga tegevusala töötajate tööpäevade arv aastas ning
#' jagatakse saadud tulemus 365 päevaga. Sisuliselt on tegemist inimtööaastate
#' arvuga. Kalendripäevade arvu (365) kasutatakse selle tõttu, et TöRis on 
#' kirjas periood, mille jooksul töötaja tööandja juures töötas, see sisaldab
#' ka nädalavahetusi, puhkusi jms. Töötajate hulka loeti kõik TöRis kirjas olevad
#' töötajad va abikaasatasusid saavad isikud ja sotsmaksukohustusega FIEde abikaasad.  
#' 
#' @returns Kaks andmefaili "employment_EMTA_alt.Rda" ja "employment_EMTA.Rda",
#' mõlemad paigutatakse kataloogi "9.testground"
prep_raw_data_EMTA_emp = function() {
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  test_loc <<- file.path(wd, "9.testground")
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
  
  library(data.table)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  # Choose file
  cat("Vali uute EMTA TöR andmete asukoht. Ütlen igaks juhuks kohe ära, et suure tõenäosusega on
      need teises formaadis, kui see skrip eeldab. Andmestik peaks sisaldama järgmisi välju:
      - isiku_anon_ID,
      - TOOANDJA_ANON_ID, 
      - emtak,
      - TOOT_LIIK_KOOD, 
      - ALG_AEG, 
      - LOP_AEG.\n 
      ja olema zipitud csv fail. Projekti raames tehtud andmepäringu vastuseks tulnud EMTA andmestikus
      kandis see fail nime 'TÖR_andmed_perioodidega.zip': \n")
  
  data_path = file.choose() %>%
    enc2native()
  
  fread_command = paste0("unzip -p ", data_path)
  andmed = fread(cmd = fread_command)

  years = readline(prompt = "Mis aastate kohta info uues andmestikus on, nt 'c(2015, 2017)':") %>%
    parse(text = .) %>%
    eval()

  # Töötame kuupäeva andmetega, mis pole Eesti locale põhised, lihtsam on ajutiselt locale ära vahetada. 
  lct <- Sys.getlocale("LC_TIME") 
  Sys.setlocale("LC_TIME", "C")  
  
  # Õige tegevusalade agregatsioon (IO tablei oma)
  dict = read_excel(file.path(dict_loc, "dictionaries.xlsx")) %>%
    .[, c("Kood EMTAK2008", "Kood IO tabel", "Tegevusala tekst IO tabel")] %>%
    as.data.table()
  
  andmed = andmed %>%
    as.data.table() %>%
    .[TOOT_LIIK_KOOD != 15] %>% # Viskame välja abikaasad, kõik muud on hõivatud
    .[TOOT_LIIK_KOOD != 6] %>%
    .[nchar(emtak) < 4, emtak2 := "01"] %>% #EMTAKi koodid kahekohaliseks (eeldame, et alla 4-kohalised koodid on põllumajanduse omad (ehk siis ees olevad nullid on ära kadunud))
    .[nchar(emtak) == 4, emtak2 := paste0("0", substr(emtak, 1,1))] %>%
    .[nchar(emtak) == 5, emtak2 := substr(emtak, 1,2)] %>%
    .[, algus:= as.Date(paste0(as.numeric(substr(ALG_AEG, 1,2)), substr(ALG_AEG, 3,9)),"%d%b%Y")] %>% # Töötamise algus ja lõppaeg
    .[, lopp:= as.Date(paste0(as.numeric(substr(LOP_AEG, 1,2)), substr(LOP_AEG, 3,9)),"%d%b%Y")] %>% 
    .[, .(isiku_anon_ID, emtak2, algus, lopp)] %>%
    merge(., dict, by.x = "emtak2", by.y = "Kood EMTAK2008", all.x = T)
  
  # Kontrollime, kas inimene aast alõpu seisuga töötas või mitte. See tähendab, et meie hõive on defineeritud aasta lõpu seisuga mingis sektoris
  # Alternatiiviks oli arvutada kokku summarsed töökuud aastas ja jagada 12.
  
  # Koopja kuude lõikes arvutuste jaoks
  andmed_baas = copy(andmed)
  
  kuud = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  aastad = years
  
  for(i in 1:length(aastad)) {
    for(j in 1:length(kuud)) {
      aasta = aastad[i]
      kuu = kuud[j]
      
      kontroll_name = paste0("1", kuu, aasta) 
      kontroll = kontroll_name %>% as.Date("%d%b%Y")
      
      # Abivahend, kasutame seda nimede vektorit hiljme, tsükli sees tähendust ei oma
      if(i==1 & j==1) {
        col_sum = kontroll_name
      } else {
        col_sum = c(col_sum, kontroll_name)        
      }

      
      if(kuu %in% c("JAN", "MAR", "MAY", "JUL", "AUG", "OCT", "DEC")) {
        kp = "31"
      } else {
        if(kuu %in% c("APR", "JUN", "SEP", "NOV")) {
          kp = "30"
        } else {
          kp = "28"
        }
      }
       
      kontroll_1 = paste0(kp, kuu, aasta) %>% as.Date("%d%b%Y")
      
      andmed = andmed %>%
        .[((algus <= kontroll | is.na(algus)) & (lopp >= kontroll | is.na(lopp)) & !(is.na(algus) & is.na(lopp))) | # Algas enne vaatlusalust kuud ja lõppes vaatlusalusel kuul  või hiljem
            (algus >= kontroll & algus <= kontroll_1) | # Algas vaatlusalusel kuul
            (lopp >= kontroll & lopp <= kontroll_1 ), # lõppes vaatlusalusel kuul 
          paste0(kontroll_name) := "Jah"]  
    }
  }
  
  # Kasutame funktsiooni nende vaatluste arvude kokku lugemiseks, kus töötamise kohta on kirjas "JAH"
  sum_mod = function(x) {
    result = sum(!is.na(x))
    return(result)
  }
  
  # Konverteerime jahtide arvu üheks (kui inimene töötas mitmes kohas, siis tahame teda ikkagi luged ühena)
  sum_mod1 = function(x) {
    result = sum(x>0)
    return(result)
  }

  andmed2 = andmed %>% 
    .[, lapply(.SD, sum_mod), by=.(isiku_anon_ID, `Kood IO tabel`, `Tegevusala tekst IO tabel`), .SDcols= col_sum] %>%
    .[, lapply(.SD, sum_mod1), by=.(isiku_anon_ID, `Kood IO tabel`, `Tegevusala tekst IO tabel`), .SDcols= col_sum]
  
  andmed2_totals = andmed2 %>%  
  .[, lapply(.SD, sum), .SDcols= col_sum]
  
  share_in_emp = function(x) {
    result = x/sum(x)
    return(result)
  }
  
  andmed3 = andmed2 %>%
    .[, lapply(.SD, sum), by=.(`Kood IO tabel`, `Tegevusala tekst IO tabel`), .SDcols= col_sum] %>%
    .[!is.na(`Kood IO tabel`)] %>%
    .[, (col_sum) := lapply(.SD, share_in_emp), .SDcols= col_sum]
  
  for(i in 1:length(col_sum)) {
    abi_sum = andmed2_totals[[i]]
    abi_name = col_sum[i]
    eval(parse(text = paste0("andmed3$`", abi_name, "` = round(andmed3$`", abi_name, "`*", abi_sum, ", 0)"))) 
  }

  andmed3 = andmed3 %>%
    .[order(`Kood IO tabel`)] %>%
    melt(., id.vars=c("Kood IO tabel", "Tegevusala tekst IO tabel")) %>%
    .[, Aasta := substr(variable, 5, 8)] %>%
    .[, .(Value = round(mean(value))), by=.(`Kood IO tabel`, `Tegevusala tekst IO tabel`, Aasta)]

  sektorid = dict$`Tegevusala tekst IO tabel` %>% unique()

  # Viime iga aasta hõive andmed reavektori kujule, kirjutame tegevusalad tulbanimedesse ja salvestame iga aasta eraldi andmeobjektina
  for(i in 2015:2020) {
    abi = andmed3[Aasta==i, Value] %>%
      t() %>%
      as.matrix()
    colnames(abi) = andmed3[Aasta==i, `Tegevusala tekst IO tabel`]
    rownames(abi) = "Tööhõive_EMTA"
    
    eval(parse(text = paste0(paste0("employment_EMTA_", i, " = abi"))))
  }
  rm(abi, dict, andmed3, andmed2)
  
  # Salvestame tulemused - tegemist on ainult taustainfoga, mida mudel ei kasuta, seega paneme selle praegu testimise kataloogi 
  save(employment_EMTA_2015, employment_EMTA_2016, employment_EMTA_2017, employment_EMTA_2018, employment_EMTA_2019, employment_EMTA_2020, 
       file = file.path(test_loc, "employment_EMTA_alt.Rda"))
  rm(employment_EMTA_2015, employment_EMTA_2016, employment_EMTA_2017, employment_EMTA_2018, employment_EMTA_2019, employment_EMTA_2020)
  

  # Sama arvutus, aga kuude põhiselt - see on arvutus, mis läheb mudelisse
  andmed = copy(andmed_baas)
  
  for(i in 2015:2020) {
    aasta_algus = paste0("01JAN", i) %>% as.Date("%d%b%Y")
    aasta_lopp = paste0("31DEC", i) %>% as.Date("%d%b%Y")
    
    # Enamasti on tööaasta pikkus ca 253-255 tööpäeva, kuid kuna TöRis on 
    # perioodid, mis sisaldavad ka kalendripäevi, siis kasutame aasta pikkusena
    # ikkagi kalendripäevade arvu (365).
    
    andmed = andmed %>%
      .[algus < aasta_algus & (lopp > aasta_lopp | is.na(lopp)), paste0("A",i) := 365]%>%
      .[algus < aasta_algus & lopp > aasta_algus & lopp < aasta_lopp, paste0("A",i) := yday(lopp)] %>%
      .[algus > aasta_algus & algus < aasta_lopp & (lopp > aasta_lopp | is.na(lopp)) , paste0("A",i) := (365 - yday(algus))] %>%
      .[algus > aasta_algus & lopp < aasta_lopp, paste0("A",i) := (yday(lopp)-yday(algus))]
  }
  
  andmed = andmed %>%
    .[, .(A2015 = round(sum(A2015/365, na.rm=T), 0),
          A2016 = round(sum(A2016/365, na.rm=T), 0),
          A2017 = round(sum(A2017/365, na.rm=T), 0),
          A2018 = round(sum(A2018/365, na.rm=T), 0),
          A2019 = round(sum(A2019/365, na.rm=T), 0),
          A2020 = round(sum(A2020/365, na.rm=T), 0)), by=.(`Kood IO tabel`, `Tegevusala tekst IO tabel`)] %>%
    .[order(`Kood IO tabel`)] %>%
    .[!is.na(`Kood IO tabel`)] # Puuduva emtakiga vaatlused (ja need koodid, mida olemas pole, viskame välja. Kaotame ca 3000 vaatlust aastas)
  
  
  # Viime iga aasta hõive andmed reavektori kujule, kirjutame tegevusalad tulbanimedesse ja salvestame iga aasta eraldi andmeobjektina
  for(i in 2015:2020) {
    muutuja = paste0("A", i)
    abi = andmed[, muutuja, with = F] %>%
      t() %>%
      as.matrix()
    colnames(abi) = andmed$`Tegevusala tekst IO tabel`
    rownames(abi) = "Tööhõive_EMTA"
    
    eval(parse(text = paste0(paste0("employment_EMTA_", i, " = abi"))))
  }
  rm(abi, andmed)
  
  
  
  # Salvestame tulemused
  filenames_abi = paste0("employment_EMTA_", as.character(years))
  filenames = filenames_abi %>%   
    paste(., collapse = ",")
  
  # Save a copy of new data - for testing purposes
  eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'employment_EMTA_new.Rda'))")))
  
  # Check the content of old 'employment_ETU.Rda'
  # Get data obj. names
  env1 = new.env()
  old_names = load(file = file.path(data_loc, 'employment_EMTA.Rda'), envir = env1)
  rm(env1)
  
  load(file = file.path(data_loc, 'employment_EMTA.Rda')) # load old data and overwrite the new
  new_names = filenames_abi[!(filenames_abi %in% old_names)]
  if(length(new_names)!=0) {
    filenames_abi1 = paste(old_names, collapse= ",") 
    filenames_abi2 = paste(new_names, collapse = ",")
    filenames = paste0(filenames_abi1,",", filenames_abi2)
    
    # Save new version of combined old and new data
    eval(parse(text = paste0("save(", filenames, ", file = file.path(test_loc, 'employment_EMTA.Rda'))")))
  }
  
  cat("Andmed on valmis. Tavaliselt luuakse kaks andmestikku ('employment_EMTA_new.Rda', 'employment_EMTA.Rda'), 
      mis salvestatakse '9.testground' kataloogi. Esimene fail sisaldab infot nende aastate kohta
      mis skripti sisendiks antud csv/zip failis olemas on. 
      
      Teine tekitatakse vaid siis, kui võrreldes juba kasutuses olevate andmetega oli sisestatud 
      andmetes mõni uus aasta juures. Kui oli, siis on fail 'employment_EMTA.Rda' kombinatsioon vanast 
      'employment_EMTA.Rda' failist ja uutest andmetest. NB! Uued andmed lisati vaid nende aastate kohta
      mille kohta andmed varem puudusid  - juba olemasolevate aastate andmeid üle ei kirjutatud.
      
      Kui tahtsidki lisada nt ühe aasta andmed juba olemasolevatele andmetele, siis kontrolli igaks juhuks  
      'employment_EMTA.Rda' sisu üle ja kui kõik on sobiv, siis kopeeri  fail '2.data' kataloogi.
          
      Kui tahtsid teha midagi muud (nt kirjutada üle varasemate aastate andmed), siis tuleb seda teha käsitsi.
      Saad selleks kombineerida vana 'employment_EMTA.Rda' ja 'employment_EMTA_new.Rda' failide sisu nii,
      nagu ise vajalikuks pead. NB! Kui kopeerid lõpuks fail 'employment_EMTA.Rda' '2.data' kataloogi, 
      siis KIRJUTAD SELLEGA VANAD ANDMED ÜLE. Tee kindlasti enne ka mingi tagavarakoopia!\n")
  
  Sys.setlocale("LC_TIME", lct)
  
}

#prep_raw_data_EMTA_emp()
