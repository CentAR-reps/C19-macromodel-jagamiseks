#' Lõpptarbimise moodul
#'   
#' Moodul leiab maksumuudatuste tulemusena toimuva hindade muutuse
#' mõju lõpptarbimisele. Hinnamuutused mõjutavad selles mudelis
#' ainult kodumajapidamiste lõpptarbimist. Valitsussektori kulutusi,
#' investeeringuid ja eksporti hinnatõus meie mudelis ei mõjuta.
#' @param data_i Hinnamooduli väljundiks olev kahetasandiline list, 
#' mis sisaldab andmeid hinnamuudatuste ja maksumuudatuste kohta.
#' @param year Aasta, mille kohta on mudelisse sisestatud viimased andmed
#' kogutoodangu ja lisandväärtuse sektoraalse jaotuse kohta.
#' @param IO_year Aasta, mille sisend-väljundtabeleid kasutatakse
#' @param time_interval Ajaühik, mille lõikes on andmed andmesisestusmooduli tabelitesse esitatud. On kaks valikut: c("kuu", "aasta")
#' @param elasticity Simulatsiooni kasutatavad nõudluse hinnaelastsused. On kolm valikut c("null", "miinusyks", "ekspert"). Esimesel juhul eeldatakse
#' et kõigi tegevusalade toodete/teenuste hinnaelastsused on nullid, teisel juhul miinus ühed ning kolmandal juhul võrdsed kirjanduse analüüsi
#' põhjal kokku koondatud väärtustega. Elastsused ise on andmete kataloogis, failis "elasticities.xlsx"   
#' @returns Ühe elemendiga list, mis sisaldab data.frame formaadis andmestikku kodumajapidamiste lõpptarbimise absoluutse muutuse kohta.
impact_of_prices_on_FD_f = function(data_i, year = 2020, IO_year = 2017, time_interval, elasticity) {
  # Üldine lähenemine:
  # - Loeme sisse elastsused ja valime neist sobivad (nullid, miinusühed või eksperthinnangu põhised)
  # - Kuna kõik hinnatõusud on väljendatuna suhtena kogutoodangusse, siis liidame need kokku (NB! praegu nende multiplikatiivsust ei arvesta)  
  # - Leiame elastsuste ja hinnatõusude korrutise tulemusena protsentuaalse kodumajapidamiste lõpptarbimise muutuse majandustegevusalade lõikes
  # - Loeme sisendväljundtabelitest sisse kodumajapidamiste absoluutse lõpptarbimise (tegevusalade lõikes), viime selle parameetris "year" näidatud aasta vääringusse
  # (kasutades selleks lisandväärtuse nominaalkasvu) ning, juhul kui sisendandmed on kuised, jagame saadud tulemuse 12ga.
  # - Korrutame kohandatud kodumajapidamiste lõpptarbimise kulutused läbi lõpptarbimise protsentuaalse muutusega ja saame lõpptarbimise
  # absoluutse muutuse
  
  # Loeme elastsused sisse  
  elasticities = read_excel(file.path(data_loc, "elasticities.xlsx"), sheet = "hinnaelastsused") 
  rownames_el = elasticities$`Tegevusala lühend`
  
  elasticities = elasticities %>%
    .[, elasticity] %>%
    as.matrix()
  
  rownames(elasticities) = rownames_el
  
  colnames(elasticities) = "Hinnaelastsused"
  
  # Leiame suhelise mõju lõpptarbimisele (%)
    # Esmalt kontrollime, kas andmetesse pole sisestatud samaaegselt mitme maksumäära muudatusi
    stsenaariumide_arv = length(data_i$Hinnamuutused$`Käibemaks`)
    
    for(i in 1:stsenaariumide_arv){
      KM_test = ("TRUE" %in% (data_i$Hinnamuutused$`Käibemaks`[[i]] != 0)) 
      SM_test = ("TRUE" %in% (data_i$Hinnamuutused$Sotsiaalmaks[[i]] != 0)) 
      TKM_test = ("TRUE" %in% (data_i$Hinnamuutused$Töötuskindlustusmaksed[[i]] != 0)) 
      
      test= c(KM_test, SM_test, TKM_test)
      if(sum(test == "TRUE")>1) {
        stop("Mitme maksu üheaegset muutmist simuleerida ei saa! Palun muuda stsenaariumi kirjeldust!")
      }
    }

    # Loeme sisse käibemaksu efektiivse nmäära ja
    # Viime käibemaksu arvutamiseks vajalikud komponendid mugavamale kujule
    load(file.path(data_loc, "effective_VAT_components.Rda"))
    eval(parse(text = paste0("effective_VAT_components_by_sector = effective_VAT_components_by_sector_", year)))
    VAT_components_abi = effective_VAT_components_by_sector %>%
      as.matrix() %>%
      t() %>%
      as.data.table()
    
    # Kuna lubame muuta ainult ühte maksu, siis võime käibemaksu ja tööjõumaksude hinnatõusud kokku liita (kui ühte maksu muudad, siis teiste hinnamuutused on alati nullid) 
    change_in_prices = data_i$Hinnamuutused$`Käibemaks` + 
      (data_i$Hinnamuutused$Sotsiaalmaks + data_i$Hinnamuutused$`Töötuskindlustusmaksed`)*(VAT_components_abi$`Efektiivne KM - keskmine` + 1)
  
    
    `Relative impact on FD` =  change_in_prices * as.numeric(elasticities)
  
  # Absoluutne mõju lõpptarbimisele
  # Absoluutne mõju lõpptarbimisele leitakse läbi lõpptarbimise korrutamise selle protsentuaalse kasvuga. 
  # Kuna sisendväljundtabelite koostamise aasta ja aasta, mille kohta on teada viimased andmed kogutoodangu ja lisandväärtuse kohta
  # (ehk siis aasta, mille väärtingus toimus lõpptarbimise kasv) ei lange kokku, siis tuleb sisendväljundtabelitest 
  # pärit lõpptarbimise näita viia nominaalse lisandväärtuse kasvu abil samale tasemele
  
  load(file.path(data_loc, "tax_base.Rda"))
  tax_base_IO = get(paste0("tax_base_", IO_year)) %>%
    rowSums() %>%
    .["Lisandväärtus"]
  
  tax_base_current = get(paste0("tax_base_", year)) %>%
    rowSums() %>%
    .["Lisandväärtus"]
  
  nominal_growth_of_VA = tax_base_current/tax_base_IO
  
  # Loeme sisse lisandväärtuse elemendid
  load(file.path(data_loc, "ibi_IO_elements.Rda"))
  household_consumption_of_domestic_goods_current = Fd %>%
    .[,"Kodumajapidamiste lõpptarbimiskulutused"] * nominal_growth_of_VA
  
  # Kui tabelis on kuised andmed, siis jagame lõpptarbimise kulutused 12ga 
  if(time_interval=="kuu") {
    household_consumption_of_domestic_goods_current = 
      household_consumption_of_domestic_goods_current/12
  }
  
  # Mõju (kodumajapidamiste) lõpptarbimisele (absoluutne)
  d_household_FD = (`Relative impact on FD` * household_consumption_of_domestic_goods_current) %>%
    as.data.frame()
  
  result = list("d_household_FD" = d_household_FD)
  
  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
# impact_of_prices_on_FD = impact_of_prices_on_FD_f(data_i = impact_of_taxes_on_prices, year = 2020, IO_year = 2017, time_interval = "kuu", elasticity = "miinusyks")
