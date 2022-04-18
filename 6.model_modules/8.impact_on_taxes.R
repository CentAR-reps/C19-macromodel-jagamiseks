#' Maksumoodul
#' 
#' Moodul võtab sisendiks lisandväärtuse mooduli väljundiks olevad 
#' muutused lisandväärtuses, selle komponentides ning impordi
#' lõpptarbimises ning arvutab nende põhjal mõju maksulaekumistele
#' (maksuliikide lõikes). Arvutused põhinevad efektiivsetel maksumääradel.
#' Kui muudetakse maksumäärasid (võimalikud on muutused käibemaksu, 
#' sotsiaalmaksu ja töötuskindlustusmaksete määrades), siis leitakse 
#' siis võetakse arvesse nii maksumäärade mõju lõpptarbimisele (läbi 
#' hinnamuutuste) ning sellest tulenevalt ka maksulaekumisele kui
#' puhtalt nominaalsete maksumäärade muutusest tulenevat mõju 
#' maksulaekumisele.    
#' @param data_i Lisandväärtuse mooduli väljund, ehk viiest elemendist 
#' koosnev list, mille neli elementi sisaldavad infot õppkasutamise muutuse 
#' mõjust kogutoodangule, lisandväärtusele, tööjõukulule ja kasumitele ning
#' viies element impordi lõpptarbimist. Iga element sisaldab omakorda neljast
#' elemendist koosnevat listi, mis eristavad kodumajapidamiste
#' lõpptarbimisest, investeeringutest, valitsuse kulutustest ning
#' ekspordist johtuvaid mõjusid. Listi elemendid on data.frame 
#' formaadis.
#' @param year Viimane aasta, mille kohta on olemas andmed kogutoodangu kohta
#' @param change_in_tax_rate Hinnamooduli väljundiks olevad maksumuudatuste indeksid.
#' Andmed on data.frame formaadis.
#' @param time_interval Ajaühik, mille lõikes on andmed andmesisestusmooduli 
#' tabelitesse esitatud. On kaks valikut: c("kuu", "aasta")
#' @returns Kuue-elemendiline list, mille viis esimest elementi on identsed
#' lisandväärtuse mooduli väljundiga. Kuues element on data.frame formaadis andmestik,
#' mis sisaldab maksulaekumise infot maksuliikide lõikes.  
impact_on_tax_revenue_f = function (data_i, year=2020, change_in_tax_rate, time_interval){
  # Üldine lähenemine
  # - Saadame lisandväärtuse mooduli väljundi muutmata kujul maksumoodulist läbi
  # - Seome maksud nende maksubaasiga (lisandväärtuse komponendid)
  # - Agregeerime maksubaasid nende allikate (erinevad lõpptarbimise komponendid) üleselt
  # (erandiks on käibemaks, kus peame eristama kodumaise lõpptarbimise poolt loodud
  # lisandväärtust (ignoreerides eksporti) ja impordi kodumaist tarbimist kui käibemaksu
  # baasi)
  # - Mõju maksutulule leiame, korrutades maksubaasi efektiivse maksumääraga
  # - Kui muudetakse maksumäärasid, siis selle mõju maksulaekumisele avaldub läbi kahe kanali:
  # -- Maksumuutus muudab hindu, mis omakorda muudab lõpptarbimist, lisandväärtust ja teisi ka maksubaase.
  # -- Maksumäära muutus muudab maksutulu ka vahetult läbi maksumäära suurenemisele - kogu majanduses
  # rakendatakse nüüd teistsugust maksumäära. Selle arvesse võtmiseks korrutatakse aastane maksutulu
  # läbi nominaale maksumäära muutuse indeksiga (eeldame, et efektiivne maksumäär muutub sama palju).
  # NB! Kui stsenaarium on defineeritud kuise ajaintervalliga, siis peame maksumäära muutusest tuleneva 
  # maksutulu arvutamisel jagama aastase maksutulu 12ga ja alles siis korrutama selle läbi 
  # nominaalse maksumäära kasvu indeksiga.
  # -- Kogumõju maksutulule on nende kahe mõju summa.
  

  # Lisandväärtuse mooduli väljund liigub muutmata kujul ka maksumoooduli väljundiks.
  result = data_i
  
  # Loeme sisse efektiivsed maksumäärda
  load(file.path(data_loc, "effective_tax_rates.Rda"))
  effective_taxes_by_sector = get(paste0("effective_taxes_by_sector_", as.character(year)))
  tax_names = rownames(effective_taxes_by_sector)
  
  # Koostame tabeli, mis seob omavahel maksud ja maksubaasid.
  # NB! Kui teed siin muudatusi, siis vaata igaks juhuks üle ka funktsioon 
  # effective_tax_rates_f() in IO_functions.R
  # NB! Käibemaksu jaoks on eraldi lahendus
  tax_base_map = matrix(rep("0", times=length(tax_names)*2), ncol=2)
  colnames(tax_base_map) = c("Baas","Base") 
  rownames(tax_base_map) = tax_names
  tax_base_map[1, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[2, "Baas"] = "Lisandväärtus"
  tax_base_map[3, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[4, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[5, "Baas"] = "Toodang"
  tax_base_map[6, "Baas"] = "Lisandväärtus"
  
  tax_base_map[1, "Base"] = "labour_cost"
  tax_base_map[2, "Base"] = "value_added"
  tax_base_map[3, "Base"] = "labour_cost"
  tax_base_map[4, "Base"] = "labour_cost"
  tax_base_map[5, "Base"] = "total_production"
  tax_base_map[6, "Base"] = "value_added"
  
  
  # Maksutulu arvutamiseks puudub üldjuhul vajadus eristada lõpptarbimist selle komponentide lõikes
  
  total_production = data_i$total_production$d_household_FD +
    data_i$total_production$d_investments_FD+
    data_i$total_production$d_govexp_FD+
    data_i$total_production$d_exports_FD
  
  labour_cost = data_i$labour_cost$d_household_FD +
    data_i$labour_cost$d_investments_FD+
    data_i$labour_cost$d_govexp_FD+
    data_i$labour_cost$d_exports_FD
  
  profits = data_i$profits$d_household_FD +
    data_i$profits$d_investments_FD+
    data_i$profits$d_govexp_FD+
    data_i$profits$d_exports_FD
  
  value_added = data_i$value_added$d_household_FD +
    data_i$value_added$d_investments_FD+
    data_i$value_added$d_govexp_FD+
    data_i$value_added$d_exports_FD
  
  # Ainsaks erandiks on käibemaksu baasiks olev lisandväärtus, kus tuleb eristada
  # kodumaise lõpptarbimise muutuse tulemusena aset leidvaid muutusi lisandväärtuses
  # ekspordi poolt põhjustatud muutustest (ekspordi lisandväärtus on maksustatud 0-käibemaksuga)
  
  value_added_VAT = data_i$value_added$d_household_FD+
    data_i$value_added$d_investments_FD+
    data_i$value_added$d_govexp_FD
  
  # Käibemaks laekub ka impordi sisetarbimiselt
  dom_imports_VAT = data_i$`FD_of_imports`$d_household_FD+
    data_i$`FD_of_imports`$d_investments_FD+
    data_i$`FD_of_imports`$d_govexp_FD
  
  
  base_names_eng = c("total_production", "profits", "labour_cost", "value_added")
  
  result_abi2 = list()
  
  load(file.path(data_loc, "tax_base.Rda"))
  tax_base_global = get(paste0("tax_base_", as.character(year)))
  
  
  # Kui ajaühik on kuu, siis tuleb arvestada, et muutus maksumääras mõjutab vaid 1/12 aastasest maksulaekumisest 
  if(time_interval=="kuu") {
    time_interval_coef = 12
  } else {
    time_interval_coef = 1
  } 
  
  # Käime maksubaaside kaupa ...
  for(base_name_eng in base_names_eng) {
    taxes_abi = rownames(tax_base_map)[tax_base_map[, "Base"] == base_name_eng] 
    base_name = tax_base_map[, "Baas"][tax_base_map[, "Base"] == base_name_eng] %>% unique()
    
    # ... läbi kõik maksud, mis nende baaside alla kuuluvad.
    for(tax in taxes_abi) {
      tax_rates = effective_taxes_by_sector[rownames(effective_taxes_by_sector) == tax]
      
      # Maksutulu muutused tulenevalt muutustest lõpptarbimises (muutus maksubaasis)
      tax_revenue_abi = get(base_name_eng) %>%
        `*` (tax_rates)
      
      # Maksud, mille määrades lubame muutusi
      # Sotsiaalmaks
      if(tax == "Sotsiaalmaks") {
        
        # Võtame maksumäära muutuse arvesse projekti otsese mõju arvutamisel
        tax_revenue_abi = tax_revenue_abi %>%
          `*` (change_in_tax_rate$Sotsiaalmaks) 
        
        # Maksumäära muutuse mõju maksulaekumisele laiemalt 
        # Leiame riigi kogu sotsmaksu tulu vanade määrade alusel 
        tax_revenue_abi2_0 = get(base_name_eng) %>% 
          `*` (0) %>%
          `+` (1) %>%
          `*` (tax_base_global[base_name,])%>%
          `*` (tax_rates) 
        
        # ... ja uute määrade korral
        tax_revenue_abi2_1 = tax_revenue_abi2_0 %>% 
          `*` (change_in_tax_rate$Sotsiaalmaks) 
        
        # Lahutame uute maksumäärade alusel tekkivast maksutulust vanade alusel tekkiva tulu ning
        # kohandame vajadusel intervalli pikkusega (kuiste näitajate arvutamisel jagame 12) ning
        # liidame sekkumise otsese mõju
        
        tax_revenue_abi = (tax_revenue_abi2_1) %>%
          `-` (tax_revenue_abi2_0) %>%
          `/` (time_interval_coef) %>%
          `+` (tax_revenue_abi) 
      }
      
      # Töötuskindlustusmaksed
      if(tax == "Töötuskindlustusmaksed") {
        
        # Lähenemine identne sotsiaalmaksu arvutamisega
        
        tax_revenue_abi = tax_revenue_abi %>%
          `*` (change_in_tax_rate$Töötuskindlustusmaksed) 

        tax_revenue_abi2_0 = get(base_name_eng) %>% 
          `*` (0) %>%
          `+` (1) %>%
          `*` (tax_base_global[base_name,]) %>%
          `*` (tax_rates) 
        
        tax_revenue_abi2_1 = tax_revenue_abi2_0 %>% 
          `*` (change_in_tax_rate$Töötuskindlustusmaksed)
        
        tax_revenue_abi = (tax_revenue_abi2_1) %>%
          `-` (tax_revenue_abi2_0) %>%
          `/` (time_interval_coef) %>%
          `+` (tax_revenue_abi) 
      }
      
      result_abi2[[tax]] = tax_revenue_abi
    }
  }
  
  # Käibemaks
  load(file.path(data_loc, "effective_VAT_components.Rda"))
  effective_VAT_components_by_sector = get(paste0("effective_VAT_components_by_sector_", as.character(year)))
  
  VAT_normal_rate = effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Efektiivne KM - tavamäär"]
  VAT_red_rate = effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Efektiivne KM - erimäär"]
  VAT_normal_turnover_share = effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Tavamääraga käibe osakaal kodumaises käibes"]
  VAT_red_turnover_share = effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Erimääraga käibe osakaal kodumaises käibes"]
  
  VAT_global_tax_base = effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Kohalik impordi lõpptarbimine"]+
    effective_VAT_components_by_sector[rownames(effective_VAT_components_by_sector) == "Kohaliku kodumaise toodangu LT lisandväärtus"]
  
  # Käibemaksu tulu muutus kodumaise nõudluse poolt loodud lisandväärtuses ja impordi sisetarbimises toimunud muutusest, ehk muutus maksubaasis
  # Käibemaksu tavamäär
  VAT_revenue_from_change_in_base_normal = (value_added_VAT+dom_imports_VAT) %>%
    `*` (VAT_normal_rate) %>%
    `*` (change_in_tax_rate$`Käibemaks 20%`) %>%
    `*` (VAT_normal_turnover_share) 
  
  # Käibemaks erimäär
  VAT_revenue_from_change_in_base_red = (value_added_VAT+dom_imports_VAT) %>%
    `*` (VAT_red_rate) %>%
    `*` (change_in_tax_rate$`Käibemaks 9%`) %>%
    `*` (VAT_red_turnover_share)
  
  VAT_revenue_from_change_in_base = VAT_revenue_from_change_in_base_normal + 
    VAT_revenue_from_change_in_base_red 
  
  
  # Käibemaksu tulu muutus, mis tuleneb maksumäära muutusest
  # Käibemaks tavamääras
  tax_revenue_0_20 = change_in_tax_rate$`Käibemaks 20%` %>%
    `*`(0) %>%
    `+`(1) %>%
    `*`(VAT_normal_rate) %>%
    `*`(VAT_normal_turnover_share) %>%
    `*`(VAT_global_tax_base) 

  tax_revenue_1_20 = tax_revenue_0_20 %>%
    `*` (change_in_tax_rate$`Käibemaks 20%`) 
  
  VAT_revenue_from_change_in_rate_20 = tax_revenue_1_20 %>%
    `-` (tax_revenue_0_20) %>%
    `/` (time_interval_coef)
  
  # Käibemaks erimääras
  tax_revenue_0_9 = change_in_tax_rate$`Käibemaks 9%` %>%
    `*`(0) %>%
    `+`(1) %>%
    `*`(VAT_red_rate) %>%
    `*`(VAT_red_turnover_share) %>%
    `*`(VAT_global_tax_base)

  tax_revenue_1_9 = tax_revenue_0_9 %>%
    `*` (change_in_tax_rate$`Käibemaks 9%`) 
  
  VAT_revenue_from_change_in_rate_9 = tax_revenue_1_9 %>%
    `-` (tax_revenue_0_9) %>%
    `/` (time_interval_coef)
  
  # Käibemaks - Lõpptarbimise muutuse ja maksumäära muutuse mõju kokku 
  VAT_revenue_from_change_in_rate = VAT_revenue_from_change_in_rate_20 + VAT_revenue_from_change_in_rate_9
  
  result_abi2$Käibemaks = VAT_revenue_from_change_in_base + VAT_revenue_from_change_in_rate
  
  # Paneme maksud õigesse järjekorda 
  tax_names_reordered = c("Füüsilise isiku tulumaks", "Ettevõtte tulumaks", "Sotsiaalmaks", "Töötuskindlustusmaksed", "Käibemaks", "Aktsiisid", "Muu (kõik ülejäänud kokku agregeeritud)")
  
  result[["taxes"]] = result_abi2[tax_names_reordered]
  
  return(result)
}

# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
# impact_of_prices_on_FD = impact_of_prices_on_FD_f(data_i = impact_of_taxes_on_prices, year = 2020, IO_year = 2017, time_interval = "aasta", elasticity = "miinusyks")
# demand_side_TP = demand_side_impact_on_TP_f(data_prices_i = impact_of_prices_on_FD, data_i = input_data, change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused, mult_type_i = "I", hh_savings_rate_i = 0)
# supply_side_impact_TP = supply_side_impact_on_TP_f(data_i = input_data, time_interval = "aasta")
# impact_on_VA_LC_PROF = impact_on_VA_LC_PROF_f(data_i = demand_side_TP, data_i_supply = supply_side_impact_TP)
# impact_on_tax_revenue = impact_on_tax_revenue_f(data_i=impact_on_VA_LC_PROF, year=2020, change_in_tax_rate = impact_of_taxes_on_prices$Maksumuutused, time_interval = "aasta")

