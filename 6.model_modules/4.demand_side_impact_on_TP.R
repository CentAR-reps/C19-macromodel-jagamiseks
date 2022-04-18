#' Nõudluspoolse kogutoodangu muutuse moodul 
#' 
#' Moodul arvutab lõpptarbimise muutuste mõju kogutoodangule.
#' Arvutus tugineb Leontiefi multiplikaatormudelile. Kasutada saab nii
#' I (otsesed ja kaudsed mõjud) kui II (otsesed, kaudsed ja tingitud
#' mõjud) tüüpi multiplikaatoreid. Viimaste puhul on oluline
#' määratleda ka säästumäär ehk see osa kodumajapidamiste 
#' sissetulekutest, mida tarbimisse ei suunata. 
#' @param data_i Andmesisestusmooduli väljundiks oleks üheksa 
#' elemendiga list, mis sisaldab infot lõpptarbimise
#' muutuse (komponentide lõikes) ning nominaalsete maksumäärade muutuse kohta 
#' (maksude lõikes). Listi elemendid on data.frame formaadis.  
#' @param data_prices_i Lõpptarbimise mooduli väljundiks olev data.frame formaadis
#' andmestik, mis sisaldab maksumuudatuste poolt esile kutsutud hinnamuutuste
#' mõju kodumajapidamiste lõpptarbimisele. 
#' @param mult_type_i Multiplikaatori tüüp. Otseste ja kaudsete mõjude arvesse 
#' võtmiseks tuleks valida "I", otseste, kaudsete ja tingitud mõjude arvesse
#' võtmiseks tuleks valida "II" (NB! Sellisel juhul tuleks muuta ka
#' säästumäära, mis vaikimisi on null). 
#' @param hh_savings_rate_i Säästumäär ehk see osa kodumajapidamiste sissetulekust,
#' mille majapidamised säästavad. Säästumäär siseneb meie mudelis ainult tingitud efektide
#' arvutustesse. Kui säästumäär on 10,5%, siis esitada kujul 10.5 
#' @param year_i Aasta mille kohta on olemas viimased kogutoodangu andmed.
#' @param change_in_tax_rate_i Hinnamooduli väljundiks olevad maksumuudatuste indeksid.
#' Andmed on data.frame formaadis.
#' @param household_income_source_i Kodumajapidamiste sissetuleku allikas. Saab olla kas:
#'  "..palk" - brutopalk 
#'  "Hüvitised töötajatele" - palgakulu, sh sotsiaalmaks ja töötuskindlustusmaks 
#' @returns Viieliikmeline list, mille neli esimest elementi sisaldavad infot kogutoodangu,
#' muutuste kohta lõpptarbimise elementide lõikes ning viimane element on muutus impordi 
#' lõpptarbimises (seda kasutatakse maksumoodulis käibemaksu tulu arvutamiseks). Andmed
#' on data.frame formaadis.
demand_side_impact_on_TP_f = function(data_prices_i, data_i, mult_type_i = "I", hh_savings_rate_i = 0, year_i=2020, change_in_tax_rate_i, household_income_source_i = "..palk") {
  # Üldine lähenemine
  # - Kui maksumuudatuste tulemusena on toimunud kodumajapidamiste lõpptarbimises muutusi, siis võtame selle arvesse
  # - Arvutame lõpptarbimise muutusest käibemaksu maha (erandiks on siin eksport). Majandus mõjutab vaid käibemaksu lõpptarbimise muutus.  
  # - Seejärel võtame maha impordi osa lõpptarbimises
  # - Saadud tulemuse jooksutame läbi multiplikaatormudeli
  
  
  # Võtame arvesse võimalike maksumuudatuste tõttu kodumajapidamiste lõpptarbimises toimunud muutused 
  data_i$d_household_FD = data_i$d_household_FD  + data_prices_i$d_household_FD
  
  # Leiame käibemaksujärgse muutuse lõpptarbimises.  
  load(file.path(data_loc, "effective_VAT_components.Rda"))
  effective_VAT_components_by_sector = get(paste0("effective_VAT_components_by_sector_", as.character(year_i)))
  
  # 2020.aastal kehtivad määrad
  nominal_tax_rates = read_xlsx(file.path(data_loc, "nominal_tax_rates_by_sector.xlsx"))
  
  # Korrigeerime määrasid, kui need on muutunud
  correction_for_FD_normal_rate = change_in_tax_rate_i$`Käibemaks 20%` %>%
    `*` (nominal_tax_rates$`Käibemaks - tavamäär`) %>%
    `*` (effective_VAT_components_by_sector["Tavamääraga käibe osakaal kodumaises käibes",])
  
  correction_for_FD_red_rate = change_in_tax_rate_i$`Käibemaks 9%` %>%
    `*` (nominal_tax_rates$`Käibemaks - vähendatud määr`) %>%
    `*` (effective_VAT_components_by_sector["Erimääraga käibe osakaal kodumaises käibes",])
  
  correction_for_FD = correction_for_FD_normal_rate + correction_for_FD_red_rate + 1
  
  # Lõpptarbimise muutused, millest käibemaks on maha arvatud.
  # investeeringutest ja valitsuse kulutustes ka, sest keskpikas perspektiivis makstakse see sellel summalt ära
  # ekspordilt käibemaksu maha ei võta.
  data_i$d_household_FD = data_i$d_household_FD / correction_for_FD
  data_i$d_investments_FD = data_i$d_investments_FD / correction_for_FD
  data_i$d_govexp_FD = data_i$d_govexp_FD / correction_for_FD
  
  
  # Arvame impordi lõpptarbimise lõpptarbimise muutusest maha 
  # Sisendväljundtabeli elemendid
  load(file.path(data_loc, "ibi_IO_elements.Rda"))
  
  # Kodumaise tarbimise osakaal
  domesic_demand_shares_in_FD = Fd/Fc
  
  types_of_FD = c("Kodumajapidamiste lõpptarbimiskulutused", 
                  "Kapitali kogumahutus põhivarasse ja väärisesemed", 
                  "Valitsemissektori lõpptarbimiskulutused",         
                  "Kaupade ja teenuste eksport FOB")                 
  
  domesic_demand_shares_in_FD = domesic_demand_shares_in_FD[, types_of_FD]  
  
  # Lisakorrektuurid
  # - Osade kohtade peal on NAd, asendame need nullidega. Antud juhul peaks see olema sisuliselt õige.
  # - "Muude transpordivahendite tootmine" on sektor, kus kodumaise lõpptarbimise osakaal tuleb 
  # investeeringute puhul suurelt negatiivne (-1.46985333 2017. aasta pakkumise ja kasutamise tabelites). Ei saa seda kasutada, praeguses versioonis on see asendatud nulliga.
  
  domesic_demand_shares_in_FD[is.na(domesic_demand_shares_in_FD)] <-0
  domesic_demand_shares_in_FD["Muude transpordivahendite tootmine", "Kapitali kogumahutus põhivarasse ja väärisesemed"] <-0
  #NB! Me ei kasuta final_demand_compnent_shares() funktsooni just nende modifikatsioonide tõttu
  
  input_data_names =  c("d_household_FD",
                        "d_investments_FD",
                        "d_govexp_FD",
                        "d_exports_FD")
  
  
  # Leiame kodumaise lõpptarbimise muutuse
  data_i_dom = data_i
  
  for(i in 1:4) {
    dom_share_abi = domesic_demand_shares_in_FD[,types_of_FD[i]]
    abi1 = dom_share_abi %>% t()
    eval(parse(text = paste0("data_i_dom$", input_data_names[i],  "= data_i$", input_data_names[i], " * abi1")))
  }
  
  # Leiame impordi lõpptarbimise muutuse
  data_i_imp = data_i
  data_i_imp$d_empl = data_i_imp$d_VAT_20 = data_i_imp$d_VAT_9 = data_i_imp$d_ST = data_i_imp$d_UIC = NULL
  
  for(i in 1:4) {
    eval(parse(text = paste0("data_i_imp$", input_data_names[i],  "= data_i$", input_data_names[i], " - data_i_dom$",input_data_names[i])))
  }
  
  col_names = names(data_i_dom$d_household_FD)
  
  # Kuna kasutame põhjana sisendandmestikku, siis tuleb ebavajalikud elemendid (näitame sisendites vaid lõpptarbimise muutusi
  # kustutada
  result = data_i_dom
  result$d_empl = NULL
  result$d_VAT_20 = NULL 
  result$d_VAT_9 = NULL
  result$d_ST = NULL
  result$d_UIC = NULL
  
  
  for(j in input_data_names) {
    for(i in col_names) {
      
      eval(parse(text = paste0("VAT_abi = correction_for_FD$`", i, "`")))
      VAT_abi = (VAT_abi-1)*100
      
      # Rakendame kodumaise lõpptarbimise muutusele multiplikaatormudelit
      # (funktsioon ibi_IO_multiplyers() on kirjeldatud '1.scripts', kataloogis asuvas failis 'IO_functions.R')
      multipliyers = ibi_IO_multiplyers(type = mult_type_i, indicator="production", matrix_form = "yes", hh_savings_rate = hh_savings_rate_i, household_income_source = household_income_source_i, VAT_rates = VAT_abi, year = year_i) 
      
      eval(parse(text = paste0("abivar = data_i_dom$", j ,"$`",i,"`"))) 
      result_abi = multipliyers %*% abivar
      
      if(i == col_names[1]) {
        result_abi2 = result_abi
      } else{
        result_abi2 = cbind(result_abi2, result_abi)
      }
    }
    colnames(result_abi2) = col_names
    result_abi2 = result_abi2 %>%
      as.data.frame()
    
    eval(parse(text = paste0("result$", j,"  = result_abi2")))
  }  
  
  # Impordi lõpptarbimist vajame käibemaksu tulu arvutamiseks, seetõttu lisame ka selle väljunditele.  
  result[["FD_of_imports"]] = data_i_imp
  
  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
# impact_of_prices_on_FD = impact_of_prices_on_FD_f(data_i = impact_of_taxes_on_prices, year = 2020, IO_year = 2017, time_interval = "kuu", elasticity = "miinusyks")
# demand_side_TP = demand_side_impact_on_TP_f(data_prices_i = impact_of_prices_on_FD, data_i = input_data, mult_type_i = "II", hh_savings_rate_i = 90, year_i = 2020, change_in_tax_rate_i = impact_of_taxes_on_prices$Maksumuutused, household_income_source_i = "..palk")
