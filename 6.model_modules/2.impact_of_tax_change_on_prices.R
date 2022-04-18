#' Hinnamoodul
#'  
#' Moodul loeb andmesisestuse mooduli väljundist sisse maksumäärade
#' muudatused ning arvutab nende põhjal maksutõusust tulenevad
#' majandustegevusalade toodete hindades toimuva muudatused.
#' @param data_i  Andmesisestusmooduli väljundiks olev 9 elemendiga list
#' @param year Aasta, mille kohta on mudelisse sisestatud viimased andmed
#' kogutoodangu ja lisandväärtuse sektoraalse jaotuse kohta. 
#' @returns Kahetasandiline list, millest esimesel tasandil jagatakse väljundid
#' hinnamuudatusteks ja maksumuudatustes. Hinnamuudatuste all on toodud eraldi andmestikes
#' a) käibemaksu määrade, b) sotsiaalmaksu määra ja c) töötuskindlustusmaksete (summaarsest) määrast
#' tulenevad muutused majandustegevusala toodete ja teenuste keskmises hinnatasemes.  
#' Maksumuudatuste all on muutused (väljendatuna indeksina, kus 1 tähendab, et määr ei muutunud) 
#' a) käibemaksu tavamääras, b) käibemaksu erimääras, c) sotsiaalkindlustusmaksu määras
#' d) töötuskindlustusmaksete (summaarses) määras.
impact_of_taxes_on_prices_f = function(data_i, year = 2020) {
  # Üldine lähenemine:
  # - Loeme sisse andmed uute ja vanade maksumäärade kohta ja konverteerime need indeksiks, mis väljendab nominaalsete maksumäära tasemete protsentuaalsed muutust (1 tähendab, et muutust polnud) 
  # - Leiame maksumäärade muutuse mõju hinnatasemele. Käibemaksu puhul tähendab see, et käibemaksu määra tõusust tulenev absoluutse
  # maksutulu kasv (eeldusel, et kogu maksutõus kantakse hinda üle) suhestatakse kogutoodangusse ning see väljendabki protsentuaalset hinnatõusu. 
  # Sellist lähenemist saame kasutada seetõttu, et käibemaksu tõust vahetarbimisena kasutatavate toodete/teenuste hinda ei mõjuta.
  # 
  # Sotsiaalmaksu ja töötuskindlustusmaksete puhul mõjutab maksutõus sisendite maksumust (ehk siis vahetarbimist), seetõttu tuleb võtta 
  # arvesse iga majandustegevusala sisendite struktuur. Kasutame selleks Leontiefi hinnamudelit. Lõpuks tuleb arvestada ka sellega,
  # et tööjõumaksud on lisandväärtuse osa, seega võimendab see nendest põhjustatud hinnatõusu 
  
  
  ##########################################
  # Loeme sisse andmed uute ja vanade maksumäärade kohta
  # Uued maksumäärad
    # Käibemaks
    new_VAT_20= data_i$d_VAT_20
    new_VAT_9= data_i$d_VAT_9
    
    # Sotsiaalmaks
    new_ST= data_i$d_ST 
    
    # Töötuskindlustusmaksed
    new_UIC= data_i$d_UIC 
  
  # Vanad (mudeli viimasel kogutoodangu ja lisandväärtuse andmetega sisustatud aastal) kehtivad maksumäärad  
    old_nominal_tax_rates = read_excel(file.path(data_loc, "nominal_tax_rates_by_sector.xlsx"))
    
    # Käibemaks
    old_VAT_20= old_nominal_tax_rates$`Käibemaks - tavamäär`
    old_VAT_9= old_nominal_tax_rates$`Käibemaks - vähendatud määr`
    
    # Sotsiaalmaks
    old_ST= old_nominal_tax_rates$Sotsiaalmaks 
    
    # Töötuskindlustusmaksed
    old_UIC= old_nominal_tax_rates$Töötuskindlustusmaksed
  
  ###########################################  
  # Konverteerime uued ja vanad maksumäärad indeksiks, mis väljendab nominaalsete maksumäära tasemete protsentuaalsed muutust (1 tähendab, et muutust polnud)  
  
  # Muutus nominaalsetes maksumäärades
  # Kui sisendandmetes info puudub, siis on indeks = 1 (ehk eeldame, et muutusi pole)
  
    # Käibemaks
    change_in_VAT_20_rate = new_VAT_20/old_VAT_20
    change_in_VAT_20_rate[is.na(change_in_VAT_20_rate)] = 1 
    change_in_VAT_9_rate = new_VAT_9/old_VAT_9
    change_in_VAT_9_rate[is.na(change_in_VAT_9_rate)] = 1
  
    # Sotsiaalmaks
    change_in_ST_rate = new_ST/old_ST
    change_in_ST_rate[is.na(change_in_ST_rate)] = 1 
  
    # Töötuskindlustusmaksed
    change_in_UIC_rate = new_UIC/old_UIC
    change_in_UIC_rate[is.na(change_in_UIC_rate)] = 1 
  
  
  ###########################################  
  # Leiame maksumäärade muutuse mõju hinnatasemele.  
  
  # Loeme sisse maksutulud (leiame need maksubaasi ja efektiivsete maksumäärade abil)
  # Oluline on seda teha just niipidi, sest efektiivsed maksumäärad võtavad juba arvesse seda
  # et statistikaameti andmetes polnud kogu maksutulu sektorite vahel ära jagatud ning seetõttu
  # tuli efektiivseid maksumäärasid skaleerida üles, nii et sektorite maksutulu kokku
  # annaks kogu maksu laekumise.
  
    
  # Loeme sisse maksubaasid
    load(file.path(data_loc, "tax_base.Rda"))
    eval(parse(text = paste0("tax_base = tax_base_", year)))
    
    
  # Loeme sisse efektiivsed maksumäärad
    # Käibemaks
    load(file.path(data_loc, "effective_VAT_components.Rda"))
    eval(parse(text = paste0("effective_VAT_components_by_sector = effective_VAT_components_by_sector_", year)))
    
    # Muud maksud
    load(file.path(data_loc, "effective_tax_rates.Rda"))
    eval(parse(text = paste0("effective_taxes_by_sector = effective_taxes_by_sector_", year)))
  
  
  # Maksutulu absoluutse tõusus suhe kogutoodangusse  
    # Konverteerime maksumäära muutused absoluutseks maksutulu muutusteks sektorite lõikes (selle võrra peaks tõusma hinnad absoluutmääras),
    # ning suhestame need kogutoodangusse. 
    
    
    # Valmistame ette õiges formaadis baastabeli - kasutame seda tulemuste salvestamiseks
    # Kuna sisendandmetes võib olla mitu erinevat tulpa maksumuudatusi (nt erinevate ajaperioodide kohta),
    # siis peab baastabel lähtuma sellest formaadist, milles andmeid sisestatakse. Võtame aluseks
    # sotsiaalmaksu uute määrade tabeli, kuigi sama hästi võiks see olla ka mõne muu maksu tabel
    base = new_ST
    base[]<-1
    colnames_base = colnames(base)
    
    
    # Käibemaks
      # Viime käibemaksu arvutamiseks vajalikud komponendid mugavamale kujule
      VAT_components_abi = effective_VAT_components_by_sector %>%
        as.matrix() %>%
        t() %>%
        as.data.table()
      
      # Muutus tavamääraga käibemaksu tulus
      VAT_20_change = base %>%
        `*` (VAT_components_abi$`Efektiivne KM - tavamäär`) %>% 
        `*` (change_in_VAT_20_rate-1) %>%
        `*` (VAT_components_abi$`Tavamääraga käibe osakaal kodumaises käibes`) %>%
        `*` (VAT_components_abi$`Kohalik impordi lõpptarbimine` + VAT_components_abi$`Kohaliku kodumaise toodangu LT lisandväärtus`)
      
      # Muutus erimääraga käibemaksu tulus
      VAT_9_change = base %>%
        `*` (VAT_components_abi$`Efektiivne KM - erimäär`) %>% 
        `*` (change_in_VAT_9_rate-1) %>%
        `*` (VAT_components_abi$`Erimääraga käibe osakaal kodumaises käibes`) %>%
        `*` (VAT_components_abi$`Kohalik impordi lõpptarbimine` + VAT_components_abi$`Kohaliku kodumaise toodangu LT lisandväärtus`)
      
      # Käibemaksu tulu muutuse suhe kogutoodangusse   
      VAT_change_ratio_to_TP = VAT_20_change %>%
        `+` (VAT_9_change) %>%
        `/`(tax_base["Toodang",])
      

    # Sotsiaalmaks
      ST_revenue = tax_base["Hüvitised töötajatele",]*effective_taxes_by_sector["Sotsiaalmaks",]
      
      ST_0 = base %>%
        `*`(ST_revenue)
      
      # Sotsiaalmaksu tulu muutuse suhe kogutoodangusse
      ST_change_ratio_to_TP = base %>%
        `*`(ST_revenue) %>%
        `*`(change_in_ST_rate) %>%
        `-`(ST_0) %>%
        `/`(tax_base["Toodang",])
    
  
    # Töötuskindlustusmaksed
      UIC_revenue = tax_base["Hüvitised töötajatele",]*effective_taxes_by_sector["Töötuskindlustusmaksed",]
      
      UIC_0 = base %>%
        `*`(UIC_revenue)
      
      # Töötuskindlustusmaksete tulu muutuse suhe kogutoodangusse
      UIC_change_ratio_to_TP = base %>%
        `*`(UIC_revenue) %>%
        `*`(change_in_UIC_rate) %>%
        `-`(UIC_0) %>%
        `/`(tax_base["Toodang",])

  # Maksumuudatuste mõju hinnatasemele
    # Käibemaksu puhul ongi käibemaksu tulu muutuse suhe kogutoodangusse võrne mõjuga hinnatasemel, seega
      change_in_price_level_VAT = VAT_change_ratio_to_TP

    # Sotsiaalmaksu ja töötuskindlustusmaksete muutuse mõju hinnatasemele
  
      # Nagu varasemalt öeldud, on lähenemine käibemaksuga võrreldes teine - suuremad tööjõukulud vahetarbimise faasis
      # kajastuvad oma proportsiooniga ka lõpptarbimises
      # Loeme sisse sisendväljundtabeli elemendid
      load(file.path(data_loc, "ibi_IO_elements.Rda"))
  
      # Mõju hindadele tuleb Leontiefi hinnamudelist (see Eurostat Manual of Supply, Use and Input-Output Tables 2008, pp 490-492)
      # Hindade muutuste vektor arvutatakse järgmise valemi abil: p = solve(I63-Adt) %*% t(w+dt), where:
      # - I64 on 63 elemendiga ühikmaatriks
      # - Adt on transponeeritud tehniliste koefitsientide maatriks
      # - w on reavektor, mis sisaldab lisandväärtuse, impordi ja netotootemaksude osakaalude summat kogutoodangus (majandustegevusalade lõikes)
      # - dt on muutus maksutuludes väljendatuna osakaaluna kogutoodangus
  
  
      # Leontiefi pöördmaatriks 
        Adt = t(Ad)
        `I-Adt` = I63-Adt
        `inv(I-Adt)` = solve(`I-Adt`)
      
      # Lisandväärtuse, impordi ja netotootemaksude osakaalude summa vektor 
        w=colSums(L[-2,])+colSums(Am)+L_NTP
      
      # Sotsiaalmaks
        change_in_price_level_ST = ST_change_ratio_to_TP
        change_in_price_level_ST[] <-0
  
        for (i in 1:ncol(ST_change_ratio_to_TP)) {
          `w+t` = w + ST_change_ratio_to_TP[,i] 
          dp = `inv(I-Adt)`%*% t(`w+t`) %>% #hinnatase pärast muutust (indeks)
            `-` (1) %>% #erinevus varasemast hinnatasemest (oli kõigi tegevusalade jaoks üks)
            round(6) #eemaldame anomaaliad
          
          change_in_price_level_ST[, i] = dp
        } 
        
        colnames(change_in_price_level_ST) <- colnames_base
        
        # Kuna tööjõumaksud on lisandväärtuse osa, siis tuleb arvestada sellega, et nende abs suurenemist võimendab parasjagu kehtiv käibemaks (võib olla stsenaariumiti erinev).
        change_in_price_level_ST = change_in_price_level_ST %>%
          as.matrix() %>%
          as.data.frame() %>%
          `*`(VAT_components_abi$`Efektiivne KM - keskmine` + 1) 
        
      
      # Töötuskindlustusmaksed 
        change_in_price_level_UIC = UIC_change_ratio_to_TP
        change_in_price_level_UIC[] <-0
        
        for (i in 1:ncol(UIC_change_ratio_to_TP)) {
          `w+t` = w + UIC_change_ratio_to_TP[,i] 
          dp = `inv(I-Adt)`%*% t(`w+t`) %>% 
            `-` (1) %>% 
            round(6) 
          
          change_in_price_level_UIC[, i] = dp
        } 
    
        colnames(change_in_price_level_UIC) = colnames_base
        
        # Kuna tööjõumaksud on lisandväärtuse osa, siis tuleb arvestada sellega, et nende abs suurenemist võimendab parasjagu kehtiv käibemaks (võib olla stsenaariumiti erinev).
        change_in_price_level_UIC = change_in_price_level_UIC %>%
          as.matrix() %>%
          as.data.frame() %>%
          `*`(VAT_components_abi$`Efektiivne KM - keskmine` + 1) 
  
  # Tulemuste salvestamine
    Hinnamuutused =   list('Käibemaks' = change_in_price_level_VAT,
                           'Sotsiaalmaks' = change_in_price_level_ST,
                           'Töötuskindlustusmaksed' = change_in_price_level_UIC)
    
    Maksumuutused =   list('Käibemaks 20%' = change_in_VAT_20_rate,
                           'Käibemaks 9%' = change_in_VAT_9_rate,
                           'Sotsiaalmaks' = change_in_ST_rate,
                           'Töötuskindlustusmaksed' = change_in_UIC_rate)
    
    
    result =   list("Hinnamuutused" = Hinnamuutused,
                    "Maksumuutused" = Maksumuutused)
  
  return(result)
}      
# input_data = input_data_f(file_path = andmed)
# impact_of_taxes_on_prices = impact_of_taxes_on_prices_f(data_i = input_data, year = 2020)
