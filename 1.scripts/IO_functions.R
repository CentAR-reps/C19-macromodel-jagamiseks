#' Load data supply and use data from Statistics Estonia
#' 
#' Function fetches data from Statistics Estonia and saves it
#' in folder "2.data".
#' This includes:
#' - RAT00001: SUPPLY TABLE AT BASIC PRICES
#' - RAT00002: USE TABEL AT PURCHASER'S PRICES
#' - RAT00003: USE TABEL AT BASIC PRICES
#' - RAT000030: USE TABLE OF IMPORTS AT BASIC PRICES
#' Data are stored in data.table format
#' Units: euro
#' @param year The year for which the data should be fetched, defaults to 2017
#' @returns Following files: "supply_table_bp.Rda", "use_table_bp.Rda", "use_table_imp_bp.Rda"
load_su_data <- function(year=2017){
  
  # The query in json format 
  query = paste0('{"query": [{"code": "Aasta","selection": {"filter": "item","values": ["', 
                 year, 
                 '"]}}],"response": {"format": "csv"}}')
  
  # Supply table at basic prices
  supply_total_bp <- content(POST("https://andmed.stat.ee/api/v1/et/stat/RAT00001",  body = query, encode = "json"), 
                             type = "text/csv",
                             encoding = "UTF-8") %>%
    as.data.table() %>%
    .[, Aasta:=NULL]
  
  # Replacing "." with zeroes and NA-s where necessary, converting from 1000000 euros to euros
  supply_total_bp[supply_total_bp=="."] <-0
  cols = names(supply_total_bp)[2:70]
  supply_total_bp[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  # Convert units from millions to original units (e.g.Meur to eur)
  to_original_units = function(x) {
    result = x*1000000
    return(result)
  }
  supply_total_bp[, (cols) := lapply(.SD, to_original_units), .SDcols = cols]
  
  cols = names(supply_total_bp)[66:70]
  rows = supply_total_bp$Toode[68:70]
  supply_total_bp[Toode %in% rows, (cols) := NA]
  
  save(supply_total_bp,file = file.path(data_loc, "supply_table_bp.Rda"))
  rm(supply_total_bp)
  
  # Use table at basic prices
  use_total_bp <- content(POST("https://andmed.stat.ee/api/v1/et/stat/RAT00003",  body = query, encode = "json"), 
                          type = "text/csv",
                          encoding = "UTF-8") %>%
    as.data.table() %>%
    .[, Aasta:=NULL]
  
  # Replacing "." with zeroes and NA-s where necessary, converting from 1000000 euros to euros
  use_total_bp[use_total_bp=="."] <-0
  cols = names(use_total_bp)[2:75]
  use_total_bp[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  use_total_bp[, (cols) := lapply(.SD, to_original_units), .SDcols = cols]
  
  
  cols = names(use_total_bp)[66:75]
  rows = use_total_bp$`Toode/Lisandväärtus`[67:68]
  use_total_bp[`Toode/Lisandväärtus` %in% rows, (cols) := NA]
  
  save(use_total_bp, file = file.path(data_loc, "use_table_bp.Rda"))
  rm(use_total_bp)
  
  # Use table at producer prices
  use_total_pp <- content(POST("https://andmed.stat.ee/api/v1/et/stat/RAT00002",  body = query, encode = "json"), 
                          type = "text/csv",
                          encoding = "UTF-8") %>%
    as.data.table() %>%
    .[, Aasta:=NULL]
  
  # Replacing "." with zeroes and NA-s where necessary, converting from 1000000 euros to euros
  use_total_pp[use_total_pp=="."] <-0
  cols = names(use_total_pp)[2:75]
  use_total_pp[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  use_total_pp[, (cols) := lapply(.SD, to_original_units), .SDcols = cols]
  cols = names(use_total_pp)[66:75]
  rows = use_total_pp$`Toode/Lisandväärtus`[69:75]
  use_total_pp[`Toode/Lisandväärtus` %in% rows, (cols) := NA]
  
  save(use_total_pp, file = file.path(data_loc, "use_table_pp.Rda"))
  rm(use_total_pp)

  # Use table of imports 
  use_imports <- content(POST("https://andmed.stat.ee/api/v1/et/stat/RAT000030",  body = query, encode = "json"), 
                              type = "text/csv",
                              encoding = "UTF-8") %>%
    as.data.table() %>%
    .[, Aasta:=NULL]
  
  # Replacing "." with zeroes and NA-s where necessary
  use_imports[use_imports=="."] <-0
  cols = names(use_imports)[2:75]
  use_imports[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  use_imports[, (cols) := lapply(.SD, to_original_units), .SDcols = cols]
  
  save(use_imports, file = file.path(data_loc, "imp_use_table_bp.Rda"))
  rm(use_imports)
  
}
#load_su_data(year=2017)

#' Extract elements from use and supply tables
#' 
#' Function extracts Vt (V transposed), m, q, q-m, gt from
#' supply table and U, Ud, Um, Y, Yd, Ym and W from
#' total use table and import use table. 
#' 
#' @returns Returns file "use_and_supply_elements.Rda", that includes the above mentioned list of matrices
use_and_supply_elements <- function() {
  
  #Load supply and use tables
  #The supply table object generated with function load_su_data()
  load(file.path(data_loc, "supply_table_bp.Rda"))
  
  #The use table in basic prices object generated with function load_su_data()
  load(file.path(data_loc, "use_table_bp.Rda"))

  #The use table in producer prices object generated with function load_su_data()
  load(file.path(data_loc, "use_table_pp.Rda"))

  #The use of imports table object generated with function load_su_data()
  load(file.path(data_loc, "imp_use_table_bp.Rda"))

  
  # Supply table elements
  # Row and column names of the intermediate consumption part of supply table
  rownames = unlist(supply_total_bp[,1][1:63])
  colnames = names(supply_total_bp)[2:64]
  
  # Vt - supply table, products by industry (matrix)
  Vt = as.matrix(supply_total_bp[1:63, 2:64])
  rownames(Vt) = rownames
  colnames(Vt) = colnames
  
  # gt - total output by industry (row vector)
  gt = as.matrix(supply_total_bp[Toode =="Toodang/Pakkumine kokku", 2:64]) 
  colnames(gt) = colnames
  rownames(gt)[1] = "Toodang/Pakkumine kokku"
  
  # q - total supply of products (column vector)
  q = as.matrix(supply_total_bp[1:63, `Kogupakkumine alushindades`])
  rownames(q) = rownames
  colnames(q)[1] = "Kogupakkumine alushindades"
  
  # m - imports of products(column vector)
  m = as.matrix(supply_total_bp[1:63, `Kaupade ja teenuste import CIF kokku`])
  rownames(m) = rownames
  colnames(m)[1] = "Kaupade ja teenuste import CIF kokku"
  
  # q_m - domestic supply of products (column vector)
  q_m = as.matrix(supply_total_bp[1:63, `Toodang kokku`])
  rownames(q_m) = rownames
  colnames(q_m)[1] = "Toodang kokku"
  
  
  # Use table elements
  # Row and column names of the intermediate consumption part of supply table
  rownames = unlist(use_total_bp[,1][1:63])
  colnames = names(use_total_bp)[2:64]
  
  # U - total use, products by industry (matrix)
  U = as.matrix(use_total_bp[1:63, 2:64])
  rownames(U) = rownames
  colnames(U) = colnames
  
  # Um - use of imports, products by industry (matrix)
  Um = as.matrix(use_imports[1:63, 2:64])
  rownames(Um) = rownames
  colnames(Um) = colnames
  
  # Ud - use of domestic products, products by industry (matrix)
  Ud = U-Um
  rownames(Ud) = rownames
  colnames(Ud) = colnames
  
  # Net taxes on products (in Estonian the term is "neto-tootemaksud")
  # There is no special designation for this in Eurostat methodology
  # We will use NTP 
  
  NTP = as.matrix(use_total_bp[`Toode/Lisandväärtus`== "Neto-tootemaksud", 2:64])
  rownames(NTP)[1] = "Neto-tootemaksud"
  
  # Elements of final demand
  final_demand = c("Kodumajapidamiste lõpptarbimiskulutused", "KTKTI lõpptarbimiskulutused", "Valitsemissektori lõpptarbimiskulutused", "Kapitali kogumahutus põhivarasse ja väärisesemed", "Varude muutus","Kaupade ja teenuste eksport FOB")
  
  # Y - final demand by elements (matrix)
  Y = as.matrix(use_total_bp[1:63, final_demand, with = FALSE])
  rownames(Y) = rownames
  
  # Y_NTP - net taxes on products (row vector) 
  Y_NTP = as.matrix(use_total_bp[`Toode/Lisandväärtus` == "Neto-tootemaksud", final_demand, with = FALSE])
  rownames(Y_NTP)[1] = "Neto-tootemaksud"
  colnames(Y_NTP) = final_demand 

  # Y_total - final demand, sum across elements  (column vector)
  Y_total = as.matrix(use_total_bp[1:63, `Lõppkasutamine kokku`])
  rownames(Y_total) = rownames
  colnames(Y_total)[1] = "Lõppkasutamine kokku"
  
  # Ym - final demand of imports by elements (matrix)
  Ym = as.matrix(use_imports[1:63, final_demand, with = FALSE])
  rownames(Ym) = rownames
  
  # Ym_total - final demand on imports, sum across elements  (column vector)
  Ym_total = as.matrix(use_imports[1:63, `Imporditud toodete lõppkasutamine kokku`])
  rownames(Ym_total) = rownames
  colnames(Ym_total)[1] = "Imporditud toodete lõppkasutamine kokku"
  
  # Yd - final domestic demand by elements (matrix)
  Yd = Y-Ym

  # Yd_total - final domestic demand, sum across elements  (column vector)
  Yd_total = Y_total-Ym_total
  colnames(Yd_total)[1] = "Kodumaiste toodete lõppkasutamine kokku"
  
  # Elements of value added
  value_added = c("Hüvitised töötajatele", "..palk", "Muud neto-tootmismaksud", "Põhivara kulum", "Tegevuse ülejääk ja segatulu, neto")
  
  # Value added element by industry (matrix)
  W = as.matrix(use_total_pp[`Toode/Lisandväärtus` %in% value_added, 2:64])
  rownames(W) = value_added
  
  # Value added total by industry (rowvector)
  W_total_t = as.matrix(use_total_bp[`Toode/Lisandväärtus` == "Lisandväärtus kokku", 2:64])
  rownames(W_total_t)[1] = "Lisandväärtus kokku"
  
  save(Vt, gt, q, m, q_m, U, Ud, Um, NTP, Y, Y_NTP, Yd, Ym, Y_total, Yd_total, Ym_total, W, W_total_t,
       file = file.path(data_loc, "use_and_supply_elements.Rda"))
}
# use_and_supply_elements()

#' Elements of industry-by-industry domestic input-output table
#' 
#' Function will create main elements of industry-by-industry domestic
#' input-output table.
#' Returns results in matrix format (sector and product names in row and colnames):  
#' Prepares following elements:
#' Tr - transformation matrix
#' D - matrix of market shares (equal to T)
#' Z - input requirements matrix   
#' Zd - input requirements matrix - domestic   
#' Zm - input requirements matrix - imports   
#' B - matrix of intermediate demand 
#' Bd - matrix of intermediate demand - domestic
#' Bm - matrix of intermediate demand - imports
#' Fc - matrix of components of final demand 
#' Fd - matrix of components of final demand - domestic
#' Fm - matrix of components of final demand - imports
#' W - matrix of components of value added (same as in use table) 
#' NTP - row vector of net production taxes (same as in use table)
#' F_NTP - row vector of NTP by final demand components (same as in use table)
#' A - matrix of technical coefficients
#' Ad - matrix of technical coefficients - domestic
#' Am - matrix of technical coefficients - imports
#' L - input requirements of value added components per unit of industry output
#' L_NTP - input requirements of net taxes on products per unit of industry output
#' I63 - Identity matrix with 63 rows and columns
#' I64 - Identity matrix with 64 rows and columns
#' 
#' @returns Returns file "ibi_IO_elements.Rda", that includes the above mentioned list of matrices 
ibi_IO_elements <- function() {
  
  # Load elements of supply and use tables
  load(file.path(data_loc, "use_and_supply_elements.Rda"))
  
  # Tr - transformation matrix
  Tr = t(Vt)%*%solve(diag(q_m[,1]))
  colnames(Tr) = rownames(Vt)
  
  # D - matrix of market shares (equal to Tr)
  D = Tr
  
  # Z - input requirements matrix   
  Z = U %*% solve(diag(gt[1,]))
  colnames(Z) = rownames(Tr)
  
  # Zd - input requirements (domestic) matrix   
  Zd = Ud %*% solve(diag(gt[1,]))
  colnames(Zd) = rownames(Tr)
  
  # Zm - input requirements (import) matrix   
  Zm = Um %*% solve(diag(gt[1,]))
  colnames(Z) = rownames(Tr)
  
  # B - matrix of intermediate demand 
  B = Tr%*%U
  
  # Bd - matrix of intermediate demand - domestic
  Bd = Tr%*%Ud
  
  # Bm - matrix of intermediate demand - imports
  Bm = Tr%*%Um
  
  # Fc - matrix of components of final demand 
  Fc = Tr%*%Y
  
  # Fd - matrix of components of final demand - domestic 
  Fd = Tr%*%Yd

  # F - matrix of components of final demand - imports
  Fm = Tr%*%Ym
  
  # W - matrix of components of value added (the same than in use table)
  W = W
  
  # NTP - row vector of net production taxes (no changes)
  NTP = NTP
  
  # F_NTP - row vector of NTP by final demand components (no changes)
  Fc_NTP = Y_NTP
  
  # A - matrix of technical coefficients
  A = Tr%*%Z
  colnames(A) = rownames(A)
  
  # A - matrix of technical coefficients - domestic
  Ad = Tr%*%Zd
  colnames(Ad) = rownames(Ad)
  
  # A - matrix of technical coefficients - imports
  Am = Tr%*%Zm
  colnames(Am) = rownames(Am)
  
  # L - input requirements of value added components per unit of industry output
  L = W%*%solve(diag(gt[1,]))
  colnames(L) = colnames(Vt)
  
  # L_NTP - input requirements of net taxes on products per unit of industry output
  L_NTP = NTP%*%solve(diag(gt[1,]))
  colnames(L_NTP) = colnames(Vt)

  # I63 - Identity matrix with 63 rows and columns
  I63 = diag(63)
  
  # I64 - Identity matrix with 64 rows and columns
  I64 = diag(64) 
  
  save(Tr, D, Z, Zd, Zm, B, Bd, Bm, Fc, Fd, Fm, W, NTP, Fc_NTP, A, Ad, Am, L, L_NTP, I63, I64,
       file = file.path(data_loc, "ibi_IO_elements.Rda"))
}  
#ibi_IO_elements()

#' Otsesed, kaudsed ja tingitud efektid
#' 
#' Funktsioon leiab otsesed, kaudsed ja tingitud efektid ehk
#' arvutab Leontiefi I ja II tüüpi multiplikaaotrid
#' 
#' @param type - multiplikaaotir tüüp: 
#' "I" - I tüüpi multiplikaator (sisaldab otseseid ja kaudseid efekte)
#' "II" - II tüüpi multiplikaator (sisaldab otsesed, kaudseid ja tingitud efekte)
#' @param indicator - indikaator, mille multiplikaatoreid arvutatakse. Saab olla:
#'  "production" - kogutoodang  (vaikimisi valitakse see)
#'  "value_added" - lisandväärtus 
#'  "labour_cost" - tööjõukulu 
#'  "profits" - kasum
#' @param matrix_form - täpsustab, millisel kujul arvutuste tulemsued esitatakse:
#'  "Yes" väljastab maatriksi, mis näitab iga tegevusala panust teiste tegevusalade multiplikaatori väärtusse. 
#'  "No" väljastab vektori, kus iga tegevusala jaoks on teiste tegevusalade panus summeeritud üheks väärtuseks (vaikimisi valitaks esee)
#' @param hh_savings_rate - säästumäär (kui nt 10.5%, siis sisesta 10.5), vaikimisi valitakse 0. Vajalik vaid II tüüpi multiplikaatorite arvutamiseks.
#' @param VAT_rates - vektor, mis sisaldab erinevat tüüpi käibe osakaaludega kaalutud käibemaksu määrasid (nt kui 20%, siis 20). Vajalik vaid II tüüpi multiplikaatorite arvutamisel 
#' @param household_income_source Kodumajapidamiste sissetuleku allikas. Saab olla kas:
#'  "..palk" - brutopalk 
#'  "Hüvitised töötajatele" - palgakulu, sh sotsiaalmaks ja töötuskindlustusmaks (vaikimisi valik)
#' @param year - aasta, mille kohta simulatsioon läbi viiakse, vaikimisi 2020.
#' @param ignroe_II_round_imp  Kas II tüüpi multiplikaatorite II ringi efektides peaks ignoreerima seda, et osa lõpptarbimisest läheb riigist välja (c("no", "yes"))        
#' @returns Returns multipliers in matrix format (sectors in rownames)  
ibi_IO_multiplyers = function(type = "I", indicator="production", matrix_form = "no", hh_savings_rate = 0, VAT_rates = 0, ignroe_II_round_imp = "no", household_income_source = "Hüvitised töötajatele", year = 2020) {
  
  # Loeme sisse sisend-väljun tabeli elemendid
  load(file.path(data_loc, "ibi_IO_elements.Rda"))
  
  # Leontief I tüüpi multiplkaatorid
  if(type == "I") {
    # Leiame pöördmaatriks
    result_abi = solve(I63-Ad)
  }
    
  if(type == "II") {
    # Leontief pöördmaatriks kodumaisele toodangule
    
    # Esmalt lisame tööjõukulu
    # Siin on mitmeid valikuvõimalusi - kas loodud lisandväärtusest jõuab kodumajapidamisteni vaid netopalk või  
    # midagi veel (osa kasumist, valituse poolsed siirded jne). Üks võimalus on keskenduda ainult palgale, mida korrigeerime säästumääraga.
    # Suure tõenäosusega alahindab see multiplikatiivseid efekte.

    # Alternatiivina saab kasutada sissetuleku allikana "hüvitisi töötajatele". See sisaldab 
    # ka tulu ja sotsiaalmaksu.See on kõige lihtsam moodus II tüüpi multiplikaatoreid arvutada, maksukorrektuuri ei tehta
    # ja eeldatakse, et kogu tööjõukulu jõuab moel või teisel majapidamisteni tagasi. See on suuresti tõsi - 
    # sotsiaalmaks läheb valdavalt pensioniks ning majapidamiste raviteenuste tarbimise finantseerimiseks ning need pole
    # ainsad siided , kuid siin on oluline silmas pidada
    # osad nendest nendest kulutustes paiknevad valitsussektori kulutuste struktuuris (nt tervishoid) ja vaid väikeses osas 
    # kodumajapidamiste omas ning pensionid ja muud siirded tööstusharude vahel sama loogika alusel jagatavad kui tööjõukulu.
    # Ehk siis - siin on mitmeid probleeme, mis muudavad selle arvutuse küllaltki ebatäpseks. 
    
    # Vaikimisi valik on siin "Hüvitised töötajatele" (eeldatavalt rohkem levinud moodus multiplikaatorite arvutamiseks), 
    # kuid mudelis soovitame kasutada siiski brutopalka ("..palk") annab konservatiivsema tulemuse (muudame selle hiljem netopalgaks)
    
    Ad_ext = rbind(Ad, L[rownames(L) == household_income_source])
    rownames(Ad_ext)[nrow(Ad_ext)] = household_income_source
    
    if(household_income_source == "..palk") {
      load(file.path(data_loc, "effective_tax_rates.Rda"))
      effective_taxes_by_sector = get(paste0("effective_taxes_by_sector_", year))
      
      load(file.path(data_loc, "tax_base.Rda"))
      tax_base = get(paste0("tax_base_", year))
      
      # Peame vahetama maksubaasi - füüsilise isiku tulumaks baasiks on "Hüvitised töötajatele",
      # meie soovime korrigeerida brutopalka ("..palk")
      income_tax_abi = effective_taxes_by_sector["Füüsilise isiku tulumaks", ] * tax_base["Hüvitised töötajatele",] 
      income_tax_rate_wage = income_tax_abi/tax_base["..palk",]
      income_tax_rate_wage[63] = 0 # Kodumajapidamiste omatarbe NaN tekitab lõpus tehnilisi probleeme, asendame selle nulliga. 
      
      Ad_ext["..palk",] = Ad_ext["..palk",] * (1-income_tax_rate_wage)
    }

    
    # Leiame tegevusalade toodangu/teenuste osakaalu kodumajapidamiste tarbimiskorvis
    # Seda tehes tuleb meeles pidada, et:
    # - osa tarbimisest on import
    # - osa on netotootemaksud
    # - osa sissetulekust säästetakse
    
    # Esmalt leiame kodumajapidamiste kogutarbimise (import + kodumaine) (kui just ei soovi seda ingoreerida)
    if(ignroe_II_round_imp == "no") {
      hh_cons_total = Fc
    } 
    
    if(ignroe_II_round_imp == "yes") {
      hh_cons_total = Fd
    }
    
    hh_cons_total = hh_cons_total %>%
      colSums() %>%
      t() %>%
      as.data.table(keep.rownames = T) %>%
      .[, `Kodumajapidamiste lõpptarbimiskulutused`]
    
    # Siis leiame kohaliku kauba/teenuse lõpptarbimise osakaalu
    hh_dem = Fd[, "Kodumajapidamiste lõpptarbimiskulutused"] %>%
      as.matrix()
    colnames(hh_dem) = "Kodumajapidamiste lõpptarbimiskulutused"
    
    hh_dem = hh_dem/hh_cons_total
    
    # Kuna kogu sissetulekut tarbimisse ei suunata, siis võtame maha säästumäära  
    hh_dem = hh_dem * (1-hh_savings_rate/100)
    
    # Meie mudelis elavdab majandust käibemaksu järgne summa. Seetõttu korrigeerime iga sektori tarbimiskulusid 
    # vektoriga, mis sisaldab erinevat tüüpi käibe osakaaludega kaalutud käibemaksu määrasid
    # See tähendab, et teoreetiliselt saab igale stsenaariumile tekkida oma II tüüpi multiplikaator (eeldusel,
    # et muudetakse käibemaksu määra)
    
    VAT_rates_mod = (VAT_rates/100)+1
    hh_dem = hh_dem/VAT_rates_mod
    
    # Lõpetuseks lisame selle vektori lõppu nulli (kodumajapidamised ei tarbi toodet "palk")
    hh_dem = rbind(hh_dem, 0)
    rownames(hh_dem)[nrow(hh_dem)] = household_income_source
    
    # Lisame kodumajapidamiste tarbimise rea laienadtud sisetarbimise maatriksile
    Ad_ext = cbind(Ad_ext, hh_dem[, 1])
    colnames(Ad_ext)[64] = "Kodumajapidamiste tarbimine"
    
    # Finally we calculate the inverse and drop the last row and last column
    result_abi = solve(I64-Ad_ext)[-nrow(Ad_ext), -ncol(Ad_ext)] 
  }
  
  # Leiame erinevate väljundnäitajate multiplikaatorid
    
    # Kogutoodang
    if(indicator == "production") {
      result = result_abi %>%
        as.matrix()
    }
    
    # Lisandväärtus
    if(indicator == "value_added") {
      
      # Leiame lisandväärtuse osakaalu kogutoodangus
      # Jätame kõrvale rea "..palk" sest, see juba sisaldub Hüvitistes töötajatele 
      # Kasutame diagonaalmaatriksit, et korrutada toodangu mutiplikaatorite tulbad (Leontiefi pöördmaatriks) läbi lisandväärtuse vektoriga 
      L_va_total = L[-2,] %>%
        colSums() %>%
        diag()
      rownames(L_va_total) = colnames(L)
      colnames(L_va_total) = colnames(L)
      
      # Maatrikskorrutis
      result = L_va_total %*% result_abi %>%
        as.matrix()
    }    
    
    # Tööjõukulu
    if(indicator == "labour_cost") {
      
      # Sama lähenemine kui lisandväärtusega, lihtsalt lisandväärtuse asemel on tööjõukulu
      emp_comp = L["Hüvitised töötajatele",] %>%
        diag()
      rownames(emp_comp) = colnames(L)
      colnames(emp_comp) = colnames(L)
      
      result = emp_comp %*% result_abi %>%
        as.matrix() 
    }    
    
    # Kasum
    if(indicator == "profits") {
      
      profits = L["Tegevuse ülejääk ja segatulu, neto",] %>%
        diag()
      rownames(profits) = colnames(L)
      colnames(profits) = colnames(L)
      
      result = profits %*% result_abi %>%
        as.matrix() 
    }
    
  # Kui multiplikaatoreid soovitakse vektorina, siis esitame tulbasummad
  if(matrix_form == "no") {
    result = result %>%
      colSums() %>%
      as.matrix() 
    colnames(result) <- "Multipliers"
  }
  
  return(result)
}


#' Shares of domestic demand in final demand
#' 
#' Function calculates for components of final demand the 
#' shares of domestic demand in final demand, by sector
#'
#' @param fd_component  Type of multiplier, can be:
#' - "household", 
#' - "government", 
#' - "investment", 
#' - "exports",
#' - "total" 
#' @returns Final demand components in matrix format (sectors stored in rownames)  
final_demand_compnent_shares = function(fd_component="household") {

  # Load industry-by-industry input-output table elements
  load(file.path(data_loc, "ibi_IO_elements.Rda"))
  
  # Bridge between abrevations and real column names of Fd
  elements_fd = c("Kodumajapidamiste lõpptarbimiskulutused",
                  "Valitsemissektori lõpptarbimiskulutused",
                  "Kapitali kogumahutus põhivarasse ja väärisesemed",
                  "Kaupade ja teenuste eksport FOB",
                  "Kokku")    
  
  elements_fd_short = c("household",
                        "government",
                        "investment",
                        "exports",
                        "total") 
  
  fd_compnent_name = elements_fd[grep(fd_component, elements_fd_short)]
  
  if(fd_compnent_name=="Kokku") {
    result_F = rowSums(Fc)
    result_Fd = rowSums(Fd)
  } else{
    result_F = Fc[, fd_compnent_name]
    result_Fd = Fd[, fd_compnent_name]
  }
  
  result_F = as.data.frame(result_F)
  result_Fd = as.data.frame(result_Fd)
  colnames(result_F) = "Kokku"  
  colnames(result_Fd) = "Kohalik"  
  
  result = cbind(result_Fd, result_F)
  result$`Kodumaise tarbimise osakaal`= result$Kohalik/result$Kokku
  result$Kohalik = NULL
  result$Kokku = NULL
  
  # Households as employers do not have some components of final demand
  # It is ok here to substitute Na with zero. Later much easier
  result$`Kodumaise tarbimise osakaal`[is.na(result$`Kodumaise tarbimise osakaal`)]<-0
  
  return(result)    
}

#' Sisendväljundtabeli laiendused
#' 
#' Laiendab sisendväljundtabeleid muude näitajatega, suhestades need tegevusalade kaupa
#' kogutoodangusse. Kui mudel leiab lõpptarbimise muudatuste mõju kogutoodnagule, siis
#' leitakse selle kaudu mõju ka nendele näitajatele. Praegu vamistab funktsoon andmed
#' ette ETU ja EMTA hõivenäitajate arvutamiseks. 
#' @param year Aasta, mille kohta soovid huvipakkuva näitaja ja kogutoondagu näitajate suhte leida.
#' @param indicator Näitaja, mida soovime kogutoodnagusse suhestada c"EMTA", "ETU"
#' @returns Fail 'IO_table_extensions.Rda' andmeobjekt maatriks formis.
extensions_to_IO_tables_f = function(year = 2020, indicator = "EMTA") {
  
  # Loeme sisse kogutoondanu
  load(file.path(data_loc, "tax_base.Rda"))
  data_TP =get(paste0("tax_base_", year)) ["Toodang",] 
    
  # Nüüd muutujad, mis meiel huvi pakuvad
  # Hõive-ETU
  if(indicator == "ETU") {

    if(file.exists(file.path(data_loc, "IO_extensions_ETU.Rda"))){
      env1 = new.env()
      old_names = load(file.path(data_loc, 'IO_extensions_ETU.Rda'), envir = env1)
      rm(env1)
      load(file.path(data_loc, 'IO_extensions_ETU.Rda'))
    }

    load(file.path(data_loc, "employment_ETU.Rda"))
    data_empl = get(paste0("employment_ETU_", as.character(year)))
    abi = data_empl/data_TP
    rownames(abi) = "ETU hõive kogutoodangu ühiku kohta"
    eval(parse(text = paste0("employment_to_TP_", year, " = abi")))
    
    if(file.exists(file.path(data_loc, "IO_extensions_ETU.Rda"))){
      fnames = c(old_names, paste0("employment_to_TP_", year)) %>%
        unique()
      
    } else {
      fnames = paste0("employment_to_TP_", year)
    }
    s_command = paste0("save(", paste0(fnames, collapse = ","), ", file = file.path(data_loc, 'IO_extensions_ETU.Rda'))")
    eval(parse(text = s_command))
  }
    
  # Hõive-EMTA
  if(indicator == "EMTA") {
    if(file.exists(file.path(data_loc, "IO_extensions_EMTA.Rda"))){
      env1 = new.env()
      old_names = load(file.path(data_loc, 'IO_extensions_EMTA.Rda'), envir = env1)
      rm(env1)
      load(file.path(data_loc, 'IO_extensions_EMTA.Rda'))
    }

    load(file.path(data_loc, "employment_EMTA.Rda"))
    data_empl = get(paste0("employment_EMTA_", as.character(year)))
    abi = data_empl/data_TP
    rownames(abi) = "EMTA hõive kogutoodangu ühiku kohta"
    eval(parse(text = paste0("employment_to_TP_", year, " = abi")))
    
    if(file.exists(file.path(data_loc, "IO_extensions_EMTA.Rda"))){
      fnames = c(old_names, paste0("employment_to_TP_", year)) %>%
        unique()
      
    } else {
      fnames = paste0("employment_to_TP_", year)
    }
    
    s_command = paste0("save(", paste0(fnames, collapse = ","), ", file = file.path(data_loc, 'IO_extensions_EMTA.Rda'))")
    eval(parse(text = s_command))
  }    
}

# extensions_to_IO_tables_f(year = 2017, indicator = "ETU")


#' Function that fetches total tax revenue from Statistics Estonia
#' 
#' Function to download RR01 or RR02 from Statistics Estonia API 
#' and saves the data locally. Units - 1000 euros 
#' @param last_year The last year for what the government and local tax data should be downloaded
#' @returns Two files in data.frame format government level tax revenue 'RR01.Rda' and local tax revenue
#' 'RR02.Rda'.
load_total_tax_revenue_data_f <- function(last_year = 2020){
  
  # RR01 - government taxes (NB! Includes some local taxes)
  # RR02 - local taxes
  
  tablenames = c("rr01", "rr02") 
  
  for(tablename in tablenames) {
    # Array of years
    years = seq(2015, last_year,1) %>%
        as.character()

    # The query in json format 
    query = paste0('
                  {
                    "query": [
                      {
                        "code": "Aasta",
                        "selection": {
                          "filter": "item",
                          "values": [', paste(years, collapse = ","),
                   ']
                        }
                      }
                    ],
                    "response": {
                      "format": "csv"
                    }
                  }
                 ')
    
    table_url=paste0("https://andmed.stat.ee/api/v1/et/stat/", tablename)

    # Convert to data.frame
    tax_income  <- content(POST(url=table_url,  body = query, encode = "json"), 
                           type = "text/csv",
                           encoding = "UTF-8") %>%  as.data.frame()
    
    # Replacing "." with zeroes
    tax_income[tax_income=="."] <-0
    
    # Replacing ".." with zeroes
    tax_income[tax_income==".."] <-0
    
    # Replacing "NA" with zeroes
    tax_income[tax_income=="NA"] <-0
    
    rownames(tax_income) = tax_income$Näitaja
    tax_income$Näitaja = NULL
    
    for(i in colnames(tax_income)) {
      eval(parse(text = paste0("tax_income$`", i, "` = as.numeric(as.character(tax_income$`", i,"`))")))
    }

    # Add filename based on the selected table and new table name based on selected table
    eval(parse(text = paste0(tablename, "<- tax_income")))
    eval(parse(text = paste0("save(",tablename,", file = file.path(data_loc, '",tablename,".Rda'))")))
  }
}
# load_total_tax_revenue_data_f(last_year = 2020)


#' Function generates table of total tax revenues by tax type
#'
#' Function generates table of total tax revenues by tax type, units are euros
#' @returns 'taxes_total.Rda' data in matrix form that includes total tax revenue 
#' (excluding pension II and III pillar) by tax type and years
total_tax_revenue_f <- function(){
  
  # Read local and gov taxes
  load(file = paste0(data_loc,"/","rr01.Rda"))
  load(file = paste0(data_loc,"/","rr02.Rda"))
  
  # Tax categories
  tax_names = c("Füüsilise isiku tulumaks", "Ettevõtte tulumaks", "Sotsiaalmaks",
    "Töötuskindlustusmaksed", "Käibemaks", "Aktsiisid", "Muu (kõik ülejäänud kokku agregeeritud)")
  
  # Set years for taxes total table
  tax_years = colnames(rr01)

  # Generate new table
  taxes_total = matrix(rep(0, times=length(tax_names)*length(tax_years)), ncol=length(tax_years))
  colnames(taxes_total) = tax_years
  rownames(taxes_total) = tax_names
  
  # Fill in the new table with the data from RR01 and RR02 tables
  for (year in tax_years){
    taxes_total[1, year] = rr01["..füüsilise isiku tulumaks*",year]
    taxes_total[2, year] = rr01["..juriidilise isiku tulumaks",year]
    taxes_total[3, year] = rr01["..sotsiaalmaks",year]
    taxes_total[4, year] = rr01["..töötuskindlustusmaks",year]
    taxes_total[5, year] = rr01["Käibemaks",year]
    taxes_total[6, year] = rr01["Aktsiisimaks",year]
    taxes_total[7, year] = rr01["Maksud*",year]-rr01["Aktsiisimaks",year]-rr01["Käibemaks",year]-rr01["..töötuskindlustusmaks",year]-
      rr01["..sotsiaalmaks",year]-rr01["Tulumaks*",year]+rr02["Maksud",year]-
      rr02["Füüsilise isiku tulumaks",year] - rr01["..kogumispension",year] # II and III pension pillar will be excluded 
  }
  
  taxes_total = taxes_total %>%
    `*`(1000) # units were in 1000 euros, we will convert them in euros
  
  # Save the new table as taxes_total.Rda
  save(taxes_total,file = file.path(data_loc, "taxes_total.Rda"))
  
}
# total_tax_revenue_f()


#' Taxes by sector
#' 
#' Function reads in taxes_by_sector.xlsx file from  folder "2.data/custom_query_raw_data" and 
#' saves "taxes_by_sector.Rda"
#' Taxes by sector Excel table must meet the following:
#' •	Sheets that should be read in must be named “Koond [year]”, e.g. “Koond 2020”
#' •	Data unit is euros
#' •	First column in table must contain tax names: Füüsilise isiku tulumaks, Ettevõtte tulumaks,
#'  Sotsiaalmaks, Töötuskindlustusmaksed, Käibemaks, Aktsiisid, Muu (kõik ülejäänud kokku agregeeritud)
#' •	Table must contain all 63 sector’s data (first row as column titles)
#' It should be noted that we are not using the VAT data that is listed in this file - it is too
#' aggregated. The model uses "effective_tax_rates_f()"
#' 
#' @param years The list of years for which the taxes by sector are available in Excel file is available
#' @returns Data object 'taxes_by_sector.Rda' that holds a matrix of taxes income for each sector

taxes_by_sector_f = function(filename="taxes_by_sector.xlsx", years=c(2015,2020)){
  
  for (year in years) {
    
    year=as.character(year)
    eval(parse(text = paste0("taxes_by_sector_current <- as.data.frame(read_excel(file.path(data_loc, 'custom_query_raw_data', filename ),sheet = 'Koond ",year,"'))" )))
    rownames(taxes_by_sector_current) = taxes_by_sector_current[,1]
    taxes_by_sector_current$...1 <- NULL
  
    # Change values in table to numeric format, units are in euros
    for (tax in rownames(taxes_by_sector_current)) {
      for (sector in colnames(taxes_by_sector_current)) {
        taxes_by_sector_current[tax,sector]=as.numeric(taxes_by_sector_current[tax, sector])
      }
    }
    
    eval(parse(text = paste0("taxes_by_sector_",year," <- as.matrix(taxes_by_sector_current)")))
  }
  
  names = paste0("taxes_by_sector_", years, collapse = ",")
  eval(parse(text = paste0("save(", names , ", file = file.path(data_loc, 'taxes_by_sector.Rda'))")))
  
}

# taxes_by_sector_f()
  
#' VAT by sector (effective rates - depreciated)
#' 
#' Function reads in file "RM - KMD 2014_2021_5.xlsx" from folder '2.data/custom_query_raw_data' modifies 
#' it and saves results in two files "VAT_by_sector.Rda" and "VAT_turnover_by_sector.Rda" 
#' First includes data on VAT, second on turnover, that is basis for VAT calculation. Data unit is 1€.
#' NB! We use two VAT rates 9% and 20% for calculating VAT (source data does not include VAT by rates). 
#' If in the future some other rates #' will be used, this function must be modified.
#' 
#' NB! We do not use the data generated by this function - the model uses "effective_tax_rates_f()"
#' 
#' @param years The list of years for which the taxes by sector are available in Excel file is available
#' @returns Data object 'VAT_by_sector.Rda' that holds a matrix of taxes VAT for each sector and 'VAT_turnover_by_sector.Rda' that includes turnover that is basis for VAT calculation

VAT_by_sector_f = function(filename="RM - KMD 2014_2021_5.xlsx", years=c(2015:2020)){
  
  # Read KMD data
  KMD_data = read_excel(file.path(data_loc, "custom_query_raw_data", filename))
  names(KMD_data) = read_excel(file.path(wd, "2.data/custom_query_raw_data/RM - KMD 2014_2021_5.xlsx"), sheet = "Labels")$lyhike %>%
    enc2utf8()
  
  # Load dictionary
  dict_baas1 = read_excel(file.path(dict_loc, "dictionaries.xlsx")) %>%
    as.data.table() %>%
    .[, `Kood EMTAK2008 number` := as.numeric(`Kood EMTAK2008`)]%>%
    .[,.(`Kood EMTAK2008 number`, `Tegevusala tekst IO tabel`)]
  
  # Mergeing dictionary with data
  KMD_data = merge(KMD_data, dict_baas1, by.x = "EMTAK", by.y = "Kood EMTAK2008 number", all.x = T) %>%
    as.data.table() %>%
    .[, Tegevusala:=`Tegevusala tekst IO tabel`] %>%
    .[, `Tegevusala tekst IO tabel`:=NULL] %>%
    .[!is.na(Tegevusala)]
  
  # We are interested in following data:
  #' K20 - turnover that was taxed with 20% VAT
  #' K9 - turnover that was taxed with 9% VAT
  #' K0 - turnover that was taxed with 0% VAT (mainly export)
  #' MaksuvabaK - turnover that is not taxed with VAT (mainly domestic)
  #' KMimp - VAT calculated from imported goods
  #' KMsisend - VAT from intermediate demand that can be deducted from "KM" 
  
  cols_kmd = c("K20", "K9", "K0", "MaksuvabaK", "KMimp", "KMsisend")

  #NB! This is important! We use two VAT rates 9% and 20% for calculating VAT. If in the future some other rates
  # will be used, this part must be modified.
  
  KMD_data = KMD_data[, lapply(.SD, sum), by=.(Aasta, Tegevusala), .SDcols=cols_kmd] %>%
    .[, KM20:=K20*0.2] %>%
    .[, KM9:=K9*0.09] %>%
    .[, .(Aasta, Tegevusala, KM20, KM9, KMimp, KMsisend, K20, K9, K0, MaksuvabaK)]
    
  for (year in years) {
    var_order = unique(dict_baas1$`Tegevusala tekst IO tabel`)        
    
    KMD_data_abi = KMD_data %>%
      .[Aasta == year] %>%
      .[,Aasta:=NULL] 

    # For some years some sectors can be missing
    KMD_data_abi2 = data.table(Tegevusala = var_order) %>%
      merge(.,KMD_data_abi, by = "Tegevusala", all.x=T) %>%
      melt(., id.var = "Tegevusala") %>%
      dcast(., variable~Tegevusala) %>%
      .[, variable:=NULL]
    
    # In cases where some activities did not have data on VAT we assume this to be 0
    KMD_data_abi2[is.na(KMD_data_abi2)] <- 0
      
    # Ajust order and turn into matrix form
    KMD_data_abi = KMD_data_abi2 %>%
      as.matrix()%>%
      .[, var_order]
    
    rm(KMD_data_abi2)
    
    # Finally correct row names
    rownames(KMD_data_abi) = c("Käibemaks 20%", "Käibemaks 9%", "Impordikäibemaks", "Sisendkäibemaks", "20% käibemaksuga käive", "9% käibemaksuga käive", "0% käibemaksuga käive", "Maksuvaba käive")
    
    KMD_data_abi_tax = KMD_data_abi[c("Käibemaks 20%", "Käibemaks 9%", "Impordikäibemaks", "Sisendkäibemaks"),]
    KMD_data_abi_turnover = KMD_data_abi[c("20% käibemaksuga käive", "9% käibemaksuga käive", "0% käibemaksuga käive", "Maksuvaba käive"),]
    
    eval(parse(text = paste0("VAT_by_sector_",year," <- KMD_data_abi_tax")))
    eval(parse(text = paste0("VAT_turnover_by_sector_",year," <- KMD_data_abi_turnover")))
  }
  
  names = paste0("VAT_by_sector_", years, collapse = ",")
  eval(parse(text = paste0("save(", names , ", file = file.path(data_loc, 'VAT_by_sector.Rda'))")))
  
  names = paste0("VAT_turnover_by_sector_", years, collapse = ",")
  eval(parse(text = paste0("save(", names , ", file = file.path(data_loc, 'VAT_turnover_by_sector.Rda'))")))
}

# VAT_by_sector_f()



#' Effective tax rates by tax base
#' 
#' Function calculates effective tax rates for all tax types
#' supported by the model. The model distinguishes following groups of taxes:
#' "Füüsilise isiku tulumaks",  "Ettevõtte tulumaks", "Sotsiaalmaks",
#' "Töötuskindlustusmaksed", "Käibemaks", "Aktsiisid", "Muu (kõik ülejäänud kokku agregeeritud)" 
#' @param years The list of years for what taxbase, tax revenue by sectors and total tax revenue is available
#' @returns Data object 'effective_tax_rates.Rda' that holds a matrix of effective tax rates (tax by sector) 
#' for years start_year-end_year
effective_tax_rates_f = function(years=c(2020), year_IO = 2017){
  
  # Load total tax revenue (NB! in thousands) and tax revenue by sector (NB! Check units! In thousands)
  load(file = file.path(data_loc,"taxes_total.Rda"))
  load(file = file.path(data_loc,"taxes_by_sector.Rda"))
  
  # Load tax base (NB! in millions)
  load(file.path(data_loc,"tax_base.Rda"))
  
  tax_names =  rownames(taxes_total)
  
  # Map taxes and bases
  tax_base_map = matrix(rep("0", times=length(tax_names)*2), ncol=2)
  colnames(tax_base_map) = c("Baas","Base") 
  rownames(tax_base_map) = tax_names
  tax_base_map[1, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[2, "Baas"] = "Lisandväärtus"
  tax_base_map[3, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[4, "Baas"] = "Hüvitised töötajatele"
  tax_base_map[5, "Baas"] = "Lisandväärtus"
  tax_base_map[6, "Baas"] = "Toodang"
  tax_base_map[7, "Baas"] = "Lisandväärtus"
  
  tax_base_map[1, "Base"] = "labour_cost"
  tax_base_map[2, "Base"] = "value_added"
  tax_base_map[3, "Base"] = "labour_cost"
  tax_base_map[4, "Base"] = "labour_cost"
  tax_base_map[5, "Base"] = "value_added"
  tax_base_map[6, "Base"] = "total_production"
  tax_base_map[7, "Base"] = "value_added"
  
  for(year in years) {

  # Generate table of adjusted taxes by sector
    
    # Get the table for the chosen year
    taxes_by_sector_current=get(paste0("taxes_by_sector_",as.character(year)))
    
    # Remove VAT, as for that we need a special solution
    names_abi = rownames(taxes_by_sector_current)[!(rownames(taxes_by_sector_current) %in% "Käibemaks")]
    taxes_by_sector_current = taxes_by_sector_current[names_abi,]
    
    # Add new table for sector weights
    taxes_by_sector_weights=taxes_by_sector_current
    # Empty the weights table
    taxes_by_sector_weights[] <- 0
    
    # Fill the table
    for (tax in rownames(taxes_by_sector_current)) {
      # Find current tax weights for all sectors
      for (sector in colnames(taxes_by_sector_current)) {
        taxes_by_sector_weights[tax,sector]=taxes_by_sector_current[tax, sector]/sum(taxes_by_sector_current[tax, ])
      }
    }
    
    # Add new table for adjusted taxes by sector
    taxes_by_sector_adjusted=taxes_by_sector_current
    taxes_by_sector_adjusted[] <- 0
    # Fill the table
    for (tax in rownames(taxes_by_sector_adjusted)) {
      # Multiply total taxes with weights to get adjusted data by sector
      for (sector in colnames(taxes_by_sector_adjusted)) {
        taxes_by_sector_adjusted[tax,sector]=taxes_by_sector_weights[tax, sector]*taxes_total[tax,as.character(year)]
      }
    }
    
    
  # Generate table of effective tax rates by sector
    tax_base_current=get(paste0("tax_base_",as.character(year)))
    
    # Add new table for effective tax rates by sectors
    effective_taxes_by_sector=taxes_by_sector_current
    effective_taxes_by_sector[] <- 0
    
    # Fill effective tax rates table
    for (tax in rownames(effective_taxes_by_sector)){
      for (sector in colnames(effective_taxes_by_sector)){
        effective_taxes_by_sector[tax,sector] = taxes_by_sector_adjusted[tax, sector]/tax_base_current[tax_base_map[tax,1],sector]
      }
    }
    
  # Now a solution for VAT
  #' VAT has several components: 
  #' 1) VAT obligation that rises from selling stuff domestically. There are two different rates: 
  #'    1a): regular rate - 20% 
  #'    1b): special rate - 9%
  #' 2) VAT obligation that rises from importing
  #' 3) VAT that is paid from intermediate consumption and can thus be deducted from the first two  
  #'
  #' If FD increases because of exports
  #' 
  #' If FD increases due to exports, then its impact on VAT revenue is ZERO. Why (data from EMTA shows clearly negative VAT revenue 
  #' for several sectors)? Well, the starting point is increase in final demand for export. This triggers production for intermediate
  #' products. The VAT for those products will be paid to EMTA, but will be returned to exporter. As exporter will not pay VAT for its
  #' value added, the net outcome for EMTA is zero. The end result would be different, if an entrepreneur that was formerly oriented
  #' on local market would decide to become exporter. In this case, the EMTA will actually suffer losses.
  #' 
  #' All in all - the negative VAT revenues from different sectors are nothing more than result of messy redistribution of VAT obligations
  #' between different sectors. No matter what the shares of domestic and export demand in overall FD are, the total VAT revenue for EMTA can 
  #' never be below zero. In the end - those who sell their products/services on local market pay VAT on their value added (either 20%, 9% 
  #' or (small share are also locally VAT exempt) and those who export don't pay VAT neither on their VA nor on their inputs. 
  #'  
  #' 
  #' FD increases because of local demand
  #' For local demand, the approach is following:
  #' For each sector we estimate: 
  #' a) The share of regular, special and VAT exempt turnover in non-export turnover
  #' b) The share of value added associated with domestic consumption - this is estimated by multiplying the VA multipliers by
  #' the local final demand for local goods and services. Then we assume that this value added in each sector is taxed with 
  #' regular and spacial VAT rates respective to share listed in (a).
  #' c) Finally - VAT is also paid on domestic final consumption of imports. This is also taxed according to the weights calculated in (a)  
  #' The  total of actual return of valued added tax is divided by the sum of this "theoretical" value added. Resulting coefficient is
  #' used to scale down the regular and spacial rates in order to obtain (pseudo) effective VAT rates. NB! These rates are the same for all sectors. 
  #'         

    load(file.path(data_loc, "VAT_turnover_by_sector.Rda"))
    VAT_turnover_by_sector =  get(paste0("VAT_turnover_by_sector_",as.character(year)))
    VAT_weights = VAT_turnover_by_sector %>%
      t() %>%
      as.data.table() %>%
      .[, Tegevusala:= colnames(VAT_turnover_by_sector)] %>%
      .[, `Kokku 20%, 9% ja MVK`:= `20% käibemaksuga käive`+ `9% käibemaksuga käive` + `Maksuvaba käive`] %>%
      .[, `:=`(`Tavamääraga käibe osakaal kodumaises käibes` =`20% käibemaksuga käive`/`Kokku 20%, 9% ja MVK`,
               `Erimääraga käibe osakaal kodumaises käibes` =`9% käibemaksuga käive`/`Kokku 20%, 9% ja MVK`,
               `Maksuvaba käibe osakaal kodumaises käibes` = `Maksuvaba käive`/`Kokku 20%, 9% ja MVK`)] %>%
      .[,.(Tegevusala, `Tavamääraga käibe osakaal kodumaises käibes`, `Erimääraga käibe osakaal kodumaises käibes`, `Maksuvaba käibe osakaal kodumaises käibes`)] 
    
    
    load(file.path(data_loc, "tax_base.Rda"))
    tax_base =  get(paste0("tax_base_",as.character(year)))
    
    load(file.path(data_loc, "ibi_IO_elements.Rda"))
    
    # Function divides change in inventory  between other components of final demand 
    inventory_correction = function(data) {
      inventory_correction = data %>%
        as.data.table() %>%
        .[, Total := `Kodumajapidamiste lõpptarbimiskulutused`+`KTKTI lõpptarbimiskulutused`+`Valitsemissektori lõpptarbimiskulutused`+`Kapitali kogumahutus põhivarasse ja väärisesemed`+ `Kaupade ja teenuste eksport FOB`] %>%
        .[, `:=`(`Kodumajapidamiste lõpptarbimiskulutused` = `Kodumajapidamiste lõpptarbimiskulutused`/Total, 
                 `KTKTI lõpptarbimiskulutused` = `KTKTI lõpptarbimiskulutused`/Total,
                 `Valitsemissektori lõpptarbimiskulutused` = `Valitsemissektori lõpptarbimiskulutused`/Total,
                 `Kapitali kogumahutus põhivarasse ja väärisesemed` = `Kapitali kogumahutus põhivarasse ja väärisesemed`/Total,
                 `Varude muutus` = 0,
                 `Kaupade ja teenuste eksport FOB` = `Kaupade ja teenuste eksport FOB`/Total)] %>%
        .[, Total :=NULL]
      
      inventory_correction[is.na(inventory_correction)] = 0
      
      inventory_correction = (inventory_correction * data[, "Varude muutus"]) %>%
        as.matrix()
      
      result = (data + inventory_correction) %>%
        as.data.table() %>%
        .[, `Varude muutus`:=NULL] %>%
        as.matrix()
      
      rownames(result) = rownames(data)
      
      return(result)  
    }
    
    Fm_corrected = inventory_correction(Fm)
    Fd_corrected = inventory_correction(Fd)
    
    # As we are using tax figures from different year than IO tables, this as to be scaled up
    # We use nominal growth of value added
    
    VA_year = sum(get(paste0("tax_base_", as.character(year)))["Lisandväärtus", ])
    VA_year_IO = sum(get(paste0("tax_base_", as.character(year_IO)))["Lisandväärtus", ])
    cor_coef =  VA_year/VA_year_IO

    
    # Local final demand of imports (excluding exports)
    FD_imp = (rowSums(Fm_corrected) - Fm_corrected[, "Kaupade ja teenuste eksport FOB"]) %>%
      `*` (cor_coef)
    FD_imp[FD_imp<0] = 0 

    # Local final demand of domestic goods/services
    FD_dom = (rowSums(Fd_corrected) - Fd_corrected[, "Kaupade ja teenuste eksport FOB"]) %>%
      `*` (cor_coef)
    FD_dom[FD_dom<0] = 0
    
    # Value added multipliers are already prepared, so we can use the function "ibi_IO_multiplyers"
    multipliyers_va = ibi_IO_multiplyers(type = "I", indicator="value_added", matrix_form = "no", hh_savings_rate = 0) 
    
    VAT_weights_with_tax_base = VAT_weights %>%
      .[, `:=`(`Kohalik impordi lõpptarbimine`=FD_imp,
               `Kohalik kodumaise toodangu lõpptarbimine` = FD_dom)] %>%
      .[, `Kohaliku kodumaise toodangu LT lisandväärtus` := multipliyers_va * FD_dom] %>%
      .[, `Teoreetiline käibemaks LV-lt` := (0.2*(`Tavamääraga käibe osakaal kodumaises käibes`)+0.09*`Erimääraga käibe osakaal kodumaises käibes`)*`Kohaliku kodumaise toodangu LT lisandväärtus`] %>%
      .[, `Teoreetiline käibemaks impordilt` := (0.2*(`Tavamääraga käibe osakaal kodumaises käibes`)+0.09*`Erimääraga käibe osakaal kodumaises käibes`)*`Kohalik impordi lõpptarbimine`] %>%
      .[, `Teoreetiline käibemaks` := `Teoreetiline käibemaks LV-lt` + `Teoreetiline käibemaks impordilt`]
    
    
    load(file.path(data_loc, "taxes_total.Rda"))
    coef_VAT = VAT_weights_with_tax_base %>%
      .[, .(`Teoreetiline käibemaks` = sum(`Teoreetiline käibemaks`))] %>%
      .[, `Empiiriline käibemaks` := taxes_total["Käibemaks", as.character(year)]] %>%
      .[, .(Kohanduskoefitsient= `Empiiriline käibemaks`/`Teoreetiline käibemaks`)] %>%
      .[, Kohanduskoefitsient]
    
    effective_VAT_components_by_sector = matrix(nrow = 8, ncol = 63)
    rownames(effective_VAT_components_by_sector) = c("Efektiivne KM - tavamäär", 
                                                     "Efektiivne KM - erimäär", 
                                                     "Tavamääraga käibe osakaal kodumaises käibes", 
                                                     "Erimääraga käibe osakaal kodumaises käibes", 
                                                     "Kohalik impordi lõpptarbimine",
                                                     "Kohalik kodumaise toodangu lõpptarbimine",
                                                     "Kohaliku kodumaise toodangu LT lisandväärtus",
                                                     "Efektiivne KM - keskmine")
    
    colnames(effective_VAT_components_by_sector) =  colnames(VAT_turnover_by_sector)
    effective_VAT_components_by_sector["Efektiivne KM - tavamäär", ] = 0.2*coef_VAT 
    effective_VAT_components_by_sector["Efektiivne KM - erimäär", ] = 0.09*coef_VAT 
    effective_VAT_components_by_sector["Tavamääraga käibe osakaal kodumaises käibes", ] = VAT_weights$`Tavamääraga käibe osakaal kodumaises käibes`
    effective_VAT_components_by_sector["Erimääraga käibe osakaal kodumaises käibes", ] = VAT_weights$`Erimääraga käibe osakaal kodumaises käibes`
    effective_VAT_components_by_sector["Kohalik impordi lõpptarbimine", ] = VAT_weights_with_tax_base$`Kohalik impordi lõpptarbimine`
    effective_VAT_components_by_sector["Kohalik kodumaise toodangu lõpptarbimine", ] = VAT_weights_with_tax_base$`Kohalik kodumaise toodangu lõpptarbimine`
    effective_VAT_components_by_sector["Kohaliku kodumaise toodangu LT lisandväärtus", ] = VAT_weights_with_tax_base$`Kohaliku kodumaise toodangu LT lisandväärtus`
  
    
    abi = effective_VAT_components_by_sector["Efektiivne KM - erimäär", ] * effective_VAT_components_by_sector["Erimääraga käibe osakaal kodumaises käibes", ]
    abi1 = effective_VAT_components_by_sector["Efektiivne KM - tavamäär", ] * effective_VAT_components_by_sector["Tavamääraga käibe osakaal kodumaises käibes", ]
    effective_VAT_components_by_sector["Efektiivne KM - keskmine", ] = abi + abi1
    rm(abi, abi1)
      
    # Final adjustments
    # consumption of domestic households should have 0 rate for all taxes (We do not model them, but as they are part of IO table, we are keeping them around)
    effective_taxes_by_sector[, enc2utf8("Kodumajapidamised  tööandjana; oma tarbeks mõeldud kaupade tootmine ja teenuste osutamine")] <- 0
    effective_VAT_components_by_sector[, enc2utf8("Kodumajapidamised  tööandjana; oma tarbeks mõeldud kaupade tootmine ja teenuste osutamine")] <-0
    # Save the results
    eval(parse(text = paste0("effective_taxes_by_sector_", year, "<- effective_taxes_by_sector")))
    eval(parse(text = paste0("effective_VAT_components_by_sector_", year, "<- effective_VAT_components_by_sector")))
  }
  
  # Save effective tax rates in single effective_tax_rates.Rda file
  names1 = paste0("effective_taxes_by_sector_", years, collapse = ",")
  names2 = paste0("effective_VAT_components_by_sector_", years, collapse = ",")
  
  eval(parse(text = paste0("save(", names1, ", file = file.path(data_loc, 'effective_tax_rates.Rda'))")))
  eval(parse(text = paste0("save(", names2, ", file = file.path(data_loc, 'effective_VAT_components.Rda'))")))
}
# effective_tax_rates_f()


#' Säästumäär
#' 
#' Funktsioon tõmbab Statistikaametist alla kodumajapidamiste sääsumäära
#' andmed ja salvestab need data_loc kataloogi.
#' @returns Data object 'household_savings_rate.Rda'that hold household savings rate info
#' in long format.  
savings_rates_f = function(){
  
  # Tõmbame statistikaametist tabeli SN10: SÄÄSTVA ARENGU NÄITAJAD ÜRO EESMÄRKIDE PÕHJAL
  query = '{"query": [],"response": {"format": "csv"}}'
  savings_rate <- content(POST("https://andmed.stat.ee/api/v1/et/stat/SN10",  body = query, encode = "json"), 
                          type = "text/csv",
                          encoding = "UTF-8") %>%
    as.data.table() %>%
    .[`Näitaja` == "1.3. Kodumajapidamiste säästumäär, %"] %>%
    melt(., id.vars = "Näitaja", value.name = "value", variable.name = "Aasta") %>%
    .[, `Näitaja` := "Kodumajapidamiste säästumäär",] %>%
    .[, value := as.numeric(value)/100]
  
  save(savings_rate, file = file.path(data_loc, 'household_savings_rate.Rda'))
}

# savings_rates_f()
