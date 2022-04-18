
# Functions that use OECD data -----------------------------------------------

#' Export shares of Estonian industries by destination country and industry
#' 
#' Function will create export shares for each Estonian industry
#' using OECD Inter-Country Input-Output (ICIO) Tables, 2018 edition
#' The destination aggregation level "country-industry"
#' Result is in data.table format and is saved to "2.data/export_module"
calculate_oecd_ibci_export_shares <- function(){
  oecd <- fread(file.path(data_loc, "export_module/ICIO2018_2015.CSV"))
  
  # Ad hoc solution to reduce OECD data file size - it's over git limit
  #oecd = oecd %>% filter(str_detect(V1, "EST"))
  #fwrite(oecd,file.path(data_loc, "export_module/ICIO2018_2015.CSV"))
  
  dictionaries <- read_excel(file.path(dict_loc, "dictionaries.xlsx"))
  colnames(dictionaries) = tolower(make.names(colnames(dictionaries)))
  
  oecd <- oecd %>% 
    rename(origin = V1)
  oecd_countries <- sort(setdiff(unique(substr(oecd$origin,1,3)), c("VAL", "OUT", "ROW", "CN1", "CN2", "MX1", "MX2")))
  
  # Take only exports from Estonia
  # Drop domestic transactions
  est_exp <- oecd %>% 
    tidyr::separate(origin, 
                    into = c("origin_country", "origin_industry"), 
                    sep = "_",
                    remove = TRUE,
                    fill = "right") %>% 
    filter(origin_country == "EST",
           origin_industry != "TAXSUB") %>% 
    dplyr::select(origin_industry, starts_with(c(oecd_countries,"ROW"))) %>% 
    dplyr::select(-starts_with(c("EST_"))) %>% 
    as.data.frame()
  
  # Final demand
  est_exp_final_demand <- est_exp %>% 
    dplyr::select(origin_industry, contains(c("HFCE","NPISH", "GGFC","GFCF","INVNT","P33"))) %>% 
    tidyr::gather(key = "destination",
                  value = "amount",
                  -origin_industry) %>% 
    filter(!is.na(amount)) %>% 
    mutate(destination = paste0(substr(destination, 1,3),"_FD")) %>% 
    group_by(origin_industry,destination) %>% 
    summarise(amount = sum(amount),
              .groups = "drop") 
  
  # Intermediates use
  est_exp_intermediate <- est_exp %>% 
    dplyr::select(-contains(c("HFCE","NPISH", "GGFC","GFCF","INVNT","P33"))) %>% 
    tidyr::gather(key = "destination",
                  value = "amount",
                  -origin_industry) %>% 
    filter(!is.na(amount))
  
  # Combine FD and IU back together
  est_exp <- bind_rows(est_exp_intermediate,
                       est_exp_final_demand)
  
  # Change OECD origin_industry aggregation to 'Kood IO tabel'
  ## Ex 1: combine 05T06, 07T08 and 09 to 05-09 
  ## Ex 2: split 01T03 to 01, 02 and 03.
  ## Ex 3: split 69T82 to 69-70, 71, 72, 73, 74-75, 77, 78, 79, 80-82; (76 is not used)
  
  # In cases like Ex2 and Ex 3 the corresponding rows are duplicated. 
  ## Note that "amount" is not meaningful anymore but because we are interested in export shares, it is not a problem
  ### at least if no mismatch between aggregation intervals exists (check is applied)
  ## assumption is that component industries have the same export structure
  
  origin_industries <- est_exp %>% 
    dplyr::select(origin_industry) %>% 
    unique() %>% 
    tidyr::separate(origin_industry, 
                    into = c("start", "end"), 
                    sep = "T",
                    remove = FALSE,
                    fill = "right") %>% 
    mutate(end = ifelse(is.na(end), start, end),
           start = as.numeric(start),
           end = as.numeric(end))
  
  origin_industries_list <- list()
  for (i in seq_len(nrow(origin_industries))){
    origin_industries_list[[i]] = data.frame(origin_industry_raw = origin_industries$origin_industry[i],
                                             origin_industry = seq(from = origin_industries$start[i],
                                                                   to = origin_industries$end[i],
                                                                   by = 1),
                                                        stringsAsFactors = FALSE)
  }
  
  origin_industries <- bind_rows(origin_industries_list)
  origin_industries <- origin_industries %>% 
    mutate(origin_industry = ifelse(origin_industry < 10, paste0("0",origin_industry), origin_industry))
  
  # Add 'Kood IO tabel'
  origin_industries <- origin_industries %>% 
    left_join(dictionaries %>% 
                dplyr::select(kood.emtak2008, kood.io.tabel, tegevusala.tekst.io.tabel),
              by = c("origin_industry" = "kood.emtak2008")) %>% 
    filter(!is.na(kood.io.tabel))
  
  # Check mismatch between aggregation intervals 
  tmp <- origin_industries %>% 
  tidyr::separate(origin_industry_raw, 
                  into = c("start_origin_industry_raw", "end_origin_industry_raw"), 
                  sep = "T",
                  remove = FALSE,
                  fill = "right") %>% 
    mutate(end_origin_industry_raw = ifelse(is.na(end_origin_industry_raw), start_origin_industry_raw, end_origin_industry_raw),
           start_origin_industry_raw = as.numeric(start_origin_industry_raw),
           end_origin_industry_raw = as.numeric(end_origin_industry_raw)) %>% 
    tidyr::separate(kood.io.tabel, 
                    into = c("start_kood.io.tabel", "end_kood.io.tabel"), 
                    sep = "-",
                    remove = FALSE,
                    fill = "right") %>% 
    mutate(end_kood.io.tabel = ifelse(is.na(end_kood.io.tabel), start_kood.io.tabel, end_kood.io.tabel),
           start_kood.io.tabel = as.numeric(start_kood.io.tabel),
           end_kood.io.tabel = as.numeric(end_kood.io.tabel)) %>% 
    filter(!(start_origin_industry_raw >= start_kood.io.tabel & end_origin_industry_raw <= end_kood.io.tabel |
               start_origin_industry_raw <= start_kood.io.tabel & end_origin_industry_raw >= end_kood.io.tabel))
  
  if (nrow(tmp) > 0){
    stop("mismatch between aggregation intervals")
  }
  
  origin_industries <- origin_industries %>% 
    dplyr::select(origin_industry_raw, kood.io.tabel, tegevusala.tekst.io.tabel) %>% 
    unique()

  est_exp <- left_join(origin_industries,
                       est_exp,
                       by = c("origin_industry_raw" = "origin_industry")) %>%
    as.data.table()


  est_exp <- est_exp %>% 
    group_by(kood.io.tabel, tegevusala.tekst.io.tabel, destination) %>% 
    summarise(amount = sum(amount),
              .groups = "drop") %>% 
    group_by(kood.io.tabel, tegevusala.tekst.io.tabel) %>% 
    mutate(total_exp = sum(amount)) %>% 
    ungroup() %>% 
    filter(total_exp > 0) %>% # industry 97-98 (Private households with employed persons) is filtered out
    mutate(export_share = amount/total_exp) 
  
  est_exp <- est_exp %>% 
    tidyr::separate(destination, 
                    into = c("destination_country", "destination_industry"), 
                    sep = "_",
                    remove = TRUE,
                    fill = "right")
  
  est_exp <- est_exp %>% 
    dplyr::select(-amount, -total_exp)
  
  # Add textual description to destination_industry codes
  
  destination_industries <- est_exp %>% 
    dplyr::select(destination_industry) %>% 
    filter(destination_industry != "FD") %>% 
    unique() %>% 
    tidyr::separate(destination_industry, 
                    into = c("start", "end"), 
                    sep = "T",
                    remove = FALSE,
                    fill = "right") %>% 
    mutate(end = ifelse(is.na(end), start, end),
           start = as.numeric(start),
           end = as.numeric(end))
  
  destination_industries_list <- list()
  for (i in seq_len(nrow(destination_industries))){
    destination_industries_list[[i]] = data.frame(destination_industry_raw = destination_industries$destination_industry[i],
                                                  destination_industry = seq(from = destination_industries$start[i],
                                                                             to = destination_industries$end[i],
                                                                             by = 1),
                                             stringsAsFactors = FALSE)
  }
  
  destination_industries <- bind_rows(destination_industries_list)
  destination_industries <- destination_industries %>% 
    mutate(destination_industry = ifelse(destination_industry < 10, paste0("0",destination_industry), destination_industry))
  
  # Add 'Kood IO tabel'
  destination_industries <- destination_industries %>% 
    left_join(dictionaries,
              by = c("destination_industry" = "kood.emtak2008")) %>% 
    filter(!is.na(kood.io.tabel))  %>% # industry 76 is not in code list
    group_by(destination_industry_raw) %>% 
    summarise(destination_industry_text = paste(tegevusala.tekst.emtak2008, collapse = "; "),
              .groups = "drop")
  
  est_exp <- est_exp %>% 
    left_join(destination_industries,
              by = c("destination_industry" = "destination_industry_raw"))
  est_exp <- est_exp %>% 
    mutate(destination_industry_text = ifelse(destination_industry == "FD", "Lõpptarbimine", destination_industry_text))
  
  # Final formatting
  
  est_exp <- est_exp %>% 
    mutate(destination_industry = str_replace(destination_industry, "T","-")) %>% 
    rename(origin_industry = kood.io.tabel,
           origin_industry_text = tegevusala.tekst.io.tabel) %>% 
    dplyr::select(origin_industry, origin_industry_text, destination_country, destination_industry, destination_industry_text, export_share)
  
  
  ibci_export_shares <- est_exp %>% 
    as.data.table()
  
  save(ibci_export_shares, file = file.path(data_loc, "export_module/ibci_export_shares.Rda"))
}

#' Export shares of Estonian industries by destination country
#' 
#' Function will create export shares for each Estonian industry
#' using OECD Inter-Country Input-Output (ICIO) Tables, 2018 edition
#' The destination aggregation is on country level
#' Output from calculate_oecd_ibci_export_shares() is used
#' Result is in data.table format and is saved to "2.data/export_module"
calculate_oecd_ibc_export_shares <- function(){
  if (!file.exists(file.path(data_loc, "export_module/ibci_export_shares.Rda"))){
    calculate_oecd_ibci_export_shares()
  }
  load(file.path(data_loc, "export_module/ibci_export_shares.Rda"))
  
  ibc_export_shares <- ibci_export_shares %>% 
    group_by(origin_industry, origin_industry_text, destination_country) %>% 
    summarise(export_share = sum(export_share),
              .groups = "drop") %>% 
    as.data.table()
  
  save(ibc_export_shares, file = file.path(data_loc, "export_module/ibc_export_shares.Rda"))
}


#' Find k most important export destinations for each industry
#' 
#' Function will find k most important export destinations for each industry
#' Other destinations are aggregated as ROW (rest of the world)
#' Result is returned in data.table format
#' @param k integer, top k parameter
#' @param destination_agr the level of destination aggregation
#'  "ci" country-industry level
#'  "c" country level

find_topK_export_destinations <- function(k, destination_agr){
  
  if (!(destination_agr %in% c("ci", "c"))){
    stop("parameter destination_agr misspecified")
  }
  
  if (destination_agr == "ci"){
    if (!file.exists(file.path(data_loc, "export_module/ibci_export_shares.Rda"))){
      calculate_oecd_ibci_export_shares()
    }
    load(file.path(data_loc, "export_module/ibci_export_shares.Rda"))
    topK_destinations <- ibci_export_shares
  }
  
  if (destination_agr == "c"){
    if (!file.exists(file.path(data_loc, "export_module/ibc_export_shares.Rda"))){
      calculate_oecd_ibc_export_shares()
    }
    load(file.path(data_loc, "export_module/ibc_export_shares.Rda"))
    topK_destinations <- ibc_export_shares
  }
  
  topK_destinations <- topK_destinations %>% 
    arrange(origin_industry, destination_country == "ROW", desc(export_share)) %>% 
    group_by(origin_industry) %>% 
    mutate(row_number = row_number()) %>% 
    ungroup()
  
  topK_destinations <- topK_destinations %>% 
    filter(row_number <= k) %>% 
    dplyr::select(-row_number)
  
  ROW_destinations<- topK_destinations %>% 
    group_by(origin_industry, origin_industry_text) %>% 
    summarise(export_share = 1 - sum(export_share),
              .groups = "drop") %>% 
    mutate(destination_country = "ROW")
  
  topK_destinations <- bind_rows(topK_destinations,
                                 ROW_destinations)
  topK_destinations <- topK_destinations %>% 
    arrange(origin_industry, destination_country == "ROW", desc(export_share)) %>% 
    as.data.table()
    
  return(topK_destinations)
}


# Functions that are using ESA data ------------------------------------------------

#' Load export data from Statistics Estonia
#' 
#' Function fetches data from Statistics Estonia and saves it
#' in folder "2.data/export_module".
#' Data are stored in data.table format, the file name is:
#' -"export_baseline.Rda"
#' @param year The year for which the data should be fetched, defaults to 2019
load_esa_export_data <- function(year=2019){
  
  # The query in json format 
  query = paste0('{"query": [{"code": "Aasta","selection": {"filter": "item","values": ["', 
                 year, 
                 '"]}},
                 {"code": "Kaubavoog","selection": {"filter": "item","values": ["E"]}},
                 {"code": "Tegevusala (EMTAK 2008)","selection": {"filter": "all","values": ["*"]}}
                 ],"response": {"format": "csv"}}')
  
  # Supply table at basic prices
  export_baseline_vk02 <- content(POST("https://andmed.stat.ee/api/v1/et/stat/VK02",  body = query, encode = "json"), 
                             type = "text/csv",
                             encoding = "UTF-8") %>%
    as.data.table() 
  
  export_baseline_vk02 <- export_baseline_vk02 %>% 
    mutate(industry_code = str_extract(`Tegevusala (EMTAK 2008)`, "\\d*")) %>% 
    filter(industry_code != "", !is.na(industry_code)) %>% 
    dplyr::select(industry_code, export_amount = `VK02: VÄLISKAUBANDUS`) %>% 
    #mutate(export_amount = export_amount / 12) %>%  # average monthly amount # The results are devided by 12 in final step (before returning results) if intervall is "kuu" 
    as.data.table()
  
  query = paste0('{"query": [{"code": "Vaatlusperiood","selection": {"filter": "item","values": ["', 
                 year, 
                 '"]}},
                 {"code": "Voog","selection": {"filter": "item","values": ["EXP"]}},
                 {"code": "Tegevusala","selection": {"filter": "all","values": ["*"]}}
                 ],"response": {"format": "csv"}}')
  
  # Supply table at basic prices
  export_baseline_vkt10 <- content(POST("https://andmed.stat.ee/api/v1/et/stat/VKT10",  body = query, encode = "json"), 
                                  type = "text/csv",
                                  encoding = "UTF-8") %>%
    as.data.table() 
  
  export_baseline_vkt10 <- export_baseline_vkt10 %>% 
    filter(Tegevusala != "TOTAL Kokku – kõik tegevusalad",
           Tegevusala != "NAL Määramata")
  
  export_baseline_vkt10$industry_text_vkt = str_split_fixed(export_baseline_vkt10$Tegevusala, " ", n = 2)[,2]
  suppressWarnings({
    export_baseline_vkt10$export_amount = as.numeric(export_baseline_vkt10[["2019 Eksport"]]) * 10**6
    
  })  
  
  export_baseline_vkt10 <- export_baseline_vkt10 %>% 
    group_by(industry_text_vkt) %>% 
    mutate(n = n(),
           i= row_number()) %>% 
    ungroup() %>% 
    filter(i == 1) # Drop rows that are "duplicated" 
  
  export_baseline_vkt10 <- export_baseline_vkt10 %>% 
    dplyr::select(industry_text_vkt, export_amount) %>% 
    #mutate(export_amount = export_amount / 12) %>%  # average monthly amount # The results are devided by 12 in final step (before returning results) if intervall is "kuu" 
    as.data.table()
    

  # Add aggregation
  dictionaries <- read_excel(file.path(dict_loc, "dictionaries.xlsx"))
  colnames(dictionaries) = tolower(make.names(colnames(dictionaries)))
  
  export_baseline_vk02 <- export_baseline_vk02 %>% 
    left_join(dictionaries,
              by = c("industry_code" = "kood.emtak2008"))
  
  export_baseline_vk02 <- export_baseline_vk02 %>% 
    group_by(kood.io.tabel, tegevusala.tekst.io.tabel) %>% 
    summarise(export_amount = sum(export_amount),
              .groups = "drop") %>% 
    rename(industry_code = kood.io.tabel,
           industry_text = tegevusala.tekst.io.tabel)
  
  export_baseline_vkt10 <- export_baseline_vkt10 %>% 
    left_join(dictionaries,
              by = c("industry_text_vkt" = "tegevusala.tekst.emtak2008"))
  
  export_baseline_vkt10 <- export_baseline_vkt10 %>% 
    filter(!is.na(kood.io.tabel),
           !is.na(export_amount)) %>% 
    group_by(kood.io.tabel, tegevusala.tekst.io.tabel) %>% 
    summarise(export_amount = sum(export_amount),
              .groups = "drop") %>% 
    rename(industry_code = kood.io.tabel,
           industry_text = tegevusala.tekst.io.tabel)
  
  
  export_baseline <- full_join(export_baseline_vk02 %>% 
                                 rename(goods = export_amount),
                               export_baseline_vkt10 %>% 
                                 rename(services = export_amount),
                               by = c("industry_code", "industry_text"))
  export_baseline$services[is.na(export_baseline$services)] = 0
  
  export_baseline <- export_baseline %>% 
    mutate(goods_and_services = goods + services) %>% 
    gather(key = "type", value = "export_amount", -industry_code, -industry_text)
  
    
  save(export_baseline, file = file.path(data_loc, "export_module/export_baseline.Rda"))
  
}


# UI functions ------------------------------------------------------------

#' Create user input table
#' 
#' Function will create a xlsx file with empty input cells 
#' First sheet is topK sheet with dimensions (62 x k) rows and nr of scenarios columns
#' Other sheets:
#' If destination_agr is country-industry (ci) then for each country is a separate sheet with nr of industries rows and nr of scenarios columns
#' If destination_agr is country (c) then all countries are on sheet countries with nr of countries rows and nr of scenarios columns
#' 
#' @param k integer, top k parameter, default 5
#' @param destination_agr the level of destination aggregation. Default "ci"
#'  "ci" country-industry level
#'  "c" country level
#' @param scenarios vector with names of scenarios. If missing, monthly timeseries from period_start to period_end are taken as scenarios
#' @param period_start date from which to start counting, start of the assessment period. Ignored, if param scenarios is specified. Default is "2029-10-01"
#' @param period_end end of the assessment period. Ignored, if param scenarios is specified. Default is "2031-03-01"

create_user_input_table <- function(k = 5, destination_agr = "ci", scenarios = NULL, period_start = "2029-10-01", period_end = "2031-03-01"){
  
  if (!(destination_agr %in% c("ci", "c"))){
    stop("parameter destination_agr misspecified")
  }
  
  if (missing("scenarios") || is.null(scenarios)){
    period_start = as.Date(period_start)
    period_end = as.Date(period_end)
    scenarios <- data.frame(time = seq.Date(period_start, period_end, by = "1 month")) %>% 
      mutate(value = NA) %>% 
      spread(key = time, value = value)
  } else{
    scenarios <- matrix(nrow = 1, ncol = length(scenarios), dimnames = list(1, scenarios)) %>% 
      as.data.frame()
    rownames(scenarios) = NULL
    
  }
  
  topK_destinations <- find_topK_export_destinations(k = k, destination_agr = destination_agr)
  
  topK_destinations <- topK_destinations %>% 
    bind_cols(scenarios)
  
  result <- list(topK = topK_destinations)
  
  if (destination_agr == "ci"){
    if (!file.exists(file.path(data_loc, "export_module/ibci_export_shares.Rda"))){
      calculate_oecd_ibci_export_shares()
    }
    load(file.path(data_loc, "export_module/ibci_export_shares.Rda"))
    destinations <- ibci_export_shares %>% 
      dplyr::select(destination_country, destination_industry, destination_industry_text) %>% 
      unique()
    
    countries <- destinations %>% 
      dplyr::select(destination_country) %>% 
      unique() %>% 
      arrange(destination_country == "ROW", destination_country)

    for (i in 1:nrow(countries)){
      country_i = countries$destination_country[i]
      result[[country_i]] = destinations %>% 
        filter(destination_country == country_i) %>% 
        dplyr::select(-destination_country) %>% 
        arrange(destination_industry) %>% 
        bind_cols(scenarios)
        
    }
  }
  
  if (destination_agr == "c"){
    if (!file.exists(file.path(data_loc, "export_module/ibc_export_shares.Rda"))){
      calculate_oecd_ibc_export_shares()
    }
    load(file.path(data_loc, "export_module/ibc_export_shares.Rda"))
    destinations <- ibc_export_shares %>% 
      dplyr::select(destination_country) %>% 
      unique() %>% 
      arrange(destination_country == "ROW", destination_country) %>% 
      bind_cols(scenarios)
    
    result$countries = destinations
  }
  
  openxlsx::write.xlsx(result, file = file.path(ui_loc, input_filename), overwrite = TRUE)
}
  

#' Calculate export demand for each industry
#' 
#' Function that calculates export demand for each industry in absolute values
#' The output is based on baseline amounts (export_baseline.Rda)
#' and user-input 
#' 
#' Output is in datatable format
#' 
#' @param input_type user input type specified in input file
#'  "relative_change" relative change in export amounts
#'  "absolute_change" absolute change in export amounts (EUR)
#'  "absolute_level" export amount in EUR
#' @param goods_services Type of export baselines
#'  "goods_and_services" - baseline export amounts include both goods and services
#'  "goods" - baseline export includes only goods
#'  "services" - baseline export includes only services 
calculate_export_demand <- function(input_type = "relative_change", goods_services = "goods_and_services"){
  
  sheetnames <- getSheetNames(file = file.path(ui_loc, input_filename))

  user_input <- lapply(sheetnames, function(x){
    openxlsx::read.xlsx(file.path(ui_loc, input_filename), sheet = x)})
  names(user_input) = sheetnames
  
  if (!file.exists(file.path(data_loc, "export_module/export_baseline.Rda"))){
    load_esa_export_data()
  }
  load(file.path(data_loc, "export_module/export_baseline.Rda")) 
  
  export_baseline <- export_baseline %>% 
    filter(type == goods_services) %>% 
    dplyr::select(-type)
  
  if (identical(names(user_input), c("topK", "countries"))){
    if (!file.exists(file.path(data_loc, "export_module/ibc_export_shares.Rda"))){
      calculate_oecd_ibc_export_shares()
    }
    load(file.path(data_loc, "export_module/ibc_export_shares.Rda"))

    export_data <- ibc_export_shares %>% 
      rename(destination = destination_country) %>% 
      dplyr::select(-origin_industry_text)
    
    topK_export_data <- user_input$topK %>% 
      rename(destination = destination_country)
    
    gen_export_data <- user_input$countries %>% 
      rename(destination = destination_country) %>% 
      gather(key = month,
             value = demand_change_gen,
             -destination)
  } else{
    if (!file.exists(file.path(data_loc, "export_module/ibci_export_shares.Rda"))){
      calculate_oecd_ibci_export_shares()
    }
    load(file.path(data_loc, "export_module/ibci_export_shares.Rda"))

    export_data <- ibci_export_shares %>% 
      tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-origin_industry_text, -destination_industry_text)
    
    topK_export_data <- user_input$topK %>% 
      tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-destination_industry_text)
    
    gen_export_data <- user_input[-1] %>%
      bind_rows(.id = "destination_country") %>% 
      tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-destination_industry_text) %>% 
      gather(key = month,
             value = demand_change_gen,
             -destination)
  }
  
  topK_export_data <- topK_export_data %>% 
    gather(key = month,
           value = demand_change,
           -origin_industry, -origin_industry_text, -destination, -export_share)
  
  months <- topK_export_data %>% 
    dplyr::select(month) %>% 
    unique() %>% 
    arrange(month)
  
  export_data <- export_data %>% 
    mutate(i = 1) %>% 
    left_join(months %>% 
                mutate(i = 1),
              by = "i") %>% 
    dplyr::select(-i)
  
  if (input_type == "relative_change"){
    export_demand <- calculate_export_demand_relative_change(export_data, topK_export_data, gen_export_data, export_baseline)
  }
  
  if (input_type == "absolute_change"){
    export_demand <- calculate_export_demand_absolute_change(export_data, topK_export_data, gen_export_data, export_baseline)
  }

  if (input_type == "absolute_level"){
    export_demand <- calculate_export_demand_absolute_level(export_data, topK_export_data, gen_export_data, export_baseline)
  }
  
  min_demand <- min(export_demand$export_demand)
  if (min_demand < 0){
    warning("Ekspordi nõudluses esineb negatiivseid väärtusi:\n",  paste(capture.output(export_demand %>% filter(export_demand < 0) %>% head(10)), collapse = "\n"))
  }
  
  export_demand_wide <- export_demand %>% 
    spread(key = month, value = export_demand) %>% 
    as.data.table()

  return(export_demand_wide)
}

#' Calculate export demand for each industry if user input type is "relative_change"
#' 
#' Technical function that calculates export demand for each industry in absolute values if user input type is "relative_change"
#' Used in function calculate_export_demand()
#' 
#' @param export_data dataframe with columns origin_industry, destination, export_share, month
#' @param topK_export_data dataframe of topK destination changes with required columns origin_industry, destination, month, demand_change
#' @param gen_export_data dataframe of general destination changes with required columns destination, month, demand_change_gen
#' @param export_baseline dataframe of baseline export amounts with columns industry_code, industry_text, export_amount
#' 
#' User input from "topK" has top priority - all non-empty cells are considered as they are
#' empty cells in "topK" can be overwritten by values in gen_export_data
#' On sheet "topK" value "ROW" indicates all other destinations that are not in topK - the corresponding non-empty value is imputed to all other destinations 
calculate_export_demand_relative_change <- function(export_data, topK_export_data, gen_export_data, export_baseline){
  
  export_data <- export_data %>% 
    left_join(topK_export_data %>% 
                filter(destination != "ROW", destination != "ROW_NA") %>% 
                dplyr::select(-export_share, -origin_industry_text) %>% 
                mutate(is_topK = TRUE),
              by = c("origin_industry", "destination", "month")) %>% 
    left_join(topK_export_data %>% 
                filter(destination == "ROW" | destination == "ROW_NA") %>%
                dplyr::select(-export_share, -origin_industry_text, -destination),
              by = c("origin_industry", "month"),
              suffix = c("", "_ROW")) %>% 
    mutate(is_topK = ifelse(is.na(is_topK), FALSE, is_topK),
           demand_change = ifelse(!is_topK, demand_change_ROW, demand_change))
  
  export_data <- export_data %>% 
    left_join(gen_export_data,
              by = c("destination", "month")) %>% 
    mutate(demand_change = ifelse(is.na(demand_change), demand_change_gen, demand_change)) %>% 
    dplyr::select(-is_topK, -demand_change_ROW, -demand_change_gen)
  
  export_data <- export_data %>% 
    mutate(demand_change = as.numeric(demand_change),
           demand_change = ifelse(is.na(demand_change), 0, demand_change),
           export_share = as.numeric(export_share))
  
  min_demand_change <- min(export_data$demand_change)
  if (min_demand_change < -1){
    warning("Vähemalt 1 sisestatud muutus on väiksem kui -1")
  }
    
  export_demand <- left_join(export_baseline,
                             export_data,
                             by = c("industry_code" = "origin_industry"))
  
  export_demand <- export_demand %>% 
    mutate(export_amount_new = (1 + demand_change) * export_share * export_amount)
  
  export_demand <- export_demand %>% 
    group_by(industry_code, industry_text, month) %>% 
    summarise(export_demand = sum(export_amount_new),
              .groups = "drop") %>% 
    as.data.table()

  
  return(export_demand)
}

#' Calculate export demand for each industry if user input type is "absolute_change"
#' 
#' Technical function that calculates export demand for each industry in absolute values if user input type is "absolute_change"
#' Used in function calculate_export_demand()
#' 
#' @param export_data dataframe with columns origin_industry, destination, export_share, month
#' @param topK_export_data dataframe of topK destination changes with required columns origin_industry, destination, month, demand_change
#' @param gen_export_data dataframe of general destination changes with required columns destination, month, demand_change_gen
#' @param export_baseline dataframe of baseline export amounts with columns industry_code, industry_text, export_amount
#' 
#' User input from "topK" has top priority - all non-empty cells are considered as they are
#' empty cells in "topK" can be imputed based on values in gen_export_data
#' On sheet "topK" value "ROW" indicates all other destinations that are not in topK - the corresponding non-empty value is imputed (by proportions) to all other destinations 
calculate_export_demand_absolute_change <- function(export_data, topK_export_data, gen_export_data, export_baseline){
  
  export_data <- export_data %>%
    left_join(export_baseline,
              by = c("origin_industry" = "industry_code")) %>%
    rename(export_amount_origin_total = export_amount) %>%
    mutate(export_share = as.numeric(export_share),
           export_amount_to_destination = export_amount_origin_total * export_share)

  export_data <- export_data %>% 
    left_join(topK_export_data %>% 
                filter(destination != "ROW", destination != "ROW_NA") %>% 
                dplyr::select(-export_share, -origin_industry_text) %>% 
                mutate(is_topK = TRUE),
              by = c("origin_industry", "destination", "month")) %>% 
    left_join(topK_export_data %>% 
                filter(destination == "ROW" | destination == "ROW_NA") %>%
                dplyr::select(-export_share, -origin_industry_text, -destination),
              by = c("origin_industry", "month"),
              suffix = c("", "_ROW")) %>% 
    mutate(is_topK = ifelse(is.na(is_topK), FALSE, is_topK))
  
  # Distribute topK ROW amount proportionally to destinations not in topK
  export_data <- export_data %>% 
    group_by(origin_industry, month, is_topK) %>% 
    mutate(demand_change_ROW_adj = demand_change_ROW * export_share/sum(export_share)) %>%  
    ungroup() %>% 
    mutate(demand_change = ifelse(!is_topK, demand_change_ROW_adj, demand_change))
  
  # Distribute general changes to destinations that are not fixed yet  
  export_data <- export_data %>% 
    left_join(gen_export_data,
              by = c("destination", "month"))  %>% 
    mutate(is_fixed = !is.na(demand_change)) %>% 
    group_by(destination, month, is_fixed) %>% 
    mutate(demand_change_gen_adj = demand_change_gen * export_amount_to_destination/sum(export_amount_to_destination)) %>% 
    ungroup() %>% 
    mutate(demand_change = ifelse(is.na(demand_change), demand_change_gen_adj, demand_change)) %>% 
    dplyr::select(-is_topK, -demand_change_ROW, -demand_change_ROW_adj, -demand_change_gen, -demand_change_gen_adj, -is_fixed, -export_amount_to_destination)
  
  export_data <- export_data %>% 
    mutate(demand_change = as.numeric(demand_change),
           demand_change = ifelse(is.na(demand_change), 0, demand_change))
  
  max_abs_demand_change <- max(abs(export_data$demand_change))
  if (max_abs_demand_change < 10**5){
    warning("Sisestatud muutuste absoluutväärtused on väikesed. Kas olete kindel, et soovite kasutada absoluutmuutusi?")
  }
  
  export_data <- export_data %>% 
    mutate(export_amount_new = export_share * export_amount_origin_total + demand_change)
  
  export_demand <- export_data %>% 
    rename(industry_code = origin_industry) %>% 
    group_by(industry_code, industry_text, month) %>% 
    summarise(export_demand = sum(export_amount_new),
              .groups = "drop") %>% 
    as.data.table()
  
  
  return(export_demand)
}


#' Calculate export demand for each industry if user input type is "absolute_level"
#' 
#' Technical function that calculates export demand for each industry in absolute values if user input type is "absolute_level"
#' Used in function calculate_export_demand()
#' 
#' @param export_data dataframe with columns origin_industry, destination, export_share, month
#' @param topK_export_data dataframe of topK destination changes with required columns origin_industry, destination, month, demand_change
#' @param gen_export_data dataframe of general destination changes with required columns destination, month, demand_change_gen
#' @param export_baseline dataframe of baseline export amounts with columns industry_code, industry_text, export_amount
#' 
#' User input from "topK" has top priority - all non-empty cells are considered as they are
#' empty cells in "topK" can be imputed based on values in gen_export_data
#' On sheet "topK" value "ROW" indicates all other destinations that are not in topK - the corresponding non-empty value is imputed (by proportions) to all other destinations 
calculate_export_demand_absolute_level <- function(export_data, topK_export_data, gen_export_data, export_baseline){
  
  export_data <- export_data %>%
    left_join(export_baseline,
              by = c("origin_industry" = "industry_code")) %>%
    rename(export_amount_origin_total = export_amount) %>%
    mutate(export_share = as.numeric(export_share),
           export_amount_to_destination = export_amount_origin_total * export_share)
  
  # Note: demand change is in current context actual amount
  export_data <- export_data %>% 
    left_join(topK_export_data %>% 
                filter(destination != "ROW", destination != "ROW_NA") %>% 
                dplyr::select(-export_share, -origin_industry_text) %>% 
                mutate(is_topK = TRUE),
              by = c("origin_industry", "destination", "month")) %>% 
    left_join(topK_export_data %>% 
                filter(destination == "ROW" | destination == "ROW_NA") %>%
                dplyr::select(-export_share, -origin_industry_text, -destination),
              by = c("origin_industry", "month"),
              suffix = c("", "_ROW")) %>% 
    mutate(is_topK = ifelse(is.na(is_topK), FALSE, is_topK))
  
  # Distribute topK ROW amount proportionally to destinations not in topK
  export_data <- export_data %>% 
    group_by(origin_industry, month, is_topK) %>% 
    mutate(demand_change_ROW_adj = demand_change_ROW * export_share/sum(export_share)) %>%  
    ungroup() %>% 
    mutate(demand_change = ifelse(!is_topK, demand_change_ROW_adj, demand_change))
  
  # Distribute general amounts to destinations that are not fixed yet  
  export_data <- export_data %>% 
    left_join(gen_export_data,
              by = c("destination", "month"))  %>% 
    mutate(is_fixed = !is.na(demand_change)) %>% 
    group_by(destination, month, is_fixed) %>% 
    mutate(demand_change_gen_adj = demand_change_gen * export_amount_to_destination/sum(export_amount_to_destination)) %>% 
    ungroup() %>% 
    mutate(demand_change = ifelse(is.na(demand_change), demand_change_gen_adj, demand_change)) %>% 
    dplyr::select(-is_topK, -demand_change_ROW, -demand_change_ROW_adj, -demand_change_gen, -demand_change_gen_adj, -is_fixed, -export_amount_to_destination)
  
  export_data <- export_data %>% 
    mutate(demand_change = as.numeric(demand_change),
           demand_change = ifelse(is.na(demand_change), export_amount_origin_total * export_share, demand_change))
  
  min_demand_change <- min(export_data$demand_change)
  if (min_demand_change < -0.1){
    warning("Sisestatud eksporditasemete seas esineb negatiivseid väärtusi. Kas olete kindel, et soovite kasutada absoluuttasemeid?")
  }

  
  export_demand <- export_data %>% 
    rename(industry_code = origin_industry) %>% 
    group_by(industry_code, industry_text, month) %>% 
    summarise(export_demand = sum(demand_change),
              .groups = "drop") %>% 
    as.data.table()
  
  
  return(export_demand)
}


#' Calculate export demand change for each industry
#' 
#' Function that calculates changes of export demand for each industry in absolute values
#' The output is based on baseline amounts (export_baseline.Rda)
#' and simulated amounts that are returned by calculate_export_demand()
#' 
#' Output is in datatable format
#' 
#' Note: with slight modifications in functions calculate_export_demand_relative_change, calculate_export_demand_absolute_change and calculate_export_demand_absolute_level
#' the change in export demand can be calculated without function calculate_export_demand but it is kept to check the existence of negative export amounts
#' 
#' @param input_type user input type specified in input_filename
#'  "relative_change" relative change in export amounts
#'  "absolute_change" absolute change in export amounts (EUR)
#'  "absolute_level" export amount in EUR
#' @param intervall Time interval of input data. Possible values are "kuu" for monthly data and "aasta" for yearly data
#' @param goods_services Type of export baselines
#'  "goods_and_services" - baseline export amounts include both goods and services
#'  "goods" - baseline export includes only goods
#'  "services" - baseline export includes only services 

calculate_export_demand_change <- function(input_type = "relative_change", intervall, goods_services = "goods_and_services"){
  
  if (!file.exists(file.path(data_loc, "export_module/export_baseline.Rda"))){
    load_esa_export_data()
  }
  load(file.path(data_loc, "export_module/export_baseline.Rda")) 
  
  export_baseline <- export_baseline %>% 
    filter(type == goods_services) %>% 
    dplyr::select(-type)
  
  export_demand_wide <- calculate_export_demand(input_type = input_type,
                                                goods_services = goods_services)

  export_demand_long <- export_demand_wide %>% 
    tidyr::gather(key = month, value = simulated_amount, -industry_code, -industry_text)
  
  export_demand_long <- export_demand_long %>% 
    left_join(export_baseline %>% dplyr::select(-industry_text),
              by = "industry_code") %>% 
    mutate(absolute_change = simulated_amount - export_amount)
  
  if (intervall == "kuu"){
    export_demand_long <- export_demand_long %>% 
      mutate(absolute_change = absolute_change/12)
  }
  export_demand_wide <- export_demand_long %>% 
    mutate(absolute_change = round(absolute_change,2)) %>% 
    dplyr::select(-simulated_amount, -export_amount) %>% 
    tidyr::spread(key = month, value = absolute_change)%>% 
    as.data.table()
  
  return(export_demand_wide)
}



# OTHER FUNCTIONS ---------------------------------------------------------

#' Calculate "proportion of export of services/goods/total to various countries"
#' 
#' Export of goods, export of services and total export are in separate columns.
#' Total export from Estonia is considered, i.e. Estonian industries are not taken into account separatelly. 
#' 
#' Raw datasets (imported automatically):
#' https://andmed.stat.ee/en/stat/majandus__valiskaubandus__teenuste_valiskaubandus/VKT13 for services
#' https://andmed.stat.ee/en/stat/majandus__valiskaubandus__valiskaubandus-alates-2004/VK10 for goods 
#' 
#' The output is used only for analytical purposes
#' The output is returned as dataframe and saved to CSV file with name *export_shares_{year}.csv*
#' 
#' @param year the year of the data

share_of_destination_countries <- function(year = 2020){
  oecd_c1 <- openxlsx::read.xlsx(file.path(data_loc, "export_module/ReadMe_ICIO_CSV.xlsx"), sheet = "Country_Industry", rows = 3:39, cols = c(2:3))
  oecd_c2 <- openxlsx::read.xlsx(file.path(data_loc, "export_module/ReadMe_ICIO_CSV.xlsx"), sheet = "Country_Industry", rows = 3:32, cols = c(4:5))
  
  colnames(oecd_c1) = colnames(oecd_c2) = c("code", "country")
  oecd_c <- bind_rows(oecd_c1,
                      oecd_c2)
  oecd_c <- oecd_c %>% 
    mutate(country = case_when(
      country == "Israel 1" ~ "Israel",
      country == "Cyprus 2" ~ "Cyprus",
      TRUE ~ country
    ))
  
  # Goods
  query = paste0('{"query": [{"code": "Aasta","selection": {"filter": "item","values": ["', 
                 year, 
                 '"]}},
                 {"code": "Kuu","selection": {"filter": "item","values": ["VALUE_CUM"]}},
                 {"code": "Riik","selection": {"filter": "all","values": ["*"]}}
                 ],"response": {"format": "csv"}}')
  
  # Supply table at basic prices
  export_dest_goods <- content(POST("https://andmed.stat.ee/api/v1/en/stat/VK09",  body = query, encode = "json"), 
                                  type = "text/csv",
                                  encoding = "UTF-8") %>%
    as.data.table() 
  
  export_dest_goods <- export_dest_goods %>% 
    dplyr::select(Country, Year, 
                  Amount = `Exports, euros`) %>% 
    dplyr::filter(Country != "Countries total")
  
  colnames(export_dest_goods) = tolower(colnames(export_dest_goods))
  
  #setdiff(oecd_c$country, export_dest_goods$country)
  export_dest_goods_iso <- export_dest_goods %>% 
    mutate(country2 = case_when(
      country == "United States of America" ~ "United States",
      country == "Korea, Republic of" ~ "Korea",
      country == "Slovakia" ~ "Slovak Republic",
      country == "China" ~ "China (People's Republic of)",
      country == "Hong Kong" ~ "Hong Kong, China",
      country == "Taiwan" ~ "Chinese Taipei",
      TRUE ~ country
    )) %>% 
    left_join(oecd_c, by = c("country2" = "country"))
  #setdiff(oecd_c$country, export_dest_goods_iso$country2)
  
  export_dest_goods_iso <- export_dest_goods_iso %>% 
    rename(country_name = country2) %>% 
    mutate(code = ifelse(is.na(code), "ROW", code),
           country_name = ifelse(code == "ROW", "Rest of the World", country_name)) %>% 
    group_by(code, country_name) %>% 
    summarise(amount = sum(amount),
              .groups = "drop") %>% 
    mutate(share = amount / sum(amount))
  
  # Services
  query = paste0('{"query": [{"code": "Vaatlusperiood","selection": {"filter": "item","values": ["', 
                 year, 
                 '"]}},
                 {"code": "Voog","selection": {"filter": "item","values": ["EXP"]}},
                 {"code": "Riik","selection": {"filter": "all","values": ["*"]}}
                 ],"response": {"format": "csv"}}')
  
  # Supply table at basic prices
  export_dest_servs <- content(POST("https://andmed.stat.ee/api/v1/en/stat/VKT13",  body = query, encode = "json"), 
                               type = "text/csv",
                               encoding = "UTF-8") %>%
    as.data.table() 
  
  export_dest_servs <- export_dest_servs %>% 
    mutate(Year = year,
           Amount = `2020 Exports` * 10**6) %>% 
    dplyr::select(Country, Year, 
                  Amount) %>% 
    dplyr::filter(!(Country %in% c("Total",
                                   "Africa",
                                   "Asia",
                                   "America",
                                   "Antarctica",
                                   "Australia and Oceania",
                                   "Europe (countries and institutions)",
                                   "European Union 2020 (countries and institutions)",
                                   "Euro area (countries and institutions)")))
  
  colnames(export_dest_servs) = tolower(colnames(export_dest_servs))
  
  #setdiff(oecd_c$country, export_dest_servs$country)
  export_dest_servs_iso <- export_dest_servs %>% 
    mutate(country2 = case_when(
      country == "South Korea" ~ "Korea",
      country == "Slovakia" ~ "Slovak Republic",
      country == "China" ~ "China (People's Republic of)",
      country == "Hong Kong (CN)" ~ "Hong Kong, China",
      country == "Russia" ~ "Russian Federation",
      country == "Taiwan (CN)" ~ "Chinese Taipei",
      country == "Vietnam" ~ "Viet Nam",
      TRUE ~ country
    )) %>% 
    left_join(oecd_c, by = c("country2" = "country"))
  #setdiff(oecd_c$country, export_dest_servs_iso$country2)
  
  export_dest_servs_iso <- export_dest_servs_iso %>% 
    rename(country_name = country2) %>% 
    mutate(code = ifelse(is.na(code), "ROW", code),
           country_name = ifelse(code == "ROW", "Rest of the World", country_name)) %>% 
    group_by(code, country_name) %>% 
    summarise(amount = sum(amount),
              .groups = "drop") %>% 
    mutate(share = amount / sum(amount))
  
  export_dest_iso <- full_join(export_dest_goods_iso,
                               export_dest_servs_iso,
                               by = c("code", "country_name"),
                               suffix = c("_goods", "_services"))
  
  export_dest_iso <- export_dest_iso %>% 
    mutate(amount_services = ifelse(is.na(amount_services), 0, amount_services),
           share_services = ifelse(is.na(share_services), 0, share_services),
           amount_total = amount_goods + amount_services,
           share_total = amount_total / sum(amount_total),
           year = year)
  
  write.csv(export_dest_iso,  file = file.path(data_loc, paste0("export_module/export_shares_",year,".csv")))

  return(export_dest_iso)
  }
