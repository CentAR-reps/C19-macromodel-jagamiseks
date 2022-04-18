create_user_input_table_app <- function(k = 5, destination_agr = "ci", scenarios = NULL, period_start = "2029-10-01", period_end = "2031-03-01"){
  
  if (!(destination_agr %in% c("ci", "c"))){
    stop("parameter destination_agr misspecified")
  }
  
  if (missing("scenarios") || is.null(scenarios)){
    period_start = as.Date(period_start)
    period_end = as.Date(period_end)
    scenarios <- data.frame(time = seq.Date(period_start, period_end, by = "1 month")) %>% 
      mutate(value = as.numeric(NA)) %>% 
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
    destinations <- ibc_export_shares %>% 
      dplyr::select(destination_country) %>% 
      unique() %>% 
      arrange(destination_country == "ROW", destination_country) %>% 
      bind_cols(scenarios)
    
    result$countries = destinations
  }
  
  return(result)
}


calculate_export_demand_app <- function(input_type = "relative_change", input_agr, topK, sihtriik, export_baseline){
  if (input_agr == "c"){
    export_data <- ibc_export_shares %>%
      rename(destination = destination_country) %>%
      dplyr::select(-origin_industry_text)
  
    topK_export_data <- topK %>%
      rename(destination = destination_country)
  
    gen_export_data <- sihtriik %>%
      rename(destination = destination_country) %>%
      gather(key = month,
             value = demand_change_gen,
             -destination)
  } else {
    export_data <- ibci_export_shares %>% 
      mutate(destination = paste(destination_country, destination_industry, sep = "_")) %>% 
      #tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-origin_industry_text, -destination_industry_text, -destination_country, -destination_industry)
    
    topK_export_data <- topK %>% 
      mutate(destination = paste(destination_country, destination_industry, sep = "_")) %>% 
      #tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-destination_industry_text, -destination_country, -destination_industry)
    
    for (i in 1:length(sihtriik)){
      c_i = names(sihtriik)[i]
      sihtriik[[i]]$destination_country = c_i
    }
    tmp2 = sihtriik %>%
      bind_rows()
    tmp2 <- bind_cols(tmp2 %>% dplyr::select(destination_country), tmp2 %>% dplyr::select(-destination_country))
    
    gen_export_data <- tmp2%>% 
      mutate(destination = paste(destination_country, destination_industry, sep = "_")) %>% 
      #tidyr::unite(col = destination, destination_country, destination_industry) %>% 
      dplyr::select(-destination_industry_text, -destination_country, -destination_industry) %>% 
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


calculate_export_demand_change_long_app <- function(input_type = "relative_change", input_agr, topK, sihtriik, intervall, 
                                                    export_baseline, goods_services){
  
  export_baseline <- export_baseline %>% 
    filter(type == goods_services) %>% 
    dplyr::select(-type)
  
  export_demand_wide <- calculate_export_demand_app(input_type = input_type, input_agr = input_agr, topK, sihtriik, export_baseline)
  
  export_demand_long <- export_demand_wide %>% 
    tidyr::gather(key = month, value = simulated_amount, -industry_code, -industry_text)
  
  export_demand_long <- export_demand_long %>% 
    left_join(export_baseline %>% dplyr::select(-industry_text),
              by = "industry_code") %>% 
    mutate(absolute_change = round(simulated_amount - export_amount,5),
           relative_change = round((simulated_amount - export_amount) / export_amount, 5),
           relative_change = ifelse(is.na(relative_change), 0 , relative_change))
  
  if (intervall == "kuu / month"){
    export_demand_long <- export_demand_long %>% 
      mutate(absolute_change = absolute_change/12,
             simulated_amount = simulated_amount/12,
             export_amount = export_amount/12)
  }
  
  return(export_demand_long)
}

calculate_export_demand_change_app <- function(export_demand_long, output_type = "abs_change"){

  if (output_type == "abs_change"){
    export_demand_wide <- export_demand_long %>%
      dplyr::select(-simulated_amount, -export_amount, -relative_change) %>%
      tidyr::spread(key = month, value = absolute_change)%>%
      as.data.table()
  }
  return(export_demand_wide)
}


#############################################################################





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
    topK_destinations <- ibci_export_shares
  }
  
  if (destination_agr == "c"){
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



# UI functions ------------------------------------------------------------




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


