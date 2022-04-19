#' Export module component that creates user input templates
#' 
#' The output is a user input file in xlsx format. The file is used by export module component export_data() 
#' @param input_filename Name of the result file (in format "name_of_result.xlsx" ). 
#' If the file does not exist it is created during the module execution. 
#' If missing, then by default"Andmete sisestamise vormi näidis-eksport.xlsx"
#' @param input_file_path Path of the input file. If not specified then "4.user_input_examples" in working directory 
#' @param overwrite Should the input file be overwritten (if it exists) or not. Options: yes/no
#' @param k integer, top k destinations, default 5
#' @param destination_agr the level of destination aggregation. Default "ci"
#'  "ci" country-industry level
#'  "c" country level
#' @param scenarios vector with names of scenarios. If missing, monthly time-series from period_start to period_end are taken as scenarios
#' @param period_start date from which to start counting, start of the assessment period. Ignored, if param scenarios is specified.
#' @param period_end end of the assessment period. Ignored, if param scenarios is specified. 
#' #' Example usage:
#' export_data_input_template(input_filename = "scenarios_a_b_c.xlsx", overwrite = "yes", k = 5, destination_agr = "ci", scenarios = c("a", "b", "c"))
export_data_input_template <- function(input_filename, input_file_path, overwrite, 
                        k = 5, destination_agr, scenarios, period_start, period_end){
  
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  
  
  if (missing("input_file_path") || is.null(input_file_path)) {
    ui_loc <<- file.path(wd, "4.user_input_examples")
  } else{
    ui_loc <<- input_file_path
  }
  
  if (missing("input_filename")) {
    input_filename <- "Andmete sisestamise vormi näidis-eksport.xlsx"
  }
  input_filename <<- input_filename
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
  source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")
  source(file.path(script_loc,"export_functions.R"), encoding = "UTF-8")
  
  if (missing("overwrite")) {
    overwrite = "no"
    if (file.exists(file.path(ui_loc, input_filename))){
      
      overwrite = prompt_options(text_promt = paste0("File <", input_filename,"> exists. Do you want to overwrite user input file?\n(Yes/No)?: "), 
                                 text_error = "This option is not available. Choose again: ",
                                 options = c("Yes", "No")) 
    }
  }

    
  if (!file.exists(file.path(ui_loc, input_filename)) || tolower(overwrite) == "yes"){
    
    if (missing("destination_agr")) {
      granularity = prompt_options(text_promt = "Choose granularity of destination markets\n(country/country_industry)?: ", 
                                 text_error = "This option is not available. Choose again: ",
                                 options = c("country", "country_industry"))
      if (granularity == "country_industry"){
        destination_agr = "ci"
      } else{
        destination_agr = "c"
      }
    }
    
    if (missing("scenarios")){
      scenarios = NULL

      if (missing("period_start")){
        period_start = "2021-01-01"
      }

      if (missing("period_end")){
        period_end = "2022-07-01"
      }
      
    }

    create_user_input_table(k = k, destination_agr = destination_agr, scenarios = scenarios, period_start = period_start, period_end = period_end)
  }
}


#' Export module component to calculate export demand
#' 
#' The output is a dataframe where
#' in rows are domestic industries (same aggregation as in other parts of the project)
#' and in columns are scenarios (e.g. scenario names or months (by default 18))).
#' Scenario names are taken from input file (input_filname).
#' The values are estimated amount of export in EUR of given industry for specified scenario / (month)
#' @param time_interval Time interval of input data. Possible values are "kuu" for monthly data and "aasta" for yearly data
#' @param input_filename Name of the input file (in xlsx format). If missing, then "Andmete sisestamise vormi näidis-eksport.xlsx"
#' @param input_file_path Path of the input file. If not specified then "4.user_input_examples" in working directory 
#' @param input_type user input type specified in input file
#'  "relative_change" relative change in export amounts
#'  "absolute_change" absolute change in export amounts (EUR)
#'  "absolute_level" export amount in EUR
#' @param goods_services Type of export baselines
#'  "goods_and_services" - baseline export amounts include both goods and services
#'  "goods" - baseline export includes only goods
#'  "services" - baseline export includes only services 
#'  Example usage:
#'  export_data(time_interval = "aasta", input_type = "absolute_change", goods_services = "goods_and_services")
export_data <- function(time_interval, input_filename, input_file_path, input_type, goods_services){
  
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  
  
  if (missing("input_file_path") || is.null(input_file_path)) {
    ui_loc <<- file.path(wd, "4.user_input_examples")
  } else{
    ui_loc <<- input_file_path
  }
  
  if (missing("input_filename") || is.null(input_filename)) {
    input_filename <- "Andmete sisestamise vormi näidis-eksport.xlsx"
  }
  input_filename <<- input_filename
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
  source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")
  source(file.path(script_loc,"export_functions.R"), encoding = "UTF-8")
  
  if (missing("time_interval")) {
    time_interval  = prompt_options(text_promt = "Kas sisestatavad andmed on kuised või aastased?\nIs the time interval of the input in months (kuu) or in years (aasta)? (vali kuu/aasta): ", 
                                text_error = "Sellist valikut pole. Vali uuesti: \nThis option is not available. Choose again:",
                                options = c("kuu", "aasta"),
                                case_sensitive = "No")
  }
  
  if (missing("input_type")) {
    input_type_code = prompt_options(text_promt = "Select input type\n-relative_change (enter 1)\n-absolute_change (enter 2)\n-absolute_level(enter 3)): ", 
                                text_error = "This option is not available. Choose again: ",
                                options = c("1", "2", "3"))
    
    input_type <- switch(input_type_code,
                             "1" = "relative_change",
                             "2" = "absolute_change",
                             "3" = "absolute_level")
  }

  if (missing("goods_services")) {
    goods_services_code = prompt_options(text_promt = "Select the type of export baselines\n-goods_and_services (enter 1)\n-goods (enter 2)\n-services (enter 3): ", 
                                text_error = "This option is not available. Choose again: ",
                                options = c("1", "2", "3"))
    
    goods_services <- switch(goods_services_code,
                             "1" = "goods_and_services",
                             "2" = "goods",
                             "3" = "services")
    
  }
  
  export_demand <- calculate_export_demand_change(input_type = input_type, intervall = time_interval, goods_services = goods_services) 
  
  return (export_demand)
}



#' Run export module manually (Export module ONLY)

run_export_module <- function(){
  
  # Locations
  wd <<- getwd()
  script_loc <<- file.path(wd, "1.scripts")  
  data_loc <<- file.path(wd, "2.data")
  dict_loc <<- file.path(wd, "3.dictionaries")
  ui_loc <<- file.path(wd, "4.user_input_examples")
  export_loc <<- file.path(wd, "6.model_modules")
  
  
  # Load necessary scripts
  source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
  source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")
  source(file.path(export_loc,"export.R"), encoding = "UTF-8")
  
  
  scenarios_ts = prompt_options(text_promt = "Would you like to use scenarios (enter 1) or timeseries? (enter 2)): ", 
                                text_error = "This option is not available. Choose again: ",
                                options = c("1", "2")) 
  
  if (scenarios_ts == "1"){
    scenarios_ui = readline("Insert scenario names, separated by comma (e.g. A,B,C): ") 
    scenarios <- unique(trimws(unlist(strsplit(scenarios_ui, ","))))
    period_start = "1900-01-01"
    period_end = "1900-01-01"
  } else{
    period_start = readline("Insert the start month of observable period (in format YYYY-mm-01): ") 
    period_end = readline("Insert the end month of observable period (in format YYYY-mm-01): ") 
    scenarios = NULL
  }
  
  export_data_input_template(input_filename = "export_user_input.xlsx", 
                             input_file_path = ui_loc, 
                             scenarios = scenarios, 
                             period_start = period_start, 
                             period_end = period_end)
  
  
  input_is_ready = prompt_options(text_promt = paste0("Input file 'export_user_input.xlsx' is in folder ", ui_loc, ". \nInsert 'OK' when the file is manually modified: "), 
                                  text_error = "This option is not available. Choose again: ",
                                  options = c("OK"))
  
  if (scenarios_ts == "1"){
    export_demand <- export_data(input_filename = "export_user_input.xlsx", input_file_path = ui_loc)
  } else{
    export_demand <- export_data(time_interval = "kuu", input_filename = "export_user_input.xlsx", input_file_path = ui_loc)
  }
  
  return(export_demand)
}
