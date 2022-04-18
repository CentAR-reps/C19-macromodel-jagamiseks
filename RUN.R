# Kataloogide asukohad
wd <<- getwd()
script_loc <<- file.path(wd, "1.scripts")  
data_loc <<- file.path(wd, "2.data")
dict_loc <<- file.path(wd, "3.dictionaries")
model_loc <<- file.path(wd, "6.model_modules")

# Loeme sisse skriptid
source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
source(file.path(script_loc,"IO_functions.R"), encoding = "UTF-8")
source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")

# Loeme sisse mudeli moodulid
source(file.path(model_loc,"0.launch_the_model.R"), encoding = "UTF-8")
source(file.path(model_loc,"1.input_data_domestic.R"), encoding = "UTF-8")
source(file.path(model_loc,"2.impact_of_tax_change_on_prices.R"), encoding = "UTF-8")
source(file.path(model_loc,"3.impact_of_price_change_on_final_demand.R"), encoding = "UTF-8")
source(file.path(model_loc,"4.demand_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"5.supply_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"6.impact_on_VA_LC_PROF.R"), encoding = "UTF-8")
source(file.path(model_loc,"8.impact_on_taxes.R"), encoding = "UTF-8")
source(file.path(model_loc,"9.demand_side_impact_on_employment.R"), encoding = "UTF-8")

# Jooniste üldiseks kujunduseks kasutame paketi "ggthemr" teemat "fresh" 
ggthemr('fresh')

# Käivitame mudeli
tulemused = run()
