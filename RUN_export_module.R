
wd <<- getwd()
model_loc <- file.path(wd, "6.model_modules")
source(file.path(model_loc,"export.R"), encoding = "UTF-8")

results = run_export_module()
