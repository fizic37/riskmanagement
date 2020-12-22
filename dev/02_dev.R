# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package("golem")
usethis::use_pipe(export = TRUE)
usethis::use_package("readr")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("stringr")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
usethis::use_package("shinydashboard")
usethis::use_package("DT")
usethis::use_package("tools")
usethis::use_package("readxl")
usethis::use_package("sodium")
usethis::use_package("shinycssloaders")
usethis::use_package("purrr")
usethis::use_package("janitor")
usethis::use_package("rlang")
usethis::use_package("utils")
usethis::use_package("stats")
usethis::use_package("shinydashboardPlus")
usethis::use_package("fresh")
usethis::use_package("lubridate")
usethis::use_package("qdapRegex")
usethis::use_package("shinybusy")
usethis::use_package("rhandsontable")
usethis::use_data(baza_date_cip,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_date_crc,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_date_crc_archived,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(cereri_plata,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_provizioane_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(view_sumar_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(slice_provizioane_plati,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(bi_grupuri,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(document_id_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(cod_partener_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(additional_cui_match,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_date_cui,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_colaterale,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_colaterale_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(view_baza_date_crc,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(baza_date_crc_sliced,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(coresp_banci,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(coresp_banci_bi,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "loginpage" ) # Name of the module
golem::add_module( name = "sidebar" )
golem::add_module( name = "portofoliu" )
golem::add_module( name = "home")
golem::add_module( name = "database_util_files")
golem::add_module( name = "database_portofoliu")
golem::add_module( name = "database_portofoliu_upload")
golem::add_module( name = "crc")
golem::add_module( name = "cip")
golem::add_module( name = "provizioane_plati")
golem::add_module( name = "database_upload_plati")
golem::add_module( name = "database_grupuri")
golem::add_module( name = "read_excel")
golem::add_module( name = "expunere_agregata")
golem::add_module( name = "garantii_colaterale")
golem::add_module( name = "read_excel_colaterale")
golem::add_module( name = "raportare_bnr")
golem::add_module( name = "compare_df")




## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils(name = "helpers",module = "portofoliu")
golem::add_fct( "helpers",module = "crc")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package

 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("RiskManagementGolem")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

