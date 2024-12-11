library(data.table)
library(shiny)
library(ggplot2)
library(ggiraph)
library(DT)
library(dplyr)
library(jsonlite)
library(openxlsx)
library(tidyverse)
library(glue)
library(shinyalert)
library(httr)
library(readxl)

# TODO: use RENV!
if (!requireNamespace("cleaned", quietly = TRUE)) {
  remotes::install_github("CIAT/cleaned@cleaned_v0.6.0")
}
library(cleaned)

source("R/helpers_global.R")

options(
  spinner.color = "#005275",
  spinner.color.background = "white",
  spinner.type = 3,
  # Set the maximum file upload size globally to 20MB
  shiny.maxRequestSize = 20 * 1024 ^ 2
)

# Auth0 info
auth0_info <- auth0::auth0_info()

# Set locale to English for formatting
original_locale <- Sys.getlocale("LC_TIME")  # Store the current locale
Sys.setlocale("LC_TIME", "C")  # Set to C locale for English

# TODO: Once the PR is accepted, switch back to the official CIAT repository
# by changing the URL and branch references to:
# URL to https://github.com/CIAT/cleaned
# Branch/Tag to cleaned_v0.6.0
# Ideally, user {renv} package to manage the package versions


# List of all parameter tables
parameters_db_names <- c(
  "lkp_climate", "lkp_climate2", "lkp_croplandsystem", "lkp_feeditem",
  "lkp_feedtype", "lkp_grassinputlevel", "lkp_grasslandman", "lkp_landcover",
  "lkp_livetype", "lkp_manureman", "lkp_organicmatter", "lkp_orgfertilizer",
  "lkp_region", "lkp_slope", "lkp_soil", "lkp_tillageregime"
)

# Read static parameters directory and files
ghg_ipcc_data <- fromJSON(
  system.file("extdata", "ghg_parameters.json", package = "cleaned"),
  flatten = TRUE
)
stock_change_para <- fromJSON(
  system.file("extdata", "stock_change_parameters.json", package = "cleaned"),
  flatten = TRUE
)
energy_parameters <- fromJSON(
  system.file("extdata", "energy_parameters.json", package = "cleaned"),
  flatten = TRUE
)
primary_excel <- "www/ReadMe.xlsx"

# Graphs Description Information
url <- paste0( 
  "https://cgiar-my.sharepoint.com/:x:/g/personal/cleaned_cgiar_org/,
   "EZRh6GUTtwNDlCTlPRhKGIUB9GbRIK-_rf3nEddPlqmfxw?e=OYR2Fn&nav=MTVfezIxRDRCNDM4LUJFMEMtNDE3My1BMEQyLTZEQzBCMkE4QTRBQ30"
)
graphs_desc <- read_sharepoint(
  url = url,
  sheet = "Graphs Information"
)

# Plots default values
max_feed_items <- max_seasons <- 20

# most recent file in data/objects
files_info <- file.info(list.files(file.path("data", "study_objects"), full.names = TRUE))
# Get the most recent file name
most_recent_file <- basename(rownames(files_info)[which.max(files_info$mtime)])
# Get the last modification date
last_modification_date <- format(
  files_info$mtime[which.max(files_info$mtime)], "%B %d, %Y"
)

# Stat Counter project and security key (.Renviron)
sc_project <- Sys.getenv("SC_PROJECT")
sc_security <- Sys.getenv("SC_SECURITY")
