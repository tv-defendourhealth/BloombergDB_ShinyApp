# Load libraries ----------------------------------------------------------


library("shiny")
library("bslib")
library("tidyverse")
library("mapview")
library("sf")
library("leaflet")
library("ggrepel")
library("DT")
library("plotly")
library("RColorBrewer")

# test 

# Source scripts -------------------------------------------------------------------------

source("2-create-maps.R")
source("3-create-scores.R")

display_db <- readRDS("data/full_db_display.rds") 

additives <- readRDS("data/additives.rds")

# Useful stuff ----------------------------------------------------------

plastic_types <- c(
  "polypropylene" = "PP",
  "polyethylene" = "PE",
  "polyvinyl chloride" = "PVC",
  "polystyrene" = "PS",
  "PET (polyethylene terephthalate)" = "PET",
  "PLA (polylactic acid)" = "PLA",
  "All plastic" = "All"
)

plastic_types_forproducts <- c(
  "polypropylene" = "PP",
  "high density polyethylene" = "HDPE",
  "low density polyethylene" = "LDPE",
  "polyvinyl chloride" = "PVC",
  "polystyrene" = "PS",
  "PET (polyethylene terephthalate)" = "PET",
  "PLA (polylactic acid)" = "PLA"
  )

metrics <- c(
  "CO2 Emissions" = "co2e",
  "Toxic Emissions" = "total_tox",
  "Health Impact (RSEI)" = "facility_RSEI_score"
)

get_plastic_name <- function(code) {
  names(plastic_types)[plastic_types == code]
}

get_metric_name <- function(code) {
  names(metrics)[metrics == code]
}

# Helper Functions --------------------------------------------------------

create_plastic_panel <- function(plastic_name) {
  plastic_code <- plastic_types[plastic_name]
  tagList(
    fluidRow(
      column(6, 
             h3(HTML(
               paste(
                 '<span style="font-size: 18px;">The </span>',
                 '<span style="font-size: 32px;"><strong>', tolower(plastic_name), '</strong></span>',
                 '<span style="font-size: 18px;"> production harm scorecard</span>'
               ))),
             dataTableOutput(paste0("scorecard_", plastic_code))
      ),
      column(6,
             img(src = "score_key.jpg", width = "50%", style = "display: block; margin: 35px auto 0;"),
             div(
               style = "text-align: center; width: 70%; margin: 10px auto;",
               p("How did we do this?", style = "font-size: 1em; font-weight: bold; margin-bottom: 10px;"),
               p(HTML("Facilities associated with each supply chain were averaged together. 
                  For CO2 emissions, toxic emissions, and RSEI scores, each supply chain was scored based on how it compared to the total distribution of data in the database.
                  For income and race disparity, the supply chain was scored based on how it compared to the US average for each metric."),
                 style = "font-size: 0.5em; margin-bottom: 10px;"),
               p(HTML("Please see the Methods tab at the top for more details"),
                 style = "font-size: 0.5em;")
             )
      )
    ),
    fluidRow(
      column(6, plotlyOutput(paste0("pie_", plastic_code))),
      column(6, uiOutput(paste0("product_info_", plastic_code)))
    )
  )
}

