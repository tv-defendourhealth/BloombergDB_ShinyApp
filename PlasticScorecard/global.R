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
library("fontawesome")
library("aos")
library("shinyjs")

# Source scripts -------------------------------------------------------------------------

source("2-create-maps.R")
source("3-create-scores.R")

display_db <- readRDS("data/full_db_display.rds") 

additives_data <- readRDS("data/additives.rds")

map_data <- readRDS("data/for_map.rds")

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
  "Toxic Emissions" = "total_tox"
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

create_production_panel <- function(plastic_name) {
    monomer <- switch(plastic_name,
                      "polypropylene" = "propylene",
                      "polyethylene" = "ethylene",
                      "polyvinychloride" = "vinyl chloride",
                      "polystyrene" = "styrene",
                      "PET (polyethylene terephthalate)" = "terephthalate and ethylene glycol",
                      "PLA (polylactic acid)" = "lactide")
  
  tagList(
   
    # Spacer and supply chain diagram explanation
    div(
      style = "margin-top: 20px;",  # Add space above the map explanation
      fa("industry", height = "2em"),
      p(paste0(tools::toTitleCase(plastic_name), " plastic is made by stringing together several monomers of ", monomer, 
               ". Below are the steps used in making ", monomer, " and the associated hazards. 
             Facilities across the U.S. are involved in the process of making ", plastic_name, "."))
    ),
    card(
      div(
        style = "text-align: center;",
        img(src = paste0(plastic_types[plastic_name], "_supplychain.png"), width = "85%")
      ),
      p(style = "font-size: 0.9em; font-style: italic;",
      paste("Red = High hazard (Greenscreen LT-1, LT-P1, BM-1),
     Orange = Moderate hazard (Greenscreen LT-2, BM-2, or unknown),
     Yellow = Low hazard (Greenscreen LT-3 or BM-3),
     Green = No hazard (Greenscreen BM-4)"))
    ),
    
    # Spacer and map explanation
    div(
      style = "margin-top: 20px;",  # Add space above the map explanation
      fa("map-location-dot", height = "2em"),
      p("The map below shows facilities across the U.S. involved in the production of ", tools::toTitleCase(plastic_name), 
        ". Select a metric from the dropdown menu to visualize data related to these facilities.
        Click on the facility to see more info about the facility.")
    ),
    
    card(
      full_screen = TRUE,
      height = "100%",
      style = css(margin = "0 -0.25rem 0 0"),
      card_body(
        selectInput(inputId = paste0("metric_", plastic_types[plastic_name]), 
                    label = "Select Metric:",
                    choices = metrics,
                    selected = "co2e"),
        leafletOutput(outputId = paste0("facility_map_", plastic_types[plastic_name]), height = "400px")
      )
    )
  )
}

create_product_panel <- function(plastic_name) {
    fluidRow(
      column(6, plotlyOutput(paste0("pie_", plastic_types_forproducts[plastic_name]))),
      column(6, uiOutput(paste0("product_info_", plastic_types_forproducts[plastic_name])))
    )
}


# create_product_panel <- function(plastic_name) {
#   plastic_code <- plastic_types_forproducts[plastic_name]
#   
#   layout_column_wrap(
#     width = 1,
#     heights_equal = "row",
#     style = css(
#       min_height = "700px",
#       margin = "1rem"
#     ),
#     card(
#       full_screen = TRUE,
#       height = "100%",
#       style = css(margin = "0 0 0 -0.25rem"),
#       card_body(
#         div(
#           style = "display: flex; align-items: center; margin-bottom: .5rem;",
#           img(src = "products-icon.png", width = "6%", style = "margin-right: 2rem;"),
#           h2("Explore where you will find each type of plastic here", 
#              style = "font-size: 1.2em; margin: 0;")
#         ),
#         navset_card_tab(
#           id = "plastic_tabs",
#           !!!lapply(names(plastic_types_forproducts)[names(plastic_types_forproducts) != "All plastic"], 
#                     function(plastic_name) {
#                       nav_panel(
#                         plastic_name,
#                         fluidRow(
#                           column(6, plotlyOutput(paste0("pie_", plastic_types_forproducts[plastic_name]))),
#                           column(6, uiOutput(paste0("product_info_", plastic_types_forproducts[plastic_name])))
#                         )
#                       )
#                     })
#         )
#       )
#     )
#   )
# }