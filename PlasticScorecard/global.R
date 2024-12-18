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
  "polyethylene terephthalate (PET)" = "PET",
  "polyethylene (PE)" = "PE",
  "polyvinyl chloride (PVC)" = "PVC",
  "polypropylene (PP)" = "PP",
  "polystyrene (PS)" = "PS",
  "polylactic acid (PLA)" = "PLA"
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
  "POC" = "pc_poc",
  "Low income" = "pc_low_income"
)

get_plastic_name <- function(code) {
  names(plastic_types)[plastic_types == code]
}

get_metric_name <- function(code) {
  names(metrics)[metrics == code]
}

plastic_descriptions <- list(
  "PP" = "Polypropylene (PP) is a \'number 5\' plastic and is considered highly versatile. 
          The versatility means you will find it in several products like toys, kitchenware, and household appliances. 
          For example, it’s often used in food containers because it’s microwave safe, medical devices because it’s sterilizable, 
          automotive parts because it can withstand high temperature and impact, and electrical components because it’s a good insulator.",
  
  "HDPE" = "High-density polyethylene (HDPE) is a \'number 2\' plastic. 
          HDPE is lightweight but strong, and often used for sturdy containers or piping. 
          You will often find HDPE in your milk jugs, shampoo bottles, and laundry detergent containers. 
          Switching to products with less packaging, like shampoo bars or powdered laundry detergent cuts down on HDPE demand.",
  
  "LDPE" = "Low-density polyethylene (LDPE) is a \'number 4\' plastic. 
            Due to its low density, LDPE is usually shapeless and transparent. 
            You will often find LDPE in products like plastic bags, ziplocks, liners, or flexible containers like squeezable bottles. 
            Switching to products, like reusable food storage bags, can cut down on LDPE demand.",
  
  "PVC" = "Polyvinyl Chloride (PVC) is a \'number 3\' plastic and can be formulated with a wide range of additives for different uses. 
          PVC is commonly used in building materials, such as pipes, cable insulation, or flooring. 
          PVC film is also often found in food packaging. 
          Many bath toys and squishy children’s toys are also made with PVC, commonly with phthalates, a known harmful additive for children’s health and development. 
          Alternative options for PVC include tile or wood flooring, natural rubber toys, and glass or stainless steel packaging.",
  
  "PS" = "Polystyrene (PS) is a \'number 6\' plastic and is largely used for packaging, including food packaging. 
          Polystyrene can be used in various forms: Expanded polystyrene (EPS) and high impact polystyrene (HIPS), are both rigid, impact-resistant plastic often used in packaging materials. 
          Polystyrene foam (also known as Styrofoam) is moisture resistant and a good thermal insulator, and you will see it used in cups, meat/poultry trays, or egg cartons. 
          You can use non-plastic alternatives to styrofoam to-go containers or use brown paper fill instead of PS in packages to reduce PS use.",
  
  "PET" = "Polyethylene terephthalate (PET) is a \'number 1\' plastic known to be lightweight and strong. 
          It is used for disposable soda and water bottles, as well as other food containers like peanut butter jars and clamshell packaging. 
          Another major use of PET is in polyester fabrics. 
          Many alternatives exist to PET plastic, such as reusable bottles or natural fibers like cotton, wool, or linen.",
  
  "PLA" = "Polylatctic acid (PLA), considered a \'number 7\' plastic, is sometimes referred to as a “bioplastic” because it is derived from resources like sugar cane or corn starch (instead of fossil fuels). 
          It is often used in products marketed as biodegradable, such as food packaging or disposable cutlery. 
          It is also the primary material used in 3D printing."
)

plastic_numbers <- list(
  "PP" = "5",
  "HDPE" = "2",
  "LDPE" = "4",
  "PVC" = "3",
  "PS" = "6",
  "PET" = "1",
  "PLA" = "7"
  )

# Helper Functions --------------------------------------------------------

create_supplychain_panel <- function(plastic_name) {
  plastic_abbrev <- plastic_types[plastic_name]
  number <- plastic_numbers[plastic_abbrev]
  monomer <- switch(plastic_name,
                    "polypropylene (PP)" = "propylene",
                    "polyethylene (PE)" = "ethylene",
                    "polyvinychloride (PVC)" = "vinyl chloride",
                    "polystyrene (PS)" = "styrene",
                    "polyethylene terephthalate (PET)" = "terephthalate and ethylene glycol",
                    "polylactic acid (PLA)" = "lactide")
  
  tagList(
    
    # Spacer and supply chain diagram explanation
    div(
      style = "text-align: left; margin: auto;",  # Add space above the map explanation
      #fa("industry", height = "2em"),
      if (plastic_name == "polyethylene (PE)") {
        div(
          style = "display: flex; align-items: center;",
          img(src = paste0("recyclesymbol_","2-4",".png"), height = "50px", style = "margin-right: 10px;"),
          h4(plastic_name)
        )
      }
      else {
        div(
          style = "display: flex; align-items: center;",
          img(src = paste0("recyclesymbol_",number,".png"), height = "50px", style = "margin-right: 10px;"),
          h4(plastic_name)
        )
      }
      ,
      p(paste0("The main component of all plastic is a polymer. 
        A polymer is a large molecule that's made up of many copies of smaller molecules, called monomers, that are chemically strung together.
        The monomer for ", plastic_name, " is ", monomer, ".")),
      p(paste0("There are many steps to go from fossil fuels to ", monomer, ". 
      Below you can see each intermediate chemical name and unique identifier number (CAS number). 
      Each of the intermediate chemicals created in the process have their own hazardous impact on human health.")),
    ),
    card(
      div(
        style = "text-align: left;",
        img(src = paste0(plastic_abbrev, "_supplychain.png"), width = "85%")
      ),
    ),
    div(
      style = "display: flex; align-items: center;",
      # TEXT:
      div(
        #style = "flex: 1; margin-left: 5%; margin-right: 5%; margin-bottom: 5%;",
        style = "flex: 1.2; text-align:left; margin: auto; margin-bottom: 5%;",
        p(HTML(paste0("To characterize the inherent hazards of chemicals, we created a \"hazard score\" that comes from the GreenScreen for Safer Chemicals. 
                    GreenScreen compiles data and scientific literature on 18 different human health and environmental hazard endpoints and assigns a corresponding score.
                    To learn more about the GreenScreen, see ",
                      tags$a(href = "https://www.greenscreenchemicals.org/assess/method", target = "_blank", "this link."))))
      ),
      
      # IMAGE: HAZARD SCORE TABLE
      div(
        #style = "flex: 1; margin-left: 5%; margin-bottom: 5%;",
        style = "flex: 1; margin: auto; margin-left: 5%; margin-bottom: 5%;",
        img(src = "hazard_score_table.png", 
            alt = "Hazard Score Table", 
            style = "width: 80%; max-width: 600px;")
      )
    )
  )
}

create_map_panel <- function(plastic_name) {
  plastic_abbrev <- plastic_types[plastic_name]
  number <- plastic_numbers[plastic_abbrev]
  monomer <- switch(plastic_name,
                    "polypropylene (PP)" = "propylene",
                    "polyethylene (PE)" = "ethylene",
                    "polyvinychloride (PVC)" = "vinyl chloride",
                    "polystyrene (PS)" = "styrene",
                    "polyethylene terephthalate (PET)" = "terephthalate and ethylene glycol",
                    "polylactic acid (PLA)" = "lactide")
  
  tagList(
    # Spacer and map explanation
    div(
      style = "text-align: left;",  # Add space above the map explanation
      #fa("map-location-dot", height = "2em"),
      if (plastic_name == "polyethylene (PE)") {
        div(
          style = "display: flex; align-items: center;",
          img(src = paste0("recyclesymbol_","2-4",".png"), height = "50px", style = "margin-right: 10px;"),
          h4(plastic_name)
        )
      }
      else {
        div(
          style = "display: flex; align-items: center;",
          img(src = paste0("recyclesymbol_",number,".png"), height = "50px", style = "margin-right: 10px;"),
          h4(plastic_name)
        )
      },
      p(HTML(paste0(
        "<p>Click on the facility to see relevant information about each facility, such as self-reported emissions, the demographics of the area (3 mile radius), and the associated supply chains (note: facilities often contribute to the making of multiple types of plastic!). 
        </p>
        <p>The facilities on the map are color coded by their hazardous impact, using different metrics (CO2 emissions, toxic emissions, income, and race - see below for more details). 
        Use the drop down menu to select a metric you’re interested in:</p>
        "
      ))
      )),
    
    div(
      card(
        full_screen = TRUE,
        height = "100%",
        style = css(margin = "0 -0.25rem 0 0"),
        card_body(
          selectInput(inputId = "selected_metric", 
                      label = "Select Metric:",
                      choices = metrics,
                      selected = "co2e"),
          leafletOutput(outputId = paste0("facility_map_", plastic_abbrev), height = "500px")
        )
      )
    ),
    div(
      style = "text-align: left;",
      p(HTML(paste0(
        "<ul> 
          <li><strong>CO2 emissions:</strong> CO2 is a greenhouse gas emitted by the burning of fossil fuels and manufacturing and is a primary contributor to a warming planet.  
        The data comes from EPA’s FLIGHT tool and reflects self-reported data. It’s worth noting that self-reported GHG data has often shown to be underreported! 
          </li> 
          <li><strong>Toxic emissions:</strong> Toxic emissions refer to the total pounds of toxic chemicals released into the air, ground, or water. 
        The data comes from the Toxic Release Inventory (TRI), an EPA tool to track toxic emissions from industry. Like CO2 emissions, the TRI data is self-reported and has been shown to be far below actual emission values.
        </li>
          <li><strong>POC:</strong> This refers to the percent of individuals within a 3 mile radius of the facility that are a race other than white alone and/or list their ethnicity as Hispanic or Latino (ie. anyone who is not white and non-Hispanic). 
        The data and definition comes from EJScreen, an Environmental Justice Screening and Mapping tool by the EPA.
        </li>
          <li><strong>Low income:</strong> This refers to the percent of individuals within a 3 mile radius of the facility that are in households where the household income is less than or equal to twice the federal poverty level. 
        The data and definition comes from EJScreen, an Environmental Justice Screening and Mapping tool by the EPA.
        </li> 
          </ul>"
      )))
    )
  )
}

create_product_panel <- function(plastic_name) {
  plastic_abbrev = plastic_types[plastic_name]
  descriptor = plastic_descriptions[plastic_abbrev]
  number = plastic_numbers[plastic_abbrev]
  div(
    style = "text-align: left; margin: auto; margin-bottom: 30%;",
    fluidRow(
      style = "display: flex; align-items: center;",
      column(1, img(src = paste0("recyclesymbol_",number,".png"), width = "100%")),
      column(11, p(descriptor))
    ),
    fluidRow(
      style = "display: flex; align-items: center;",
      column(6, plotlyOutput(paste0("pie_", plastic_abbrev))),
      column(6, uiOutput(paste0("product_info_", plastic_abbrev)))
    )
  )
}
