#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


ui <- page_navbar(
  
  title = "Plastic Scorecard",
  #theme = bs_theme(version = 5, bootswatch = "united"),
  theme = bs_theme(
    bg = "#fcfcfa",
    fg = "#101010",
    primary = "#a4c24a",
    secondary = "#e68c37",
    success = "#2cc0e6",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  id = "home_page",
  
# -------------------------------------------------------------------------

nav_panel("Home",
          # -------------------------------------------------------------------------
          # INTRO IMAGE & TITLE
           layout_column_wrap(
             width = 1,
             heights_equal = "row",
             style = css(
               background = "url('plastic_bottles.jpg') no-repeat center center",
               background_size = "cover",
               min_height = "400px"
             ),
             card(
               full_screen = FALSE,
               height = "400px",
               style = "background-color: rgba(0, 0, 0, 0.6); border: none;",  # Make background transparent
               card_body(
                 class = "d-flex align-items-center justify-content-center",
                 h1("Welcome to Defend Our Health's Plastic Scorecard", 
                    style = "font-size: 24px; font-weight: bold; color: white; text-align: center;")
               )
             )
           ),
          # -------------------------------------------------------------------------
          # INTRO TEXT ABOUT THE SCORECARD
           layout_column_wrap(
             width = 1,
             heights_equal = "row",
             style = css(
               min_height = "300px"  # Adjust this value as needed
             ),
             card(
               class = "m-4", #adds margin
               full_screen = FALSE,
               height = "100%",
               card_header("Welcome to the Plastic Scorecard Project"),
               card_body(
                 "The Plastic Scorecard is an initiative aimed at identifying the harms of various types of plastic commonly used today, through the lens of plastic production. 
                  Our goal is to provide comprehensive data and analysis to help individuals, businesses, and policymakers make informed decisions about plastic use.
                  We identified the associated plastic type for each production facility in the United States and compared their harms in several key metrics: 
                  Climate impact, Toxic pollution, Health impact, and Environmental Justice."
               ),
               card_body(
                 "Below you can explore how each type of plastic scored in these harm areas, as well as examine the many commonly used additives and their associated health harms. 
                  The full dataset is also available in the Explore Data tab."
               )
             )
           ),
           
          # -------------------------------------------------------------------------
             # SCORECARD
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            style = css(
              min_height = "700px",  # Adjust this value as needed
              margin = "1rem",  # Outer margin for the entire row
              padding = "0 0.5rem"  # Inner padding between cards (half on each side)
            ),
             card(
               full_screen = TRUE,
               height = "100%",
               style = css(margin = "0 0 0 -0.25rem"),
               card_body(
                 div(
                   style = "display: flex; align-items: center; margin-bottom: .5rem;",
                   img(src = "factory-icon.png", width = "6%", style = "margin-right: 2rem;"),
                   h2("Explore the harms of production for each type of plastic here", 
                      style = "font-size: 1.2em; margin: 0;")
                 ),
                 navset_card_tab(
                   id = "plastic_tabs",
                   !!!lapply(names(plastic_types), function(plastic_name) {
                     plastic_code <- plastic_types[plastic_name]
                     nav_panel(
                       title = plastic_name,
                       if (plastic_code == "All") {
                         fluidRow(
                           column(12, 
                                  h3(HTML(paste('<span style="font-size: 18px;">the production harm scorecard for</span>
                                                <span style="font-size: 32px;"><strong>all plastic</strong></span>'))),
                                  dataTableOutput(paste0("scorecard_", plastic_code))
                           )
                         )
                       } else {
                         fluidRow(
                           column(6, 
                                  h3(HTML(
                                    paste(
                                      '<span style="font-size: 18px;">The </span>',
                                      '<span style="font-size: 32px;"><strong>', (plastic_name), '</strong></span>',
                                      '<span style="font-size: 18px;"> production harm scorecard</span>'
                                    ))),
                                  dataTableOutput(paste0("scorecard_", plastic_code))
                           ),
                           column(6,
                                  img(src = "score_key.jpg", width = "60%", style = "display: block; margin: 100px auto 0;")
                           ),
                           div(
                             style = "text-align: left; width: 70%; ",
                             p("How did we do this?", style = "font-size: .8em; font-weight: bold; font-style: italic; margin-bottom: 8px; margin-top: 12px;"),
                             p(HTML("Facilities associated with each supply chain were averaged together. 
                                 For CO2 emissions, toxic emissions, and RSEI scores, each supply chain was scored based on how it compared to the total distribution of data in the database.
                                 For income and race disparity, the supply chain was scored based on how it compared to the US average for each metric."),
                               style = "font-size: 0.75em; margin-bottom: 8px; font-style: italic;"),
                             p(HTML("Please see the Methods tab at the top for more details"),
                               style = "font-size: 0.75em; font-style: italic;")
                           )
                         )
                       }
                     )
                   })
                 )
               )
             )
           ),
          
          # -------------------------------------------------------------------------
          # CONSUMER PRODUCTS
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            style = css(
              min_height = "700px",
              margin = "1rem"
            ),
            card(
              full_screen = TRUE,
              height = "100%",
              style = css(margin = "0 0 0 -0.25rem"),
              card_body(
                div(
                  style = "display: flex; align-items: center; margin-bottom: .5rem;",
                  img(src = "products-icon.png", width = "6%", style = "margin-right: 2rem;"),
                  h2("Explore where you will find each type of plastic here", 
                     style = "font-size: 1.2em; margin: 0;")
                ),
                navset_card_tab(
                  id = "plastic_tabs",
                  !!!lapply(names(plastic_types_forproducts)[names(plastic_types_forproducts) != "All plastic"], 
                            function(plastic_name) {
                    nav_panel(
                      plastic_name,
                      fluidRow(
                        column(6, plotlyOutput(paste0("pie_",
                                                      plastic_types_forproducts[plastic_name]))),
                        column(6,
                               uiOutput(paste0("product_info_", plastic_types_forproducts[plastic_name])))
                      )
                    )
                  })
                )
              )
            )
          ),
          # -------------------------------------------------------------------------
          # MAP
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            style = css(
              min_height = "600px",  # Adjust this value as needed
              margin = "1rem",  # Outer margin for the entire row
              padding = "0 0.5rem"  # Inner padding between cards (half on each side)
            ),
            # MAP 
            card(
              #class = "m-4",
              full_screen = TRUE,
              height = "100%",
              style = css(
                margin = "0 -0.25rem 0 0"  # Negative right margin to counteract padding
              ),
              div(
                style = "display: flex; align-items: center; margin-bottom: .5rem;",
                img(src = "map-icon.png", width = "6%", style = "margin-right: 2rem;"),
                h2("Explore each plastic production facility and its harms here", 
                   style = "font-size: 1.2em; margin: 0;")
              ),
              card_body(
                selectInput("metric", "Select Metric:",
                            choices = metrics,
                            selected = "co2e"),
                selectInput("plastic_type", "Select Plastic Type:",
                            choices = plastic_types,
                            selected = "All"),
                leafletOutput("facility_map", height = "400px")
              )
            )),
          # -------------------------------------------------------------------------
          # CHEM ADDITIVES
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            style = css(
              min_height = "600px",  # Adjust this value as needed
              margin = "1rem",  # Outer margin for the entire row
              padding = "0 0.5rem"  # Inner padding between cards (half on each side)
            ),
            card(
              #class = "m-4",
              full_screen = TRUE,
              height = "100%",
              style = css(
                margin = "0 -0.25rem 0 0"  # Negative right margin to counteract padding
              ),
              div(
                style = "display: flex; align-items: center; margin-bottom: .5rem;",
                img(src = "chemical-icon.png", width = "6%", style = "margin-right: 2rem;"),
                h2("Explore some of the chemical additives found in each plastic type here", 
                   style = "font-size: 1.2em; margin: 0;")
              ),
              card_body(
                div(DTOutput("additives_table"), style = "font-size:85%")
              )
            ))
),

# -------------------------------------------------------------------------

  nav_panel("Methods", value = "methods_panel",
            h2("Methods"),
            p("This section will describe the methods used in our analysis.")
  ),
  
# -------------------------------------------------------------------------

  nav_panel("Sources",
            h2("Sources"),
            p("Here you can find information about our data sources.")
  ),
  
# -------------------------------------------------------------------------

  nav_panel("Explore Data",
            h2("Explore the Data"),
            
            # Dropdowns for filtering
            fluidRow(
              column(3, selectInput("owner_name", "Owner Name", choices = c("All", unique(display_db$`Owner Name`)))),
              column(3, selectInput("state", "State", choices = c("All", unique(display_db$State)))),
              column(3, selectInput("city", "City", choices = c("All", unique(display_db$City)))),
              column(3, selectInput("supply_chain", "Associated supply chains", choices = c("All", unique(unlist(strsplit(display_db$`Associated supply chains`, ",\\s*"))))))
            ),
            
            div(DTOutput("table"), style = "font-size:85%")
  )
)


