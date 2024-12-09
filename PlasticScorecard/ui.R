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
    primary = "#a5ce41",
    secondary = "#d2702b",
    success = "#00a7d4",
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
          # INTRO TEXT ABOUT THE WEBSITE
           layout_column_wrap(
             width = 1,
             heights_equal = "row",
             style = css(
               min_height = "250px"  # Adjust this value as needed
             ),
             card(
               class = "m-4", #adds margin
               full_screen = FALSE,
               height = "100%",
               card_header("Welcome to the Plastic Scorecard Project"),
               card_body(HTML(
                 "The Plastic Scorecard is an initiative aimed at identifying the harms of various types of plastic commonly used today. 
                  Our goal is to present comprehensive data and analysis to help individuals, businesses, and policymakers make informed decisions about plastic use.
                  Here we present information about 6 of the most commonly used plastics. 
                 <br><br>
                 Click through the following buttons to learn about each plastic from production to final products.")
               )
             )
           ),
          
          # CREATE BUTTONS -------------------------------------------------------------------------
          
          tags$head(
            tags$style(HTML("
                            .button-container {
                            display: flex;
                            align-items: center;
                            justify-content: center;
                            height: 0vh; 
                            }
                            .arrow {
                            font-size: 36px;
                            margin: 0 20px;
                            }
                            .btn-icon {
                            font-size: 48px;
                            width: 120px !important;
                            height: 120px !important;
                            border-radius: 50%;
                            display: flex;
                            align-items: center;
                            justify-content: center;
                            background-color: #f8f9fa;
                            border: 2px solid #dee2e6;
                            transition: all 0.3s ease;
                            }
                            .btn-icon: hover {
                            background-color: #e9ecef; 
                            transform: scale(1.05);
                            }"))
          ),
          
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            style = css(
              min_height = "300px"  # Adjust this value as needed
            ),
            card(
              class = "borderless-card",
              style = "border: none; box-shadow: none;",
              full_screen = FALSE,
              height = "100%",
              card_body(
                tags$div(
                  class = "button-container",
                  style = "display: flex; align-items: center; justify-content: center; height: 100%;",
                  tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    actionButton("production_btn", "", icon = icon("industry"), class = "btn-icon"), #FONT AWESOME ICONS
                    tags$div(style = "text-align: center; margin-top: 10px;", "Click to explore Production")
                  ),
                  tags$span(class = "arrow", HTML("&rarr;")),
                  tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    actionButton("additives_btn", "", icon = icon("flask"), class = "btn-icon"),
                    tags$div(style = "text-align: center; margin-top: 10px;", "Click to explore Additives")
                  ),
                  tags$span(class = "arrow", HTML("&rarr;")),
                  tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    actionButton("products_btn", "", icon = icon("bottle-water"), class = "btn-icon"),
                    tags$div(style = "text-align: center; margin-top: 10px;", "Click to explore Products")
                  )
                )
              )
            )
          ),
          

          # PRODUCTION TABS ---------------------------------------------------------   
         
          uiOutput("dynamic_content"),
          
          # -------------------------------------------------------------------------
          # CHEM ADDITIVES
          # layout_column_wrap(
          #   width = 1,
          #   heights_equal = "row",
          #   style = css(
          #     min_height = "600px",  # Adjust this value as needed
          #     margin = "1rem",  # Outer margin for the entire row
          #     padding = "0 0.5rem"  # Inner padding between cards (half on each side)
          #   ),
          #   card(
          #     full_screen = TRUE,
          #     height = "100%",
          #     style = css(
          #       margin = "0 -0.25rem 0 0"  # Negative right margin to counteract padding
          #     ),
          #     div(
          #       style = "display: flex; align-items: center; margin-bottom: .5rem;",
          #       img(src = "chemical-icon.png", width = "6%", style = "margin-right: 2rem;"),
          #       h2("Explore some of the chemical additives found in each plastic type here", 
          #          style = "font-size: 1.2em; margin: 0;")
          #     ),
          #     card_body(
          #       div(DTOutput("additives_table"), style = "font-size:85%")
          #     )
          #   )),
          
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
          )
          ), #End of Home page

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


