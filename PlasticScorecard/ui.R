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
                 h1("Learn about the harms of plastic production", 
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
               min_height = "370px",  # Adjust this value as needed
               margin_left = "10%",   
               margin_right = "10%",
               margin_top = "2%"
             ),
             HTML("<p>Fossil fuel-based plastics pose a major pollution crisis across every stage of their life cycle.
                 We often think of plastic pollution at the final stage: the waste and microplastics that pollute our oceans, landfills, and ecosystems.
                 However, the <strong>production phase</strong> – from fossil fuel extraction, to the manufacture of plastic products – is extremely polluting.</p>
                  <p>Here we provide data to illustrate three phases of the plastic production process:</p>
                  <ol style=\"padding-left: 40px;\">
                      <li>Petrochemical facilities <i class=\"fas fa-industry\" style=\"margin-left: 10px;\"></i></li>
                      <li>Chemical additives <i class=\"fas fa-flask\" style=\"margin-left: 10px;\"></i></li>
                      <li>Final products <i class=\"fas fa-bottle-water\" style=\"margin-left: 10px;\"></i></li>
                  </ol>
                  <p><strong>Click on the buttons below to explore each phase.</strong></p>"
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
                            margin: 0 0px;
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
              min_height = "150px"  # Adjust this value as needed
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
                    tags$div(style = "text-align: center; margin-top: 5px;", "Click to learn about U.S. petrochemical facilities")
                  ),
                  tags$span(class = "arrow", HTML("&rarr;")),
                  tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    actionButton("additives_btn", "", icon = icon("flask"), class = "btn-icon"),
                    tags$div(style = "text-align: center; margin-top: 5px;", "Click to learn about plastic chemical additives")
                  ),
                  tags$span(class = "arrow", HTML("&rarr;")),
                  tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    actionButton("products_btn", "", icon = icon("bottle-water"), class = "btn-icon"),
                    tags$div(style = "text-align: center; margin-top: 5px;", "Click to learn about plastic in products")
                  )
                )
              )
            )
          ),
          
          # CONTENT FOR EACH TAB ---------------------------------------------------------   
         
          #aos(uiOutput("dynamic_content"), animation = "fade-up")
          uiOutput("dynamic_content")
          
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


