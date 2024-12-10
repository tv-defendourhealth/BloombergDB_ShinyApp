#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Generate scorecard ----------------------------------------------------------

generate_scorecard <- function() {
  CO2_scores <- calculate_emission_score(db, "co2_perU", summary_by_plastic, "avg_co2e_perU", "known_supply_chains", cutoffs <- c(.5,1,2,5))
  TOX_scores <- calculate_emission_score(db, "tox_perU", summary_by_plastic, "avg_tox_perU", "known_supply_chains", cutoffs <- c(.5,1,2,5))
  RSEI_scores <- calculate_emission_score(db, "facility_RSEI_score", summary_by_plastic, "avg_rsei", "known_supply_chains", cutoffs <- c(.5,1,2,5))
  income_scores <- calculate_demographic_score(summary_by_plastic, "avg_income", "known_supply_chains", US_low_income, cutoffs <- c(0, 2, 5, 10))
  race_scores <- calculate_demographic_score(summary_by_plastic, "avg_poc", "known_supply_chains", US_POC, cutoffs <- c(0, 2, 5, 10))
  
  # Create combined score dataframe
  combined_scores <- CO2_scores %>%
    select(supply_chain, co2_score = score) %>%
    left_join(select(TOX_scores, supply_chain, tox_score = score), by = "supply_chain") %>%
    left_join(select(RSEI_scores, supply_chain, rsei_score = score), by = "supply_chain") %>%
    left_join(select(income_scores, supply_chain, income_score = score), by = "supply_chain") %>%
    left_join(select(race_scores, supply_chain, race_score = score), by = "supply_chain")
  
  return(combined_scores)
}


# Server function ----------------------------------------------------------

server <- function(input, output, session) {

  # --------------------- TRACK THE ACTIVE BUTTON PRESS (NULL VAL TOO)
  active_tab <- reactiveVal(NULL)
  
  # Observe button clicks and update the active tab
  observeEvent(input$production_btn, {
    active_tab("production")
    #delay(500, { session$sendCustomMessage(type = 'scroll', message = 'dynamic_content') })
    #session$sendCustomMessage(type = 'scroll', message = 'dynamic_content')  # Call custom scroll
    #shinyjs::scrollTo("dynamic_content")
  })
  
  observeEvent(input$additives_btn, {
    active_tab("additives")
    #delay(500, { session$sendCustomMessage(type = 'scroll', message = 'dynamic_content') })
    #session$sendCustomMessage(type = 'scroll', message = 'dynamic_content')  # Call custom scroll
    #shinyjs::scrollTo("dynamic_content")
  })
  
  observeEvent(input$products_btn, {
    active_tab("products")
    #delay(500, { session$sendCustomMessage(type = 'scroll', message = 'dynamic_content') })
    #session$sendCustomMessage(type = 'scroll', message = 'dynamic_content')  # Call custom scroll
    #shinyjs::scrollTo("dynamic_content")
  })
  
  # --------------------- RENDER DYNAMIC CONTENT BASED ON ACTIVE BUTTON PRESS
  output$dynamic_content <- renderUI({
    req(active_tab())  # Ensure that active_tab is not NULL
    
    if (active_tab() == "production") {
        
      return(
          div(
            id = "dynamic_content",
            style = "width: 100%; margin: auto; min-height: 1000px;",  # Adjust height as needed
            navset_card_tab(
              id = "plastic_tabs",
              !!!lapply(names(plastic_types)[names(plastic_types) != "All plastic"], function(plastic_name) {
                nav_panel(
                  title = plastic_name,
                  create_production_panel(plastic_name) # Supply chain diagram
                  #div(data-aos = "fade-up", create_production_panel(plastic_name))
                  #aos(create_production_panel(plastic_name), animation = "fade-up")
                  )
                })
              )
            )
        )  
      } 
    
    else if (active_tab() == "additives") {
      
      return(
          div(
          id = "dynamic_content",
          style = "width: 100%; margin: auto; min-height: 1000px;",  # Adjust height as needed 
          tagList(
            card(
              full_screen = TRUE,
              style = "margin-bottom: 20px;",
              card_body(
                div(
                  style = "margin-top: 20px;",
                  fa("flask", height = "2em"),
                  p("Fossil-fuel plastics always require the addition of toxic additives, various chemicals that impart different properties.
                There are thousands of types of additives that can change the plastic color, strength, durability, flexibility, heat resistance, flammabilty, and more.  
                Industry does not disclose all the additives they use so it's impossible to create a comprehensive list.
                Below are some of the additives we have found to be used in each type of plastic and the associated hazard scores.
                ", 
                    style = "font-size: .9em;"),
                  p(style = "font-size: 0.9em; font-style: italic;",
                    paste("Red/3 = High hazard (Greenscreen LT-1, LT-P1, BM-1),
                 Orange/2 = Moderate hazard (Greenscreen LT-2, BM-2, or unknown),
                 Yellow/1 = Low hazard (Greenscreen LT-3 or BM-3),
                 Green/0 = No hazard (Greenscreen BM-4)")),
                ),
                card(
                  full_screen = TRUE,
                  height = "100%",
                  card_body(
                    div(uiOutput("filter_ui"),
                        DTOutput("additives_table"), style = "font-size:85%")
                    )
                  )
                )
              )
            )
          )
      )
      }
    
    else if (active_tab() == "products") {
      return(
        div(
          id = "dynamic_content",
          style = "width: 100%; margin: auto; min-height: 1000px;",  # Adjust height as needed
          navset_card_tab(
            id = "plastic_tabs",
            !!!lapply(names(plastic_types_forproducts), function(plastic_name) {
              nav_panel(
                title = plastic_name,
                create_product_panel(plastic_name) # Supply chain diagram
              )
            })
          )
        )
      )  
    }
    
    # If no button has been clicked, return NULL (blank)
    return(NULL)
  })
  
  # --------------------- CREATE MAPS
  observe({
    req(active_tab() == "production")  # Only run if in production tab
    
    for(full_name in names(plastic_types)) {
      local({
        local_full_name <- full_name
        local_abbreviation <- plastic_types[local_full_name]
        
        output[[paste0("facility_map_", local_abbreviation)]] <- renderLeaflet({
          create_custom_map(map_data,
                            input[[paste0("metric_", local_abbreviation)]],
                            get_metric_name(input[[paste0("metric_", local_abbreviation)]]),
                            local_abbreviation, 
                            color_palette = c("#fee8c8", "#fdbb84", "#e34a33"))
        })
      })
    }
  })
  
  # --------------------- CREATE ADDITIVE TABLE
  additives_data$plastic <- as.factor(additives_data$plastic)
  additives_data$category <- as.factor(additives_data$category)
  
  output$filter_ui <- renderUI({
    req(active_tab() == "additives")  # Only show filters when in additives tab
    
    fluidRow(
      column(6,
             selectInput("plastic_filter", "Filter by Plastic:",
                         choices = c("All", levels(additives_data$plastic)),  # Include all unique values
                         selected = "All")
      ),
      column(6,
             selectInput("category_filter", "Filter by Additive Category:",
                         choices = c("All", levels(additives_data$category)),  # Include all unique values
                         selected = "All")
      )
    )
  })
  
  output$additives_table <- renderDT({
    req(active_tab() == "additives")  # Only run if in additives tab
    
    # Filter data based on selections
    filtered_data <- additives_data
    
    if (input$plastic_filter != "All") {
      filtered_data <- filtered_data[filtered_data$plastic == input$plastic_filter, ]
    }
    
    if (input$category_filter != "All") {
      filtered_data <- filtered_data[filtered_data$category == input$category_filter, ]
    }
    
    datatable(
      filtered_data,
      options = list(
        pageLength = 200,
        autoWidth = TRUE,
        dom = 'Bfrtip',  # Include buttons and filtering
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  # Optional: Add export buttons
      )
    ) %>% 
      formatStyle(
        'hazard_score',
        backgroundColor = styleEqual(c(0, 1, 2, 3), 
                                     c('green','yellow','orange','red')),
        color = styleEqual(c(0, 1, 2, 3), c('white','black','black','white'))
      )
  })
  
  
  # --------------------- PRODUCT CATEGORIES
  
    # Load data:
  marketshare <- readRDS("data/marketshare.rds")
  categories <- readRDS("data/categories.rds")

  # Create pie charts for each plastic type:
  lapply((names(plastic_types_forproducts)), function(plastic_name) {
    
    plastic_abbrev <- plastic_types_forproducts[plastic_name]
    
    source_info <- marketshare[marketshare$plastic == plastic_abbrev, "source_s"][1,]

     output[[paste0("pie_", plastic_abbrev)]] <- renderPlotly({
      
      data <- marketshare[marketshare$plastic == plastic_abbrev, ]
      
      # Define the number of categories
      num_categories <- nrow(data)
      # Create a color palette function with RdBu
      color_palette <- colorRampPalette(brewer.pal(9, "YlGn")) 
      # Generate the exact number of colors needed
      chart_colors <- color_palette(num_categories)
      
      p <- plot_ly(data, labels = ~category, values = ~percent_plastic, type = "pie",
              marker = list(colors = chart_colors),
              source = paste0("pie_", plastic_abbrev),
              hoverinfo = "label+percent",
              textinfo = "label+percent")  %>%
        layout(title = paste(names(plastic_types_forproducts), "Market Share"),
               showlegend = FALSE,
               paper_bgcolor = 'rgba(0,0,0,0)', # transparent background
               plot_bgcolor = 'rgba(0,0,0,0)',
               font = list(color = 'white'),
               annotations = list( #add the source!
                 list(
                   x = 0.5,
                   #y = -0.1,
                   y = 0,
                   text = paste("Source:", source_info),
                   showarrow = FALSE,
                   xref = 'paper',
                   yref = 'paper',
                   xanchor = 'center',
                   yanchor = 'top',
                   font = list(size = 8, color = 'white')
                 )
                 )) %>%
        event_register("plotly_hover") %>%
        event_register("plotly_unhover")
        
        p
    })
  })

  # Handle the ability to hover/unhover and display product types
  lapply(names(plastic_types_forproducts), function(plastic_name) {

    plastic_abbrev <- plastic_types_forproducts[plastic_name]
    
    # Hover event:
    suppressWarnings(observeEvent(event_data("plotly_hover", source = paste0("pie_", plastic_abbrev)), {
      hover_data <- event_data("plotly_hover", source = paste0("pie_", plastic_abbrev))
      
      output[[paste0("product_info_", plastic_abbrev)]] <- renderUI({
        if (!is.null(hover_data) && !is.null(hover_data$pointNumber)) {
          data <- marketshare[marketshare$plastic == plastic_abbrev, ]
          category <- data$category[hover_data$pointNumber + 1] # +1 because it's 0-indexed
          
          category_info <- categories %>% 
            filter(plastic == !!plastic_abbrev, category == !!category)
          
          if (nrow(category_info) > 0) {
            # Separate subcategories with images and without
            with_images <- category_info %>% filter(!is.na(jpg_name))
            without_images <- category_info %>% filter(is.na(jpg_name)) %>% pull(subcategories)
            
            tagList(
              h5(paste("Example", category, "products made from", plastic_abbrev)),
              
              # Display subcategories with images
              if (nrow(with_images) > 0) {
                div(style = "display: flex; flex-wrap: wrap;",
                    lapply(1:nrow(with_images), function(i) {
                      div(style = "margin: 10px; text-align: center;",
                          img(src = file.path(with_images$jpg_name[i]), 
                              style = "max-width: 100px; max-height: 100px;"),
                          p(with_images$subcategories[i])
                      )
                    })
                )
              },
              
              # Display other examples as text
              if (length(without_images) > 0) {
                tagList(
                  h6("Other examples of products:"),
                  tags$ul(
                    lapply(without_images, function(product) {
                      tags$li(product)
                    })
                  )
                )
              }
            )
          } else {
            p("No product examples found for this category.")
          }
        } 
      })
    }))
    
    # Unhover event:
    suppressWarnings(observeEvent(event_data("plotly_unhover", source = paste0("pie_",plastic_abbrev)), {
      output[[paste0("product_info_", plastic_abbrev)]] <- renderUI({
        
        data <- marketshare[marketshare$plastic == plastic_abbrev, ]
        descriptor <- data$descriptor[1]
        
        HTML(descriptor)
      })
    }, ignoreNULL = FALSE))
  })


  
  # FOR FULL DATA TABLE: -------------------------------------------------------------------------
  
  filtered_data <- reactive({
    data <- display_db
    
    if (input$owner_name != "All") {
      data <- data[data$`Owner Name` == input$owner_name, ]
    }
    
    if (input$state != "All") {
      data <- data[data$State == input$state, ]
    }
    
    if (input$city != "All") {
      data <- data[data$City == input$city, ]
    }
    
    if (input$supply_chain != "All") {
      data <- data[grepl(input$supply_chain, data$`Associated supply chains`), ]
    }
    
    return(data)
  })
  
  # Render the filtered DataTable
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 200,
        autoWidth = TRUE
      )
    )
  })
  
}