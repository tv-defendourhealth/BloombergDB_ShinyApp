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

scorecard <- generate_scorecard()

# Server function ----------------------------------------------------------

server <- function(input, output, session) {
  
  # FOR MAP: -------------------------------------------------------------------------
  
  # Load the data
  map_data <- readRDS("data/for_map.rds")
  
  # Render the map
  output$facility_map <- renderLeaflet({
       
    # Call the custom map generation function sourced in 2-create-maps.Rmd
    create_custom_map(map_data, 
                      input$metric, 
                      get_metric_name(input$metric), #metric name
                      input$plastic_type, 
                      color_palette = c("#fee8c8", "#fdbb84", "#e34a33"))
  })
  
  # FOR SCORECARDS: -------------------------------------------------------------------------
  
  # Generate scorecards for each plastic type
  lapply(plastic_types, function(plastic_code) {
    output[[paste0("scorecard_", plastic_code)]] <- renderDataTable({
      if (plastic_code == "All") {
        # For the "All" tab, use all data
        data <- scorecard[-c(7),] #remove the last row which is NA  
        names(data)[names(data) == "supply_chain"] <- "Supply Chain"
        names(data)[names(data) == "co2_score"] <- get_metric_name("co2e")
        names(data)[names(data) == "tox_score"] <- get_metric_name("total_tox")
        names(data)[names(data) == "rsei_score"] <- get_metric_name("facility_RSEI_score")
        names(data)[names(data) == "income_score"] <- "Income Disparity"
        names(data)[names(data) == "race_score"] <- "Racial Disparity"
        
        # Define color mapping
        color_map <- c("1" = "#2cc0e6", "2" = "#ffc55e", "3" = "#e68c37", "4" = "#d03232", "5" = "#650202")
        
        # Create the datatable
        dt <- datatable(data, 
                        options = list(pageLength = -1, 
                                       dom = 't'),
                        rownames = FALSE,
                        class= "compact cell-border",
                        style = "bootstrap")
        
        # Apply styling to each score column
        score_columns <- c("Supply Chain", "CO2 Emissions", "Toxic Emissions", "Health Impact (RSEI)", "Income Disparity", "Racial Disparity")
        for (col in score_columns) {
          dt <- dt %>% formatStyle(
            col,
            backgroundColor = styleEqual(names(color_map), color_map),
            fontWeight = 'bold',
            fontSize = "80%"
          )
        }
        
        dt
      } 
      else {
        # For other tabs, filter by plastic_code
        data <- scorecard %>%
          filter(supply_chain == plastic_code) %>%
          select(-supply_chain) %>%
          pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
          mutate(
            Metric = case_when(
              Metric == "co2_score" ~ get_metric_name("co2e"),
              Metric == "tox_score" ~ get_metric_name("total_tox"),
              Metric == "rsei_score" ~ get_metric_name("facility_RSEI_score"),
              Metric == "income_score" ~ "Income Disparity",
              Metric == "race_score" ~ "Racial Disparity",
              TRUE ~ Metric
            )
          )
        
        # Define color mapping
        color_map <- c("1" = "#51a0e5", "2" = "#ffc55e", "3" = "#f59b3a", "4" = "#d03232", "5" = "#650202")
        
        datatable(data, 
                  options = list(pageLength = -1, 
                                 dom = 't'),
                  rownames = FALSE) %>%
          formatStyle(
            'Score',
            backgroundColor = styleEqual(names(color_map), color_map),
            fontWeight = 'bold'
          )
      } 
    })
  })
  
  # CONSUMER PRODUCTS: -------------------------------------------------------------------------
  

    # Load data:
  marketshare <- readRDS("data/marketshare.rds")
  categories <- readRDS("data/categories.rds")

  # Create pie charts for each plastic type:
  lapply((plastic_types_forproducts), function(plastic_code) {
    
    source_info <- marketshare[marketshare$plastic == plastic_code, "source_s"][1,]

     output[[paste0("pie_", plastic_code)]] <- renderPlotly({
      
      data <- marketshare[marketshare$plastic == plastic_code, ]
      
      # Define the number of categories
      num_categories <- nrow(data)
      # Create a color palette function with RdBu
      color_palette <- colorRampPalette(brewer.pal(9, "YlGn")) 
      # Generate the exact number of colors needed
      chart_colors <- color_palette(num_categories)
      
      p <- plot_ly(data, labels = ~category, values = ~percent_plastic, type = "pie",
              marker = list(colors = chart_colors),
              source = paste0("pie_", plastic_code),
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
  lapply((plastic_types_forproducts), function(plastic_code) {

    # Hover event:
    observeEvent(event_data("plotly_hover", source = paste0("pie_", plastic_code)), {
      hover_data <- event_data("plotly_hover", source = paste0("pie_", plastic_code))
      
      output[[paste0("product_info_", plastic_code)]] <- renderUI({
        if (!is.null(hover_data) && !is.null(hover_data$pointNumber)) {
          data <- marketshare[marketshare$plastic == plastic_code, ]
          category <- data$category[hover_data$pointNumber + 1] # +1 because it's 0-indexed
          
          category_info <- categories %>% 
            filter(plastic == !!plastic_code, category == !!category)
          
          if (nrow(category_info) > 0) {
            # Separate subcategories with images and without
            with_images <- category_info %>% filter(!is.na(jpg_name))
            without_images <- category_info %>% filter(is.na(jpg_name)) %>% pull(subcategories)
            
            tagList(
              h5(paste("Example", category, "products made from", plastic_code)),
              
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
    })
    
    # Unhover event:
    observeEvent(event_data("plotly_unhover", source = paste0("pie_",plastic_code)), {
      output[[paste0("product_info_", plastic_code)]] <- renderUI({
        
        data <- marketshare[marketshare$plastic == plastic_code, ]
        descriptor <- data$descriptor[1]
        
        HTML(descriptor)
      })
    }, ignoreNULL = FALSE)
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
  
  # FOR ADDITIVES TABLE: -------------------------------------------------------------------------
  # Render the filtered DataTable
  
  output$additives_table <- renderDT({
    datatable(
      additives,
      options = list(
        pageLength = 200,
        autoWidth = TRUE
      )
    )
  })
}