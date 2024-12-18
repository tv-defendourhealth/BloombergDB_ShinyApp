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
          style = "margin-left: 10%; margin-right: 10%; margin-bottom: 20%; min-height: 1000px;",
          card(
            # ------ SUPPLY CHAIN PANEL
            card_body(HTML("
              <p>The building blocks that make up plastic are produced in petrochemical facilities across the world, with many clustered in regions of the U.S. like the Gulf Coast and the Ohio River Valley. 
              These facilities expose thousands of people, primarily low-income communities and people of color, to the toxic fumes that come from plastic production.
              </p>
              <p>Here, we highlight the toxic burden of plastic production in the United States for six commonly used types of plastic:
              </p>
              <ol style=\"padding-left: 40px;\">
              <li> Polypropylene (PP)</li>
              <li> Polyethylene (PE)</li>
              <li> Polyvinyl chloride (PVC)</li>
              <li> Polystyrene (PS)</li>
              <li> Polyethylene terephthalate (PET)</li>
              <li> Polylactic acid (PLA)</li>
              </ol>
              <p>Below there are tabs for each type of plastic. Click on the tabs to explore the petrochemical facility production of each type of plastic. 
              </p>
                           "
            )),
            div(
              id = "dynamic_content",
              style = "width: 90%; margin: auto; text-align: center; min-height: 700px;",  # Adjust height as needed
              navset_card_tab(
                id = "plastic_tabs",
                !!!lapply(names(plastic_types), function(plastic_name) {
                  nav_panel(
                    title = plastic_name,
                    create_supplychain_panel(plastic_name) # Supply chain diagram
                  )
                })
              )
            ),
            # ------ MAP PANEL
            card_body(HTML("<p>Each of the steps in the process of making plastic (shown above) is made in different facilities across the United States. 
            These facilities transport the intermediate chemicals to each other via pipeline, trains, and other means of transport. 
            The transport itself increases the risk of hazard exposure, such as in the case of pipeline accidents or train derailments. 
            </p>
            <p>Each tab below corresponds to a plastic type and displays a map of all the facilities in the U.S. that are associated with the making of that plastic. 
            </p>
            "
            )),
            div(
              id = "dynamic_content",
              style = "width: 90%; margin: auto; text-align: center; min-height: 700px;",  # Adjust height as needed
              navset_card_tab(
                id = "plastic_tabs",
                !!!lapply(names(plastic_types), function(plastic_name) {
                  nav_panel(
                    title = plastic_name,
                    create_map_panel(plastic_name) # Supply chain diagram
                  )
                })
              )
            )
          ),
          
        )
        )  
      } 
    
    else if (active_tab() == "additives") {
      
      return(
          div(
          id = "dynamic_content",
          style = "margin-left: 10%; margin-right: 10%; margin-bottom: 20%; min-height: 1000px;",
          tagList(
            card(
              full_screen = TRUE,
              style = "margin-bottom: 20px;",
              card_body(
                  
                # TEXT INTRO
                div(
                  #style = "text-align: left; margin-left: 5%; margin-right: 5%;",
                  style = "text-align: left; margin: auto;",
                  HTML("<p>Plastic polymers alone are not sufficient to be used in final products. 
                    Fossil-fuel plastics always require the addition of toxic “additives”, various chemicals that impart different properties. 
                    </p>
                    <p>There are thousands of types of additives that can change the plastic color, strength, durability, flexibility, heat resistance, flammability, and more. 
                    We have categorized chemical additives into the following categories:
                    </p>"
                  )
                ),
                
                # IMAGE: ADDITIVE CATEGORY TABLE 
                div(
                  style = "text-align: center; margin-bottom: 5%;",  # Centering the image
                  img(src = "additive_category_table.png", 
                      alt = "Additive Categories", 
                      style = "width: 50%; max-width: 1000px;"),  # Adjust width as needed
                ),
                
                # HAZARD SCORE EXPLANATION
                div(
                  style = "display: flex; align-items: center;",
                  # TEXT:
                  div(
                    #style = "flex: 1; margin-left: 5%; margin-right: 5%; margin-bottom: 5%;",
                    style = "flex: 1.2; margin: auto; margin-bottom: 5%;",
                    p(HTML(paste0("To characterize the inherent hazards of each additive, we created a \"hazard score\" that comes from the GreenScreen for Safer Chemicals. 
                    GreenScreen compiles data and scientific literature on 18 different human health and environmental hazard endpoints and assigns a corresponding score.
                    To learn more about GreenScreen, see ",
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
                  ),
                
                # SUMMARY BAR CHART OF HAZARD SCORES
                div(
                  style = "display: flex; align-items: center;",
                  div(
                    style = "flex: 1; margin: auto;",
                    p("A vast majority of additives in our dataset were considered a High (red) or Moderate (orange) hazard, as you can see in the following chart")
                  ),
                  div(
                    style = "flex: 1.5; margin: auto;",
                    plotOutput("hazard_score_plot")
                  )
                )
                ),
              
              # EXPLANATION OF TABLE HERE:
              div(
                style = "text-align: left; margin: auto;",
                HTML("<p>Industry does not disclose all the additives they use so it's impossible to create a comprehensive list. 
                Below, however, are some of the additives we have found to be used in each type of plastic and the associated hazard scores. 
                You can use the drop-down menus at the top to filter the data by plastic type or additive category. 
                You can also sort each column as needed.</p>")
              ),
             
              # ADDITIVE TABLE HERE:
              div(
                style = "margin-left: 5%; margin-right: 5%;",
                card(
                  full_screen = TRUE,
                  height = "100%",
                  card_body(
                    div(uiOutput("filter_ui"),
                        DTOutput("additives_table"), style = "font-size:75%")
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
          style = "margin-left: 10%; margin-right: 10%; margin-bottom: 20%; min-height: 1000px;",
          navset_card_tab(
            id = "plastic_tabs",
            !!!lapply(names(plastic_types), function(plastic_name) {
              if (plastic_name == "polyethylene (PE)") { # treat PE separately to show HDPE and LDPE
                nav_panel(
                  title = plastic_name, 
                  div(
                    style = "text-align: left; margin: auto;",
                    p("Polyethylene is produced in a low density (LDPE) form and a high density (HDPE) form.
                      We show data for both LDPE and HDPE below:")
                  ),
                  div(
                    style = "text-align: left; margin: auto;",
                    fluidRow(
                      style = "display: flex; align-items: center;",
                      column(1, img(src = paste0("recyclesymbol_","2",".png"), width = "100%")),
                      column(11, p(plastic_descriptions["HDPE"]))
                    ),
                    fluidRow(
                      style = "display: flex; align-items: center;",
                      column(6, plotlyOutput(paste0("pie_", "HDPE"))),
                      column(6, uiOutput(paste0("product_info_", "HDPE")))
                    )
                  ),
                  div(
                    style = "text-align: left; margin: auto;",
                    fluidRow(
                      style = "display: flex; align-items: center;",
                      column(1, img(src = paste0("recyclesymbol_","4",".png"), width = "100%")),
                      column(11, p(plastic_descriptions["LDPE"]))
                    ),
                    fluidRow(
                      style = "display: flex; align-items: center;",
                      column(6, plotlyOutput(paste0("pie_", "LDPE"))),
                      column(6, uiOutput(paste0("product_info_", "LDPE")))
                    )
                  )
                )
              } else {
                nav_panel(
                  title = plastic_name,
                  create_product_panel(plastic_name) # Supply chain diagram
                )
              }
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
          selected_metric <- input$selected_metric
          
          if(selected_metric %in% c("pc_low_income", "pc_poc")) {
            create_custom_map_dem(map_data,
                                  selected_metric,
                                  get_metric_name(selected_metric),
                                  local_abbreviation, 
                                  color_palette = c("#fee8c8", "#fdbb84", "#e34a33"))
          } else {
            create_custom_map_perc(map_data,
                                   selected_metric,
                                   get_metric_name(selected_metric),
                                   local_abbreviation, 
                                   color_palette = c("#fee8c8", "#fdbb84", "#e34a33"))
          }
        })
      })
    }
  })
  
  # --------------------- CREATE ADDITIVE TABLE
  additives_data$"Plastic used in" <- as.factor(additives_data$"Plastic used in")
  additives_data$"Additive category" <- as.factor(additives_data$"Additive category")
  
  output$filter_ui <- renderUI({
    req(active_tab() == "additives")  # Only show filters when in additives tab
    
    fluidRow(
      column(6,
             selectInput("plastic_filter", "Filter by Plastic:",
                         choices = c("All", levels(additives_data$"Plastic used in")),  # Include all unique values
                         selected = "All")
      ),
      column(6,
             selectInput("category_filter", "Filter by Additive Category:",
                         choices = c("All", levels(additives_data$"Additive category")),  # Include all unique values
                         selected = "All")
      )
    )
  })
  
  output$additives_table <- renderDT({
    req(active_tab() == "additives")  # Only run if in additives tab
    
    # Filter data based on selections
    filtered_data <- additives_data
    
    if (input$plastic_filter != "All") {
      filtered_data <- filtered_data[filtered_data$"Plastic used in" == input$plastic_filter, ]
    }
    
    if (input$category_filter != "All") {
      filtered_data <- filtered_data[filtered_data$"Additive category" == input$category_filter, ]
    }
    
    datatable(
      filtered_data,
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        dom = 'Bfrtip',  # Include buttons and filtering
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  # Optional: Add export buttons
        #order = list(list(5, 'desc'))
      ),
      #rownames = FALSE
    ) %>% 
      formatStyle(
        'Hazard score',
        backgroundColor = styleEqual(c(0, 1, 2, 3), 
                                     c('#00a589','#fac541','#fc6c43','#de0f3f')),
        color = styleEqual(c(0, 1, 2, 3), c('black','black','white','white'))
      )
  })
  
  # --------------------- CREATE ADDITIVE HAZARD PLOT
  summarized_data <- additives_data %>%
    group_by(`Plastic used in`, `Hazard score`) %>%
    summarise(count = n()) %>%
    group_by(`Plastic used in`) %>%
    mutate(proportion = count / sum(count))
  
  hazard_3_proportions <- summarized_data %>%
    group_by(`Plastic used in`) %>%
    filter(`Hazard score` == 3 | `Hazard score` == "3") %>%  # Include both numeric and character "3"
    arrange(desc(proportion)) %>%
    pull(`Plastic used in`)
  hazard_3_proportions <- unname(hazard_3_proportions)
 
  output$hazard_score_plot <- renderPlot({
    ggplot(additives_data, aes(x = factor(`Plastic used in`, levels = rev(hazard_3_proportions)), 
                               fill = factor(`Hazard score`))) +
      geom_bar(position = "fill", width = 0.6) +
      scale_fill_manual(values = c("0" = "#00a589", "1" = "#fac541", "2" = "#fc6c43", "3" = "#de0f3f")) +
      labs(title = "Proportion of additives with each hazard type",
           y = "Plastic Type",
           x = "Proportion",
           fill = "Hazard Score") +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Arial", face = "bold", size = 12),
        axis.text = element_text(family = "Arial", face = "bold"),
        legend.text = element_text(family = "Arial", face = "bold"),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.text.x = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      coord_flip()
  }, bg = "transparent")
  
  
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
      # Create a color palette function with Oranges
      color_palette <- colorRampPalette(brewer.pal(9, "Oranges")) 
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
            
            div(
              style = "margin: auto;",
              
              p(HTML(paste("<span style='font-size: 16px; font-style: italic; font-weight: bold;'>", category, "products made from", plastic_abbrev,"</span>"))),
              
              # Display subcategories with images
              if (nrow(with_images) > 0) {
                div(style = "display: flex; flex-wrap: wrap; margin-bottom: 5%;",
                    lapply(1:nrow(with_images), function(i) {
                      div(style = "margin: 10px; text-align: center;",
                          img(src = file.path(with_images$jpg_name[i]), 
                              style = "height: 150px;"),
                          p(with_images$subcategories[i])
                      )
                    })
                )
              },
              
              # Display other examples as text
              if (length(without_images) > 0) {
                div(
                  style = "margin: auto;;",
                  h6("Other examples:", style = "margin-bottom: 10px; font-style: italic; font-weight: bold;"),
                  tags$ul(
                    #style = "list-style-type: none; padding-left: 0;",
                    lapply(without_images, function(product) {
                      tags$li(product, style = "margin-bottom: 5px;")
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

        HTML(paste0("Hover over the pie chart to see examples of each type of ", plastic_abbrev, " product."))
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