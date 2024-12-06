
# Load necessary dataframes

for_map <- readRDS("data/for_map.rds")


# Mapping: Prep data and create a function to make custom maps V2:

# Function to filter by plastic type: 
filter_by_supply_chain <- function(data, supply_chain_type) {
  data %>%
    filter(grepl(supply_chain_type, known_supply_chains, fixed = TRUE))}

# Recreate function to create custom map but with filtering for plastic:
create_custom_map <- function(data, variable, variable_label, supply_chain_type = "All", color_palette = c("#fee8c8", "#fdbb84", "#e34a33")) {
  
  # Calculate percentile for the input variable
  data <- data %>%
    mutate(!!paste0(variable, "_perc") := percent_rank(!!sym(variable)) * 100)
  
  # Use the newly created percentile variable
  percentile_var <- paste0(variable, "_perc")
  
  # Filter by supply chain type if specified
  if (!supply_chain_type == "All") {
    data <- filter_by_supply_chain(data, supply_chain_type)
  }
  
  # Filter out NA values
  data <- data %>%
    filter(!is.na(latitude) & !is.na(longitude) & !is.na(!!sym(percentile_var)))
  
  # Create custom messages with HTML formatting
  data <- data %>%
    mutate(info = paste0(
      "<b>", plant_name, "</b><br>",
      variable_label, ": ", round(!!sym(percentile_var), 0), " percentile</b><br>",
      "<br>",
      "Known supply chains: ", known_supply_chains, "<br>",
      "Total CO2 emissions: ", round(co2e/1000000, 2), " MMT<br>",
      "Total toxic emissions: ", round(total_tox, 2), " lbs<br>",
      "POC: ", round(pc_poc, 2), " %<br>",
      "Low income: ", round(pc_low_income, 2), " %"
    ))
  
  
  # Create color palette function
  pal <- colorNumeric(palette = color_palette, domain = data[[percentile_var]])
  
  # Create map title
  if (!is.null(supply_chain_type)) {
    maptitle <- paste("Supply chain:", supply_chain_type)}
  else {
    maptitle <- "Supply chain: All plastics" 
  }
  
  
  # Create the map
  map <- leaflet(data) %>%
    addTiles() %>%
    #addControl(html = paste0("<h3>",maptitle,"</h3>"), position = "topright") %>% 
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      #radius = ~(get(percentile_var) / 10) + 2,
      radius = 5,
      color = ~pal(get(percentile_var)),
      fillOpacity = 0.8,
      popup = ~info,
      label = ~plant_name
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~(get(percentile_var)),
      title = paste(variable_label, "Percentile"),
      opacity = 1
    )
  
  return(map)
}

