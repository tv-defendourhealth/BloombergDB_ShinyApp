
# Load necessary dataframes

for_map <- readRDS("data/for_map.rds")


# Mapping: Prep data and create a function to make custom maps V2:

# Create custom color schemes for variables:
color_palettes <- list(
  co2e_perc = colorNumeric(palette = "YlOrRd", domain = 0:100),
  total_tox_perc = colorNumeric(palette = "PuRd", domain = 0:100),
  pc_low_income = colorNumeric(palette = "Blues", domain = for_map$pc_low_income),
  pc_poc = colorNumeric(palette = "BuPu", domain = for_map$pc_poc)
)

# Function to filter by plastic type: 
filter_by_supply_chain <- function(data, supply_chain_type) {
  data %>%
    filter(grepl(supply_chain_type, known_supply_chains, fixed = TRUE))}

# Recreate function to create custom map but with filtering for plastic, converting metric variable to percentile:
create_custom_map_perc <- function(data, variable, variable_label, supply_chain_type = "All", color_palette = c("#fee8c8", "#fdbb84", "#e34a33")) {
  
  # Calculate percentile for the input variable
  data <- data %>%
    mutate(!!paste0(variable, "_perc") := percent_rank(!!sym(variable)) * 100)
  
  # Use the newly created percentile variable
  percentile_var <- paste0(variable, "_perc")
  
  # Create color palette function
  #pal <- colorNumeric(palette = color_palette, domain = data[[percentile_var]])
  pal <- color_palettes[[percentile_var]]
  
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
      "Total CO2 emissions: ", round(co2e/1000000, 2), " million metric tons (MMT)<br>",
      "Total toxic emissions: ", round(total_tox, 2), " metric tons (MT)<br>",
      "POC: ", round(pc_poc, 2), " %<br>",
      "Low income: ", round(pc_low_income, 2), " %"
    ))
  
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


# Recreate function to create custom map but with filtering for plastic, suited for demographic data where we do not use percentile scores:
create_custom_map_dem <- function(data, variable, variable_label, supply_chain_type = "All", color_palette = c("#fee8c8", "#fdbb84", "#e34a33")) {
  
  # Filter by supply chain type if specified
  if (!supply_chain_type == "All") {
    data <- filter_by_supply_chain(data, supply_chain_type)
  }
  
  # Filter out NA values
  data <- data %>%
    filter(!is.na(latitude) & !is.na(longitude) & !is.na(!!sym(variable)))
  
  #for custom title:
  if (grepl("Racial", variable_label)) {
    custom_legend <- "% who identify as POC in 3mi radius"
  } else {
    custom_legend <- "% who are low income in 3mi radius"
  }
  
  # Create custom messages with HTML formatting
  data <- data %>%
    mutate(info = paste0(
      "<b>", plant_name, "</b><br>",
      custom_legend, ": ", round(!!sym(variable), 0), " %</b><br>",
      "<br>",
      "Known supply chains: ", known_supply_chains, "<br>",
      "Total CO2 emissions: ", round(co2e/1000000, 2), " million metric tons (MMT)<br>",
      "Total toxic emissions: ", round(total_tox, 2), " metric tons (MT)<br>",
      "% who identify as POC: ", round(pc_poc, 2), " %<br>",
      "% who are low income: ", round(pc_low_income, 2), " %"
    ))
  
  
  # Create color palette function
  pal <- color_palettes[[variable]]
  

  # Create the map
  map <- leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 5,
      color = ~pal(get(variable)),
      fillOpacity = 0.8,
      popup = ~info,
      label = ~plant_name
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~(get(variable)),
      title = custom_legend,#paste(variable_label, "in 3mi radius (%)"),
      opacity = 1
    )
  
  return(map)
}

