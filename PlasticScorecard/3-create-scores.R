library(dplyr)
library(purrr)

# Load necessary dataframes

db <- readRDS("data/db.rds")
summary_by_plastic <- readRDS("data/summary_by_plastic.rds")

# Function to create scores for CO2, Tox, Health/RSEI

calculate_emission_score <- function(db, db_var, summary_db, summary_var, id_var = "known_supply_chains", cutoffs) {
  # Ensure variables are symbols
  db_var <- ensym(db_var)
  summary_var <- ensym(summary_var)
  id_var <- ensym(id_var)
  
  # Calculate median of db_var
  median_val <- median(pull(db, !!db_var), na.rm = TRUE)
  
  # Calculate score boundaries
  boundaries <- cutoffs * median_val
  
  # Create labels for the scores
  labels <- paste(c("Below", paste(cutoffs[-length(cutoffs)], "to", cutoffs[-1], "times"), paste("Above",cutoffs[length(cutoffs)],"times")),
                  "median")
  
  # Create a function to assign scores
  assign_score <- function(x) {
    score <- findInterval(x, boundaries) + 1
    return(score)
  }
  
  # Create the score dataframe
  score_df <- data.frame(
    supply_chain = pull(summary_db, !!id_var),
    score = assign_score(pull(summary_db, !!summary_var)),
    label = factor(assign_score(pull(summary_db, !!summary_var)), 
                   levels = 1:length(labels), 
                   labels = labels),
    value = pull(summary_db, !!summary_var)
  )
  
  # Add percentage of median column
  score_df$percent_of_median <- (score_df$value / median_val) * 100
  
  return(score_df)
}


# Function to create demographic scores

US_low_income <- 27.6 #27.6% of US lived at double the poverty level or below in 2021 https://www.americanprogress.org/data-view/poverty-data/?yearFilter=2021&national=2021

US_POC <- 100-58.4 #58.4% of people in US are white, the remaining are POC. According to latest census: https://www.census.gov/quickfacts/fact/table/US/RHI825223#RHI825223

calculate_demographic_score <- function(summary_db, summary_var, id_var = "known_supply_chains", US_avg, cutoffs) {
  # Ensure variables are symbols
  summary_var <- ensym(summary_var)
  id_var <- ensym(id_var)
  
  # Calculate score boundaries
  boundaries <- cutoffs + US_avg
  
  # Create labels for the scores
  labels <- paste(c("Below", paste(cutoffs[-length(cutoffs)], "to", cutoffs[-1], "points above"), paste("More than",cutoffs[length(cutoffs)],"points above")), "US average")
  
  # Create a function to assign scores
  assign_score <- function(x) {
    score <- findInterval(x, boundaries) + 1
    return(score)
  }
  
  # Create the score dataframe
  score_df <- data.frame(
    supply_chain = pull(summary_db, !!id_var),
    score = assign_score(pull(summary_db, !!summary_var)),
    label = factor(assign_score(pull(summary_db, !!summary_var)), 
                   levels = 1:length(labels), 
                   labels = labels),
    value = pull(summary_db, !!summary_var)
  )
  
  # Add percentage of median column
  score_df$rel_US_avg <- (score_df$value - US_avg)
  
  return(score_df)
}

# # Calculate scores

# CO2_scores <- calculate_emission_score(db, "co2_perU", summary_by_plastic, "avg_co2e_perU", "known_supply_chains", cutoffs <- c(.5,1,2,5))
# 
# TOX_scores <- calculate_emission_score(db, "tox_perU", summary_by_plastic, "avg_tox_perU", "known_supply_chains", cutoffs <- c(.5,1,2,5))
# 
# RSEI_scores <- calculate_emission_score(db, "facility_RSEI_score", summary_by_plastic, "avg_rsei", "known_supply_chains", cutoffs <- c(.5,1,2,5))
#
# income_scores <- calculate_demographic_score(summary_by_plastic, "avg_income", "known_supply_chains", US_low_income, pc_cutoffs <- c(0, 2, 5, 10))
# 
# race_scores <- calculate_demographic_score(summary_by_plastic, "avg_poc", "known_supply_chains", US_POC, pc_cutoffs <- c(0, 2, 5, 10))

# Create combined score dataframe

# combined_scores <- CO2_scores %>% 
#   select(supply_chain, co2_score = score) %>% 
#   left_join(select(TOX_scores, supply_chain, tox_score = score), by = "supply_chain") %>% 
#   left_join(select(RSEI_scores, supply_chain, rsei_score = score), by = "supply_chain") %>% 
#   left_join(select(income_scores, supply_chain, income_score = score), by = "supply_chain") %>% 
#   left_join(select(race_scores, supply_chain, race_score = score), by = "supply_chain") 
