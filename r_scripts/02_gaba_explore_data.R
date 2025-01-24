#---------------------------------------------------------------#
# DATA EXPLORATION
#---------------------------------------------------------------#

#In this script we explore and summarise the behavioural data (e.g. histograms)
#Create transformed and standardised response and co-variate variables

### LIBRARIES ###
library(tidyverse)

### LOAD DATA ###
gaba_data <- readRDS("./data/02_gaba_data.rds")

### HELPER FUNCTION ###
# Function to calculate mean, sd, min, and max for multiple columns and format as mean (min - max)
summary_stats <- function(data, group_cols, value_cols) {
  # Iterate over each column in value_cols and calculate stats
  result <- lapply(value_cols, function(col) {
    data %>%
      group_by(across(all_of(group_cols))) %>% # Group by the specified columns
      summarise(
        mean_value = mean(.data[[col]], na.rm = TRUE),
        sd_value = sd(.data[[col]], na.rm = TRUE),
        min_value = min(.data[[col]], na.rm = TRUE),
        max_value = max(.data[[col]], na.rm = TRUE)
      ) %>%
      mutate(summary = paste0(
        round(mean_value, 2), " (+/-", round(sd_value, 2), "; ", 
        round(min_value, 2), " - ", round(max_value, 2), ")"
      )) %>%
      select(-mean_value, -sd_value, -min_value, -max_value) %>%
      rename(!!col := summary) # Rename the summary column to the original column name
  })
  
  # Combine the results for each column into a single dataframe
  result_df <- Reduce(function(x, y) full_join(x, y, by = group_cols), result)
  
  return(result_df)
}

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
#----------------------------------#
#### Data summaries and spread ####
#----------------------------------#

# > 1.1. Distance moved #####

#How many trials did individuals not move
gaba_data %>% 
  filter(DistMove == 0) %>% 
  group_by(Temp, Treatment, Stage) %>% 
  summarise(rows = n()) 

# Calculate percentage of rows with DistMove == 0
percentage_zero <- gaba_data %>%
  summarise(
    total_rows = n(),                              # Total rows in dataset
    zero_rows = sum(DistMove == 0)                 # Rows where DistMove == 0
  ) %>%
  mutate(percentage = (zero_rows / total_rows) * 100) # Calculate percentage

# View the result
print(percentage_zero)
#individuals did not leave the shelter in 6.7% of trials

### Summary statistics ###
summary_stats(gaba_data, c('Treatment', 'Temp', 'Stage'), 'DistMove')

### Histograms ###
#not transformed
ggplot(gaba_data, 
       aes(DistMove, fill = as.factor(Temp))) + 
  geom_histogram(binwidth = 100)+
  theme_classic() #skewed

#sqrt transformed
ggplot(gaba_data, 
       aes(sqrt(DistMove), fill = as.factor(Treatment))) + 
  geom_histogram(binwidth = 2)+
  theme_classic() #skewed

#new column with sqrt transformation
gaba_data$DistMove_sqrt <- sqrt(gaba_data$DistMove)

# > 1.2. Time spent in shelter ####

### Summary statistics ###
summary_stats(gaba_data, c('Treatment', 'Temp', 'Stage'), 'TimeInShelter')

ggplot(gaba_data, 
       aes(TimeInShelter, fill = as.factor(Treatment))) + 
  geom_histogram(binwidth = 10)+
  theme_classic() #skewed


#-------------------------------------------------------------------------------------------#

#Scaling response variables and continuous covariates
gaba_data$TimeInShelter_z <- scale(gaba_data$TimeInShelter)
gaba_data$FreqArena_sqrt_z <- scale(gaba_data$FreqArena_sqrt)
gaba_data$DistMove_sqrt_z <- scale(gaba_data$DistMove_sqrt)
gaba_data$Weight_z <- scale(gaba_data$Weight)
gaba_data$trial_num_cent <- gaba_data$trial_num - 1
gaba_data$trial_stage_cent <- as.numeric(gaba_data$trial_stage) - 1

#------------------------------------------------------------------------------------------#

#save data file
saveRDS(gaba_data, "./data/03_gaba_data.rds")

#-----------------------------------------------------------------------------------------#

#END OF SCRIPT
  