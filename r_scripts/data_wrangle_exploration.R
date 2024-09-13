#---------------------------------------------------------------#
# Initial data cleaning - 10 degree trout #
#---------------------------------------------------------------#

### LIBRARIES ###
library(tidyverse)

### DIRECTORIES ###
raw_data_path <- "./raw_data/"

### LOAD DATA ###
gaba_10_data <- read.csv(paste0(raw_data_path, "gabapentin_10deg_data.csv"), header = TRUE)

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

#---------------------------------------------------------------------------------------------------------------#

#First I am going to create a column called stage to identify which period of the experiment
#each trial belongs too

gaba_10_data <- gaba_10_data %>%
  mutate(Stage = case_when(
    grepl("^Pre", Trial) ~ "Pre",
    grepl("^Post", Trial) ~ "Post",
    grepl("^Dep", Trial) ~ "Dep"
  ),
  # Convert to factor and specify the order
  Stage = factor(Stage, levels = c("Pre", "Post", "Dep")))


#---------------------------------------#
# Data summary ####
#---------------------------------------#

#> 1. How many unique ID's per gabapentin treatment ####
gaba_10_data %>% 
  group_by(Treatment) %>% 
  summarise(n_ids = length(unique(ID)))
#control - 43
#exposed - 42
#total - 85

# > 2. How many trials per individual and treatment ####

# Create new columns Pre, Post, Dep indicating how many trials each individual participated in
trial_summary <- gaba_10_data %>%
  mutate(Stage = case_when(
    grepl("^Pre", Trial) ~ "Pre",
    grepl("^Post", Trial) ~ "Post",
    grepl("^Dep", Trial) ~ "Dep"
  )) %>%
  group_by(ID, Treatment, Stage) %>%
  summarise(NumTrials = sum(!is.na(DistMove)), .groups = 'drop') %>%
  pivot_wider(names_from = Stage, values_from = NumTrials, values_fill = 0)

# Add the total number of trials an individual participated in
trial_summary <- trial_summary %>%
  rowwise() %>%
  mutate(TotalTrials = sum(c_across(Pre:Dep), na.rm = TRUE))

#treatment summary
summary_stats(trial_summary, "Treatment", c("Pre", "Post", "Dep", "TotalTrials"))
#similar trials per treatment which is good. 

# Add trial info to main dataframe
gaba_10_data <- gaba_10_data %>%
  left_join(trial_summary, by = "ID")



# > 3. What percentage of individuals completed all trials per stage ####

#Define thresholds for completion
full_trials <- list(Pre = 4, Post = 4, Dep = 4, TotalTrials = 12)

#Create a column indicating if the individual completed the full set for each stage
trial_summary$Completed_Pre <- trial_summary$Pre == full_trials$Pre
trial_summary$Completed_Post <- trial_summary$Post == full_trials$Post
trial_summary$Completed_Dep <- trial_summary$Dep == full_trials$Dep
trial_summary$Completed_TotalTrials <- trial_summary$TotalTrials == full_trials$TotalTrials

# Group by Treatment and calculate the percentage of individuals who completed each stage
trial_summary %>%
  group_by(Treatment) %>%
  summarise(
    Completed_Pre_Count = sum(Completed_Pre),
    Completed_Post_Count = sum(Completed_Post),
    Completed_Dep_Count = sum(Completed_Dep),
    Completed_TotalTrials_Count = sum(Completed_TotalTrials),
    Total_Individuals = n(),
    Percent_Completed_Pre = mean(Completed_Pre) * 100,
    Percent_Completed_Post = mean(Completed_Post) * 100,
    Percent_Completed_Dep = mean(Completed_Dep) * 100,
    Percent_Completed_TotalTrials = mean(Completed_TotalTrials) * 100
  ) %>%
  mutate(
    Pre = paste0(round(Percent_Completed_Pre, 1), " (", Completed_Pre_Count, "/", Total_Individuals, ")"),
    Post = paste0(round(Percent_Completed_Post, 1), " (", Completed_Post_Count, "/", Total_Individuals, ")"),
    Dep = paste0(round(Percent_Completed_Dep, 1), " (", Completed_Dep_Count, "/", Total_Individuals, ")"),
    Total = paste0(round(Percent_Completed_TotalTrials, 1), " (", Completed_TotalTrials_Count, "/", Total_Individuals, ")")
  ) %>%
  select(Treatment, Pre, Post, Dep, Total)


# > 4. Fish size summary ####

#Extract all distinct indiviuals 
fish_size_sum <- 
  gaba_10_data %>%
  select(ID, Treatment, Length, Weight) %>% 
  distinct()
  
#86 individuals here
#85 unique indiviuals identified earlier
#Suggest that there maybe a duplicated ID with different length and weight
#Need to explore

fish_size_sum %>% 
  group_by(ID) %>% 
  filter(n_distinct(Length, Weight) > 1) %>% 
  arrange(ID)
#C_10_2_Blue has duplicated rows
#replace NAs

gaba_10_data <- gaba_10_data %>%
  mutate(Length = ifelse(is.na(Length) & ID == "C_10_2_Blue" & Treatment == "control", 8.3, Length),
         Weight = ifelse(is.na(Weight) & ID == "C_10_2_Blue" & Treatment == "control", 5.6, Weight))

#rerun fish_size_sum to check issue is fixed

#fish size summary
summary_stats(fish_size_sum, "Treatment", c("Length", "Weight"))


#> 5. Behaviour summary ####

#>>> 5.1. Distance and Time move
summary_stats(gaba_10_data, c('Stage', 'Treatment'), c('DistMove', 'TimeMove'))

#>>> 5.2. Arena use
summary_stats(gaba_10_data, c('Stage', 'Treatment'), c('TimeArena', 'FreqArena'))

#>>> 5.3. Partial arena use
summary_stats(gaba_10_data, c('Stage', 'Treatment'), c('TimePartial', 'FreqPartial'))
