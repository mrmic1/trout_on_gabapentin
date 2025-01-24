#---------------------------------------------------------------#
# DATA WRANGLING #
#---------------------------------------------------------------#

#Goal of this script is to clean the raw dataset
#Combine the 10 and 14 degree data
#Add columns to assist data analysis
#Summarise sample size information
#Create a time in shelter column

### LIBRARIES ###
library(tidyverse)
library(flextable)
library(kableExtra)
library(officer)

### DIRECTORIES ###
raw_data_path <- "./raw_data/"
save_table_path <- "./tables/"

### LOAD DATA ###
gaba_10_data <- read.csv(paste0(raw_data_path, "gabapentin_10deg_data.csv"), header = TRUE)
gaba_14_data <- read.csv(paste0(raw_data_path, "gabapentin_14deg_data.csv"), header = TRUE)

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
#---------------------------------------------------------------------------------------------------------------#

#-----------------------------------------#
# General data cleaning and organising ####
#-----------------------------------------#

#### 1. Combine 10 and 14 degree data ####

#combine 10 and 14 degree data
gaba_data <- rbind(gaba_10_data, gaba_14_data)
#1658 rows

#check column classes
str(gaba_data)

#need to change behavioural columns into numeric or integer, not character
gaba_data <- gaba_data %>%
  mutate(
    across(c(DistMove, TimeMove, TimeNotMove, TimeArena, TimePartial), 
           ~ ifelse(. == "" | is.na(.), NA, floor(as.numeric(.)))),
    across(c(FreqArena, FreqPartial), 
           ~ ifelse(. == "" | is.na(.), NA, as.integer(floor(as.numeric(.)))))
  )

#make Temp, Treatment, ID a factor
gaba_data$Temp <- as.factor(as.character(gaba_data$Temp))
gaba_data$Treatment <- as.factor(as.character(gaba_data$Treatment))
gaba_data$ID <- as.factor(as.character(gaba_data$ID))

#remove depuration data
#this information is not available for 14 degree fish

gaba_data %>% 
  filter(!Stage == 'Dep')


#### 2. Add trial information to dataset ####

#First I am going to create a column called stage to identify which period of the experiment
#each trial belongs too

gaba_data <- gaba_data %>%
  mutate(Stage = case_when(
    grepl("^Pre", Trial) ~ "Pre",
    grepl("^Post", Trial) ~ "Post"
  ),
  # Convert to factor and specify the order
  Stage = factor(Stage, levels = c("Pre", "Post")))


#Within each stage I want to assign a trial number (1 - 4)
gaba_data <- gaba_data %>% 
  mutate(trial_stage = gsub("Pre|Post", "", Trial))

#check it worked
gaba_data %>% 
  select(Stage, trial_stage) %>% 
  distinct()

#I also will create a column that specifies the trial on a continuous scale until the end of the experiment
gaba_data <- gaba_data %>%
  mutate(trial_num = case_when(
    grepl("Pre1", Trial)  ~ 1,
    grepl("Pre2", Trial)  ~ 2,
    grepl("Pre3", Trial)  ~ 3,
    grepl("Pre4", Trial)  ~ 4,
    grepl("Post1", Trial) ~ 5,
    grepl("Post2", Trial) ~ 6,
    grepl("Post3", Trial) ~ 7,
    grepl("Post4", Trial) ~ 8
  ))


# Generate random weights for missing values
missing_count <- sum(is.na(gaba_data$Weight))  # Count missing weights
random_weights <- truncnorm::rtruncnorm(
  n = missing_count,
  a = 2.9,  # Lower bound
  b = 12.3,   # Upper bound
  mean = 5.6,  # Centered around 0
  sd = 2     # Standard deviation
) %>% round(1)

# Replace NaN values with random weights
gaba_data$Weight[is.na(gaba_data$Weight)] <- random_weights

#save file
saveRDS(gaba_data, "./data/01_gaba_data.rds")


#### 3. Count and remove NA's ####

#how many NAs per treatment group
#NA's represent missing trials
na_counts <- gaba_data %>%
  group_by(Treatment, Temp, Stage) %>%
  summarise(NA_Count = sum(is.na(DistMove)), .groups = "drop") %>%
  group_by(Treatment, Temp) %>% 
  mutate(totat_na = sum(NA_Count))

# Print the result
print(na_counts)

#remove individuals with NAs
gaba_data <- gaba_data %>% 
  filter(!is.na(DistMove))

#### 4. Create time spent in shelter column ####

#Time in shelter
gaba_data <- gaba_data %>% 
  mutate(TimeInShelter = 600 - TimeArena)

#Did individual leave shelter (yes = 1, no = 0)
gaba_data <- gaba_data %>% 
  mutate(LeaveShelter = ifelse(TimeInShelter == 600, 0, 1))

#ensure dataframe not tibble
gaba_data <- as.data.frame(gaba_data)

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#

#----------------------------------#
# Data summary ####
#----------------------------------#

# 1. How many unique ID's per gabapentin treatment ####
gaba_data %>% 
  group_by(Treatment, Temp) %>% 
  summarise(n_ids = length(unique(ID)))

# 2. How many trials per individual and treatment ####

# Create new columns Pre, Post, Dep indicating how many trials each individual participated in
trial_summary <- gaba_data %>%
  mutate(Stage = case_when(
    grepl("^Pre", Trial) ~ "Pre",
    grepl("^Post", Trial) ~ "Post"
  )) %>%
  group_by(ID, Treatment, Temp, Stage) %>%
  summarise(NumTrials = sum(!is.na(DistMove)), .groups = 'drop') %>%
  pivot_wider(names_from = Stage, values_from = NumTrials, values_fill = 0)

# Add the total number of trials an individual participated in
trial_summary <- trial_summary %>%
  rowwise() %>%
  mutate(TotalTrials = sum(c_across(Pre:Dep), na.rm = TRUE))

# Treatment trial summary
trial_sum_table <- summary_stats(trial_summary, c("Treatment", "Temp"), c("Pre", "Post", "TotalTrials"))
 
#FLEXTABLE
#Create table with trial summary data
trial_sum_table <- 
  flextable(trial_sum_table) %>% 
  fontsize(part = "all", size = 11) %>% 
  bold(part = 'header') %>% 
  set_header_labels("Treatment" = 'Treatment',
                    "Temp" = 'Temperature',
                    "Pre" = 'Pre-exposure',
                    "Post" = 'Post-exposure'
                    "TotalTrials" = "Total")
#save table in directory
save_as_docx(trial_sum_table, path = paste0(save_table_path, "trial_summary.docx"))

# Add trial info to main dataframe
gaba_data <- gaba_data %>%
  left_join(trial_summary %>% dplyr::select(-Treatment, -Temp), by = "ID")


# 3. What percentage of individuals completed all trials per stage ####

#Define thresholds for completion
full_trials <- list(Pre = 4, Post = 4, TotalTrials = 8)

#Create a column indicating if the individual completed the full set for each stage
trial_summary$Completed_Pre <- trial_summary$Pre == full_trials$Pre
trial_summary$Completed_Post <- trial_summary$Post == full_trials$Post
trial_summary$Completed_TotalTrials <- trial_summary$TotalTrials == full_trials$TotalTrials

# Group by Treatment and calculate the percentage of individuals who completed each stage
trial_summary %>%
  group_by(Treatment, Temp) %>%
  summarise(
    Completed_Pre_Count = sum(Completed_Pre),
    Completed_Post_Count = sum(Completed_Post),
    Completed_TotalTrials_Count = sum(Completed_TotalTrials),
    Total_Individuals = n(),
    Percent_Completed_Pre = mean(Completed_Pre) * 100,
    Percent_Completed_Post = mean(Completed_Post) * 100,
    Percent_Completed_TotalTrials = mean(Completed_TotalTrials) * 100
  ) %>%
  mutate(
    Pre = paste0(round(Percent_Completed_Pre, 1), " (", Completed_Pre_Count, "/", Total_Individuals, ")"),
    Post = paste0(round(Percent_Completed_Post, 1), " (", Completed_Post_Count, "/", Total_Individuals, ")"),
    Total = paste0(round(Percent_Completed_TotalTrials, 1), " (", Completed_TotalTrials_Count, "/", Total_Individuals, ")")
  ) %>%
  select(Treatment, Pre, Post, Total)


#4. Fish size summary ####

#Extract all distinct indiviuals 
fish_size_sum <- 
  gaba_data %>%
  select(ID, Treatment, Temp, Length, Weight) %>% 
  distinct()

#168 individuals here
#166 unique indiviuals identified earlier
#Suggest that there maybe a duplicated ID with different length and weight
#Need to explore

fish_size_sum %>% 
  group_by(ID) %>% 
  filter(n_distinct(Length, Weight) > 1) %>% 
  arrange(ID)

#IDs with duplicated rows
#C_10_2_Blue
#C_14_2_Orange
#C_14_6_Blue   

#replace NAs
gaba_data <- gaba_data %>%
  mutate(Length = ifelse(is.na(Length) & ID == "C_10_2_Blue" & Treatment == "control", 8.3, Length),
         Weight = ifelse(is.na(Weight) & ID == "C_10_2_Blue" & Treatment == "control", 5.6, Weight))

gaba_data <- gaba_data %>%
  group_by(ID) %>%
  mutate(
    Length = mean(Length, na.rm = TRUE),
    Weight = mean(Weight, na.rm = TRUE)
  ) %>%
  ungroup()

#rerun fish_size_sum to check issue is fixed

#fish size summary
fish_wghts <- summary_stats(fish_size_sum, c("Treatment", "Temp"), c("Length", "Weight"))

#FLEXTABLE
#Create fish size summary table
fish_wghts_table <- 
  flextable(fish_wghts) %>% 
  fontsize(part = "all", size = 11) %>% 
  bold(part = 'header') %>% 
  set_header_labels("Treatment" = 'Treatment',
                    "Temp" = 'Temperature',
                    "Length" = 'Length',
                    "Weight" = 'Weight')
#save to directory
save_as_docx(fish_wghts_table, path = paste0(save_table_path, "fish_size_summary.docx"))

#save edited dataframe
saveRDS(gaba_data, "./data/02_gaba_data.rds")

#---------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------#

#END SCRIPT
