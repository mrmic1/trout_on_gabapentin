#------------------------------------------------#
# DIFFERENCES IN VARIANCE #############-
#------------------------------------------------#

library(tidyverse)
library(brms)
library(broom)
library(bayesplot)
library(loo)
library(performance)
library(rethinking)
library(tidybayes)
library(purrr)
library(emmeans)
library(glue)

### DIRECTORIES ###
save_table_path <- "./tables/"

### LOAD DATASET ###
gaba_data <- readRDS("./data/gaba_data.rds")

### LOAD MODELS ###
dist_moved_mod_brms <- readRDS('./models/distance_moved_model.rds')
shelter_time_mod_brms <- readRDS('./models/shelter_time_mod_brms.rds')

#FOR PLOTTING
clrs <- c("#1F449C", "#F05039")

#--------------------------------------------------------------------------------------------#

#----------------------------------------------------#
# DISTANCE MOVED ####
#----------------------------------------------------#

#1. Extract variance comps ####

### INDIVIDUALS ###

#Extract individual variance estimates
get_variables(dist_moved_mod_brms)[19:34]

#Among-individual variation draws
#Control
va_pre_control_10 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePre:Treatment:Tempcontrol:10"^2
va_post_control_10 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePost:Treatment:Tempcontrol:10"^2
va_pre_control_14 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePre:Treatment:Tempcontrol:14"^2
va_post_control_14 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePost:Treatment:Tempcontrol:14"^2

#Exposed
va_pre_exposed_10 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePre:Treatment:Tempexposed:10"^2
va_post_exposed_10 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePost:Treatment:Tempexposed:10"^2
va_pre_exposed_14 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePre:Treatment:Tempexposed:14"^2
va_post_exposed_14 <- as_draws_df(dist_moved_mod_brms)$"sd_ID__StagePost:Treatment:Tempexposed:14"^2

#Put among-individualvariance estimates into a dataframe 
dist_va_data = data.frame(
  va_pre_control_10,
  va_post_control_10,
  va_pre_control_14,
  va_post_control_14,
  va_pre_exposed_10,
  va_post_exposed_10,
  va_pre_exposed_14 ,
  va_post_exposed_14
)


dist_va_data_long <- dist_va_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "va_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "va"  # Name of the column that holds the values
  )


#Within-individual variation
get_variables(dist_moved_mod_brms)[11:18]

#Control
vw_pre_control_10 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentcontrol:Temp10:StagePre"^2
vw_post_control_10 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentcontrol:Temp10:StagePost"^2
vw_pre_control_14 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentcontrol:Temp14:StagePre"^2
vw_post_control_14 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentcontrol:Temp14:StagePost"^2

#Exposed
vw_pre_exposed_10 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentexposed:Temp10:StagePre"^2
vw_post_exposed_10 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentexposed:Temp10:StagePost"^2
vw_pre_exposed_14 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentexposed:Temp14:StagePre"^2
vw_post_exposed_14 <- exp(as_draws_df(dist_moved_mod_brms))$"b_sigma_Treatmentexposed:Temp14:StagePost"^2

dist_vw_data = data.frame(
  vw_pre_control_10,
  vw_post_control_10,
  vw_pre_control_14,
  vw_post_control_14,
  vw_pre_exposed_10,
  vw_post_exposed_10,
  vw_pre_exposed_14 ,
  vw_post_exposed_14
)

dist_vw_data_long <- dist_vw_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "vw_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "vw"  # Name of the column that holds the values
  )

### TANKS ###

#Extract tank variance estimates
#Among-tank only
get_variables(dist_moved_mod_brms)[27:34]

#Control
va_tank_pre_control_10 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePre:Treatment:Tempcontrol:10"^2
va_tank_post_control_10 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePost:Treatment:Tempcontrol:10"^2
va_tank_pre_control_14 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePre:Treatment:Tempcontrol:14"^2
va_tank_post_control_14 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePost:Treatment:Tempcontrol:14"^2

#Exposed
va_tank_pre_exposed_10 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePre:Treatment:Tempexposed:10"^2
va_tank_post_exposed_10 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePost:Treatment:Tempexposed:10"^2
va_tank_pre_exposed_14 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePre:Treatment:Tempexposed:14"^2
va_tank_post_exposed_14 <- as_draws_df(dist_moved_mod_brms)$"sd_Tank__StagePost:Treatment:Tempexposed:14"^2

#Put among-individualvariance estimates into a dataframe 
dist_va_tank_data = data.frame(
  va_tank_pre_control_10,
  va_tank_post_control_10,
  va_tank_pre_control_14,
  va_tank_post_control_14,
  va_tank_pre_exposed_10,
  va_tank_post_exposed_10,
  va_tank_pre_exposed_14 ,
  va_tank_post_exposed_14
)

dist_va_tank_data_long <- dist_va_tank_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "va_tank_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "va_tank"  # Name of the column that holds the values
  )

### COMBINE INTO VARIANCE TABLE ###
#combine by column
dist_var_data <- cbind(dist_va_data_long, dist_vw_data_long[,4], dist_va_tank_data_long[,4]) #only need the 4th column from vw_long
dist_var_data_wide <- cbind(dist_va_data, dist_vw_data, dist_va_tank_data) 

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

#2. Calculate repeatability ####

#individual
dist_variance_data = 
  dist_var_data %>%
  mutate(ind_rpt = va/(va + va_tank + vw))

#tank
dist_variance_data = 
  dist_variance_data %>% 
  mutate(tank_rpt = va_tank/(va_tank + vw))

# Define new stage names (for plotting purposes mainly)
stage_new_names <- c("pre" = "Pre",
                     "post" = "Post")

treatment_new_names <- c("control" = "Control",
                         "exposed" = "Exposed")

# Convert 'stage' to a factor with the specified order
dist_variance_data <- 
  dist_variance_data %>%
  mutate(Stage = factor(Stage, levels = c("pre", "post"),
                        labels = stage_new_names),
         Treatment = factor(Treatment, levels = c("control", "exposed"),
                            labels = treatment_new_names))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#3. Repeatability estimates and plot ####

# Mean estimate for each stage and treatment
dist_repeatability = 
  dist_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(R_ind = round(mean(ind_rpt), 2),
            lowerCI_R = round(rethinking::HPDI(ind_rpt, prob = 0.89)[1], 2),
            upperCI_R = round(rethinking::HPDI(ind_rpt, prob = 0.89)[2], 2)) %>% 
  mutate(
    R_ind = glue("{R_ind} ({lowerCI_R} - {upperCI_R})")) %>% 
  select(Treatment, Temp, Stage, R_ind)

print(dist_repeatability)

# Repeatability plot
(dist_move_repeat_plot  = 
    ggplot(dist_variance_data, 
           aes(x = Treatment, y = ind_rpt, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Repeatability", x = "") +
    ylim(0, 1) +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/distance_moved/dist_moved_repeatability_plot.pdf", 
       plot = dist_move_repeat_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#

#4. VA estimates and plot ####

# Mean estimate for each stage and treatment
dist_VA = dist_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(VA = round(mean(va), 2),
            lowerCI_VA = round(rethinking::HPDI(va, prob = 0.89)[1], 2),
            upperCI_VA = round(rethinking::HPDI(va, prob = 0.89)[2], 2)) %>% 
  mutate(
    VA = glue("{VA} ({lowerCI_VA} - {upperCI_VA})")) %>% 
  select(Treatment, Temp, Stage, VA)

print(dist_VA)

#Distance moved VA plot
(dist_VA_plot  = 
    ggplot(dist_variance_data, 
           aes(x = Treatment, y = va, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Among-individual variance", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/distance_moved/dist_moved_VA_plot.pdf", 
       plot = dist_VA_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')


#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#

#5. VW estimates and plot ####

# Mean estimate for each stage and treatment - this will be used to create a table
VW_dist = dist_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(VW = round(mean(vw), 2),
            lowerCI_VW = round(rethinking::HPDI(vw, prob = 0.89)[1], 2),
            upperCI_VW = round(rethinking::HPDI(vw, prob = 0.89)[2], 2)) %>% 
  mutate(
    VW = glue("{VW} ({lowerCI_VW} - {upperCI_VW})")) %>% 
  select(Treatment, Temp, Stage, VW)

print(VW_dist)

#Within-individual plot
(dist_VW_plot  = 
    ggplot(dist_variance_data, 
           aes(x = Treatment, y = vw, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Within-individual variance", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/distance_moved/dist_moved_VW_plot.pdf", 
       plot = dist_VW_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#


#6. Create variance table ####

dist_variance_table <- cbind(dist_VA,VW_dist[4], dist_repeatability[4])
print(dist_variance_table)

## FLEXTABLE ##
(dist_variance_table <- 
    flextable(dist_variance_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header') %>% 
    set_header_labels("Temp" = 'Temperature',
                      "R_ind" = 'R'))

save_as_docx(dist_variance_table, path = paste0(save_table_path, "dist_variance_table.docx"))


#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#

#7. Calculate variance change #####

#turn to wide format
# Add a unique identifier for each row
dist_variance_data <- 
  dist_variance_data %>%
  group_by(Treatment, Temp, Stage) %>%
  mutate(row_id = row_number()) %>%
  ungroup()


#### > 7.1. Post - Pre ####

dist_variance_stage_wide <- 
  dist_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = Stage,
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Treatment, Temp, row_id)
  ) %>%
  select(-row_id) # Remove the identifier if no longer needed

#check
head(dist_variance_stage_wide)

# Calculate the difference between Pre and Post values for each treatment and temperature group
dist_pre_post_variance_diff <- 
  dist_variance_stage_wide %>%
  #Calculate variance change between pre- and post scores
  mutate(
    va_diff = va_Post - va_Pre,
    vw_diff = vw_Post - vw_Pre,
    r_diff = ind_rpt_Post - ind_rpt_Pre
  ) %>% 
  #rename variable
  rename(r_Pre = ind_rpt_Pre,
         r_Post = ind_rpt_Post) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_Pre),
         vw_perc_change = 100 * (vw_diff/vw_Pre),
         r_perc_change = 100 * (r_diff/r_Pre)) %>% 
  #Re-order variables 
  select(Treatment, Temp, 
         va_Pre, va_Post, va_diff, va_perc_change, 
         vw_Pre, vw_Post, vw_diff, vw_perc_change,
         r_Pre, r_Post, r_diff, r_perc_change) 

#check
head(dist_pre_post_variance_diff)

#Calculate mean and 89% CIs for variance component
#Among-individual variation 
(dist_VA_pre_post_difference <- 
  dist_pre_post_variance_diff %>% 
  group_by(Treatment, Temp) %>% 
  summarise(va_diff_mean = round(mean(va_diff ), 2),
            lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
            upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
            va_diff_perc = round(mean(va_perc_change), 2),
            lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
            upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))


#Within-individual variation
(dist_VW_pre_post_difference <- 
    dist_pre_post_variance_diff %>% 
    group_by(Treatment, Temp) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(dist_R_pre_post_difference <- 
    dist_pre_post_variance_diff %>% 
    group_by(Treatment, Temp) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

dist_stage_contrast <-
  cbind(dist_VA_pre_post_difference, dist_VW_pre_post_difference[-c(1,2)], dist_R_pre_post_difference[-c(1,2)])

#create flex-table
dist_stage_variance_contrast_table <- 
  dist_stage_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Treatment, Temp,  "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(dist_stage_variance_contrast_table = 
    qflextable(dist_stage_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(dist_stage_variance_contrast_table, 
             path = "./tables/distance_moved/dist_delta_stage_contrast_table.docx")


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#


#### > 7.2 Exposed - Control  ####

#Calculate treatment differences at each stage
dist_variance_treatment_wide <- 
  dist_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = Treatment,
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Temp, Stage, row_id)
  ) %>%
  select(-row_id) # Remove the identifier if no longer needed

#check
head(dist_variance_treatment_wide)

# Calculate the difference between Control and Exposed values for each treatment and temperature group
dist_treatment_variance_diff <- 
  dist_variance_treatment_wide %>%
  #Calculate variance change between Control- and Exposed scores
  mutate(
    va_diff = va_Exposed - va_Control,
    vw_diff = vw_Exposed - vw_Control,
    r_diff = ind_rpt_Exposed - ind_rpt_Control
  ) %>% 
  #rename variable
  rename(r_Control = ind_rpt_Control,
         r_Exposed = ind_rpt_Exposed) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_Control),
         vw_perc_change = 100 * (vw_diff/vw_Control),
         r_perc_change = 100 * (r_diff/r_Control)) %>% 
  #Re-order variables 
  select(Temp, Stage, 
         va_Control, va_Exposed, va_diff, va_perc_change, 
         vw_Control, vw_Exposed, vw_diff, vw_perc_change,
         r_Control, r_Exposed, r_diff, r_perc_change) 

#check
head(dist_treatment_variance_diff)

#Calculate mean and 89% CIs for each variance component
#Among-individual variation 
(dist_VA_treatment_difference <- 
    dist_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(va_diff_mean = round(mean(va_diff ), 2),
              lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
              upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
              va_diff_perc = round(mean(va_perc_change), 2),
              lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
              upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))


#Within-individual variation
(dist_VW_treatment_difference <- 
    dist_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(dist_R_treatment_difference <- 
    dist_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

dist_treatment_contrast <-
  cbind(dist_VA_treatment_difference, dist_VW_treatment_difference[-c(1,2)], dist_R_treatment_difference[-c(1,2)])

#create flex-table
dist_treatment_variance_contrast_table <- 
  dist_treatment_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Stage, Temp,  "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(dist_treatment_variance_contrast_table = 
    qflextable(dist_treatment_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(dist_treatment_variance_contrast_table, 
             path = "./tables/distance_moved/dist_delta_treatment_contrast_table.docx")


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#

#### > 7.3 Temperature differences ####

#Calculate temperature differences at the pre-exposure stage (i.e. before treatment was applied)

dist_variance_temp_wide <- 
  dist_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = 'Temp',
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Stage, Treatment, row_id)
  ) %>%
  select(-row_id, -Treatment) # Remove the identifier if no longer needed

#check
head(dist_variance_temp_wide)

# Calculate the difference between 10 and 14 values for each temp and temperature group
dist_temp_variance_diff <- 
  dist_variance_temp_wide %>%
  #Calculate variance change between 10- and 14 scores
  mutate(
    va_diff = va_14 - va_10,
    vw_diff = vw_14 - vw_10,
    r_diff = ind_rpt_14 - ind_rpt_10
  ) %>% 
  #rename variable
  rename(r_10 = ind_rpt_10,
         r_14 = ind_rpt_14) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_10),
         vw_perc_change = 100 * (vw_diff/vw_10),
         r_perc_change = 100 * (r_diff/r_10)) %>% 
  #Re-order variables 
  select(Stage, va_10, va_14, va_diff, va_perc_change, 
         vw_10, vw_14, vw_diff, vw_perc_change,
         r_10, r_14, r_diff, r_perc_change) 

#check
head(dist_temp_variance_diff)

#Calculate mean and 89% CIs for variance component
#Among-individual variation 
(dist_VA_temp_difference <- 
    dist_temp_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(va_diff_mean = round(mean(va_diff ), 2),
              lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
              upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
              va_diff_perc = round(mean(va_perc_change), 2),
              lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
              upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))

#Within-individual variation
(dist_VW_temp_difference <- 
    dist_temp_pre_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(dist_R_temp_difference <- 
    dist_temp_pre_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

dist_temp_variance_contrast <-
  cbind(dist_VA_temp_difference, dist_VW_temp_difference[-1],dist_R_temp_difference[-1])

#create flex-table
dist_temp_variance_contrast_table <- 
  dist_temp_variance_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Stage, "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(dist_temp_variance_contrast_table = 
    qflextable(dist_temp_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(dist_temp_variance_contrast_table, 
             path = "./tables/distance_moved/dist_delta_temp_contrast_table.docx")


#---------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------#

#----------------------------------------------------#
# TIME SPENT IN SHELTER ####
#----------------------------------------------------#

#1. Extract variance comps ####

### INDIVIDUALS ###

#Extract individual variance estimates
get_variables(shelter_time_mod_brms)[19:34]

#Among-individual variation draws
#Control
va_pre_control_10 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePre:Treatment:Tempcontrol:10"^2
va_post_control_10 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePost:Treatment:Tempcontrol:10"^2
va_pre_control_14 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePre:Treatment:Tempcontrol:14"^2
va_post_control_14 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePost:Treatment:Tempcontrol:14"^2

#Exposed
va_pre_exposed_10 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePre:Treatment:Tempexposed:10"^2
va_post_exposed_10 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePost:Treatment:Tempexposed:10"^2
va_pre_exposed_14 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePre:Treatment:Tempexposed:14"^2
va_post_exposed_14 <- as_draws_df(shelter_time_mod_brms)$"sd_ID__StagePost:Treatment:Tempexposed:14"^2

#Put among-individualvariance estimates into a dataframe 
shelter_va_data = data.frame(
  va_pre_control_10,
  va_post_control_10,
  va_pre_control_14,
  va_post_control_14,
  va_pre_exposed_10,
  va_post_exposed_10,
  va_pre_exposed_14 ,
  va_post_exposed_14
)


shelter_va_data_long <- shelter_va_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "va_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "va"  # Name of the column that holds the values
  )


#Within-individual variation
get_variables(shelter_time_mod_brms)[11:18]

#Control
vw_pre_control_10 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentcontrol:Temp10:StagePre"^2
vw_post_control_10 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentcontrol:Temp10:StagePost"^2
vw_pre_control_14 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentcontrol:Temp14:StagePre"^2
vw_post_control_14 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentcontrol:Temp14:StagePost"^2

#Exposed
vw_pre_exposed_10 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentexposed:Temp10:StagePre"^2
vw_post_exposed_10 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentexposed:Temp10:StagePost"^2
vw_pre_exposed_14 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentexposed:Temp14:StagePre"^2
vw_post_exposed_14 <- exp(as_draws_df(shelter_time_mod_brms))$"b_sigma_Treatmentexposed:Temp14:StagePost"^2

shelter_vw_data = data.frame(
  vw_pre_control_10,
  vw_post_control_10,
  vw_pre_control_14,
  vw_post_control_14,
  vw_pre_exposed_10,
  vw_post_exposed_10,
  vw_pre_exposed_14 ,
  vw_post_exposed_14
)

shelter_vw_data_long <- shelter_vw_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "vw_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "vw"  # Name of the column that holds the values
  )

### TANKS ###

#Extract tank variance estimates
#Among-tank only
get_variables(shelter_time_mod_brms)[27:34]

#Control
va_tank_pre_control_10 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePre:Treatment:Tempcontrol:10"^2
va_tank_post_control_10 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePost:Treatment:Tempcontrol:10"^2
va_tank_pre_control_14 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePre:Treatment:Tempcontrol:14"^2
va_tank_post_control_14 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePost:Treatment:Tempcontrol:14"^2

#Exposed
va_tank_pre_exposed_10 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePre:Treatment:Tempexposed:10"^2
va_tank_post_exposed_10 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePost:Treatment:Tempexposed:10"^2
va_tank_pre_exposed_14 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePre:Treatment:Tempexposed:14"^2
va_tank_post_exposed_14 <- as_draws_df(shelter_time_mod_brms)$"sd_Tank__StagePost:Treatment:Tempexposed:14"^2

#Put among-individualvariance estimates into a dataframe 
shelter_va_tank_data = data.frame(
  va_tank_pre_control_10,
  va_tank_post_control_10,
  va_tank_pre_control_14,
  va_tank_post_control_14,
  va_tank_pre_exposed_10,
  va_tank_post_exposed_10,
  va_tank_pre_exposed_14 ,
  va_tank_post_exposed_14
)

shelter_va_tank_data_long <- shelter_va_tank_data %>%
  pivot_longer(
    cols = everything(),  # Select all columns
    names_pattern = "va_tank_(.*)_(.*)_(.*)",  # Regex to capture 'stage' and 'treatment'
    names_to = c("Stage", "Treatment", "Temp"),  # New column names
    values_to = "va_tank"  # Name of the column that holds the values
  )

### COMBINE INTO VARIANCE TABLE ###
#combine by column
shelter_var_data <- cbind(shelter_va_data_long, shelter_vw_data_long[,4], shelter_va_tank_data_long[,4]) #only need the 4th column from vw_long
shelter_var_data_wide <- cbind(shelter_va_data, shelter_vw_data, shelter_va_tank_data) 

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

#2. Calculate repeatability ####

#individual
shelter_variance_data = 
  shelter_var_data %>%
  mutate(ind_rpt = va/(va + va_tank + vw))

#tank
shelter_variance_data = 
  shelter_variance_data %>% 
  mutate(tank_rpt = va_tank/(va_tank + vw))

# Define new stage names (for plotting purposes mainly)
stage_new_names <- c("pre" = "Pre",
                     "post" = "Post")

treatment_new_names <- c("control" = "Control",
                         "exposed" = "Exposed")

# Convert 'stage' to a factor with the specified order
shelter_variance_data <- 
  shelter_variance_data %>%
  mutate(Stage = factor(Stage, levels = c("pre", "post"),
                        labels = stage_new_names),
         Treatment = factor(Treatment, levels = c("control", "exposed"),
                            labels = treatment_new_names))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#3. Repeatability estimates and plot ####

# Mean estimate for each stage and treatment
shelter_repeatability = 
  shelter_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(R_ind = round(mean(ind_rpt), 2),
            lowerCI_R = round(rethinking::HPDI(ind_rpt, prob = 0.89)[1], 2),
            upperCI_R = round(rethinking::HPDI(ind_rpt, prob = 0.89)[2], 2)) %>% 
  mutate(
    R_ind = glue("{R_ind} ({lowerCI_R} - {upperCI_R})")) %>% 
  select(Treatment, Temp, Stage, R_ind)

print(shelter_repeatability)

# Repeatability plot
(shelter_move_repeat_plot  = 
    ggplot(shelter_variance_data, 
           aes(x = Treatment, y = ind_rpt, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Repeatability", x = "") +
    ylim(0, 1) +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/shelter_time/shelter_time_repeatability_plot.pdf", 
       plot = shelter_move_repeat_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#

#4. VA estimates and plot ####

# Mean estimate for each stage and treatment
shelter_VA = shelter_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(VA = round(mean(va), 2),
            lowerCI_VA = round(rethinking::HPDI(va, prob = 0.89)[1], 2),
            upperCI_VA = round(rethinking::HPDI(va, prob = 0.89)[2], 2)) %>% 
  mutate(
    VA = glue("{VA} ({lowerCI_VA} - {upperCI_VA})")) %>% 
  select(Treatment, Temp, Stage, VA)

print(shelter_VA)

#shelterance moved VA plot
(shelter_VA_plot  = 
    ggplot(shelter_variance_data, 
           aes(x = Treatment, y = va, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Among-individual variance", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/shelter_time/shelter_time_VA_plot.pdf", 
       plot = shelter_VA_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')


#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#

#5. VW estimates and plot ####

# Mean estimate for each stage and treatment - this will be used to create a table
VW_shelter = shelter_variance_data %>%
  group_by(Treatment, Temp, Stage) %>% 
  summarise(VW = round(mean(vw), 2),
            lowerCI_VW = round(rethinking::HPDI(vw, prob = 0.89)[1], 2),
            upperCI_VW = round(rethinking::HPDI(vw, prob = 0.89)[2], 2)) %>% 
  mutate(
    VW = glue("{VW} ({lowerCI_VW} - {upperCI_VW})")) %>% 
  select(Treatment, Temp, Stage, VW)

print(VW_shelter)

#Within-individual plot
(shelter_VW_plot  = 
    ggplot(shelter_variance_data, 
           aes(x = Treatment, y = vw, fill = Stage)) +
    stat_halfeye(.width = c(.89), 
                 point_size = 2,
                 position = position_dodge(0.6), 
                 scale = 0.5,
                 alpha = 0.7,
                 interval_alpha = 1,
                 point_fill = 'black',
                 color = 'black')+ 
    facet_grid(~Temp) +
    theme_ggdist() +
    scale_fill_manual(values = clrs) + # Fill color for raw data points
    scale_color_manual(values = clrs) +
    scale_x_discrete(expand = expansion(mult = c(0.6, 0.6))) +
    labs(y = "Within-individual variance", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot without the legend
ggsave("./plots/shelter_time/shelter_time_VW_plot.pdf", 
       plot = shelter_VW_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#6. Create variance table ####

shelter_variance_table <- cbind(shelter_VA,VW_shelter[4], shelter_repeatability[4])
print(shelter_variance_table)

## FLEXTABLE ##
(shelter_variance_table <- 
    flextable(shelter_variance_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header') %>% 
    set_header_labels("Temp" = 'Temperature',
                      "R_ind" = 'R'))

save_as_docx(shelter_variance_table, path = paste0(save_table_path, "shelter_time/shelter_variance_table.docx"))


#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#

#7. Calculate variance change #####

#turn to wide format
# Add a unique identifier for each row
shelter_variance_data <- 
  shelter_variance_data %>%
  group_by(Treatment, Temp, Stage) %>%
  mutate(row_id = row_number()) %>%
  ungroup()


#### > 7.1. Post - Pre ####

shelter_variance_stage_wide <- 
  shelter_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = Stage,
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Treatment, Temp, row_id)
  ) %>%
  select(-row_id) # Remove the identifier if no longer needed

#check
head(shelter_variance_stage_wide)

# Calculate the difference between Pre and Post values for each treatment and temperature group
shelter_pre_post_variance_diff <- 
  shelter_variance_stage_wide %>%
  #Calculate variance change between pre- and post scores
  mutate(
    va_diff = va_Post - va_Pre,
    vw_diff = vw_Post - vw_Pre,
    r_diff = ind_rpt_Post - ind_rpt_Pre
  ) %>% 
  #rename variable
  rename(r_Pre = ind_rpt_Pre,
         r_Post = ind_rpt_Post) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_Pre),
         vw_perc_change = 100 * (vw_diff/vw_Pre),
         r_perc_change = 100 * (r_diff/r_Pre)) %>% 
  #Re-order variables 
  select(Treatment, Temp, 
         va_Pre, va_Post, va_diff, va_perc_change, 
         vw_Pre, vw_Post, vw_diff, vw_perc_change,
         r_Pre, r_Post, r_diff, r_perc_change) 

#check
head(shelter_pre_post_variance_diff)

#Calculate mean and 89% CIs for variance component
#Among-individual variation 
(shelter_VA_pre_post_difference <- 
    shelter_pre_post_variance_diff %>% 
    group_by(Treatment, Temp) %>% 
    summarise(va_diff_mean = round(mean(va_diff ), 2),
              lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
              upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
              va_diff_perc = round(mean(va_perc_change), 2),
              lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
              upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))


#Within-individual variation
(shelter_VW_pre_post_difference <- 
    shelter_pre_post_variance_diff %>% 
    group_by(Treatment, Temp) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(shelter_R_pre_post_difference <- 
    shelter_pre_post_variance_diff %>% 
    group_by(Treatment, Temp) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

shelter_stage_contrast <-
  cbind(shelter_VA_pre_post_difference, shelter_VW_pre_post_difference[-c(1,2)], shelter_R_pre_post_difference[-c(1,2)])

#create flex-table
shelter_stage_variance_contrast_table <- 
  shelter_stage_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Treatment, Temp,  "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(shelter_stage_variance_contrast_table = 
    qflextable(shelter_stage_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(shelter_stage_variance_contrast_table, 
             path = "./tables/shelter_time/shelter_delta_stage_contrast_table.docx")


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#


#### > 7.2 Exposed - Control  ####

#Calculate treatment differences at each stage
shelter_variance_treatment_wide <- 
  shelter_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = Treatment,
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Temp, Stage, row_id)
  ) %>%
  select(-row_id) # Remove the identifier if no longer needed

#check
head(shelter_variance_treatment_wide)

# Calculate the difference between Control and Exposed values for each treatment and temperature group
shelter_treatment_variance_diff <- 
  shelter_variance_treatment_wide %>%
  #Calculate variance change between Control- and Exposed scores
  mutate(
    va_diff = va_Exposed - va_Control,
    vw_diff = vw_Exposed - vw_Control,
    r_diff = ind_rpt_Exposed - ind_rpt_Control
  ) %>% 
  #rename variable
  rename(r_Control = ind_rpt_Control,
         r_Exposed = ind_rpt_Exposed) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_Control),
         vw_perc_change = 100 * (vw_diff/vw_Control),
         r_perc_change = 100 * (r_diff/r_Control)) %>% 
  #Re-order variables 
  select(Temp, Stage, 
         va_Control, va_Exposed, va_diff, va_perc_change, 
         vw_Control, vw_Exposed, vw_diff, vw_perc_change,
         r_Control, r_Exposed, r_diff, r_perc_change) 

#check
head(shelter_treatment_variance_diff)

#Calculate mean and 89% CIs for each variance component
#Among-individual variation 
(shelter_VA_treatment_difference <- 
    shelter_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(va_diff_mean = round(mean(va_diff ), 2),
              lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
              upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
              va_diff_perc = round(mean(va_perc_change), 2),
              lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
              upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))


#Within-individual variation
(shelter_VW_treatment_difference <- 
    shelter_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(shelter_R_treatment_difference <- 
    shelter_treatment_variance_diff %>% 
    group_by(Temp, Stage) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

shelter_treatment_contrast <-
  cbind(shelter_VA_treatment_difference, shelter_VW_treatment_difference[-c(1,2)], shelter_R_treatment_difference[-c(1,2)])

#create flex-table
shelter_treatment_variance_contrast_table <- 
  shelter_treatment_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Stage, Temp,  "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(shelter_treatment_variance_contrast_table = 
    qflextable(shelter_treatment_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(shelter_treatment_variance_contrast_table, 
             path = "./tables/shelter_time/shelter_delta_treatment_contrast_table.docx")


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#

#### > 7.3 Temperature differences ####

#Calculate temperature differences at the pre-exposure stage (i.e. before treatment was applied)

shelter_variance_temp_wide <- 
  shelter_variance_data %>%
  select(-va_tank, -tank_rpt) %>% 
  pivot_wider(
    names_from = 'Temp',
    values_from = c(va, vw,ind_rpt),
    id_cols = c(Stage, Treatment, row_id)
  ) %>%
  select(-row_id, -Treatment) # Remove the identifier if no longer needed

#check
head(shelter_variance_temp_wide)

# Calculate the difference between 10 and 14 values for each temp and temperature group
shelter_temp_variance_diff <- 
  shelter_variance_temp_wide %>%
  #Calculate variance change between 10- and 14 scores
  mutate(
    va_diff = va_14 - va_10,
    vw_diff = vw_14 - vw_10,
    r_diff = ind_rpt_14 - ind_rpt_10
  ) %>% 
  #rename variable
  rename(r_10 = ind_rpt_10,
         r_14 = ind_rpt_14) %>% 
  #Calculate % change
  mutate(va_perc_change = 100 * (va_diff/va_10),
         vw_perc_change = 100 * (vw_diff/vw_10),
         r_perc_change = 100 * (r_diff/r_10)) %>% 
  #Re-order variables 
  select(Stage, va_10, va_14, va_diff, va_perc_change, 
         vw_10, vw_14, vw_diff, vw_perc_change,
         r_10, r_14, r_diff, r_perc_change) 

#check
head(shelter_temp_variance_diff)

#Calculate mean and 89% CIs for variance component
#Among-individual variation 
(shelter_VA_temp_difference <- 
    shelter_temp_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(va_diff_mean = round(mean(va_diff ), 2),
              lowerCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[1], 2),
              upperCI_va_diff = round(rethinking::HPDI(va_diff , prob = 0.89)[2], 2),
              va_diff_perc = round(mean(va_perc_change), 2),
              lowerCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[1], 2),
              upperCI_va_perc_diff = round(rethinking::HPDI(va_perc_change  , prob = 0.89)[2], 2)))

#Within-individual variation
(shelter_VW_temp_difference <- 
    shelter_temp_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(vw_diff_mean = round(mean(vw_diff ), 2),
              lowerCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[1], 2),
              upperCI_vw_diff = round(rethinking::HPDI(vw_diff , prob = 0.89)[2], 2),
              vw_diff_perc = round(mean(vw_perc_change), 2),
              lowerCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[1], 2),
              upperCI_vw_perc_diff = round(rethinking::HPDI(vw_perc_change  , prob = 0.89)[2], 2)))


#Repeatability
(shelter_R_temp_difference <- 
    shelter_temp_variance_diff %>% 
    group_by(Stage) %>% 
    summarise(r_diff_mean = round(mean(r_diff ), 2),
              lowerCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[1], 2),
              upperCI_r_diff = round(rethinking::HPDI(r_diff , prob = 0.89)[2], 2),
              r_diff_perc = round(mean(r_perc_change), 2),
              lowerCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[1], 2),
              upperCI_r_perc_diff = round(rethinking::HPDI(r_perc_change  , prob = 0.89)[2], 2)))


### CREATE CONTRAST TABLE ###

shelter_temp_variance_contrast <-
  cbind(shelter_VA_temp_difference, shelter_VW_temp_difference[-1],shelter_R_temp_difference[-1])

#create flex-table
shelter_temp_variance_contrast_table <- 
  shelter_temp_variance_contrast %>%
  mutate(
    "ΔVa" = glue("{va_diff_mean} ({lowerCI_va_diff} - {upperCI_va_diff})"),
    "%Va" = glue("{va_diff_perc } ({lowerCI_va_perc_diff } - {upperCI_va_perc_diff})"),
    "ΔVw" = glue("{vw_diff_mean} ({lowerCI_vw_diff} - {upperCI_vw_diff})"),
    "%Vw" = glue("{vw_diff_perc } ({lowerCI_vw_perc_diff } - {upperCI_vw_perc_diff})"),
    "ΔR" = glue("{r_diff_mean} ({lowerCI_r_diff} - {upperCI_r_diff})"),
    "%R" = glue("{r_diff_perc } ({lowerCI_r_perc_diff } - {upperCI_r_perc_diff})"),
  ) %>%
  select(Stage, "ΔVa",  "%Va", "ΔVw", "%Vw", "ΔR","%R")

(shelter_temp_variance_contrast_table = 
    qflextable(shelter_temp_variance_contrast_table) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header'))

#save the table as word doc.
save_as_docx(shelter_temp_variance_contrast_table, 
             path = "./tables/shelter_time/shelter_delta_temp_contrast_table.docx")


#---------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------#


#END OF SCRIPT