#------------------------------------------------#
############ MEAN LEVEL ANALYSIS #############-
#------------------------------------------------#

### LOAD LIBRARIES ###
library(tidyverse)
library(brms)
library(rethinking)
library(tidybayes)
library(emmeans)
library(flextable)
library(kableExtra)

### DIRECTORIES ###
save_table_path <- "./tables/"

### LOAD DATASET ###
gaba_data <- readRDS("./data/gaba_data.rds")

### FOR PLOTTING ###
clrs <- c("#1F449C", "#F05039")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------#
# Distance moved ####
#------------------------------------------#

#load model
dist_moved_mod_brms <- readRDS('./models/distance_moved_model.rds')

#1.1. Parameter summary table ####

#Model summary
print(summary(dist_moved_mod_brms, prob = 0.89), digits = 3) #All Rhat = 1 (good).

#FLEXTABLE
#extract summary table from brms
dist_moved_b_params <- 
  summary(
    dist_moved_mod_brms, prob = 0.89)$fixed 

#table cleaning
#round to three decimal points
(dist_moved_b_params <- 
    rownames_to_column(dist_moved_b_params, "Parameters") %>% 
    mutate_if(is.numeric, round, 3) %>% 
    dplyr::select(-Rhat, -Bulk_ESS, -Tail_ESS) %>% 
    dplyr::filter(!str_starts(Parameters, 'sigma_'))) 

#use flextable to create a table in a word document  
(dist_moved_table = 
    qflextable(dist_moved_b_params) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header') %>% 
    compose(i = 2, j = 1, as_paragraph(as_chunk('Gabapentin treatment (Exposed)'))) %>% 
    compose(i = 3, j = 1, as_paragraph(as_chunk('Temperature (14°C)')))%>% 
    compose(i = 4, j = 1, as_paragraph(as_chunk('Stage (Post-treatment)')))%>% 
    compose(i = 5, j = 1, as_paragraph(as_chunk('Trial number')))%>% 
    compose(i = 6, j = 1, as_paragraph(as_chunk('Fish weight')))%>% 
    compose(i = 7, j = 1, as_paragraph(as_chunk('Treatment * Temperature')))%>% 
    compose(i = 8, j = 1, as_paragraph(as_chunk('Treatment * Stage')))%>% 
    compose(i = 9, j = 1, as_paragraph(as_chunk('Temp * Stage')))%>% 
    compose(i = 10, j = 1, as_paragraph(as_chunk('Treatment * Temp * Stage'))))  #change row 2 and column 1 

#save in directory
save_as_docx(dist_moved_table, path = paste0(save_table_path, "distance_moved_model_parameter_table.docx"))


#1.2. Mean summary and plot ####

#To convert to distance moved to original scale
#Helps with interpreting the results
dist_mean <- mean(gaba_data$DistMove)
dist_sd <- sd(gaba_data$DistMove)

dist_moved_draws <- 
  dist_moved_mod_brms %>% 
  emmeans( ~ Temp*Treatment*Stage,
           at = list(trial_stage_cent = mean(gaba_data$trial_stage_cent),
                     Weight_z = mean(gaba_data$Weight_z),
                     ID = NA,
                     Tank = NA),
           epred = TRUE) %>% 
  #gather posterior mean draws
  gather_emmeans_draws() %>% 
  #convert response variable back to original scale
  mutate(
    .value = (.value * dist_sd) + dist_mean
  )

#check
head(dist_moved_draws)

#Model predicted means and 89% CIs for each treatment, temperature and stage
(dist_moved_draws_means <- 
    dist_moved_draws %>% 
    mean_hdi(.width = 0.89))

#Mean distance moved figure (Figure 1A)
(avg_dist_move_plot  = 
    ggplot(dist_moved_draws, 
           aes(x = Treatment, y = .value, fill = Stage)) +
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
    labs(y = "Distance moved (mm)", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot
ggsave("./plots/distance_moved/dist_moved_pred_mean_plot.pdf", 
       plot = avg_dist_move_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')


#1.3. Planned contrasts in distance moved between stages ####

#### > 1.3.1. Pre v Post-exposure ####

#Gather posterior draws for planned pre- post-exposure contrast
dist_pre_post_contrast_draws <-
  dist_moved_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Control 10°C" = c(-1, 0, 0, 0, 1, 0, 0, 0),
      "Exposed 10°C" = c(0, -1, 0, 0, 0, 1, 0, 0),
      "Control 14°C" = c(0, 0, -1, 0, 0, 0, 1, 0),
      "Exposed 14°C" = c(0, 0, 0, -1, 0, 0, 0, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(dist_pre_post_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of DistMove
# Annotate with percentage change calculations
dist_pre_post_contrast_means <- 
  dist_pre_post_contrast_draws %>% 
    select(-.chain, -.iteration, -.draw) %>% 
    mean_hdi(.width = 0.89) %>% 
    mutate(
      .value = .value * dist_sd, # Back-transform
      .lower = .lower * dist_sd, # Back-transform lower CI
      .upper = .upper * dist_sd  # Back-transform upper CI
    ) %>% 
  #Extract pre-exposure mean scores for percentage change calculations
    mutate(
      pre_stage_mean = as.numeric(c(
        dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Pre") %>% pull(.value),
        dist_moved_draws_means %>% filter(Treatment == "exposed" & Temp == '10' & Stage == "Pre") %>% pull(.value),
        dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Pre") %>% pull(.value),
        dist_moved_draws_means %>% filter(Treatment == "exposed" & Temp == '14' & Stage == "Pre") %>% pull(.value)
      )) # Ensure numeric vector
    ) %>% 
    #Calculate percentage change
    mutate(
      perc_change = (.value / pre_stage_mean) * 100,          # Percent change
      lower_perc_change = (.lower / pre_stage_mean) * 100,   # Lower CI percent change
      upper_perc_change = (.upper / pre_stage_mean) * 100    # Upper CI percent change
    ) %>% 
  #remove unneeded columns
  select(-.width, -.point, -.interval, -pre_stage_mean)

print(dist_pre_post_contrast_means)

 
#### _________Pre vs. Post contrast plot ####

#reordering levels to help with plotting
dist_pre_post_contrast_draws <- 
  dist_pre_post_contrast_draws %>% 
  mutate(contrast = as_factor(contrast),
         contrast = fct_relevel(contrast, c("Exposed 10°C", 
                                            "Control 10°C", 
                                            "Exposed 14°C", 
                                            "Control 14°C")))

#For faceting purposes
dist_pre_post_contrast_draws <- 
  dist_pre_post_contrast_draws %>%
  mutate(Temp = case_when(
    grepl("10°C", contrast) ~ 10,
    grepl("14°C", contrast) ~ 14
  ))

#plot
(dist_post_pre_contrast_plot <- 
    ggplot(dist_pre_post_contrast_draws, 
           aes(x = .value, y = contrast, 
               fill = contrast)) +
    stat_pointinterval(aes(fill = contrast), 
                       .width = c(0.89), 
                       point_size = 2,  # Increase the size of the point
                       interval_size = 0.8,
                       point_interval = 'mean_hdi')  +
    labs(x = "Difference in median marginal effects", 
         y = '') +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') +
    ggokabeito::scale_fill_okabe_ito(alpha = 0.5) + 
    facet_wrap( ~ Temp , scales = "free_y", nrow = 2) +
    xlim(-1.5, 1.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 10, color = 'black'),
          axis.text.x = element_text(size = 10, color = 'black'),
          axis.title = element_blank(),
          strip.background = element_blank()))

#save in directory
ggsave(
  filename = "./plots/distance_moved/dist_10_mean_contrast_plot.pdf", 
  dist_10_contrast_plot,
  width = 5.7,
  height = 8.5,
  units = 'cm')

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#### > 1.3.2. Exposed v Control ####

#Gather posterior draws for planned pre- post-exposure contrast
dist_treatment_contrast_draws <-
  dist_moved_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Pre 10°C" = c(-1, 1, 0, 0, 0, 0, 0, 0),
      "Pre 14°C" = c(0, 0, -1, 1, 0, 0, 0, 0),
      "Post 10°C" = c(0, 0, 0, 0, -1, 1, 0, 0),
      "Post 14°C" = c(0, 0, 0, 0, 0, 0, -1, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(dist_treatment_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of DistMove
# Annotate with percentage change calculations
dist_treatment_contrast_means <- 
  dist_treatment_contrast_draws %>% 
  select(-.chain, -.iteration, -.draw) %>% 
  mean_hdi(.width = 0.89) %>% 
  mutate(
    .value = .value * dist_sd, # Back-transform
    .lower = .lower * dist_sd, # Back-transform lower CI
    .upper = .upper * dist_sd  # Back-transform upper CI
  ) %>% 
  #Extract pre-exposure mean scores for percentage change calculations
  mutate(
    control_mean = as.numeric(c(
      dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Pre") %>% pull(.value),
      dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Pre") %>% pull(.value),
      dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Post") %>% pull(.value),
      dist_moved_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Post") %>% pull(.value)
    )) # Ensure numeric vector
  ) %>% 
  #Calculate percentage change
  mutate(
    perc_change = (.value / control_mean) * 100,          # Percent change
    lower_perc_change = (.lower / control_mean) * 100,   # Lower CI percent change
    upper_perc_change = (.upper / control_mean) * 100    # Upper CI percent change
  ) %>% 
  #remove unneeded columns
  select(-.width, -.point, -.interval, -control_mean)

print(dist_treatment_contrast_means)


#### _________Exposed v Control contrast plot ####

#reordering levels to help with plotting
dist_treatment_contrast_draws <- 
  dist_treatment_contrast_draws %>% 
  mutate(contrast = as_factor(contrast),
         contrast = fct_relevel(contrast, c("Post 10°C", 
                                            "Pre 10°C", 
                                            "Post 14°C", 
                                            "Pre 14°C")))

#For faceting purposes
dist_treatment_contrast_draws <- 
  dist_treatment_contrast_draws %>%
  mutate(Temp = case_when(
    grepl("10°C", contrast) ~ 10,
    grepl("14°C", contrast) ~ 14
  ))

#plot
(dist_treatment_contrast_plot <- 
    ggplot(dist_treatment_contrast_draws, 
           aes(x = .value, y = contrast, 
               fill = contrast)) +
    stat_pointinterval(aes(fill = contrast), 
                       .width = c(0.89), 
                       point_size = 2,  # Increase the size of the point
                       interval_size = 0.8,
                       point_interval = 'mean_hdi')  +
    labs(x = "Difference in median marginal effects", 
         y = '') +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') +
    ggokabeito::scale_fill_okabe_ito(alpha = 0.5) + 
    facet_wrap( ~ Temp , scales = "free_y", nrow = 2) +
    xlim(-1.5, 1.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 10, color = 'black'),
          axis.text.x = element_text(size = 10, color = 'black'),
          axis.title = element_blank(),
          strip.background = element_blank()))

#save in directory
ggsave(
  filename = "./plots/distance_moved/dist_10_mean_contrast_plot.pdf", 
  dist_10_contrast_plot,
  width = 5.7,
  height = 8.5,
  units = 'cm')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#### > 1.3.3. Temperature pre-exposure contrast ####

#Gather posterior draws for planned pre- post-exposure contrast
dist_temp_contrast_draws <-
  dist_moved_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Pre 10°C - 14°C" = c(-1, -1, 1, 1, 0, 0, 0, 0),
      "Post 10°C - 14°C" = c(0, 0, 0, 0, -1, -1, 1, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(dist_temp_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of DistMove
# Annotate with percentage change calculations
dist_temp_contrast_means <- 
  dist_temp_contrast_draws %>% 
  select(-.chain, -.iteration, -.draw) %>% 
  mean_hdi(.width = 0.89) %>% 
  mutate(
    .value = .value * dist_sd, # Back-transform
    .lower = .lower * dist_sd, # Back-transform lower CI
    .upper = .upper * dist_sd  # Back-transform upper CI
  ) %>%  
  #remove unneeded columns
  select(-.width, -.point, -.interval)

print(dist_temp_contrast_means)


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# Time spent in shelter ####

# 2.1. Parameter summary table ####

#load model
shelter_time_mod_brms <- readRDS('./models/shelter_time_mod_brms.rds')
#Model summary
print(summary(shelter_time_mod_brms, prob = 0.89), digits = 3) #All Rhat = 1 (good).

#FLEXTABLE
#extract summary table from brms
shelter_time_b_params <- 
  summary(
    shelter_time_mod_brms, prob = 0.89)$fixed 

(shelter_time_b_params <- 
    rownames_to_column(shelter_time_b_params, "Parameters") %>% 
    mutate_if(is.numeric, round, 3) %>% 
    dplyr::select(-Rhat, -Bulk_ESS, -Tail_ESS) %>% 
    dplyr::filter(!str_starts(Parameters, 'sigma_'))) 

#use flextable to create a table in a word document  
(shelter_time_table = 
    qflextable(shelter_time_b_params) %>% 
    fontsize(part = "all", size = 11) %>% 
    bold(part = 'header') %>% 
    compose(i = 2, j = 1, as_paragraph(as_chunk('Gabapentin treatment (Exposed)'))) %>% 
    compose(i = 3, j = 1, as_paragraph(as_chunk('Temperature (14°C)')))%>% 
    compose(i = 4, j = 1, as_paragraph(as_chunk('Stage (Post-treatment)')))%>% 
    compose(i = 5, j = 1, as_paragraph(as_chunk('Trial number')))%>% 
    compose(i = 6, j = 1, as_paragraph(as_chunk('Fish weight')))%>% 
    compose(i = 7, j = 1, as_paragraph(as_chunk('Treatment * Temperature')))%>% 
    compose(i = 8, j = 1, as_paragraph(as_chunk('Treatment * Stage')))%>% 
    compose(i = 9, j = 1, as_paragraph(as_chunk('Temp * Stage')))%>% 
    compose(i = 10, j = 1, as_paragraph(as_chunk('Treatment * Temp * Stage'))))  #change row 2 and column 1 

save_as_docx(shelter_time_table, path = paste0(save_table_path, "shelter_time_model_parameter_table.docx"))


#Conditional effects
shelter_time_cond_plots <- plot(conditional_effects(shelter_time_mod_brms, prob = 0.89) , plot = F)

#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#

#2.2. Mean summary and plot ####

#To convert to shelter time to original scale
#Helps with interpreting the results
shelter_mean <- mean(gaba_data$TimeInShelter)
shelter_sd <- sd(gaba_data$TimeInShelter)

shelter_time_draws <- 
  shelter_time_mod_brms %>% 
  emmeans( ~ Temp*Treatment*Stage,
           at = list(trial_stage_cent = mean(gaba_data$trial_stage_cent),
                     Weight_z = mean(gaba_data$Weight_z),
                     ID = NA,
                     Tank = NA),
           epred = TRUE) %>% 
  #gather posterior mean draws
  gather_emmeans_draws() %>% 
  #convert response variable back to original scale
  mutate(
    .value = (.value * shelter_sd) + shelter_mean
  )

#check
head(shelter_time_draws)

#Model predicted means and 89% CIs for each treatment, temperature and stage
(shelter_time_draws_means <- 
    shelter_time_draws %>% 
    mean_hdi(.width = 0.89))

#Mean shelterance time figure (Figure 1A)
(avg_shelter_time_plot  = 
    ggplot(shelter_time_draws, 
           aes(x = Treatment, y = .value, fill = Stage)) +
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
    labs(y = "Time spent in shelter (s)", x = "") +
    theme(panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.text = element_text(size = 10, color = 'black'),
          axis.title = element_text(size = 12, color = 'black'),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1))
)  # Increase space between facets

# Save the plot
ggsave("./plots/shelter_time/shelter_time_pred_mean_plot.pdf", 
       plot = avg_shelter_time_plot, 
       width = 10.5, 
       height = 10.5,
       units = 'cm')


#2.3. Planned contrasts in shelter time between stages ####

#### > 2.3.1. Pre v Post-exposure ####

#Gather posterior draws for planned pre- post-exposure contrast
shelter_pre_post_contrast_draws <-
  shelter_time_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Control 10°C" = c(-1, 0, 0, 0, 1, 0, 0, 0),
      "Exposed 10°C" = c(0, -1, 0, 0, 0, 1, 0, 0),
      "Control 14°C" = c(0, 0, -1, 0, 0, 0, 1, 0),
      "Exposed 14°C" = c(0, 0, 0, -1, 0, 0, 0, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(shelter_pre_post_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of shelter time
# Annotate with percentage change calculations
shelter_pre_post_contrast_means <- 
  shelter_pre_post_contrast_draws %>% 
  select(-.chain, -.iteration, -.draw) %>% 
  mean_hdi(.width = 0.89) %>% 
  mutate(
    .value = .value * shelter_sd, # Back-transform
    .lower = .lower * shelter_sd, # Back-transform lower CI
    .upper = .upper * shelter_sd  # Back-transform upper CI
  ) %>% 
  #Extract pre-exposure mean scores for percentage change calculations
  mutate(
    pre_stage_mean = as.numeric(c(
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Pre") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "exposed" & Temp == '10' & Stage == "Pre") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Pre") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "exposed" & Temp == '14' & Stage == "Pre") %>% pull(.value)
    )) # Ensure numeric vector
  ) %>% 
  #Calculate percentage change
  mutate(
    perc_change = (.value / pre_stage_mean) * 100,          # Percent change
    lower_perc_change = (.lower / pre_stage_mean) * 100,   # Lower CI percent change
    upper_perc_change = (.upper / pre_stage_mean) * 100    # Upper CI percent change
  ) %>% 
  #remove unneeded columns
  select(-.width, -.point, -.interval, -pre_stage_mean)

print(shelter_pre_post_contrast_means)


#### _________Pre vs. Post contrast plot ####

#reordering levels to help with plotting
shelter_pre_post_contrast_draws <- 
  shelter_pre_post_contrast_draws %>% 
  mutate(contrast = as_factor(contrast),
         contrast = fct_relevel(contrast, c("Exposed 10°C", 
                                            "Control 10°C", 
                                            "Exposed 14°C", 
                                            "Control 14°C")))

#For faceting purposes
shelter_pre_post_contrast_draws <- 
  shelter_pre_post_contrast_draws %>%
  mutate(Temp = case_when(
    grepl("10°C", contrast) ~ 10,
    grepl("14°C", contrast) ~ 14
  ))

#plot
(shelter_post_pre_contrast_plot <- 
    ggplot(shelter_pre_post_contrast_draws, 
           aes(x = .value, y = contrast, 
               fill = contrast)) +
    stat_pointinterval(aes(fill = contrast), 
                       .width = c(0.89), 
                       point_size = 2,  # Increase the size of the point
                       interval_size = 0.8,
                       point_interval = 'mean_hdi')  +
    labs(x = "Difference in median marginal effects", 
         y = '') +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') +
    ggokabeito::scale_fill_okabe_ito(alpha = 0.5) + 
    facet_wrap( ~ Temp , scales = "free_y", nrow = 2) +
    xlim(-1.5, 1.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 10, color = 'black'),
          axis.text.x = element_text(size = 10, color = 'black'),
          axis.title = element_blank(),
          strip.background = element_blank()))

#save in directory
ggsave(
  filename = "./plots/shelterance_time/shelter_10_mean_contrast_plot.pdf", 
  shelter_10_contrast_plot,
  width = 5.7,
  height = 8.5,
  units = 'cm')

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#### > 2.3.2. Exposed v Control ####

#Gather posterior draws for planned pre- post-exposure contrast
shelter_treatment_contrast_draws <-
  shelter_time_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Pre 10°C" = c(-1, 1, 0, 0, 0, 0, 0, 0),
      "Pre 14°C" = c(0, 0, -1, 1, 0, 0, 0, 0),
      "Post 10°C" = c(0, 0, 0, 0, -1, 1, 0, 0),
      "Post 14°C" = c(0, 0, 0, 0, 0, 0, -1, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(shelter_treatment_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of shelterMove
# Annotate with percentage change calculations
shelter_treatment_contrast_means <- 
  shelter_treatment_contrast_draws %>% 
  select(-.chain, -.iteration, -.draw) %>% 
  mean_hdi(.width = 0.89) %>% 
  mutate(
    .value = .value * shelter_sd, # Back-transform
    .lower = .lower * shelter_sd, # Back-transform lower CI
    .upper = .upper * shelter_sd  # Back-transform upper CI
  ) %>% 
  #Extract pre-exposure mean scores for percentage change calculations
  mutate(
    control_mean = as.numeric(c(
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Pre") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Pre") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '10' & Stage == "Post") %>% pull(.value),
      shelter_time_draws_means %>% filter(Treatment == "control" & Temp == '14' & Stage == "Post") %>% pull(.value)
    )) # Ensure numeric vector
  ) %>% 
  #Calculate percentage change
  mutate(
    perc_change = (.value / control_mean) * 100,          # Percent change
    lower_perc_change = (.lower / control_mean) * 100,   # Lower CI percent change
    upper_perc_change = (.upper / control_mean) * 100    # Upper CI percent change
  ) %>% 
  #remove unneeded columns
  select(-.width, -.point, -.interval, -control_mean)

print(shelter_treatment_contrast_means)


#### _________Exposed v Control contrast plot ####

#reordering levels to help with plotting
shelter_treatment_contrast_draws <- 
  shelter_treatment_contrast_draws %>% 
  mutate(contrast = as_factor(contrast),
         contrast = fct_relevel(contrast, c("Post 10°C", 
                                            "Pre 10°C", 
                                            "Post 14°C", 
                                            "Pre 14°C")))

#For faceting purposes
shelter_treatment_contrast_draws <- 
  shelter_treatment_contrast_draws %>%
  mutate(Temp = case_when(
    grepl("10°C", contrast) ~ 10,
    grepl("14°C", contrast) ~ 14
  ))

#plot
(shelter_treatment_contrast_plot <- 
    ggplot(shelter_treatment_contrast_draws, 
           aes(x = .value, y = contrast, 
               fill = contrast)) +
    stat_pointinterval(aes(fill = contrast), 
                       .width = c(0.89), 
                       point_size = 2,  # Increase the size of the point
                       interval_size = 0.8,
                       point_interval = 'mean_hdi')  +
    labs(x = "Difference in median marginal effects", 
         y = '') +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') +
    ggokabeito::scale_fill_okabe_ito(alpha = 0.5) + 
    facet_wrap( ~ Temp , scales = "free_y", nrow = 2) +
    xlim(-1.5, 1.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 10, color = 'black'),
          axis.text.x = element_text(size = 10, color = 'black'),
          axis.title = element_blank(),
          strip.background = element_blank()))

#save in directory
ggsave(
  filename = "./plots/shelterance_time/shelter_10_mean_contrast_plot.pdf", 
  shelter_10_contrast_plot,
  width = 5.7,
  height = 8.5,
  units = 'cm')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#### > 1.3.3. Temperature pre-exposure contrast ####

#Gather posterior draws for planned pre- post-exposure contrast
shelter_temp_contrast_draws <-
  shelter_time_mod_brms %>%
  emmeans(
    ~Treatment*Temp*Stage,
    at = list(
      trial_stage_cent = mean(gaba_data$trial_stage_cent),
      Weight_z = mean(gaba_data$Weight_z),
      ID = NA, # Random effects excluded
      Tank = NA # Random effects excluded
    ),
    epred = TRUE
  ) %>%
  #list of contrasts
  contrast(
    list(
      "Pre 10°C - 14°C" = c(-1, -1, 1, 1, 0, 0, 0, 0),
      "Post 10°C - 14°C" = c(0, 0, 0, 0, -1, -1, 1, 1)),
    adjust = "none"
  ) %>%
  gather_emmeans_draws()

print(shelter_temp_contrast_draws)

# Calculate mean differences and back-transform to the original scale
# Back-transform values using standard deviation of shelterMove
# Annotate with percentage change calculations
shelter_temp_contrast_means <- 
  shelter_temp_contrast_draws %>% 
  select(-.chain, -.iteration, -.draw) %>% 
  mean_hdi(.width = 0.89) %>% 
  mutate(
    .value = .value * shelter_sd, # Back-transform
    .lower = .lower * shelter_sd, # Back-transform lower CI
    .upper = .upper * shelter_sd  # Back-transform upper CI
  ) %>%  
  #remove unneeded columns
  select(-.width, -.point, -.interval)

print(shelter_temp_contrast_means)

#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#

#END OF SCRIPT