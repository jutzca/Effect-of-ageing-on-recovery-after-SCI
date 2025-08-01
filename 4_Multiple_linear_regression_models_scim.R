## ---------------------------
##
## Script name: 4_Multiple_linear_regression_models_scim.R
##
## Purpose of script: To describe the EMSCI cohort and create a table for publication
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2024-09-11
##
## Copyright (c) Catherine Jutzeler, 2025
## Email: catherine.jutzeler@hest.ethz.ch
##
## ---------------------------
##
## Data source: European Multicenter Study about Spinal Cord Injury: 2000-2023
##
## Notes: This analysis is for the publication Pavese et al, 2025 submitted to Neurology
##   
## ---------------------------
##
## load up the packages we will need:  
##
library(data.table)
library(ggplot2)
library(table1)
library(dplyr)
library(plyr)
library(MatchIt) 
library(ggridges)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(data.table)){install.packages("data.table")}
#if(!require(table1)){install.packages("table1")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(plyr)){install.packages("plyr")}
#if(!require(MatchIt)){install.packages("MatchIt")}
##
## ---------------------------
##
## R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
## ---------------------------
##
## Set working directory 
setwd("/Github/Ageing-in-Spinal-Cord-Injury/")
##
## ---------------------------
##
## Set output directorypaths
outdir_figures='/Github/Ageing-in-Spinal-Cord-Injury/Figures'
outdir_tables='/Github/Ageing-in-Spinal-Cord-Injury/Tables'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original data set
emsci.filtered<- read.csv("./Data/emsci.original.scim.cohort.filtered.csv", sep = ',', header = T,  na.strings=c("","NA"))
names(emsci.filtered)


# Relevel AIS.grades.baseline

# Reorder the levels using dplyr's reorder function
emsci.filtered <- emsci.filtered %>%
  mutate(AIS.grades.baseline = reorder(AIS.grades.baseline, 
                                       factor(AIS.grades.baseline, levels = c("A", "B", "C", "D"))))

# Check the new order of levels
levels(emsci.filtered$AIS.grades.baseline)

# ------ Multiple linear regression models using delta total MS as outcome ------

# Fit a linear regression model
delta_scim_overall.lm.model <- lm(SCIM23_TotalScore_delta ~ yeardoi+AgeAtDOI + Sex  + new_nli_cat + plegia + MS_tot_baseline + AIS.grades.baseline, data = emsci.filtered)

# View the summary of the regression model
summary(delta_scim_overall.lm.model)


# Create Table
tab_model(
  delta_scim_overall.lm.model,
  pred.labels = c("Intercept", "Year of injury", "Age at Injury", "Sex: Male", "NLI: C5-C8 vs C1-C4", "NLI: L1-S3 vs C1-C4", "NLI: T1-T6 vs  C1-C4", "NLI: T7-T12 vs  C1-C4",
                  "Plegia: Tetra", "Baseline TMS",
                  "AIS B vs AIS A","AIS C vs AIS A", "AIS D vs AIS A"),
  dv.labels = c("Effect of Age on Delta SCIM Score"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value",
  digits.p = 3
)

# Plot model output
par(mfrow = c(2, 2)) # Create a 2x2 grid of plots
plot(delta_scim_overall.lm.model)


delta_scim_overall.lm.model.plot <- ggplot(data = emsci.filtered, aes(x = AgeAtDOI, y = SCIM23_TotalScore_delta)) +
  geom_point(colour="black", alpha=0.2) +                       # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line without shaded confidence interval
  labs(x = "AgeAtDOI", y = "SCIM23_TotalScore_delta", title = "Effect of Age on Delta SCIM ") +
  xlab("Age at Injury")+ylab("Delta SCIM (52 weeks - baseline)")+
  theme_minimal()
delta_scim_overall.lm.model.plot


ggsave(
  "delta_scim_overall.lm.model.plot.pdf",
  plot = delta_scim_overall.lm.model.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 3,
  units = "in",
  dpi = 300
)

dev.off()


# ------ Multiple linear regression models using delta total MS as outcome, models are stratified for AIS subgroups ------

# Create an empty list to store model objects
model_list <- list()


# Fit a linear regression model for each unique AIS grade level
for (grade_level in unique(emsci.filtered$AIS.grades.baseline)) {
  # Subset the data for the current grade level
  subset_data <- emsci.filtered %>% filter(AIS.grades.baseline == grade_level)
  
  # Fit a linear regression model
  model <- lm(SCIM23_TotalScore_delta ~ yeardoi+AgeAtDOI + Sex + new_nli_cat+ plegia + SCIM23_TotalScore_baseline, data = subset_data)
  
  # Store the model in the list
  model_list[[as.character(grade_level)]] <- model
  
}

# Combine model summaries using tab_model
combined_summary <- tab_model(model_list)

# Print the combined model summary
print(combined_summary)


# Create a box plot of TMS scores for each AIS_grade

my_colors <- c("#F3F2F2", "#9C1402", "#2ECC71", "#F1C40F", "#9B59B6")

ggplot(data = emsci.filtered, aes(x = age_groups_relabelled, y = SCIM23_TotalScore_delta)) +
  geom_violin(trim = FALSE) +  # Create the violin plot
  geom_boxplot(width = 0.2, fill = "white") +  
  facet_wrap(~ AIS.grades.baseline) +  # Create separate plots for each AIS_grade
  geom_violin(trim = FALSE, fill = my_colors[1]) +  # Create the violin plot
  geom_boxplot(width = 0.2, fill = "white", color = my_colors[2]) +  # Create the box plot
  labs(x = "AIS Grade", y = "TMS Scores", title = "Delta SCIM Scores by AIS Grade") +
  theme_light() +  # Minimalistic plot theme
  theme(plot.title = element_text(size = 16, face = "bold"),  # Title customization
        axis.title.x = element_text(size = 14, face = "bold"),  # X-axis label customization
        axis.title.y = element_text(size = 14, face = "bold"),  # Y-axis label customization
        axis.text.x = element_text(size = 12),  # X-axis text customization
        axis.text.y = element_text(size = 12),  # Y-axis text customization
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),  # Remove grid lines
        legend.position = "none")  # Remove legend



# ------ Multiple linear regression models using delta SCIM as outcome, models are stratified for motor complete (AIS A) vs incomplete (AISB-D) ------



# Create new grouping of AIS grades: mt_complete and mt_incomplete

# Create new variable with two levels: "A" and "BCD"
emsci.filtered$mt_complete_incomplete <- ifelse(emsci.filtered$AIS.grades.baseline == "A", "A", "BCD")

# Convert to factor (optional)
emsci.filtered$mt_complete_incomplete <- factor(emsci.filtered$mt_complete_incomplete, levels = c("A", "BCD"))


# Create an empty list to store model objects
model_list <- list()


# Fit a linear regression model for each unique AIS grade level
for (grade_level in unique(emsci.filtered$mt_complete_incomplete)) {
  # Subset the data for the current grade level
  subset_data <- emsci.filtered %>% filter(emsci.filtered$mt_complete_incomplete == grade_level)
  
  # Fit a linear regression model
  model <- lm(SCIM23_TotalScore_delta ~ yeardoi+AgeAtDOI + Sex + new_nli_cat+ plegia + SCIM23_TotalScore_baseline, data = subset_data)
  
  # Store the model in the list
  model_list[[as.character(grade_level)]] <- model
  
}

# Combine model summaries using tab_model
combined_summary <- tab_model(model_list)

# Print the combined model summary
print(combined_summary)


# Create a box plot of TMS scores for each AIS_grade

my_colors <- c("#F3F2F2", "#9C1402", "#2ECC71", "#F1C40F", "#9B59B6")

ggplot(data = emsci.filtered, aes(x = age_groups_relabelled, y = SCIM23_TotalScore_delta)) +
  geom_violin(trim = FALSE) +  # Create the violin plot
  geom_boxplot(width = 0.2, fill = "white") +  
  facet_wrap(~ emsci.filtered$mt_complete_incomplete) +  # Create separate plots for each AIS_grade
  geom_violin(trim = FALSE, fill = my_colors[1]) +  # Create the violin plot
  geom_boxplot(width = 0.2, fill = "white", color = my_colors[2]) +  # Create the box plot
  labs(x = "AIS Grade", y = "TMS Scores", title = "Delta TMS Scores by AIS Grade") +
  theme_light() +  # Minimalistic plot theme
  theme(plot.title = element_text(size = 16, face = "bold"),  # Title customization
        axis.title.x = element_text(size = 14, face = "bold"),  # X-axis label customization
        axis.title.y = element_text(size = 14, face = "bold"),  # Y-axis label customization
        axis.text.x = element_text(size = 12),  # X-axis text customization
        axis.text.y = element_text(size = 12),  # Y-axis text customization
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(),  # Remove grid lines
        legend.position = "none")  # Remove legend












#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####





