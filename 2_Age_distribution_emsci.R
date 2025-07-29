## ---------------------------
##
## Script name: 2_Age_distribution_emsci.R
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
emsci.filtered<- read.csv("./Data/emsci.original.filtered.csv", sep = ',', header = T,  na.strings=c("","NA"))
names(emsci.filtered)

#### ---------------------------Age distribution over time: Data analysis ---------------------------

#---------- Calculate the mean and sd of age per year --------#
emsci.filtered.2 <- emsci.filtered %>%
  dplyr::select(yeardoi, AgeAtDOI, Sex)%>%
  dplyr::group_by(yeardoi, Sex) %>%
  dplyr::mutate(mean.age =sprintf("%0.2f", mean(AgeAtDOI)))%>%
  dplyr::mutate(sd.age =sprintf("%0.2f", sd(AgeAtDOI)))%>%
  dplyr::select(yeardoi, Sex, mean.age,sd.age)%>%
  dplyr::distinct()%>%
  dplyr::arrange(Sex, yeardoi)%>%
  as.data.frame()


emsci.filtered.acute <- subset(emsci.filtered, emsci.filtered$ExamStage=="acute I")

#---------- Calculate the change in age distribution over time - OVERALL --------#
age_model.overall <-lm(AgeAtDOI~yeardoi, data=emsci.filtered)
summary(age_model.overall)
nobs(age_model.overall)

#---------- Calculate the change in age distribution over time - OVERALL FEMALE --------#
age_model.overall.female <-lm(AgeAtDOI~yeardoi, data=subset((emsci.filtered), Sex=='f'))
summary(age_model.overall.female)
nobs(age_model.overall.female)

#---------- Calculate the change in age distribution over time - OVERALL MALE --------#
age_model.overall.male <-lm(AgeAtDOI~yeardoi, data=subset((emsci.filtered), Sex=='m'))
summary(age_model.overall.male)
nobs(age_model.overall.male)


# Create table with model summary
tab_model(
  age_model.overall, age_model.overall.female, age_model.overall.male,
  pred.labels = c("Intercept", "Year of injury", "AIS B vs AIS A", "AIS C vs AIS A", "AIS D vs AIS A", "Plegia: tetra"),
  dv.labels = c("Overall", "Female Patients", "Male Patients"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value",
  digits.p = 3
)


#---------- Visualization: Change in age distribution - OVERALL --------#
# Set theme
theme_set(theme_ridges())

# Change labels of levels 
levels(emsci.filtered$Sex) <- c("f", "m")

# Create plot
age_overall <- ggplot(
  emsci.filtered, 
  aes(y = as.factor(yeardoi) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury: Overall') +
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        #legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_overall

ggsave(
  "age.overall.pdf",
  plot = age_overall,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()





# With bins of 3 years

emsci.filtered.with_bins= emsci.filtered %>% mutate(points_bin = cut(yeardoi, breaks=c(2000, 2003, 2006, 2009, 2012,2015, 2018, 2023)))


# Create plot
age_overall.withbins <- ggplot(
  emsci.filtered.with_bins, 
  aes(y = as.factor(points_bin) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury: Overall') +
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        #legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_overall.withbins

ggsave(
  "age.overall_bins.pdf",
  plot = age_overall.withbins,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


# Create plot age_overall_stratified_by_sex
age_overall_stratified_by_sex <- ggplot(
  emsci.filtered, 
  aes(y = as.factor(yeardoi) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury: Stratified by Sex') +facet_grid(.~emsci.filtered$Sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_overall_stratified_by_sex

ggsave(
  "age_overall_stratified_by_sex.pdf",
  plot = age_overall_stratified_by_sex,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

# Create plot age_overall_stratified_by_ais_grade
age_overall_stratified_by_ais_grade <- ggplot(
  emsci.filtered, 
  aes(y = as.factor(yeardoi) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury: Stratified by Baseline AIS Grade') +facet_grid(.~emsci.filtered$AIS.grades.baseline)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_overall_stratified_by_ais_grade

ggsave(
  "age_overall_stratified_by_ais_grade.pdf",
  plot = age_overall_stratified_by_ais_grade,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

