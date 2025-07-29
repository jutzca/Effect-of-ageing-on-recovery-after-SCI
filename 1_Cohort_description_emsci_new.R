## ---------------------------
##
## Script name: 1_Cohort_Description_emsci_new
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
library(table1)
library(dplyr)
library(plyr)
library(MatchIt) 
library("labelled")
library(tidyverse)
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
outdir_tables='/Documents/Github/Ageing-in-Spinal-Cord-Injury/Tables'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original data set
emsci.original<- read.csv("./Data/emsci_data_2023_extended.csv", sep = ',', header = T,  na.strings=c("","NA"))
names(emsci.original)

# Create 'AIS.baseline' variable based on AIS grades at very acute stage. In case that values is not available then acute I.
emsci.original<- emsci.original %>%
  arrange(Patientennummer, ExamStage) %>%
  dplyr::group_by(Patientennummer) %>%
  dplyr::mutate(AIS.grades.baseline = ifelse("acute I" %in% ExamStage, 
                                    first(AIS[ExamStage == "acute I"]),
                                    ifelse("very acute" %in% ExamStage, 
                                           first(AIS[ExamStage == "very acute"]), 
                                           NA)))

# Create a new variable 'new_nli': '1"="C1-C4", "2"="C5-C8", "3" = "T1-T6", "4"="T7-T12", "5" = "L-S"
emsci.original <- emsci.original %>%
  mutate(new_nli = case_when(
    NLI %in% c("C1", "C2", "C3", "C4") ~ "1",
    NLI %in% c("C5", "C6", "C7", "C8") ~ "2",
    NLI %in% c("T1", "T2", "T3", "T4", "T5", "T6") ~ "3",
    NLI %in% c("T7", "T8", "T9", "T10", "T11", "T12") ~ "4",
    NLI %in% c("L1", "L2", "L3", "L4", "L5", "S1", "S2", "S3") ~ "5",
    NLI %in% c("NA", "NT", "") ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Create a new variable 'new_nli_cat': '1"="C1-C4", "2"="C5-C8", "3" = "T1-T6", "4"="T7-T12", "5" = "L-S"
emsci.original <- emsci.original %>%
  mutate(new_nli_cat = case_when(
    NLI %in% c("C1", "C2", "C3", "C4") ~ "C1-C4",
    NLI %in% c("C5", "C6", "C7", "C8") ~ "C5-C8",
    NLI %in% c("T1", "T2", "T3", "T4", "T5", "T6") ~ "T1-T6",
    NLI %in% c("T7", "T8", "T9", "T10", "T11", "T12") ~ "T7-12",
    NLI %in% c("L1", "L2", "L3", "L4", "L5", "S1", "S2", "S3") ~ "L1-S3",
    NLI %in% c("NA", "NT", "") ~ NA_character_,
    TRUE ~ NA_character_
  ))



# Create a new variable 'plegia': 'tetra"="C1-T1", "para"="T2-LS", 
emsci.original <- emsci.original %>%
  mutate(plegia = case_when(
    NLI %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "T1") ~ "tetra",
    NLI %in% c("T1", "T2", "T3", "T4", "T5", "T6","T7", "T8", "T9", "T10", "T11", "T12", "L1", "L2", "L3", "L4", "L5", "S1", "S2", "S3") ~ "para",
    NLI %in% c("NA", "NT", "") ~ NA_character_,
    TRUE ~ NA_character_
  ))




# Create 'MS_tot_baseline' variable based on MS_TOT grades at very acute stage. In case that values is not available then acute I.
emsci.original<- emsci.original %>%
  arrange(Patientennummer, ExamStage) %>%
  dplyr::group_by(Patientennummer) %>%
  dplyr::mutate(MS_tot_baseline = ifelse("acute I" %in% ExamStage, 
                                             first(MS_TOT[ExamStage == "acute I"]),
                                             ifelse("very acute" %in% ExamStage, 
                                                    first(MS_TOT[ExamStage == "very acute"]), 
                                                    NA))) %>% 
                                              mutate(MS_tot_baseline = as.numeric(MS_tot_baseline))
  


# Create 'SCIM23_TotalScore_baseline' variable based on SCIM23_TotalScore grades at very acute stage. In case that values is not available then acute I.
emsci.original<- emsci.original %>%
  arrange(Patientennummer, ExamStage) %>%
  dplyr::group_by(Patientennummer) %>%
  dplyr::mutate(SCIM23_TotalScore_baseline = ifelse("acute I" %in% ExamStage, 
                                         first(SCIM23_TotalScore[ExamStage == "acute I"]),
                                         ifelse("very acute" %in% ExamStage, 
                                                first(SCIM23_TotalScore[ExamStage == "very acute"]), 
                                                NA))) %>%
                                          mutate(SCIM23_TotalScore = as.numeric(SCIM23_TotalScore))


# Calculate MS_TOT_delta
emsci.original <- emsci.original %>%
  group_by(Patientennummer) %>%
  dplyr::mutate(MS_TOT = as.numeric(MS_TOT))%>%
  dplyr:: mutate(MS_TOT_delta = ifelse(
    "chronic" %in% ExamStage & !is.na(MS_TOT[ExamStage == "chronic"]),
    ifelse("acute I" %in% ExamStage, 
           MS_TOT[ExamStage == "chronic"] - MS_TOT[ExamStage == "acute I"],
           ifelse("very acute" %in% ExamStage,
                  MS_TOT[ExamStage == "chronic"] - ExamStage[ExamStage == "very acute"],
                  NA_real_
           )
    ),
    NA_real_
  )) %>%
  ungroup()


emsci.original <- emsci.original %>%
  dplyr::group_by(Patientennummer) %>%
  dplyr::mutate(MS_TOT_delta = first(MS_TOT_delta)) %>%
  ungroup()


# Calculate SCIM23_TotalScore_delta
emsci.original <- emsci.original %>%
  group_by(Patientennummer) %>%
  dplyr::mutate(SCIM23_TotalScore = as.numeric(SCIM23_TotalScore))%>%
  dplyr:: mutate(SCIM23_TotalScore_delta = ifelse(
    "chronic" %in% ExamStage & !is.na(SCIM23_TotalScore[ExamStage == "chronic"]),
    ifelse("acute I" %in% ExamStage, 
           SCIM23_TotalScore[ExamStage == "chronic"] - SCIM23_TotalScore[ExamStage == "acute I"],
           ifelse("very acute" %in% ExamStage,
                  SCIM23_TotalScore[ExamStage == "chronic"] - ExamStage[ExamStage == "very acute"],
                  NA_real_
           )
    ),
    NA_real_
  )) %>%
  ungroup()


emsci.original <- emsci.original %>%
  dplyr::group_by(Patientennummer) %>%
  dplyr::mutate(SCIM23_TotalScore_delta = first(SCIM23_TotalScore_delta)) %>%
  
  ungroup()


# Remove NTs and AIS E from the variable AIS
emsci.original.filtered <- emsci.original %>% 
                          filter(!(AIS.grades.baseline %in% c("E", "NT"))) %>% # remove NTs and AIS E from the variable AIS
                          filter(!is.na(AIS.grades.baseline))   %>% 
                          filter(!is.na(AgeAtDOI)) %>% # remove NAs in Age at Injury
                          filter(!is.na(Sex)) %>%
                          filter(!is.na(new_nli)) %>%
                          filter(!is.na(MS_TOT_delta)) %>%
  filter(!is.na(SCIM23_TotalScore_delta))%>%
                          ungroup()

# Create age groups
labs <- c(paste(seq(0, 99, by = 50), seq(0 + 50 - 1, 100 - 1, by = 50),
                sep = "-"))
labs

# Add new variable AgeGroup to the main dataframe
emsci.original.filtered$AgeGroup <- cut(emsci.original.filtered$AgeAtDOI, breaks = c(seq(0, 99, by = 50), Inf), labels = labs, right = FALSE)

emsci.original.filtered$age_groups_relabelled<-plyr::revalue(as.factor(emsci.original.filtered$AgeGroup), c("0-49"="Age <50 years", "50-99"="Age ≤ 50 years"))


write.csv(emsci.original.filtered, "data/emsci.original.filtered.csv")



####------------------- Cohort Description -------------------####


# Create subset of data with just one timepoint
emsci.original.baseline <- distinct(subset(emsci.original.filtered, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

##### Create Summary Table ##### 

# 1. Formatting of table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(emsci.original.baseline$Sex) <- c("Female", "Male")
levels(emsci.original.baseline$AIS.grades.baseline) <- c("A", "B", "C", "D")
new_nli_relabelled<-plyr::revalue(as.factor(emsci.original.baseline$new_nli), c("1"="C1-C4", "2"="C5-C8", "3" = "T1-T6", "4"="T7-T12", "5" = "L-S"))
sex_relabelled<-plyr::revalue(as.factor(emsci.original.baseline$Sex), c("m"="Male", "f"="Female"))


# Relabel variables
label(sex_relabelled) <- "Sex"
label(emsci.original.baseline$AgeAtDOI) <- "Age at injury"
label(new_nli_relabelled)<- "Neurological level of injury"
label(emsci.original.baseline$AIS.grades.baseline) <- "AIS Score"
label(emsci.original.baseline$MS_tot_baseline) <- "Baseline total motor score"
label(emsci.original.baseline$SCIM23_TotalScore_baseline) <- "Baseline SCIM23 total score"
label(emsci.original.baseline$MS_TOT_delta) <- "Delta total motor score"
label(emsci.original.baseline$SCIM23_TotalScore_delta) <- "Delta SCIM23 total score"

# Assign units to Age at Injury and Year of Injury
units(emsci.original.baseline$AgeAtDOI) <- "years"

# 2. Create table
table1::table1(~ AgeAtDOI+sex_relabelled+AIS.grades.baseline+new_nli_relabelled+MS_tot_baseline+MS_TOT_delta+SCIM23_TotalScore_baseline+SCIM23_TotalScore_delta, data = emsci.original.baseline,
               render.continuous=c(.="Mean (SD)", .="Median [Q1-Q3]"))


# # 3. Create table for age groups
# 
# # Create age groups
# labs <- c(paste(seq(0, 94, by = 50), seq(0 + 50 - 1, 100 - 1, by = 40),
#                 sep = "-"))
# labs
# 
# # Add new variable AgeGroup to the main dataframe
# emsci.original.baseline$AgeGroup <- cut(emsci.original.baseline$AgeAtDOI, breaks = c(seq(0, 89, by = 50), Inf), labels = labs, right = FALSE)
# 
# age_groups_relabelled<-plyr::revalue(as.factor(emsci.original.baseline$AgeGroup), c("0-49"="Age <50 years", "50-89"="Age ≤ 50 years"))


# Function to compute the p-value for continuous or categorical variables
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


# Create Table by age groups and with p-values
table1::table1(~ AgeAtDOI+sex_relabelled+AIS.grades.baseline+new_nli_relabelled+MS_tot_baseline+ MS_TOT_delta+SCIM23_TotalScore_baseline+SCIM23_TotalScore_delta|age_groups_relabelled, data = emsci.original.baseline,
               render.continuous=c(.="Mean (SD)", .="Median [Q1-Q3]"),
               overall=F,  extra.col=list('P-value'=pvalue))


######------Creation of table: Excluded cohort------

emsci.missing<-anti_join(emsci.original,emsci.original.baseline, by ="Patientennummer")

emsci.missing.unique<-distinct(emsci.missing, Patientennummer, .keep_all = TRUE)

# 1. Formatting of table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(emsci.missing.unique$Sex) <- c("Female", "Male")
levels(emsci.missing.unique$AIS.grades.baseline) <- c("A", "B", "C", "D")
new_nli_relabelled<-plyr::revalue(as.factor(emsci.missing.unique$new_nli), c("1"="C1-C4", "2"="C5-C8", "3" = "T1-T6", "4"="T7-T12", "5" = "L-S"))
sex_relabelled<-plyr::revalue(as.factor(emsci.missing.unique$Sex), c("m"="Male", "f"="Female"))


# Relabel variables
label(sex_relabelled) <- "Sex"
label(emsci.missing.unique$AgeAtDOI) <- "Age at injury"
label(new_nli_relabelled)<- "Neurological level of injury"
label(emsci.missing.unique$AIS.grades.baseline) <- "AIS Score"
label(emsci.missing.unique$MS_tot_baseline) <- "Baseline total motor score"
label(emsci.missing.unique$SCIM23_TotalScore_baseline) <- "Baseline SCIM23 total score"
label(emsci.missing.unique$MS_TOT_delta) <- "Delta total motor score"
label(emsci.missing.unique$SCIM23_TotalScore_delta) <- "Delta SCIM23 total score"

# Assign units to Age at Injury and Year of Injury
units(emsci.missing.unique$AgeAtDOI) <- "years"

# 2. Create table
table1::table1(~ AgeAtDOI+sex_relabelled+AIS.grades.baseline+new_nli_relabelled+MS_tot_baseline+MS_TOT_delta+SCIM23_TotalScore_baseline+SCIM23_TotalScore_delta, data = emsci.missing.unique,
               render.continuous=c(.="Mean (SD)", .="Median [Q1-Q3]"))

#------Comparison between included and excluded EMSCI cohorts------

#sex
prop.test(x=c(559,1943), n=c(980, 3105),
          conf.level=0.95)


prop.test(x=c(882, 252, 380, 728), n=c(1040,321, 579, 1107), 
          conf.level=0.95)


#age
emsci.missing.unique$status <- 'excluded cohort'
emsci.original.baseline$status <- 'included cohort'
merged_data<-dplyr::bind_rows(emsci.missing.unique,emsci.original.baseline)

#Welch Two Sample t-test
t.test(AgeAtDOI ~ status, data = merged_data)

#Here's a quick visualization of the difference:
ggplot(merged_data, aes(x=AgeAtDOI, fill = status)) + 
  geom_histogram(alpha = .5, bins = 20, position = "identity") + 
  theme_classic()


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####




