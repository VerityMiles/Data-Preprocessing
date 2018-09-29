#Importing packages
library(readr)
#library(xlsx)
library(readxl)
#library(foreign)
#library(gdata)
#library(rvest)
library(dplyr)
library(tidyr)
#library(deductive)
#library(validate)
#library(Hmisc)
#library(forecast)
#library(stringr)
#library(lubridate)
#library(outliers)
#library(MVN)
#library(infotheo)
#library(MASS)
#library(caret)
#library(ggplot2)
#library(knitr)

#Bringing in census data
education <- read_excel("allVic_education.xls", skip = 8, col_names = TRUE)
education <- education[-1,-1] #Removing first row and column
colnames(education)[1] <- "SA1"

totalrow <- which(education$SA1 == 'Total')
education <- education[1:totalrow-1,] #Removing empty rows at end of data frame

#Tidyr-ing
edu_tidy <- gather(education, "Ed_level", "People", 2:length(education))
#View(edu_tidy)
edu_tidy <- edu_tidy %>% mutate(Ed_level = factor(Ed_level, levels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                                                       "Bachelor Degree Level", "Advanced Diploma and Diploma Level", "Certificate III & IV Level",
                                                                       "Secondary Education - Years 10 and above", "Certificate I & II Level", "Secondary Education - Years 9 and below",
                                                                       "Supplementary Codes", "Not stated", "Not applicable", "Total"),
                                                  labels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                                             "Bachelor Degree Level", "Advanced Diploma and Diploma Level", "Certificate III & IV Level",
                                                             "Secondary Education - Years 10 and above", "Certificate I & II Level", "Secondary Education - Years 9 and below",
                                                             "Supplementary Codes", "Not stated", "Not applicable", "Total"),
                                                  ordered = TRUE))
edu_tidy <- transform(edu_tidy, SA1 = as.numeric(SA1))


#Bringing in lookup data
geo_lookup <- read_csv("SA1_LGA_LookUp.csv")
View(geo_lookup)

#Adding LGA column to education data
edu_tidy_join <- edu_tidy %>% left_join(geo_lookup[,c("SA1_7DIG16", "LGA_NAME17")], by= c("SA1" = "SA1_7DIG16"))
View(edu_tidy_join)
