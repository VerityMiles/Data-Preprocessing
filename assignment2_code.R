library(readr)
library(mosaic)
library(tidyr)
library(dplyr)

#Import WHO dataset
WHO_data <- read_csv("WHO.csv")

#Tidy task 1
WHO_data <- WHO_data %>% gather(code, value, 5:60)

#Tidy task 2
WHO_data <- WHO_data %>%
  separate(code, c("new", "var", "sex"), sep = "_") %>%
  separate(sex, c("sex", "age"), sep = 1)

#Tidy task 3
WHO_data <- WHO_data %>%
  spread(var, value)

#Tidy task 4
WHO_data <- WHO_data %>% mutate(sex = as.factor(sex), age = as.factor(age))
#Haven't got the re-naming of age to work yet
WHO_data$age <- factor(WHO_data$age,
                       levels = c("<15", "15-24", "25-34", "35-44", "45-54", "55-64", "65>="),
                       ordered = TRUE)

#Task 5
WHO_data <- WHO_data[-c(2, 5)]

countries <- c("Australia", "Germany", "Solomon Islands")
WHO_subset <- filter(WHO_data, country %in% countries)


#Import surveys and species data
surveys <- read_csv("surveys.csv")
species <- read_csv("species.csv")

#Task 6
surveys_combined <- left_join(surveys, species[, c("species_id", "genus", "species", "taxa")], by = "species_id")

#Task 7
cotton_rat <- filter(surveys_combined, genus == "Sigmodon" & species == "fulviventer")
#Or can just use species id as unique identifier

mean_toadd <-cotton_rat %>%
  group_by(month) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE)) 

cotton_rat <- left_join(cotton_rat, mean_toadd, by = "month")

mean_toadd <-cotton_rat %>%
  group_by(month) %>%
  summarise(mean_hind = mean(hindfoot_length, na.rm = TRUE)) 

cotton_rat <- left_join(cotton_rat, mean_toadd, by = "month")

#Task 8
unique(surveys_combined$year)
surveys_combined_year <- filter(surveys_combined, year == 1987)

calculate_mean <- surveys_combined_year %>%
  group_by(species_id) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))


