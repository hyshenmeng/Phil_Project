##### Obesity Reporting #####


# Load libraries
library(tidyverse)
library(haven)
library(scales)


# Set path
setwd("D:/Temp/Project/Obesity")


# Import data from SAS
data0 <- read_sas("./data/obesity_data.sas7bdat")


# Check data types
glimpse(data0)


# Convert data types
data1 <- data0 %>%
  mutate(BMI_GROUP = factor(BMI_GROUP),
         AGE_GROUP = factor(AGE_GROUP),
         ETHNICITY = factor(ETHNICITY),
         CENSUS_REGION = factor(CENSUS_REGION, order = TRUE, 
                                levels = c("Midwest", "Northeast", "South", "West", "Unknown")),
         SEX = factor(SEX, order = TRUE, levels = c("Female", "Male", "Gender Not Specified")))

# Line graph by BMI_GROUP and YEAR
year_tb1 <- data1 %>%
  group_by(YEAR, BMI_GROUP) %>%
  summarise(N_VISITS = n_distinct(VISIT_ID2))
xtabs(N_VISITS ~ BMI_GROUP + YEAR, data = year_tb1)

year_tb1 %>%
  ggplot(aes(x = YEAR,  y = N_VISITS, color = BMI_GROUP)) + 
  geom_line() +
  geom_text(aes(label = N_VISITS), vjust = -.5) +
  labs(title = "Patient Visits by Year", y = "Number of Visits", fill = "BMI_GROUP") +
  facet_wrap(~BMI_GROUP, ncol = 2)


# Bar graph of BMI_GROUP by CENSUS_REGION
region_tb1 <- data1 %>%
  group_by(CENSUS_REGION, BMI_GROUP) %>%
  summarise(N_VISITS = n_distinct(VISIT_ID2)) %>%
  mutate(ROW_PERC = round(100 * N_VISITS / sum(N_VISITS), 2))
xtabs(N_VISITS ~ BMI_GROUP + CENSUS_REGION, data = region_tb1)

region_tb1 %>%
  filter(CENSUS_REGION != "Unknown") %>%
  ggplot(aes(x = BMI_GROUP,  y = ROW_PERC, fill = BMI_GROUP)) + 
    geom_bar(stat ="identity") +
    geom_text(aes(label = ROW_PERC), vjust = -.5) +
    labs(title = "Patient Visits by Census Division", y = "Percent (%)", fill = "BMI_GROUP") +
    facet_wrap(~CENSUS_REGION, ncol = 2)


# Bar graph of BMI_GROUP by SEX
sex_tb1 <- data1 %>%
  group_by(SEX, BMI_GROUP) %>%
  summarise(N_VISITS = n_distinct(VISIT_ID2)) %>%
  mutate(ROW_PERC = round(100 * N_VISITS / sum(N_VISITS), 2))
xtabs(N_VISITS ~ BMI_GROUP + SEX, data = sex_tb1)

sex_tb1 %>%
  filter(SEX != "Gender Not Specified") %>%
  ggplot(aes(x = BMI_GROUP,  y = ROW_PERC, fill = BMI_GROUP)) + 
    geom_bar(stat ="identity") +
    geom_text(aes(label = ROW_PERC), vjust = -.5) +
    labs(title = "Patient Visits by Gender", y = "Percent (%)", fill = "BMI_GROUP") +
    facet_wrap(~SEX, ncol = 2)


# Bar graph of BMI_GROUP by AGE
age_tb1 <- data1 %>%
  group_by(AGE_GROUP, BMI_GROUP) %>%
  summarise(N_VISITS = n_distinct(VISIT_ID2)) %>%
  mutate(ROW_PERC = round(100 * N_VISITS / sum(N_VISITS), 2))
xtabs(N_VISITS ~ BMI_GROUP + AGE_GROUP, data = age_tb1)

age_tb1 %>%
  filter(AGE_GROUP != "Age Not Specified") %>%
  ggplot(aes(x = BMI_GROUP,  y = ROW_PERC, fill = BMI_GROUP)) + 
    geom_bar(stat ="identity") +
    geom_text(aes(label = ROW_PERC), vjust = -.5) +
    labs(title = "Patient Visits by Age", y = "Percent (%)", fill = "BMI_GROUP") +
    facet_wrap(~AGE_GROUP, ncol = 2)

  
# Bar graph of BMI_GROUP by RACE
race_tb1 <- data1 %>%
  group_by(ETHNICITY, BMI_GROUP) %>%
  summarise(N_VISITS = n_distinct(VISIT_ID2)) %>%
  mutate(ROW_PERC = round(100 * N_VISITS / sum(N_VISITS), 2))
xtabs(N_VISITS ~ BMI_GROUP + ETHNICITY, data = race_tb1)

race_tb1 %>%
  filter(ETHNICITY != "Other or Unspecified") %>%
  ggplot(aes(x = BMI_GROUP,  y = ROW_PERC, fill = BMI_GROUP)) + 
  geom_bar(stat ="identity") +
  geom_text(aes(label = ROW_PERC), vjust = -.5) +
  labs(title = "Patient Visits by Race", y = "Percent (%)", fill = "BMI_GROUP") +
  facet_wrap(~ETHNICITY, ncol = 2)

