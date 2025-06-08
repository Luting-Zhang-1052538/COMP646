
rm(list = ls())

library(DBI)
library(dbplyr)
library(duckdb)
library(readxl)
library(writexl)
library(tidyverse)

options(tibble.width=Inf)


#### Load the wage_rate.xlsx correctly

setwd("D:/Lincoln/S1 25/COMP646/exercise")
getwd()
dir.create("data")
wage_rate <- read_excel('data/wage_rate.xlsx')

# change column names and tidy data - 1
wage_rate_clean <- read_excel('data/wage_rate.xlsx')|>
  rename(Gender = Gnder,
         Experience = experience,
         WageRate = Wage_Rate)|> # change column names
  mutate(
  Experience = if_else(Experience == 'Four','4', Experience), #clean Four into 4
  Experience = as.numeric(Experience),#change Experience datatype
  Gender = if_else(Gender == 'Fmale', 'Female', Gender), 
  Gender = if_else(Gender == 'male', 'Male', Gender)
  ) 


# change column names and tidy data - 2
wage_rate_clean <- read_excel('data/wage_rate.xlsx', 
                col_names = c("Gender","Experience","WageRate"), # change column names
                skip = 1)|>
                mutate(
                  Experience = if_else(Experience == 'Four','4', Experience), #clean Four into 4
                  Experience = as.numeric(Experience)) # change Experience datatype


#### How do I compute the average wage rate by gender? Correct all necessary data issues.

wage_rate_by_gender <- wage_rate_clean |>
  mutate(
    Gender = if_else(Gender =="Fmale","Female", Gender), #tidy up gender
    Gender = if_else(Gender == "male", "Male", Gender) #tidy up gender
  )|> 
  group_by(Gender)|>
  summarise(AverageWage = mean(WageRate, na.rm = TRUE)) 


#### Compute the number of observations, average wage rate and average work experience by gender.

wage_rate_by_gender <- wage_rate_clean |>
  mutate(
    Gender = if_else(Gender =="Fmale","Female", Gender), #tidy up gender
    Gender = if_else(Gender == "male", "Male", Gender) #tidy up gender
  )|> 
  group_by(Gender)|>
  summarise(AverageWage = mean(WageRate, na.rm = TRUE), # calculate average wage
            Observations = n(), #count no of observation
            AverageWorkExperience = mean(Experience, na.rm = TRUE)) # calculate Average Work experience
            
            
 ## The data shows that males and females have similar average work experience (23.5 vs. 23.3 years), 
 ## but males earn a slightly higher average wage (57.3) compared to females (48.1).
wage_rate_by_gender
  
#### Separate the data by gender
#filter by Female
Female <- wage_rate_by_gender |>
  filter(Gender == 'Female')

#filter by Male
Male <- wage_rate_by_gender |>
  filter(Gender == 'Male')


#### How do I run a regression of wage rate on experience for each gender?

female_model <- summary(lm(WageRate ~ Experience, data = filter(wage_rate_clean, Gender == "Female")))
                        
                        
summary(female_model)

male_model <- summary(lm(WageRate ~ Experience, data = filter(wage_rate_clean, Gender == "Male")))
summary(male_model)


#### Visualize the relation between wage and experience and color the data points by gender
ggplot(wage_rate_clean, aes(x=WageRate, y=Experience, color = Gender))+
  geom_point()+
  labs(title = "Wage Rate vs Experience by Gender", 
       x = "Wage_Rate", 
       y = "Experience")

#### Add a regression line for males and females
ggplot(wage_rate_clean, aes(x = WageRate, y = Experience, color = Gender)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(
    title = "Wage Rate vs Experience by Gender", 
    x = "Wage Rate", 
    y = "Experience"
  )

#### Connect to a DuckDB database and name it 'WageRateDB'?

WageRateDB<-DBI::dbConnect(duckdb::duckdb(),abdir="WageRateDB")

#### Load the wage rate dataset of 1990, 2000, 2010, and 2020

dbWriteTable(WageRateDB, "WageRate1990", read_excel("data/wage_rate_1990.xlsx"))
dbWriteTable(WageRateDB, "WageRate2000", read_excel("data/wage_rate_2000.xlsx"))
dbWriteTable(WageRateDB, "WageRate2010", read_excel("data/wage_rate_2010.xlsx"))
dbWriteTable(WageRateDB, "WageRate2020", read_excel("data/wage_rate_2020.xlsx"))
        
####  Show which tables are stored in the WageRateDB database
dbListTables(WageRateDB)

#### Calculate the annual income by multiplying the wage rate by 40 working hours per week and 52 weeks per year
# assign each excel into a tibble

WR1990 <- tbl(WageRateDB, "WageRate1990")
WR2000 <- tbl(WageRateDB, "WageRate2000")
WR2010 <- tbl(WageRateDB, "WageRate2010")
WR2020 <- tbl(WageRateDB, "WageRate2020")

# calculate each dataframe seperately
WR1990_AI <- WR1990|>
  mutate(AnnualIncome = WageRate*40*52)
WR2000_AI <- WR2000|>
  mutate(AnnualIncome = WageRate*40*52)
WR2010_AI <- WR2010|>
  mutate(AnnualIncome = WageRate*40*52)
WR2020_AI <- WR2020|>
  mutate(AnnualIncome = WageRate*40*52)

#### Create the annual income using an SQL query
WR1990_AI |> show_query()
#<SQL>
  #SELECT WageRate1990.*, (WageRate * 40.0) * 52.0 AS AnnualIncome
#FROM WageRate1990
#> 
WR2000_AI |> show_query() 
#<SQL>
  #SELECT WageRate2000.*, (WageRate * 40.0) * 52.0 AS AnnualIncome
#FROM WageRate2000
#> 
WR2010_AI |> show_query() 
#<SQL>
  #SELECT WageRate2010.*, (WageRate * 40.0) * 52.0 AS AnnualIncome
#FROM WageRate2010
#> 
  
WR2020_AI |> show_query() 
#<SQL>
  #SELECT WageRate2020.*, (WageRate * 40.0) * 52.0 AS AnnualIncome
#FROM WageRate2020
#> 

#### Calculate the number of observations, average income, and average experience by gender of individuals with more than 10 years of work experience
WR1990|>
  filter(Experience >10)|>
  group_by(Gender)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )

WR2000|>
  filter(Experience >10)|>
  group_by(Gender)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )
WR2010|>
  filter(Experience >10)|>
  group_by(Gender)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )
WR2020|>
  filter(Experience >10)|>
  group_by(Gender)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )
  
#### Compute the average income by experience level.

WR1990|>
  group_by(Experience)|>
  summarise(
    AverageIncome = mean(WageRate),
  )|>
  arrange(Experience)

WR2000|>
  group_by(Experience)|>
  summarise(
    AverageIncome = mean(WageRate),
  )|>
  arrange(Experience)

WR2010|>
  group_by(Experience)|>
  summarise(
    AverageIncome = mean(WageRate),
  )|>
  arrange(Experience)

WR2020|>
  group_by(Experience)|>
  summarise(
    AverageIncome = mean(WageRate),
  )|>
  arrange(Experience)

#### Merge data from multiple years (1990, 2000, 2010, and 2020) into one tibble and add a 'Year' column.
# must use dataframe, not tibble
year1990 <- read_excel("data/wage_rate_1990.xlsx") |>mutate(Year = "1990")
year2000 <- read_excel("data/wage_rate_2000.xlsx") |>mutate(Year = "2000")
year2010 <- read_excel("data/wage_rate_2010.xlsx") |>mutate(Year = "2010")
year2020 <- read_excel("data/wage_rate_2020.xlsx") |>mutate(Year = "2020")
Merge_WR <- bind_rows(year1990, year2000, year2010, year2020) 

#### Calculate the number of observations, average income, and average experience by gender and year
Merge_WR |>
  group_by(Gender,Year)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )


#### Repeat the previous step but sort by gender
Merge_WR |>
  group_by(Gender,Year)|>
  summarise(
    AverageIncome = mean(WageRate),
    AverageExperience = mean(Experience),
    Observation = n()
  )|>
  arrange(Gender)


#### Disconnect the database
DBI::dbDisconnect(WageRateDB)