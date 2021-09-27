#Code for the article Vinichenko V., Cherp A., Jewell J. (2021).
#Historical precedents and feasibility of rapid coal and gas decline required for 
#the 1.5°C target. One Earth.
#
#Calculating fossil fuel decline rates for individual scenarios
#and summary statistics using IPCC scenario data

#Working directory should be set to the directory containing input files

#Necessary packages (should be installed)

library(dplyr)
library(tidyr)
library(stringr)

#The code uses data from files (converted to csv)
# iamc15_scenario_data_all_regions_r2.0.xlsx
# sr15_metadata_indicators_r2.0.xlsx
# which can be freely downloaded from
# https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/
# Exact data import statements may depend on your version of csv
# (e.g. using read.csv, or read.table) 

#Method of calculating system size (total electricity supply) used for normalization: 
#"mean" - mean between the beginning and the end of the period (used in the main analysis)
#"start" <-  at the beginning of the period
#"end" <- at the end of the period

method <- "mean"

#For category recoding 
cats <- c("1.5C low overshoot" = "LO15C", "1.5C high overshoot" = "HO15C", "Below 1.5C" = "B15C",
          "Higher 2C" = "H2C", "Lower 2C" = "L2C")

r <- read.table("iamc15_scenario_data_all_regions_r2.0.csv", 
                  header = T, sep = ";", stringsAsFactors = F)

#Metadata (scenario category)
meta <- read.table("sr15_metadata_indicators_r2.0.csv", 
                    header = T, sep = ";", stringsAsFactors = F) %>%
  mutate(Type = ifelse(category %in% c("Higher 2C", "Lower 2C"), "2C", NA),
         Type = ifelse(category %in% c("1.5C low overshoot", "1.5C high overshoot", "Below 1.5C" ), "15C", Type),
         Category = recode(category, !!!cats)) %>%
  filter(!is.na(Type)) %>%
  select(Model = model, Scenario = scenario, Category, Type)
  
r1 <- r %>% filter(Variable %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Oil",
                                   "Secondary Energy|Electricity|Gas", "Secondary Energy|Electricity|Coal")) %>%
  select(Model, Scenario, Region, Variable, X2020, X2030, X2040, X2050) %>%
  gather(Year, Value, 5:8) %>%
  mutate(Fuel = recode(Variable, "Secondary Energy|Electricity" = "Total", 
                       "Secondary Energy|Electricity|Oil" = "Oil",
                        "Secondary Energy|Electricity|Gas" = "Gas",
                       "Secondary Energy|Electricity|Coal" = "Coal"), #Total <- total electricity supply
         Year = as.integer(substr(Year, 2,5)),
         Value = Value * 277.8 #Convert EJ to TWh
         ) %>%
  select(-Variable) %>%
  filter(Region != "R5ROWO") %>% #The "Rest of the world" region is represented inconsistently in 
                                 #different model, so we don't analyze it
  inner_join(meta)

r.tot <- r1 %>% filter(Fuel == "Total") %>%
  select(Model, Scenario, Region, Year, Total = Value)

r2 <- r1 %>% inner_join(r.tot)

#Lagged dataset
r.lag <- r2 %>% mutate(Year = Year + 10) %>%
  select(Model, Scenario, Region, Fuel, Year, Total0 = Total, Value0 = Value)

r3 <- r2 %>% inner_join(r.lag)

#Defining system size for normalizaion
if (method == "mean") {
  r4 <- r3 %>% mutate(Size = (Total + Total0) / 2)
} else if (method == "start") {
  r4 <- r3 %>% mutate(Size = Total0)
} else if (method == "end") {
  r4 <- r3 %>% mutate(Size = Total)
}

r5 <- r4 %>% mutate(Rate = (Value - Value0)/Size,
                    Period = str_c(Year - 10, "-", Year))

##hange rates for all Model/Scenario/Region/Period/Fuel
#Output: Model, Scenario, Region, Fuel, Rate (10-year change rate),
#Size (system size or total electricity supply used in calculating rate),
#Category – subtype (e.g. "LO15C" – low overshoot 1.5C compatible) 
#Type (scenario type - 1.5C compatble or 2C compatible) 
r6 <- r5 %>% select(Model, Scenario, Region, Fuel, Period, Rate, Size, Category, Type)

write.csv(r6, "scenario_rates_test.csv", row.names = F)

#Summary statistics for scenario groups (defined by Period, Fuel, and Type)
#Output: Model, Scenario, Region, Type (scenario type - 1.5C compatble or 2C compatible),
#Min - minimum rate, Q1 - 1st quartlie, Med - median rate, 
#Q3 - 3rd quartlie, Max - maximum rate
r.summary <- r6 %>% group_by(Region, Period, Fuel, Type) %>%
  summarize(Min = min(Rate), Q1 = quantile(Rate)[2], Med = median(Rate),
            Q3 = quantile(Rate)[4], Max = max(Rate)) %>%
  ungroup

write.csv(r.summary, "scenario_summary_test.csv", row.names = F)
