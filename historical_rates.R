#Code for the article Vinichenko V., Cherp A., Jewell J. (2021).
#Historical precedents and feasibility of rapid coal and gas decline required for 
#the 1.5°C target. One Earth.
#
#Calculation of historical fossil fuel decline rates
#
#The code expects electricity generation data (csv) in the following format:
#Country (string) - country
#Year (integer) - year
#Fuel (string) - fuel, source of electricity generation* 
#Value (number) - electricity generation from the given source in the given year
#*Note: "Total" in the Fuel column means total electricity supply
#(electricity production plus net imports) necessary for normalizing decline rates
#For our paper we use World Energy Balances data available from IEA.
#With this code, we release a dataset with two fictitious countries for code testing purposes.

#Working directory should be set to the directory containing input files

#Necessary packages (should be installed)
library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(roll)

g <- read.csv("electricity_balances.csv", stringsAsFactors = F)

#Period for which change rates are calculated
n <- 10 

#System size (total electricity supply) used for normalization: 
#"mean" - mean between the beginning and the end of the period (used in the main analysis)
#"start" <-  at the beginning of the period
#"end" <- at the end of the period

method <- "mean"

###3-year moving average to smooth the data
g1 <- g %>% group_by(Country, Fuel) %>%
  arrange(Year) %>%
  mutate(Value = rollmean(Value, 3, na.pad = T, align = "center")) %>%
  filter(!(is.na(Value))) %>%
  ungroup


g.tot <- g1 %>% filter(Fuel == "Total") %>%
  select(Country, Year, Total = Value)

g2 <- g1 %>% inner_join(g.tot)

#Dataset lagged by n years 
g.lag <- g2 %>% mutate(Year = Year + n) %>%
  rename(Total0 = Total, Value0 = Value)

#Combining the datasets

g3 <- g2 %>% inner_join(g.lag)

#Defining system size for normalizaion
if (method == "mean") {
  g4 <- g3 %>% mutate(Size = (Total + Total0) / 2)
} else if (method == "start") {
  g4 <- g3 %>% mutate(Size = Total0)
} else if (method == "end") {
  g4 <- g3 %>% mutate(Size = Total)
}
  
#Calculating change rates
#Note: change rates for Total = normalized rates for total demand change
g5 <- g4 %>% mutate(Rate = (Value - Value0) / Size,
                    Demand.Rate = (Total - Total0) / Size, #Normalized demand change, used in building the feasibility space
                    Period = str_c(Year - n, "-", Year),
                    Episode = str_c(Country, substr(Period, 3,5), substr(Period, 8, 9)))

#Will need for substitution analysis
#Change in total supply/demand is inverted, since demand decline
#"substitutes" decline in energy sources like increase in other sources
g.subst <- g5 %>% select(Episode, Subst = Fuel, Subst.Rate = Rate) %>%
  mutate(Subst.Rate = ifelse(Subst == "Total", - Subst.Rate, Subst.Rate),
         Subst = ifelse(Subst == "Total", "Demand", Subst)) %>%
  filter(Subst.Rate > 0)

#Identify maximum value within n years after the end of the period
#If it's greater than the value at the beginning of he period,
#remove the episode (full rebound)

g6 <- g5 %>% group_by(Country, Fuel) %>%
  arrange(desc(Year)) %>%
  mutate(Max.After = roll_max(Value, n + 1, min_obs = 1)) %>%
  arrange(Year) %>%
  ungroup %>%
  filter(Max.After < Value0) 
  
  
#Selecting non-overlapping episodes with the highest decline rates 
#for each country/fuel combination
#Rank is according to the decline rate
result <- data.frame()
for (cnt in unique(g$Country)) {
  for (f in c("Oil", "Coal", "Gas")) {
    gt <- g6 %>% filter(Country == cnt, Fuel == f, Rate < 0) #Leaving only cases of decline
    rank <- 1
    while (nrow(gt) > 0) {
      gt1 <- gt %>% filter(Rate == min(Rate))
      gt2 <- gt1[1,] %>% mutate(Rank = rank)
      yr <- gt2$Year
      
      gt <- gt %>% filter(Year <= yr - n | Year >= yr + n) #Removing overlapping periods
      rank <- rank + 1
      result <- result %>% rbind(gt2)
    }
  }
}

#Adding primary substituting source

result1 <- result %>% select(Episode, Country, Period, Fuel, Rate, Demand.Rate, Size) %>%
  inner_join(g.subst) %>%
  group_by(Episode, Fuel) %>%
  slice_max(Subst.Rate, with_ties = F) %>%
  ungroup %>%
  rename(Primary = Subst, Primary.Change = Subst.Rate) %>%
  mutate(Episode = str_c(Episode, substr(Fuel, 1,1)))

write.csv(result1, "historical_rates_test.csv", row.names = F)

#'Significant' episodes: "ten-year decline rates faster than -5% and
# average annual total electricity supply >100 TWh/year "
result2 <- result1 %>% filter(Rate < -0.05, Size > 1e5) %>%
  arrange(Rate)

write.csv(result2, "significant_episodes_test.csv", row.names = F)

  
