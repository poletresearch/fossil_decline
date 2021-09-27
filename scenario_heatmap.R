#Code for the article Vinichenko V., Cherp A., Jewell J. (2021).
#Historical precedents and feasibility of rapid coal and gas decline required for 
#the 1.5°C target. One Earth.
#
#Producing a feasibility heatmap for 1.5C-compatible scenarios
#See Experimental Procedures and caption for Table S8 for details.

#Working directory should be set to the directory containing input files

#Necessary packages (should be installed)

library(dplyr)
library(tidyr)
library(stringr)

#Order of feasibility levels (from the most to the least feasible)

lev <- c(W = 1, A = 2, B = 3, D = 4, C = 5)

#Inverted vector of levels
lev.rev <- names(lev)
names(lev.rev) <- lev

#Vector for recoding region names
regions <- c("R5ASIA" = "Asia", "R5LAM" = "LAM", "R5MAF" = "MAF",
             "R5OECD90+EU" = "OECD", "R5REF" = "REF", "World" = "World")

map <- read.csv("feasibility_map.csv", stringsAsFactors = F)


r <- read.csv("scenario_rates.csv", stringsAsFactors = F) %>%
  filter(Type == "15C" , Region != "World", Fuel != "Oil") %>%
  mutate(Region = recode(Region, !!!regions),
         Overshoot = ifelse(Category == "HO15C", "HO", "NO/LO")) %>%
  select(-Type, - Category)

r.tot <- r %>% filter(Fuel == "Total") %>%
  rename(Demand.Rate = Rate) %>%
  select(- Fuel)

r1 <- r %>% filter(Fuel != "Total") %>%
  merge(r.tot) 

##Feasibility scores for each Region/Period/Fuel combination
r2 <- r1 %>% merge(map) %>%
  filter(Rate > Ymin, Rate <= Ymax, Demand.Rate > Xmin, Demand.Rate <= Xmax) %>%
  right_join(r1) %>%
  mutate(Zone = ifelse(Rate > -0.05, "W", Zone),
         Zone = ifelse(is.na(Zone), "C", Zone))

 

r3 <- r2 %>%
  mutate(Zone = ifelse(Zone == "W", "", Zone),
         Label = str_c(Fuel, ".", Region, ".", substr(Period, 3,5), substr(Period, 8,9))) %>%
  select(Model, Scenario, Overshoot, Zone, Label) %>%
  spread(Label, Zone)

write.csv(r3, "heatmap_full_test.csv", row.names = F)

##Summary feasibility scores (least feasible period for each Region/Fuel combination)
##plus the least feasible Period/Region/Fuel combination for the entire scenario

r4 <- r2 %>% mutate(Zone.N = lev[Zone]) %>%
  group_by(Model, Scenario, Overshoot, Fuel, Region) %>%
  arrange(desc(Zone.N)) %>%
  slice(1) %>%
  ungroup %>%
  mutate(Zone = lev.rev[Zone.N],
         Zone.Lab = ifelse(Zone != "W", str_c(Zone, " (", substr(Period, 3,5), substr(Period, 8,9), ")"), ""),
         Label = str_c("Max.", Fuel, ".", Region)) %>%
  select(Model, Scenario, Overshoot, Label, Zone.Lab) %>%
  spread(Label, Zone.Lab)
  
r.all <- r2 %>% mutate(Zone.N = lev[Zone]) %>%
  filter(Model != "POLES EMF33") %>% #The total score is not determined for POLAS that has obly Maf and REF
  group_by(Model, Scenario) %>%
  summarize(Zone.N = max(Zone.N)) %>%
  ungroup %>%
  mutate(Zone.All = lev.rev[Zone.N]) %>%
  select(-Zone.N) 

r5 <- r4 %>% left_join(r.all)


write.csv(r5, "heatmap_summary_test.csv", row.names = F)



  