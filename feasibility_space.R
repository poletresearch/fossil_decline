#Code for the article Vinichenko V., Cherp A., Jewell J. (2021).
#Historical precedents and feasibility of rapid coal and gas decline required for 
#the 1.5°C target. One Earth.
#
#Producing a feasibility space for fossil fuel decline based 
#on significant historical episodes

#Working directory should be set to the directory containing input files

#Necessary packages (should be installed)

library(dplyr)
library(tidyr)
library(stringr)

r <- read.csv("significant_episodes.csv", stringsAsFactors = F)


#Adjustment for the UK and Indonesia (see Experimental Procedures)
r1 <- r %>% mutate(Demand.Rate = ifelse(Episode == "ID07-17O", Demand.Rate - 0.002, Demand.Rate),
                   Rate = ifelse(Episode == "GB07-17C", -0.299, Rate))

result <- data.frame()

#Assigning episodes to rectangles
r2 <- r1 %>% mutate(Xmin = floor(Demand.Rate/0.15) * 0.15,
                    Ymin = floor(Rate/0.05) * 0.05,
                    LogSize = log10(Size) - 1)

#Calculating scores for rectangles:
#Xmin, Ymin - coordinates of the bottom left corner of rectangle
#Xmax, Ymax - coordinates of the bottom left corner of rectangle
#N - number of episodes
#NW - weighted score (number of episodes weigthed by log system size))

r3 <- r2 %>% group_by(Xmin, Ymin) %>%
  summarize(N = n(),
            NW = sum(LogSize)) %>%
  ungroup %>%
  mutate(Xmax = Xmin + 0.15, Ymax = Ymin + 0.05) %>%
  select(Xmin, Ymin, Xmax, Ymax, N, NW)

r4 <- r3 %>% mutate(Xmin = round(Xmin / 0.15), Ymin = round(Ymin / 0.05)) %>%
  select(- Xmax, -Ymax)

#Producing augmented score NWA (see experimental procedures)
xs <- seq(-3, 4, 1)
ys = seq(-6, -2, 1)

result <- data.frame()
for (x in xs) {
  for (y in ys) {
    rt <- r4 %>% filter(Xmin >= x, Ymin <= y)
    rz <- r4 %>% filter(Xmin == x, Ymin == y)
    if (nrow(rt > 0)) {
      if (nrow(rz > 0)) {
        res <- data.frame(Xmin = x, Ymin = y, N = rz$N[1], NW = rz$NW[1], 
                          NWA = sum(rt$NW))
      } else {
        res <- data.frame(Xmin = x, Ymin = y, N = 0, NW = 0, 
                          NWA = sum(rt$NW))
      }
    } else {
      res <- data.frame(Xmin = x, Ymin = y, N = 0, NW = 0, 
                        NWA = 0)
      
    }
    result <- result %>% rbind(res)
  }
}

r5 <- result %>% mutate(Xmin = Xmin * 0.15, Ymin = Ymin * 0.05, Xmax = Xmin + 0.15, Ymax = Ymin + 0.05)

#Assigning zone codes (see Experimental Procedures)

r6 <- result %>% mutate(Xmin = Xmin * 0.15, Ymin = Ymin * 0.05, Xmax = Xmin + 0.15, Ymax = Ymin + 0.05) %>%
   mutate(Zone = ifelse(NWA > 10, "A", "C"),
                        Zone = ifelse(NWA > 0 & NWA <= 10, "B", Zone),
                        Zone = ifelse(Xmin < -0.15, "D", Zone)) %>%  #Zone D - crisis-driven decline (demand decline > 15%) 
  select(Xmin, Ymin, Xmax, Ymax, N, NW, NWA, Zone)

###Output:
#Xmin, Ymin - coordinates of the bottom left corner of rectangle
#Xmax, Ymax - coordinates of the bottom left corner of rectangle
#N - number of episodes
#NW - weighted score (number of episodes weigthed by log system size))
#NWA - augmented feasibility score (see Experimental Procedure)
#Zone - feasibility zone code (see Experimental Procedure)

write.csv(r6, "feasibility_map_test.csv", row.names = F)
