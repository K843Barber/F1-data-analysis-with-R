#---- Load in following packages ----
library(tidyverse)
library(swirl)

#---- Load in following files ----
circuits <- read.csv("circuits.csv", T, ",")
constructorResults <- read.csv("constructorResults.csv", T, ",")
constructors <- read.csv("constructors.csv", T, ",")
constructorStandings <- read.csv("constructorStandings.csv", T, ",")
drivers <- read.csv("drivers.csv", T, ",")
driverStandings <- read.csv("driverStandings.csv", T, ",")
lapTimes <- read.csv("lapTimes.csv", T, ",")
pitStops <- read.csv("pitStops.csv", T, ",")
qualifying <- read.csv("qualifying.csv", T, ",")
races <- read.csv("races.csv", T, ",")
results <- read.csv("results.csv", T, ",")
seasons <- read.csv("seasons.csv", T, ",")
status <- read.csv("status.csv", T, ",")

#---- Points in a season bar graph: 2007 - 2011 ----

results1 <- left_join(races, results, by = "raceId")
results2 <- left_join(results1, drivers, by = "driverId")

# 2007
results2 %>%
    group_by(driverId) %>%
    filter(year == 2007) %>%
    mutate(Overall = cumsum(points)) %>%
    filter(round == 17) %>%
  ggplot(aes(reorder(forename, Overall), Overall, fill = Overall)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  labs(x = "driver", y = "Season points", title = "2007 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2)

#2008
results2 %>%
  group_by(driverId) %>%
  filter(year == 2008) %>%
  mutate(Overall = cumsum(points)) %>%
  filter(round == 17) %>%
  ggplot(aes(reorder(forename, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season Points", title = "2008 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2)

#2009
results2 %>%
    group_by(driverId) %>%
    filter(year == 2009) %>%
    mutate(Overall = cumsum(points)) %>%
    filter(round == 17) %>%
    ggplot(aes(reorder(forename, Overall), Overall, fill = Overall)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90), 
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", y = "Season points", title = "2009 WDC") +
    geom_text(aes(label = Overall), vjust = -0.2)

#2010
results2 %>%
  group_by(driverId) %>%
  filter(year == 2010) %>%
  mutate(Overall = cumsum(points)) %>%
  filter(round == 19) %>%
  ggplot(aes(reorder(surname, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season points", title = "2010 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2)

#2011
results2 %>% 
  group_by(driverId) %>% 
  filter(year == 2011) %>% 
  mutate(Overall = cumsum(points)) %>% 
  filter(round == 19) %>% 
  ggplot(aes(reorder(surname, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season points", title = "2011 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2)

#---- Wins in a season bar graph: 2003 - 2007 ----

# 2003
results2 %>%
  filter(year == 2003, position == 1) %>%
  group_by(driverId, driverRef) %>%
    summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "Win distribution in 2003") + 
  geom_text(aes(label = driverRef), vjust = -0.2)

#2004
results2 %>%
  filter(year == 2004, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2004 Win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2)

#2005
results2 %>%
  filter(year == 2005, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "2005 Win distribution") +
    geom_text(aes(label = Wins), vjust = -0.2)

#2006
results2 %>%
  filter(year == 2006, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2006 Win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2)

#2007
results2 %>%
  filter(year == 2007, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2007 win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2)


#---- Poles in a season bar graph: 2003/2009/2010 ----

qualifying1 <- left_join(races, qualifying, by = "raceId")
qualifying2 <- left_join(qualifying1, drivers, by = "driverId")

#2003
qualifying2 %>%
  filter(year == 2003, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "2003 Poles") +
    geom_text(aes(label = Poles), vjust = -0.2)

#2009
qualifying2 %>%
  filter(year == 2009, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2009 Poles") +
  geom_text(aes(label = Poles), vjust = -0.2)

#2010
qualifying2 %>%
  filter(year == 2010, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2010 Poles") +
  geom_text(aes(label = Poles), vjust = -0.2)

#---- Constructors ----
constructor_points <- left_join(constructorResults, constructors, by = "constructorId")
constructor_points1 <- left_join(constructor_points, races, by = "raceId")

#1998
constructor_points1 %>%
  filter(year == 1998) %>%
  group_by(constructorRef) %>%
  mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point() + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season Points", title = "1998 Team Points")

#2007
constructor_points1 %>%
  filter(year == 2007) %>%
    group_by(constructorRef) %>%
    mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point() + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season Points", title = "2007 Team points")

#2010
constructor_points1 %>%
  filter(year == 2010) %>% 
    group_by(constructorRef) %>%
    mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point() + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season points", title = "2010 Team points")


#----- Fastest Laps in a season ----


FL <- left_join(results, races, by = "raceId")
FL1 <- left_join(FL, drivers, by = "driverId")

View(FL2000 <- FL1 %>%
  filter(year == 2008, round == 1))

#---- Career Wins ----

career_wins <- left_join(races, results, by = "raceId")
career_wins_1 <- left_join(career_wins, drivers, by = "driverId")

career_wins_1 %>%
  filter(position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
    mutate(Wins = sum(count)) %>%
    filter(Wins > 10) %>%
    ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) + 
             geom_bar(stat = "identity") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "Career Wins") +
  coord_flip() + geom_text(aes(label = Wins), hjust = -0.2)


#---- Career Poles ----

poles <- left_join(results, races, by = "raceId")
poles1 <- left_join(poles, drivers, by = "driverId")

poles1 %>%
  filter(grid == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(poles = sum(count)) %>%
    filter(poles > 10) %>%
  ggplot(aes(reorder(driverRef, poles), poles, fill = poles)) + 
           geom_bar(stat = "identity") + 
  coord_flip() + geom_text(aes(label = poles), hjust = -0.2)

#---- Top 3's ----
top3 <- left_join(races, results, by = "raceId")
top3_1 <- left_join(top3, drivers,by  = "driverId")

top3_1 %>%
  filter(position == 1 | position == 2 | position == 3) %>%
  group_by(driverRef, driverId) %>%
  summarise(count = n()) %>%
  mutate(top3 = sum(count)) %>%
  filter(top3 > 50) %>%
  ggplot(aes(reorder(driverRef, top3), top3, fill = top3)) + 
    geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45)) +
    geom_text(aes(label = top3), vjust = -0.2)
