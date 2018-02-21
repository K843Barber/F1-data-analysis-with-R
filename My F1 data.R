library(tidyverse)
library(swirl)

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

View(races)
View(results)

results1 <- left_join(races, results, by = "raceId")
results2 <- left_join(results1, drivers, by = "driverId")
View(results2)

(y2007 <- results2 %>%
    group_by(driverId) %>%
    filter(year == 2007) %>%
    mutate(Overall = cumsum(points)) %>%
    filter(round == 17) %>%
  ggplot(aes(reorder(forename, Overall), 
                  Overall, fill = Overall)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "driver", y = "Season points", title = "2007 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2))

(y2008 <- results2 %>%
  group_by(driverId) %>%
  filter(year == 2008) %>%
  mutate(Overall = cumsum(points)) %>%
  filter(round == 17) %>%
  ggplot(aes(reorder(forename, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season Points", title = "2008 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2))

(y2009 <- results2 %>%
    group_by(driverId) %>%
    filter(year == 2009) %>%
    mutate(Overall = cumsum(points)) %>%
    filter(round == 17) %>%
    ggplot(aes(reorder(forename, Overall), Overall, fill = Overall)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90), 
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", y = "Season points", title = "2009 WDC") +
    geom_text(aes(label = Overall), vjust = -0.2))

(y2010 <- results2 %>%
  group_by(driverId) %>%
  filter(year == 2010) %>%
  mutate(Overall = cumsum(points)) %>%
  filter(round == 19) %>%
  ggplot(aes(reorder(surname, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season points", title = "2010 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2))

(y2011 <- results2 %>% 
  group_by(driverId) %>% 
  filter(year == 2011) %>% 
  mutate(Overall = cumsum(points)) %>% 
  filter(round == 19) %>% 
  ggplot(aes(reorder(surname, Overall), Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", y = "Season points", title = "2011 WDC") +
  geom_text(aes(label = Overall), vjust = -0.2))

#---- Wins in a season bar graph: 2003 - 2007 ----

(y2003wins <- results2 %>%
  filter(year == 2003, position == 1) %>%
  group_by(driverId, driverRef) %>%
    summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity")) +
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "Win distribution in 2003")

(y2004wins <- results2 %>%
  filter(year == 2004, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity")) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2004 Win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2)

(y2005wins <- results2 %>%
  filter(year == 2005, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity")) +
    theme(axis.text.x = element_text(angle = 45),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "2005 Win distribution") +
    geom_text(aes(label = Wins), vjust = -0.2)

(y2006wins <- results2 %>%
  filter(year == 2006, position == 1)) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2006 Win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2)

(y2007wins <- results2 %>%
  filter(year == 2007, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Wins = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2007 win distribution") +
  geom_text(aes(label = Wins), vjust = -0.2))


#---- Poles in a season bar graph: 1996 - 2001 ----

qualifying1 <- left_join(races, qualifying, by = "raceId")
qualifying2 <- left_join(qualifying1, drivers, by = "driverId")

(y2003quali <- qualifying2 %>%
  filter(year == 2003, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "2003 Poles") +
    geom_text(aes(label = Poles), vjust = -0.2))

(y2009quali <- qualifying2 %>%
  filter(year == 2009, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2009 Poles") +
  geom_text(aes(label = Poles), vjust = -0.2))

(y2010quali <- qualifying2 %>%
  filter(year == 2010, position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
  mutate(Poles = sum(count)) %>%
  ggplot(aes(reorder(driverRef, Poles), Poles, fill = Poles)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Driver", title = "2010 Poles") +
  geom_text(aes(label = Poles), vjust = -0.2))

#---- Constructors ----
constructor_points <- left_join(constructorResults, constructors, by = "constructorId")
constructor_points1 <- left_join(constructor_points, races, by = "raceId")
View(constructor_points1)  

(year_2007 <- constructor_points1 %>%
  filter(year == 2007) %>%
    group_by(constructorRef) %>%
    mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point()) + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season Points", title = "2007 Team points")

(y2010 <- constructor_points1 %>%
  filter(year == 2010) %>% 
    group_by(constructorRef) %>%
    mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point() + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season points", title = "2010 Team points"))

(y1998 <- constructor_points1 %>%
  filter(year == 1998) %>%
  group_by(constructorRef) %>%
  mutate(total_points = cumsum(points)) %>%
  ggplot(aes(round, total_points, colour = constructorRef)) +
  geom_point() + geom_smooth(alpha = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Season Points", title = "1998 Team Points"))

#----- Fastest Laps in a season ----


FL <- left_join(results, races, by = "raceId")
FL1 <- left_join(FL, drivers, by = "driverId")

View(FL2000 <- FL1 %>%
  filter(year == 2008, round == 1))

#---- Career Wins ----

career_wins <- left_join(races, results, by = "raceId")
career_wins_1 <- left_join(career_wins, drivers, by = "driverId")

View(career_wins_1)

(career_wins2 <- career_wins_1 %>%
  filter(position == 1) %>%
  group_by(driverId, driverRef) %>%
  summarise(count = n()) %>%
    mutate(Wins = sum(count)) %>%
    filter(Wins > 10) %>%
    ggplot(aes(reorder(driverRef, Wins), Wins, fill = Wins)) + 
             geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Driver", title = "Career Wins"))
  
  