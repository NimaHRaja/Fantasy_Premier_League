library(dplyr)
Week1 <- read.csv("C:/Nima - Archive/Projects/Fantasy_Premier_League/25-26/FPL_Week2.csv")
Week2 <- read.csv("C:/Nima - Archive/Projects/Fantasy_Premier_League/25-26/FPL_Week3.csv")





full_join(Week1, Week2, by = c("Manager", "Player")) %>% View()

Week2 %>% group_by(Position,Player) %>% summarise(num_managers = n()) %>% View()

full_join(Week1, Week2, by = c("Manager", "Player", "Position")) %>% filter(is.na(Week.x) | is.na(Week.y)) %>% 
  group_by(Manager) %>% mutate(tot = n()) %>% filter(tot != 15) %>% 
  mutate(counter = if_else(is.na(Week.x),1,-1)) %>% 
  ungroup() %>% 
  group_by(Player, Position) %>% 
  summarise(change = sum(counter)) %>% 
  View()

Week2 %>% group_by(Manager, Position) %>% summarise(a = n()) %>% 
  group_by(Position) %>% summarise(max(a), min(a)) %>%
  View()

