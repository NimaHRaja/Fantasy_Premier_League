library(fplscrapR)
library(dplyr)
library(reshape2)
library(ggplot2)



get_a_league_similarity <- function(league_id, gw, min_similarity){
  
  
  league_entries <- get_league_entries(league_id)
  
  DF <- 
    inner_join(
      get_entry_player_picks(league_entries$entry,gw), 
      league_entries, 
      by = "entry") %>% 
    select(playername, player_name, multiplier)
  
  inner_join(DF, DF, by = "playername", relationship = "many-to-many") %>% 
    
    mutate(shared = pmin(multiplier.x, multiplier.y)) %>% 
    group_by(player_name.x, player_name.y) %>% 
    summarise(shared = sum(shared)) %>% 
    
    dcast(player_name.x ~ player_name.y, value.var = "shared") %>% 
    melt(na.rm = FALSE) %>% 
    replace(is.na(.), 0)%>% 
    filter(value < 12) %>% 
    filter(value >= min_similarity) %>% 
    rename(player1 = player_name.x) %>% 
    rename(player2 = variable) %>% 
    
    ggplot(aes(x = player1, y = player2, label = value, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
    geom_text() + 
    theme(axis.text.x = element_text(angle = -90, hjust = 0))
}

get_a_league_similarity(league_id = 725602, gw = 3, min_similarity = 0)