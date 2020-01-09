library(tidyverse)

raptor_original <- read_csv("data/modern_RAPTOR_by_team.csv")

# check out the data

raptor_for_all_seasons <- raptor_original %>% 
  
  select(
    player_name, 
    player_id, 
    season, 
    season_type, 
    team, 
    poss, 
    mp, 
    raptor_total, 
    war_total
  )


raptor <- raptor_for_all_seasons %>% 
  
  filter(
    season_type == "RS"
  )

raptor

# loook at the top 5 raptor seasons
raptor %>% 
  
  arrange(-raptor_total) %>% 
  slice(1:5)

raptor %>% 
  
  filter(
    poss >= 100
  ) %>% 
  
  arrange(-raptor_total) %>% 
  slice(1:5)


raptor %>% 
  
  ggplot(aes(poss)) + 
  geom_histogram()

# top five raptor teams
raptor %>% 
  
  arrange(-war_total) %>% 
  slice(1:5)

team_war <- raptor %>% 
  
  group_by(
    team, 
    season
  ) %>% 
  
  summarise(
    war_total = sum(war_total)
  ) %>% 
  
  ungroup()

team_war %>% 
  arrange(-war_total) %>% 
  slice(1:5)

team_war %>% 
  arrange(war_total) %>% 
  slice(1:5)

# best year over year growth
yoy_change <- raptor %>% 
  
  group_by(
    player_name, 
    player_id, 
    season
  ) %>% 
  
  summarize(
    raptor_total = weighted.mean(raptor_total, w = poss),
    poss = sum(poss), 
    war_total = sum(war_total) 
  ) %>% 
  
  ungroup() %>% 
  
  filter(poss >= 3000) %>% 
  
  group_by(
    player_id
  ) %>% 
  
  mutate(
    last_war = lag(war_total),
    last_raptor = lag(raptor_total)
  ) %>% 
  
  filter(
    !is.na(last_war)
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    war_difference = war_total - last_war, 
    raptor_difference = raptor_total - last_raptor
  )

yoy_change %>% 
  
  arrange(-war_difference)


contenders_only <- raptor_for_all_seasons %>% 
  
  group_by(player_id, season) %>% 
  
  filter("PO" %in% season_type & "RS" %in% season_type, sum(poss) > 5000) %>% 
  
  ungroup() %>% 
  
  group_by(
    player_id, season, season_type
  ) %>% 
  
  summarize(
    raptor_total = weighted.mean(raptor_total, poss)
  ) %>% 
  
  ungroup()


under_pressure <- contenders_only %>% 
  
  pivot_wider(
    names_from = "season_type", 
    values_from = "raptor_total"
  )

under_pressure %>% 
  
  
  filter(PO > -30, PO < 30) %>% 
  ggplot(aes(RS, PO)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

lm(PO ~ RS, data = under_pressure) %>% summary()

