library(tidyverse)
library(janitor)

Game_of_Thrones <- read_csv("Game_of_Thrones.csv")
View(Game_of_Thrones)

str(Game_of_Thrones)

colnames(Game_of_Thrones)
glimpse(Game_of_Thrones)

Game_of_Thrones <- clean_names(Game_of_Thrones)

#1.All average ratings compare 3 web
#2.Average ratings by season compare 3 web
#3.Average ratings of each episode by season
#4. Top 5 directors who have worked with the most
#5. Which episodes are the top 5 directors taking care of?
#6. Top 5 most viewed people
#7. The top 5 photographer who has worked together the most

#1 
rating_of_tree <- Game_of_Thrones %>%
 select(im_db_rating,rotten_tomatoes_rating_percentage,metacritic_ratings) %>%
  summarise(mean_imdb = mean(im_db_rating), 
            mean_rottrn = mean(rotten_tomatoes_rating_percentage)/10,
            mean_metacritic = mean(metacritic_ratings))

rating_of_tree %>%
  gather() %>%
  ggplot(aes(key,value)) +
  geom_col()

rating_of_tree

#2
rating_of_season <- Game_of_Thrones %>%
  select(season,
         im_db_rating,
         rotten_tomatoes_rating_percentage,
         metacritic_ratings) %>%
  group_by(season) %>%
  summarise(mean_imdb = mean(im_db_rating), 
            mean_rottrn = mean(rotten_tomatoes_rating_percentage)/10,
            mean_metacritic = mean(metacritic_ratings))

rating_of_season %>%
  pivot_longer(-season, names_to = "variable", values_to = "value") %>%
  ggplot(aes(season, value, colour= variable))+
  geom_point()+
  geom_line()

rating_of_season

#3
rating_of_season4 <- Game_of_Thrones %>%
  filter(season == 4) %>%
  select(no_of_episode_season,
         im_db_rating,
         rotten_tomatoes_rating_percentage,
         metacritic_ratings) %>%
  group_by(no_of_episode_season) %>%
  summarise(mean_imdb = mean(im_db_rating), 
            mean_rottrn = mean(rotten_tomatoes_rating_percentage)/10,
            mean_metacritic = mean(metacritic_ratings))
  

rating_of_season4 %>%
  pivot_longer(-no_of_episode_season, names_to = "variable", values_to = "value") %>%
  ggplot(aes(no_of_episode_season, value, colour= variable))+
  geom_point()+
  geom_line()

#4
Game_of_Thrones %>%
  select(directed_by) %>%
  group_by(directed_by) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n >= 5)

#5
  Game_of_Thrones %>%
    filter(directed_by == 'David Nutter') %>%
    select(1,2,4,5,6,7,8,9,11,12,13,14,15,19)
  
  Game_of_Thrones %>%
    filter(directed_by == 'Alan Taylor') %>%
    select(1,2,4,5,6,7,8,9,11,12,13,14,15,19)

#6
  Game_of_Thrones %>%
    select(1,2,4,9) %>%
    arrange(desc(u_s_viewers_millions))

#7
  Game_of_Thrones %>%
    select(cinematography_by) %>%
    group_by(cinematography_by) %>%
    count() %>%
    arrange(desc(n)) %>%
    filter(n >= 5)




