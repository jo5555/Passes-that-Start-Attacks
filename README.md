# Passes-that-Start-Attacks

#Change Competition and Season to you liking and just run the code

library(StatsBombR)
library(tidyverse)
library(ggsoccer)
library(ggiraph)
library(extrafont)
loadfonts(device = "win")


Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)

#                                        Change HERE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
Matches = Matches %>% filter(competition.competition_name=="FIFA World Cup" & season.season_name=="2018")
#                                        Change HERE ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)


# data360 = data360 %>% rename(id = event_uuid)

# events = events %>% left_join(data360, by = c("id" = "id"))

# events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

data <- events %>%
  filter( (type.name == "Pass") | lag(type.name == "Pass", 1)| lag(type.name == "Pass", 2)| lag(type.name == "Pass", 3))

data <- data %>%
  mutate(carry.distance = carry.end_location.x - location.x)

data <- data %>%
  filter ((type.name == "Carry" & carry.distance >= 30) | lead(type.name == "Carry" & carry.distance >= 30, 2) )

data <- data %>%
  filter ((type.name == "Pass") | lag(type.name == "Pass", 1) )

dist <- data.frame(data$carry.end_location.x, data$carry.end_location.y, data$carry.distance)
dist <- dist %>% slice(rep(1:n(), each = 2))
dist <- na.omit(dist)
data$carry.distance <- dist$data.carry.distance
data$carry.end_location.x <- dist$data.carry.end_location.x
data$carry.end_location.y <- dist$data.carry.end_location.y
rm(dist)

allpass <- filter(data, type.name == "Pass")

passselect <- filter(data, type.name == "Pass")

passselect <- data.frame(passselect$play_pattern.name, passselect$team.name, passselect$player.name, passselect$location.x, passselect$location.y, passselect$carry.end_location.x, passselect$carry.end_location.y, passselect$carry.distance)
passselect = passselect %>% rename_all(funs(str_replace_all(., "passselect.", "")))


final <- data.frame(unique(passselect$player.name))
final <- final[order(final[,'unique.passselect.player.name.']), ]
final <- cbind(final, table(passselect$player.name))
final <- data.frame(final)
final = rename(final, numpasses = V2)
final = rename(final, "name" = "final")


# dista <- passselect %>%
#  group_by(player.name, team.name) %>%
#  summarise(carrytotal = sum(carry.distance), xlocation = mean(location.x), ylocation = mean(location.y), carryx = mean(carry.end_location.x), carryy = mean(carry.end_location.y))
#final <- cbind(final, dista)
#rm(dista)
#final <- final[, -c(3)]

passselect$player.name <- stri_enc_toutf8(passselect$player.name, is_unknown_8bit = FALSE, validate = FALSE)



ggplot(passselect) +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "#1b1f2b", colour = "white") +
  geom_point_interactive(aes(x = location.x, y = location.y, color = player.name, data_id = player.name)) +
  geom_segment_interactive(aes(x = location.x, y = location.y, xend = carry.end_location.x, yend = carry.end_location.y, colour = player.name, alpha = 0.5, data_id = player.name)) +
  geom_text_interactive(aes(x = 50, y = 82,
                            label = paste(player.name),
                            data_id = player.name
  ),
  color = "#ffffff",
  size = 2.5,
  hjust = "center",
  # total transparency to hide text when cursor is not hovering over squares
  alpha = 0
  ) +
  theme_pitch() +
  annotate("segment",
    x = 60 - (50 / 2),
    y = -4,
    xend = 60 + (50 / 2),
    yend = -4,
    arrow = arrow(length = unit(0.03, "npc"), type = "closed"), colour = "#9396a4") +
  facet_wrap(vars(team.name), strip.position = "bottom") +
  ggtitle("Sparking Attacks")+
  labs(subtitle = "Passes that Started Long Carries \n", 
       caption = "Carries must be 30+ yards \n Dots represent pass postion \n Lines represent carry distance and end location \n Colors represent player whose pass started the attack") +
  theme(panel.background = element_rect(fill = "#1b1f2b"), legend.position = "none",
        plot.background = element_rect(fill = "#272930"),
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(family = "Bahnschrift", colour = "#ffffff", size = 14, vjust = 2),
        plot.title = element_text(family = "Bahnschrift", face = "bold", color = "#ffffff", size = 27),
        plot.subtitle = element_text(family = "Bahnschrift", color = "#d4d7e1", size = 20),
        plot.caption = element_text(family = "Bahnschrift", color = alpha("#d0d1d7", 0.5), size = 10))


try({
  if (length(unique(events$team.name))=24){
    ggsave(paste(Matches[1,13], Matches[1,15], "fastattack.png", sep = ""), height = 2450, width = 2275, units = "px")
  }else if (length(unique(events$team.name))=24) {
    ggsave(paste(Matches[1,13], Matches[1,15], "fastattack.png", sep = ""), height = 2550, width = 2275, units = "px")
  }
}, silent=TRUE)



