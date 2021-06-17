library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)
library(jpeg)

dat <- NULL
ach <- NULL

ss <- "https://docs.google.com/spreadsheets/d/1sPBxBR7zy37fjZFdv4se75NWl52v1NxHfPWACWI02gQ/edit?usp=sharing"
for(i in c("ScenarioList", "GamesPlayed", "AdditionalScenarios")){
  dat[[i]] <- googlesheets4::read_sheet(ss, sheet = i)  
}

ach$global <- 
  dat$ScenarioList %>%
    filter(Scenario %in% dat$GamesPlayed$scenario,
           !is.na(Global)) %>%
    select(Global) %>%
    mutate(ach = Global %>% strsplit(",")) %>%
    unnest(ach)

ach$party <- 
  dat$ScenarioList %>%
  filter(Scenario %in% dat$GamesPlayed$scenario,
         !is.na(Party)) %>%
  select(Party) %>%
  mutate(ach = Party %>% strsplit(",")) %>%
  unnest(ach)

ach$total <- c(ach$party$ach, ach$global$ach)

scenariosAvailable <- dat$ScenarioList %>%
  filter(Scenario %in% dat$GamesPlayed$scenario) %>%
  select(New1, New2, New3) %>%
  gather() %>%
  na.omit() %>%
  select(value) %>%
  rbind(dat$AdditionalScenarios$scenario) %>%
  unique() %>%
  arrange(value) %>%
  rename(Scenario = value) %>%
  left_join(dat$ScenarioList, by = "Scenario") %>%
  select(Scenario, ReqPos1, ReqPos2, ReqPos3, ReqPosAlt) %>%
  mutate(available = case_when(is.na(ReqPos1) | ReqPos1 %in% ach$total ~ T,
                               T ~ F)) %>%
  mutate(available = case_when((is.na(ReqPos2) | ReqPos2 %in% ach$total) &
                                 available ~ T,
                               T ~ F)) %>%
  mutate(available = case_when((is.na(ReqPos3) | ReqPos3 %in% ach$total) &
                                 available ~ T,
                               T ~ F)) %>%
  mutate(available = case_when((!available & is.na(ReqPosAlt)) ~ F,
                               (is.na(ReqPosAlt) | ReqPosAlt %in% ach$total) ~ T,
                               T ~ F)) %>%
  filter(available)

dat$ScenarioList %>%
  mutate(y = Location %>% substr(1,1) %>% factor(levels = LETTERS[26:1]),
         x = Location %>% substr(3,5) %>% factor(levels = 1:20),
         status = case_when((Scenario %in% scenariosAvailable$Scenario) & #unlocked, unplayed, unblocked
                              !(Scenario %in% dat$GamesPlayed$scenario) &
                              !(ReqNeg %in% ach$total) ~ "available",
                            Scenario %in% dat$GamesPlayed$scenario ~ "complete",
                            ReqNeg %in% ach$total ~ "unavailable",
                            TRUE ~ "unknown")) %>%
  ggplot() +
  annotation_custom(rasterGrob(readJPEG("map.jpeg"), 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_label(aes(x = x,
                y = y,
                label = paste0(Scenario, ": ", Description),
                fill = status),
             size = 2,
             #position = position_dodge()) +
             position = position_jitter(width = .5, height = .5)) +
  scale_fill_manual(values=c("available" = "cyan", "complete" = "green", "unavailable" = "red", "unknown" = "000000")) +
  theme(legend.position = "none")

  