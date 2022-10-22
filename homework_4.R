
# ------------------------------------------------------------
# Assignment 3 - Data Wrangling
# Nicolas Herrera
# ------------------------------------------------------------

# Libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)

# Question 1
polls_adjusted <- read_csv("covid_approval_polls_adjusted.csv")
polls_adjusted <- tibble(polls_adjusted)

#Question 2
polls_adjusted <- polls_adjusted %>% 
  group_by(subject) %>% 
  mutate(average_approval= mean(approve_adjusted , na.rm=T) )

#Question 3
approval_graph <- ggplot(data = polls_adjusted, aes(x = approve_adjusted)) +
  geom_histogram() +
  xlab("Approval") + 
  ylab("Count") + 
  labs(title = "Adjusted Approval Ratings") +
  theme_minimal()

#Question 4

approval_graph_facet <- polls_adjusted %>% 
  filter(subject != "=======") %>%
  ggplot( aes(x = approve_adjusted , fill=subject)) +
  geom_histogram() +
  xlab("Approval") + 
  ylab("Count") + 
  labs(title = "American approval of Biden and Trump's response to coronavirus" ,
       subtitle = "From 2020-2022" , fill ="President") +
  facet_wrap(vars(subject)) +
  theme_minimal()

#Question 5
mean_line <- polls_adjusted %>% 
  filter(subject != "=======") %>%
  ggplot( aes(x = approve_adjusted , fill=subject)) +
  geom_histogram() +
  xlab("Approval") + 
  ylab("Count") + 
  labs(title = "American approval of Biden and Trump's response to coronavirus" ,
       subtitle = "From 2020-2022" , fill ="President") +
  facet_wrap(vars(subject)) +
  theme_minimal() +
  geom_vline(aes(xintercept=average_approval) , linetype="dashed")

#Question 6
approval_final <- mean_line +
  scale_fill_manual(values=c("#008FD5" ,"#FF2700")) +
  theme(legend.position = "bottom" ,
        legend.direction = "horizontal") 


# Question 3.a
polls_q3 <- polls_adjusted %>%
  mutate(end_date = lubridate::mdy(enddate)) %>%
  mutate(approve_fraction = approve_adjusted/100) %>%
  filter(party == "D" | party == "R" | party == "I")


# Question 3.b
polls_q3 %>%
  ggplot(aes(x = end_date, y = approve_fraction, color = party)) +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  labs(color = "Party") +
  labs(title = "Nicolas: Approval of President’s Handling of Covid-19 Pandemic",
       subtitle = "From 2020−2022") +
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("#008FD5", "#77AB43", "#FF2700")) +
  geom_vline(data = polls_q3, aes(xintercept = lubridate::as_date("2021-01-20")), lty = "dashed") +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent)

# Question 4.a
toplines <- read_csv("covid_approval_toplines.csv")

# Question 4.b
toplines_biden <- toplines %>%
  filter(subject == "Biden", 
         party == "D" | party == "R" | party == "I") %>%
  mutate(model_date = lubridate::mdy(modeldate))


#  Question 4.c  
toplines_biden <- toplines_biden %>%
  mutate(party_description = 
           ifelse(party == "D", "Democrats", ifelse(party == "R", "Republicans", "Independents") ) ) %>%
  mutate(approve_estimate_frac = approve_estimate/100)

# Question 4.d
library(ggrepel)
library(scales)

final_graph <- toplines_biden %>%
  mutate(label = ifelse(model_date ==  max(model_date),
                        party_description, NA_character_)) %>% ## do not need to modify 
  ggplot(aes(x = model_date, y = approve_estimate_frac, color = party)) + ## need to fill in
  geom_line() + ## do not need to modify
  geom_text_repel(aes(label = label),
                  nudge_x = 10, na.rm = T,
                  xlim = as_date(c("2022-07-01", "2022-10-01"))) + ## do not need to modify
  geom_vline(aes(xintercept = as_date("2021-01-20")), linetype = "dashed") + ## need to fill in
  annotate("text", x = as_date("2021-01-20"), y = 0.05,
           label = "Biden sworn into office", size = 3,
           hjust = -0.1) + ## do not need to modify
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700") ) + ## need to fill in
  scale_y_continuous(labels = scales::percent) + ## do not need to modify
  coord_cartesian(ylim = c(.1,1), clip = "off") + ## do not need to modify
  scale_x_date(limits = c(as_date("2020-12-01"), as_date("2022-10-01"))) + ## do not need to modify
  labs(title = "Do Americans approve of Biden response to the coronavirus crisis?",
       subtitle = "A calculation of the share of all Americans who approve of the handling of the coronavirus outbreak",
       x = "",
       y = "") + ## need to fill in
  theme_minimal() + ## do not need to modify
  theme(legend.position = "none") ## need to fill in
