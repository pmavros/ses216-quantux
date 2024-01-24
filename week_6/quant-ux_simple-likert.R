############ 
# SES216 - Quant UX Class - Telecom Paris
# Synthetic response distributions exercice 1
# Author: Panos Mavros
############


# This task simulates a simple example of how the processing and presentation of data from responses (and other data of course), 
# can skew what inferences and decisions are made. The plots and stats generated below are used as a discussion exercise in class.


# load libraries
library(tidyverse)
library(paletteer)

# set seed for random numbers
set.seed(16)

# create a list of typical reponses to a likert scale, in the correct order
likertscale <- c("Stongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

# simulate 100 people responding to one question.
# sample 100 times, with replacement, giving equal probability to each likert item
task_1 <- tibble(response = sample(likertscale, size = 100, replace = T, prob = rep(.2, 5)))

# convert the response to an ordered factor, 
#which will help us plot the responses in the correct order (not alphabetically)
task_1$response <- factor(task_1$response, labels = likertscale, ordered = T)

write_csv(task_1, "~/Desktop/task_1.csv")

#group 1
# is provided with tests

# group 2
# bar chart showing count of responses for each likert level
task_1 %>% 
  ggplot( aes(response, fill = response) ) +
  geom_bar() + ggpubr::theme_pubr() + scale_fill_paletteer_d("basetheme::brutal")

# group 3
# we create a new metric, positive - nevative responses.  
# all responses in "Neutral", "Agree", "Strongly Agree" are considered 'positive' (see with the next group where we do the opposite)

task_1 %>% 
  mutate(response = if_else(response %in% c("Neutral", "Agree", "Strongly Agree"), "Positive", "Negative")) %>% 
  ggplot( aes(response, fill = response) ) +
  geom_bar()+ ggpubr::theme_pubr() + scale_fill_paletteer_d("basetheme::brutal")

# group 4
# we create a new metric, positive - nevative responses.  
# all responses in  "Agree", "Strongly Agree" are considered 'positive', but "Neutral" is considered a negative response instead.

task_1 %>% 
  mutate(response = if_else(response %in% c("Agree", "Strongly Agree"), "Positive", "Negative")) %>% 
  ggplot( aes(response, fill = response) ) +
  geom_bar()+ ggpubr::theme_pubr() + scale_fill_paletteer_d("basetheme::brutal")

# group 5
# all responses in  "Agree", "Strongly Agree" are considered 'positive', but "Neutral" is excluded completely.

task_1 %>% 
  filter(response != "Neutral") %>% 
  mutate(response = if_else(response %in% c("Agree", "Strongly Agree"), "Positive", "Negative")) %>% 
  ggplot( aes(response, fill = response) ) +
  geom_bar() + ggpubr::theme_pubr() + scale_fill_paletteer_d("basetheme::brutal")


# group 6
# all responses in  "Agree", "Strongly Agree" are considered 'positive', but "Neutral" is excluded completely.
# this group only receives a text with a number - a different way of presenting messages.

task_1 %>% 
  filter(response != "Neutral") %>% 
  mutate(response = if_else(response %in% c("Agree", "Strongly Agree"), "Positive", "Negative")) %>% 
  count(response)


