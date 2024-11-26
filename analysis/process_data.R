library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "massive_memory"

#read experiment data
exp_data <- read.csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id=participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract json data
info <- exp_data %>% 
  filter(trial_type == "survey-text"&trial_index==449) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,experiment_purpose:technical_issues)

#join into exp_data
exp_data <- exp_data %>%
  left_join(info)

#memory repeat check
memory_repeat_check <- exp_data %>%
  filter(trial_phase=="Memory") %>%
  mutate(
    correct_response = ifelse(repeated=="FALSE","no response","response")
  ) %>%
  mutate(
    is_right = case_when(
      correct_response == "no response" & rt == "null" ~ 1,
      correct_response == "no response" & rt != "null" ~ 0,
      correct_response == "response" & rt == "null" ~ 0,
      correct_response == "response" & rt != "null" ~ 1,
    )
  )

summarize_memory_repeat_check <- memory_repeat_check %>%
  group_by(random_id,correct_response) %>%
  summarize(
    #N=n(),
    accuracy = mean(is_right)
  ) %>%
  pivot_wider(names_from = correct_response,values_from=accuracy) %>%
  mutate(
    repeat_false_alarm_rate = 1-`no response`
  ) %>%
  rename(
    repeat_hit_rate = response
  ) %>%
  select(random_id,repeat_false_alarm_rate,repeat_hit_rate)

#join into exp_data
exp_data <- exp_data %>%
  left_join(summarize_memory_repeat_check)

#filter and select relevant data
processed_data <- exp_data %>%
  mutate(confidence_response = lead(response)) %>%
  filter(trial_phase=="Test") %>%
  mutate(confidence_response=as.numeric(as.character(confidence_response))) %>%
  mutate(
    is_right = ifelse(correct,1,0)
  ) %>%
  relocate(
    is_right,.after="correct"
  ) %>%
  select(participant_id, random_id,repeat_false_alarm_rate,repeat_hit_rate,trial_index,time_elapsed,trial_phase,response,rt,confidence_response, correct:choice,experiment_purpose:technical_issues) %>%
  group_by(participant_id) %>%
  mutate(
    trial_number=seq(n())
  ) %>%
  relocate(
    trial_number,.after="trial_index"
  ) %>%
  #extract stimulus information
  mutate(
    trial_kind = case_when(
      str_detect(correct_image,"EXEMPLAR") ~ "exemplar",
      str_detect(correct_image,"STATE") ~ "state"
    )
  ) %>%
  relocate(
    trial_kind,.after="trial_phase"
  ) %>%
  #make sure rt is numeric
  mutate(
    rt=as.numeric(as.character(rt))
  )

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
