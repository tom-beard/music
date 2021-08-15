# visualise chord tables

library(tidyverse)
library(fs)
library(here)
library(glue)
library(pichor)

chord_tables <- read_csv(here("chord_tables.csv"), col_types = cols(
  table_name = col_character(),
  row = col_integer(),
  osc_1 = col_double(),
  osc_2 = col_double(),
  osc_3 = col_double(),
  osc_4 = col_double()
))

chord_list <- chord_tables %>% 
  filter(table_name == "4-note Chords") %>% 
  pivot_longer(starts_with("osc_"), names_to = "osc", values_to = "semitones") %>% 
  select(row, semitones) %>% 
  mutate(semitones = semitones + 1) %>% 
  group_by(row) %>% 
  summarise(notes = list(c(semitones))) %>% 
  pull(notes)

keys_chords %>% 
  highlight_key_sequence(key_sequence = chord_list,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  ggpiano() + 
  facet_wrap(vars(seq_name), ncol = 6)

ggsave("chord_test.pdf", width = 10, height = 10, units = "in")
