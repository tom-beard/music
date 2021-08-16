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

make_chord_list_from_table <- function(chord_tables, selected_table_name) {
  chord_tables %>% 
    filter(table_name == selected_table_name) %>% 
    pivot_longer(starts_with("osc_"), names_to = "osc", values_to = "semitones", values_drop_na = TRUE) %>% 
    select(row, semitones) %>% 
    mutate(semitones = semitones + 1) %>% 
    group_by(row) %>% 
    summarise(notes = list(c(semitones))) %>% 
    pull(notes)
}

chord_list <- make_chord_list_from_table(chord_tables, "3-note Chords")
chord_list <- make_chord_list_from_table(chord_tables, "4-note Chords")

keys_chords %>% 
  highlight_key_sequence(key_sequence = chord_list,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  ggpiano() + 
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(seq_name), ncol = 8)

ggsave("chord_test.pdf", width = 10, height = 10, units = "in")


# extended keyboard -------------------------------------------------------

chord_list <- make_chord_list_from_table(chord_tables, "Stradella Chords")

#' Extend keyboard (currently downwards only, by up to an octave)
#'
#' @param lower_keys_required Number of extra keys required (<=12)
#'
#' @return A version of the keys_chords dataframe with extra keys at the start.
#'
#' @examples
extend_keyboard <- function(lower_keys_required) {
  if (lower_keys_required < 1) {
    return(pichor::keys_chords)
  }
  octave_spacing <- 0.5
  lower_keys <- pichor::keys_chords %>%
    slice((12 - lower_keys_required + 1):12) %>% 
    mutate(across(c(xmin, xmax, label_x), ~ .x - octave_spacing)) %>% 
    mutate(key = key - 12)
  
  bind_rows(lower_keys, pichor::keys_chords)
}

lower_keys_required <- 1 - range(chord_list)[1]

extend_keyboard(lower_keys_required) %>% 
  highlight_key_sequence(key_sequence = chord_list,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  ggpiano() + 
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(seq_name), ncol = 8)
