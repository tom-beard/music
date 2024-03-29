# visualise chord tables

library(tidyverse)
library(fs)
library(here)
library(glue)
# remotes::install_github("mikldk/pichor")
library(pichor)
library(ggforce)
library(gridExtra)


# define main functions ---------------------------------------------------

read_chord_tables <- function(chord_file_path) {
  read_csv(chord_file_path, col_types = cols(
    table_name = col_character(),
    row = col_integer(),
    osc_1 = col_double(),
    osc_2 = col_double(),
    osc_3 = col_double(),
    osc_4 = col_double()
  ))
}

make_tidy_chords_from_table <- function(chord_tables, selected_table_name) {
  if (!endsWith(selected_table_name, " Chords")) {
    selected_table_name <- paste(selected_table_name, "Chords")
  }
  chord_tables %>% 
    filter(table_name == selected_table_name) %>% 
    pivot_longer(starts_with("osc_"), names_to = "osc", values_to = "semitones", values_drop_na = TRUE) %>% 
    select(row, osc, semitones) %>% 
    mutate(osc = str_remove(osc, "^osc_"))
}

make_chord_list_from_table <- function(chord_tables, selected_table_name) {
  chord_tables %>% 
    make_tidy_chords_from_table(selected_table_name) %>% 
    mutate(semitones = semitones + 1) %>% 
    group_by(row) %>% 
    summarise(notes = list(c(semitones))) %>% 
    pull(notes)
}

plot_semitone_pattern <- function(chord_tables, selected_table_name) {
  hlines <- c(0, 4, 7, 12) # maj 3rd, 5th, octave
  tidy_chords <- make_tidy_chords_from_table(chord_tables, selected_table_name)
  semitone_range <- range(tidy_chords$semitones)
  # breaks should be more generalised than this
  if (semitone_range[2] > 24) {
    breaks <- 12 * (0:5)
  } else {
    breaks <- semitone_range[1]:max(max(hlines), semitone_range[2])
  }
  tidy_chords %>% 
    ggplot(aes(x = row, y = semitones, colour = osc, group = osc)) +
    geom_hline(yintercept = hlines, colour = "grey80", size = 1) +
    geom_step() +
    geom_point() +
    scale_x_reverse() +
    scale_y_continuous(breaks = breaks) +
    labs(x = "", y = "semitones",
         title = str_glue("{selected_table_name} chords: semitone pattern")) +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
}

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

get_normalised_inversion <- function(inversion, input_chord) {
  inverted_chord <- get_keys_inversion(input_chord, inversion)
  inverted_chord - inverted_chord[1]
}

# initialise --------------------------------------------------------------

chord_tables <- read_chord_tables(here("chord_tables.csv"))

# semitone visalisation ---------------------------------------------------

plot_semitone_pattern(chord_tables, "3-note")
plot_semitone_pattern(chord_tables, "4-note")
plot_semitone_pattern(chord_tables, "Harmonic")
plot_semitone_pattern(chord_tables, "Stradella")

# examine Stradella chords -------------------------------------------------------

chord_list <- make_chord_list_from_table(chord_tables, "Stradella")

lower_keys_required <- 1 - range(chord_list)[1]

extend_keyboard(lower_keys_required) %>% 
  highlight_key_sequence(key_sequence = chord_list,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  ggpiano() + 
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(seq_name), ncol = 8)

# examine harmonic chords -------------------------------------------------

bind_rows(
  make_tidy_chords_from_table(chord_tables, "4-note") %>% add_column(table_type = "ET"),
  make_tidy_chords_from_table(chord_tables, "Harmonic") %>% add_column(table_type = "Harmonic")
) %>% 
  pivot_wider(names_from = table_type, values_from = semitones)

# Harmonic chords aren't arranged like the Equal Tempered ones
  
# find labels for common chords -------------------------------------------

chord_defns <- list()

# chord_defns$`3-note Chords` <- tribble(
chord_defns$`3-note` <- tribble(
  ~label, ~chord,
  "maj", construct_chord_major("C"),
  "min", construct_chord_minor("C"),
  "dim", construct_chord_raw("C", c(3, 3)),
  "aug", construct_chord_raw("C", c(4, 4)),
  "sus2", construct_chord_raw("C", c(2, 5)),
  "sus4", construct_chord_raw("C", c(5, 2))
)

# chord_defns$`4-note Chords` <- tribble(
chord_defns$`4-note` <- tribble(
  ~label, ~chord,
  "maj7", construct_chord_major_7("C"),
  "dom7", construct_chord_dominant_7("C"),
  "min7", construct_chord_raw("C", c(3, 4, 3)),
  "min maj7", construct_chord_raw("C", c(3, 4, 4)),
  "half dim7", construct_chord_raw("C", c(3, 3, 4)),
  "dim7", construct_chord_raw("C", c(3, 3, 3)),
  "aug7", construct_chord_raw("C", c(4, 4, 2)),
  "dom7 flat5", construct_chord_raw("C", c(4, 2, 4)),
  "maj7 flat5", construct_chord_raw("C", c(4, 2, 5)),
  "aug maj7", construct_chord_raw("C", c(4, 4, 3)),
  "dim maj7", construct_chord_raw("C", c(3, 3, 5)),
  "dom7 sus4", construct_chord_raw("C", c(5, 2, 3)),
  "dom7 sus2", construct_chord_raw("C", c(2, 5, 3))
)

# to do: extract into functions; generalise to Stradella chords

chord_table_name <- "3-note"
chord_table_name <- "4-note"

num_notes <- make_tidy_chords_from_table(chord_tables, chord_table_name) %>% 
  pull(osc) %>% 
  as.integer() %>% 
  max()

chord_labels <- chord_defns[[chord_table_name]] %>% 
  expand(nesting(label, chord), inversion = 0:(num_notes - 1)) %>% 
  mutate(semitones = map2(inversion, chord, get_normalised_inversion)) %>% 
  arrange(semitones, inversion) %>% 
  mutate(full_label = ifelse(inversion == 0, label, paste(label, "inv", inversion))) %>% 
  unnest(semitones) %>% 
  group_by(full_label) %>% 
  mutate(osc = paste0("osc_", row_number())) %>% 
  ungroup() %>% 
  pivot_wider(names_from = osc, values_from = semitones) %>% 
  group_by(across(starts_with("osc_"))) %>% 
  arrange(across(starts_with("osc_")), inversion) %>%
  slice(1) %>% 
  select(full_label, starts_with("osc_")) %>% 
  ungroup()

chord_row_labels <- chord_tables %>% 
  filter(table_name == paste(chord_table_name, "Chords")) %>% 
  left_join(chord_labels, by = paste("osc", 1:num_notes, sep = "_")) %>%
  mutate(row_label = ifelse(is.na(full_label), as.character(row), glue("{row} ({full_label})"))) %>% 
  select(row, row_label) %>% 
  mutate(row_label = factor(row_label, ordered = TRUE, levels = unique(row_label)))

chord_list <- make_chord_list_from_table(chord_tables, chord_table_name)

keys_chords %>% 
  highlight_key_sequence(key_sequence = chord_list,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  left_join(chord_row_labels, by = c("seq_no" = "row")) %>%
  ggpiano() + 
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(row_label), ncol = 8) +
  theme(strip.text = element_text(margin = margin(b = 1, t = 0)))

ggsave(str_glue("chord-chart-{chord_table_name}-chords.pdf"), width = 17, height = 10, units = "in")


# compact multi-page version for mobile ----------------------------------------------

rows_per_page <- 4
cols_per_page <- 2
num_pages <- ceiling(length(chord_list) / (rows_per_page * cols_per_page))

plot_chord_page <- function(this_page) {
  keys_chords %>% 
    highlight_key_sequence(key_sequence = chord_list,
                           new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
    left_join(chord_row_labels, by = c("seq_no" = "row")) %>%
    ggpiano() + 
    coord_fixed(ratio = 0.5) +
    facet_wrap_paginate(vars(row_label), nrow = rows_per_page, ncol = cols_per_page,
                        page = this_page) +
    theme(strip.text = element_text(margin = margin(b = 1, t = 0)))
}

all_chord_pages <- 1:num_pages %>% 
  map(plot_chord_page) %>% 
  marrangeGrob(nrow = 1, ncol = 1,
               top = quote(paste0("E352 ", chord_table_name, " chords: page ", g, " of ", npages)))

ggsave(str_glue("chord-chart-{chord_table_name}-chords-multipage.pdf"), all_chord_pages,
       width = 7, height = 9, units = "in")


# look for 9ths in 3-note chords -----------------------------------------------------------

chord_list_9ths <- chord_tables %>% 
  filter(osc_2 == 12 + 2 | osc_3 == 12 + 2) %>% 
  make_chord_list_from_table("3-note")

keys_chords %>% 
  highlight_key_sequence(key_sequence = chord_list_9ths,
                         new_color = "lightblue", keep_color = "lightblue", remove_color = NULL) %>% 
  # left_join(chord_row_labels, by = c("seq_no" = "row")) %>%
  ggpiano() + 
  coord_fixed(ratio = 0.5) +
  facet_wrap(vars(seq_no)) +
  # facet_wrap(vars(row_label), ncol = 8) +
  theme(strip.text = element_text(margin = margin(b = 1, t = 0)))

# note: this seq_no doesn't match the E352 chord numbers

