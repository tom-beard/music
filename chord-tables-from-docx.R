# read chord tables from Synthtech doc

library(tidyverse)
library(fs)
library(here)
library(officer)

read_chord_tables_from_docx <- function(docx_path) {
  docx_df <- docx_path %>% read_docx() %>% docx_summary() %>% as_tibble()
  
  tables_df <- docx_df %>%
    filter(content_type == "table cell") %>%
    select(doc_index, text, row_id, cell_id) %>% 
    pivot_wider(names_from = cell_id, values_from = text)
  
  table_indices <- tables_df %>% pull(doc_index) %>% unique()
  
  table_names <- docx_df %>%
    filter(content_type == "paragraph") %>% 
    filter(str_detect(text, "hords[:]?$")) %>% 
    pull(text) %>% 
    str_replace(":", "") %>% 
    str_replace("chords", "Chords") %>% 
    as_tibble() %>% 
    add_column(doc_index = table_indices) %>% 
    rename(table_name = value)
  
  tables_df %>% 
    left_join(table_names, by = "doc_index") %>% 
    select(table_name, row = `1`, osc_1 = `2`, osc_2 = `3`, osc_3 = `4`, osc_4 = `5`) %>% 
    filter(row != "Row") %>% 
    mutate(across(starts_with("osc_"), as.numeric))
}

docx_path <- path("D:", "modular", "firmware", "E352_V16", "E370 Release Notes V16.docx")

chord_tables <- read_chord_tables_from_docx(docx_path)

chord_tables %>% write_csv(here("chord_tables.csv"))
