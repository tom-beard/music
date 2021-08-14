# read chord tables from Synthtech doc

library(tidyverse)
library(fs)
library(here)
library(glue)
library(skimr)
library(janitor)
library(officer)

docx_path <- path("D:", "modular", "firmware", "E352_V16", "E370 Release Notes V16.docx")

docx_obj <- read_docx(docx_path)

str(docx_obj)
