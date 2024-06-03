# Processes + creates all data files for r shiny app
# **03 and 04 take a LONG time**

source(here::here("scripts", "01_read_data.R"))
source(here::here("scripts", "02_run_text2sdg.R"))
source(here::here("scripts", "03_identify_usc_authors.R"))
source(here::here("scripts", "04_get_usc_author_info.R"))