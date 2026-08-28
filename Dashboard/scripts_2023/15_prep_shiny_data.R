# =============================================================================
# 15_prep_shiny_data.R
# Pre-compute ALL data the Shiny app needs and write to shiny_app/ as CSVs.
# Run this script (or include it in run_all.R) whenever the upstream data
# changes.  The Shiny app then only reads static files — zero heavy wrangling
# at runtime.
# =============================================================================

library(dplyr)
library(tidyr)
library(reshape2)
library(here)

# ---------------------------------------------------------------------------
# 0. Paths — adjust if your project layout differs
# ---------------------------------------------------------------------------
DATA_IN  <- here("shiny_app")   # where numbered pipeline outputs live
SHINY    <- here("shiny_app")        # destination folder read by app.R

# ---------------------------------------------------------------------------
# 1. Load base tables  (same join the old app.R did at startup)
# ---------------------------------------------------------------------------
message("Loading base tables...")

usc_authors <- read.csv(file.path(DATA_IN, "14_authors_dept_corrected.csv"),
                        stringsAsFactors = FALSE) %>%
  rename(Division = Div, Department = Dept)

usc_bridge <- read.csv(file.path(SHINY, "07_bridge_manual_edited.csv"),
                       stringsAsFactors = FALSE)

usc_pubs_sdgs <- read.csv(file.path(DATA_IN, "09_pubs_sdg_manual_fixed.csv"),
                           stringsAsFactors = FALSE) %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

dei_joined_raw <- read.csv(file.path(DATA_IN, "10_dei_pubs_ordered.csv"),
                            stringsAsFactors = FALSE)

# Core join used by almost every chart
tmp        <- merge(usc_pubs_sdgs, usc_bridge, by = c("pubID", "Link"))
usc_joined <- merge(tmp, usc_authors, by = "authorID")

message("Base tables loaded. Rows in usc_joined: ", nrow(usc_joined))

# Keep only authors with publications in the active five-year data.
# The Shiny app will not need to load historical-only authors.
active_authors <- usc_authors %>%
  semi_join(
    usc_joined %>% distinct(authorID),
    by = "authorID"
  )

write.csv(
  active_authors,
  file.path(SHINY, "precomp_active_authors.csv"),
  row.names = FALSE
)

message(
  "Active authors saved: ",
  n_distinct(active_authors$authorID),
  " author IDs"
)

# ---------------------------------------------------------------------------
# 2. Helpers
# ---------------------------------------------------------------------------

# Binarise SDG columns (0/1) without touching the source frame
binarise_sdgs <- function(df) {
  df %>% mutate(across(starts_with("SDG"), ~ as.integer(. != 0)))
}

# Assign the single highest-priority sustainability category
one_cat <- function(all_cats_string) {
  dplyr::case_when(
    grepl("Focused",   all_cats_string) ~ "Sustainability-Focused",
    grepl("Inclusive", all_cats_string) ~ "Sustainability-Inclusive",
    grepl("SDG-Related", all_cats_string) ~ "SDG-Related",
    TRUE                                ~ "Not Related"
  )
}

# ---------------------------------------------------------------------------
# 3. Tab 2 (SDG word-cloud tab) — SDG total by year
#    One row per SDG × Year with count of publications that mention that SDG
# ---------------------------------------------------------------------------
message("Computing: sdg_totals_by_year.csv ...")

sdg_by_year <- usc_pubs_sdgs %>%
  binarise_sdgs() %>%
  group_by(Year) %>%
  summarise( across(  starts_with("SDG"), function(x) sum(x, na.rm = TRUE)  ), .groups = "drop")

# Pivot to long form: Year | sdg_num | n
sdg_by_year_long <- sdg_by_year %>%
  pivot_longer(starts_with("SDG"), names_to = "sdg_col", values_to = "n") %>%
  mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
  select(Year, sdg_num, n)

write.csv(sdg_by_year_long,
          file.path(SHINY, "precomp_sdg_totals_by_year.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Tab 3 — year_sdg_barplot  (SDG counts per year, for barplot)
#    Same data as above but keep wide for the bar chart pivot
# ---------------------------------------------------------------------------
message("Computing: year_sdg_barplot_data.csv ...")

year_sdg_bar <- usc_pubs_sdgs %>%
  binarise_sdgs() %>%
  group_by(Year) %>%
  summarise(across(starts_with("SDG"), sum, na.rm = TRUE), .groups = "drop")

write.csv(year_sdg_bar,
          file.path(SHINY, "precomp_year_sdg_barplot.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 5. Tab 3 — pie2_plotly  (scholars by sustainability category × year)
# ---------------------------------------------------------------------------
message("Computing: scholars_by_sust_cat_year.csv ...")

scholars_sust_year <- usc_joined %>%
  group_by(authorID, Year) %>%
  summarise(
    all_cats = paste(unique(sustainability_category), collapse = ";"),
    .groups = "drop"
  ) %>%
  mutate(one_sustainability_category = one_cat(all_cats)) %>%
  mutate(one_sustainability_category = factor(
    one_sustainability_category,
    levels = c("Sustainability-Focused", "Sustainability-Inclusive",
               "SDG-Related", "Not Related")
  )) %>%
  group_by(Year, one_sustainability_category) %>%
  count() %>%
  ungroup()

write.csv(scholars_sust_year,
          file.path(SHINY, "precomp_scholars_sust_cat_year.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 6. Tab 3 — pie3_plotly  (departments by sustainability category × year)
# ---------------------------------------------------------------------------
message("Computing: depts_by_sust_cat_year.csv ...")

depts_sust_year <- usc_joined %>%
  group_by(Department, Year) %>%
  summarise(
    all_cats = paste(unique(sustainability_category), collapse = ";"),
    .groups = "drop"
  ) %>%
  mutate(one_sustainability_category = one_cat(all_cats)) %>%
  mutate(one_sustainability_category = factor(
    one_sustainability_category,
    levels = c("Sustainability-Focused", "Sustainability-Inclusive",
               "SDG-Related", "Not Related")
  )) %>%
  group_by(Year, one_sustainability_category) %>%
  count() %>%
  ungroup()

write.csv(depts_sust_year,
          file.path(SHINY, "precomp_depts_sust_cat_year.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 7. Tab 4 — pubs_to_bar  (dept × SDG counts, per division)
#    Wide format: Division | Department | SDG.01 … SDG.17
# ---------------------------------------------------------------------------
message("Computing: dept_sdg_counts_by_division.csv ...")

# dept_sdg_div <- usc_joined %>%
#   select(Division, Department, starts_with("SDG")) %>%
#   binarise_sdgs() %>%
#   group_by(Division, Department) %>%
#   summarise(across(starts_with("SDG"), sum, na.rm = TRUE), .groups = "drop")

# Count each publication once per department, even when several authors
# from that department worked on the same publication.
dept_sdg_div <- usc_joined %>%
  select(Division, Department, pubID, starts_with("SDG")) %>%
  distinct(Division, Department, pubID, .keep_all = TRUE) %>%
  binarise_sdgs() %>%
  group_by(Division, Department) %>%
  summarise(
    across(starts_with("SDG"), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(dept_sdg_div,
          file.path(SHINY, "precomp_dept_sdg_by_division.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 8. Tab 4 — pubs_to_treemap  (division-level SDG sums)
# ---------------------------------------------------------------------------
message("Computing: division_sdg_sums.csv ...")

# div_sdg_sum <- usc_joined %>%
#   select(Division, starts_with("SDG")) %>%
#   binarise_sdgs() %>%
#   group_by(Division) %>%
#   summarise(across(starts_with("SDG"), sum, na.rm = TRUE), .groups = "drop")

# Count each publication once per division, even when it is connected
# to multiple authors or departments in that division.
div_sdg_sum <- usc_joined %>%
  select(Division, pubID, starts_with("SDG")) %>%
  distinct(Division, pubID, .keep_all = TRUE) %>%
  binarise_sdgs() %>%
  group_by(Division) %>%
  summarise(
    across(starts_with("SDG"), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(div_sdg_sum,
          file.path(SHINY, "precomp_division_sdg_sums.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 9. Tab 5 — top_authors_sdg_table  (pub count per author × SDG × Division)
# ---------------------------------------------------------------------------
message("Computing: author_sdg_pubcounts.csv ...")

# author_sdg_pubs <- usc_joined %>%
#   select(Division, authorID, name, pubID, Link, starts_with("SDG")) %>%
#   distinct(Division, authorID, name, pubID, Link, .keep_all = TRUE) %>%
#   pivot_longer(starts_with("SDG"), names_to = "sdg_col", values_to = "val") %>%
#   filter(val != 0) %>%
#   mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
#   group_by(Division, authorID, name, sdg_num) %>%
#   summarise(n_pubs = n(), .groups = "drop")
# Keep publication IDs so the app can count each publication only once,
# even when an author belongs to multiple selected divisions.

author_sdg_pubs <- usc_joined %>%
  select(Division, authorID, name, pubID, Link, starts_with("SDG")) %>%
  distinct(Division, authorID, name, pubID, Link, .keep_all = TRUE) %>%
  pivot_longer(
    starts_with("SDG"),
    names_to = "sdg_col",
    values_to = "val"
  ) %>%
  filter(val != 0) %>%
  mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
  select(Division, authorID, name, pubID, sdg_num) %>%
  distinct()

write.csv(author_sdg_pubs,
          file.path(SHINY, "precomp_author_sdg_pubcounts.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 10. Tab 5 — top_authors_keywords_plot  (keyword sum per author × SDG × Division)
# ---------------------------------------------------------------------------
message("Computing: author_sdg_keyword_sums.csv ...")

# author_sdg_kw <- usc_joined %>%
#   select(Division, authorID, name, starts_with("SDG")) %>%
#   pivot_longer(starts_with("SDG"), names_to = "sdg_col", values_to = "val") %>%
#   filter(val != 0) %>%
#   mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
#   group_by(Division, authorID, name, sdg_num) %>%
#   summarise(kw_sum = sum(val, na.rm = TRUE), .groups = "drop")
# 
# write.csv(author_sdg_kw,
#           file.path(SHINY, "precomp_author_sdg_keyword_sums.csv"),
#           row.names = FALSE)

# Keep publication IDs so keyword counts are not repeated when an author
# belongs to multiple departments or divisions.
author_sdg_kw <- usc_joined %>%
  select(Division, authorID, name, pubID, starts_with("SDG")) %>%
  distinct(Division, authorID, name, pubID, .keep_all = TRUE) %>%
  pivot_longer(
    starts_with("SDG"),
    names_to = "sdg_col",
    values_to = "val"
  ) %>%
  filter(val != 0) %>%
  mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
  select(Division, authorID, name, pubID, sdg_num, val) %>%
  distinct()

write.csv(
  author_sdg_kw,
  file.path(SHINY, "precomp_author_sdg_keyword_sums.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 11. Tab 5 — top_departments_sdg_table  (dept pub count × SDG × Division)
# ---------------------------------------------------------------------------
message("Computing: dept_sdg_pubcounts.csv ...")

dept_sdg_pubs <- usc_joined %>%
  filter(Department != "", Department != "Other") %>%
  select(Division, Department, pubID, starts_with("SDG")) %>%
  distinct() %>%
  pivot_longer(starts_with("SDG"), names_to = "sdg_col", values_to = "val") %>%
  filter(val != 0) %>%
  mutate(sdg_num = as.integer(sub("SDG\\.0*", "", sdg_col))) %>%
  count(Division, Department, sdg_num, name = "n_pubs")

write.csv(dept_sdg_pubs,
          file.path(SHINY, "precomp_dept_sdg_pubcounts.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 12. Tab 5 — pub_by_school_sdg_table  (full publication rows per SDG × Division)
#     Heavy table — pre-filter & pre-process abstract truncation here
# ---------------------------------------------------------------------------
message("Computing: pubs_by_sdg_division.csv ...")

truncate_abstract <- function(x, n_words = 50) {
  words <- strsplit(x, " ")[[1]]
  if (length(words) <= n_words) x else paste0(paste(words[1:n_words], collapse = " "), "...")
}

pubs_sdg_div <- usc_joined %>%
  group_by(pubID, Division, sustainability_category, all_SDGs, Titles,
           Year, Source.title, Cited.by, Abstract, Open.Access) %>%
  summarise(
    Authors   = paste(sort(unique(name)),     collapse = "; "),
    Divisions = paste(sort(unique(Division)), collapse = "; "),
    .groups = "drop"
  ) %>%
  # keep SDG columns for server-side filtering
  left_join(
    usc_pubs_sdgs %>% select(pubID, starts_with("SDG")),
    by = "pubID"
  ) %>%
  distinct(pubID, Division, .keep_all = TRUE) %>%
  arrange(desc(Year))

# truncate abstracts once
pubs_sdg_div$Abstract <- sapply(pubs_sdg_div$Abstract, truncate_abstract)

write.csv(pubs_sdg_div,
          file.path(SHINY, "precomp_pubs_by_sdg_division.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 13. Tab 6 — author_sdg_barplot  (per-author SDG binary pub counts)
# ---------------------------------------------------------------------------
message("Computing: author_sdg_bar.csv ...")

author_sdg_bar <- usc_joined %>%
  select(authorID, pubID, starts_with("SDG")) %>%
  distinct(authorID, pubID, .keep_all = TRUE) %>%
  mutate(across(starts_with("SDG"), ~ as.integer(. != 0))) %>%
  group_by(authorID) %>%
  summarise(across(starts_with("SDG"), sum, na.rm = TRUE), .groups = "drop")

write.csv(author_sdg_bar,
          file.path(SHINY, "precomp_author_sdg_bar.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 14. Tab 6 — author_pub_table  (publications per author)
# ---------------------------------------------------------------------------
message("Computing: author_pub_table.csv ...")

author_pubs <- usc_joined %>%
  select(authorID, all_SDGs, Titles, Year, Link) %>%
  distinct() %>%
  mutate(url = paste0("<a href='", Link, "' target='_blank'>", Link, "</a>")) %>%
  select(-Link) %>%
  arrange(authorID, desc(Year))

write.csv(author_pubs,
          file.path(SHINY, "precomp_author_pubs.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 15. Tab 8 — dei_table  (pre-grouped DEI publications)
# ---------------------------------------------------------------------------
message("Computing: dei_table.csv ...")

dei_table <- dei_joined_raw %>%
  mutate(Titles_linked = paste0("<a href='", Link, "' target='_blank'>", Titles, "</a>")) %>%
  group_by(pubID) %>%
  mutate(
    Authors   = paste(sort(unique(name)), collapse = "; "),
    Divisions = paste(sort(unique(Div)),  collapse = "; ")
  ) %>%
  ungroup() %>%
  # truncate abstracts
  mutate(Abstract = sapply(Abstract, truncate_abstract)) %>%
  select(DEI_3.3_keywords, sustainability_category, all_SDGs,
         Titles_linked, Authors, Divisions, Year,
         Source.title, Cited.by, Abstract, Open.Access) %>%
  distinct()

write.csv(dei_table,
          file.path(SHINY, "precomp_dei_table.csv"),
          row.names = FALSE)

# Keep a plain version for download (no HTML links)
dei_download <- dei_joined_raw %>%
  group_by(pubID) %>%
  mutate(
    Authors   = paste(sort(unique(name)), collapse = "; "),
    Divisions = paste(sort(unique(Div)),  collapse = "; ")
  ) %>%
  ungroup() %>%
  distinct()

write.csv(dei_download,
          file.path(SHINY, "precomp_dei_download.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 16. Tab 3 — year choices list (needed for selectInput)
# ---------------------------------------------------------------------------
message("Computing: year_choices.csv ...")

write.csv(
  data.frame(Year = sort(unique(usc_pubs_sdgs$Year))),
  file.path(SHINY, "precomp_year_choices.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
message("===== 15_prep_shiny_data.R complete. All precomp_ CSVs written to: ", SHINY, " =====")
