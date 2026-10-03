library(dplyr)
library(stringr)

# ==============================================================================
# 14_apply_dept_mapping.R
#
# Maintains a persistent Dept/Div mapping table and applies it to the final
# author data (data_processed/07_authors_manual_edited.csv).
#
# Each run:
#   1. Load existing mapping (data_raw/div_dept_mapping.csv), or start fresh.
#   2. Find any new unique (Dept, Div) pairs not yet in the mapping.
#   3. Auto-match new pairs against data_raw/usc_departments_new.csv:
#        a. exact      – Dept == Department.Group AND Div == School.Institute.Center
#        b. exact_dept – Dept == Department.Group (any Div)
#        c. pattern    – Dept matches the Pattern regex
#        d. fuzzy      – fuzzy string similarity >= FUZZY_THRESHOLD
#      If Dept == "Other": new_Dept stays "Other", Div resolved separately.
#      Auto-matched  → fill new_Dept/new_Div, manual_correction_required = FALSE
#      Unrecognised  → copy old values to new_Dept/new_Div,
#                      manual_correction_required = TRUE
#   4. Append new rows to mapping and save.
#   5. Apply all FALSE rows to author data (overwrite Dept and Div only).
#   6. Save updated author data to shiny_app/14_authors_dept_corrected.csv.
#
# To fix a flagged row:
#   Open data_raw/div_dept_mapping.csv, fill new_Div/new_Dept,
#   set manual_correction_required = FALSE, then re-run this script.
# ==============================================================================

AUTHORS_FILE   <- "data_processed/07_authors_manual_edited.csv"
AUTHORS_OUT    <- "shiny_app/14_authors_dept_corrected.csv"
NEW_DEPTS_FILE <- "data_raw/usc_departments_Mar_30_2026.csv"
MAPPING_FILE   <- "data_raw/div_dept_mapping.csv"
FUZZY_THRESHOLD <- 0.75

# ── load reference ─────────────────────────────────────────────────────────────
new_depts <- read.csv(NEW_DEPTS_FILE, stringsAsFactors = FALSE) %>%
  rename(canonical_Div  = School.Institute.Center,
         canonical_Dept = Department.Group)

canonical_depts <- unique(trimws(new_depts$canonical_Dept))
canonical_divs  <- unique(trimws(new_depts$canonical_Div))

# ── fuzzy helper ───────────────────────────────────────────────────────────────
best_fuzzy <- function(query, choices, threshold = FUZZY_THRESHOLD) {
  if (length(choices) == 0 || is.na(query) || query == "") return(NA_character_)
  dists  <- utils::adist(tolower(query), tolower(choices))[1, ]
  lens   <- pmax(nchar(query), nchar(choices))
  ratios <- 1 - dists / lens
  best_i <- which.max(ratios)
  if (ratios[best_i] >= threshold) choices[best_i] else NA_character_
}

# ── match a single Div to canonical list ──────────────────────────────────────
match_div <- function(old_div) {
  v <- trimws(old_div)
  if (v %in% canonical_divs)                     return(list(div = v,    method = "exact"))
  if (tolower(v) %in% tolower(canonical_divs)) {
    matched <- canonical_divs[tolower(canonical_divs) == tolower(v)][1]
    return(list(div = matched, method = "exact_ci"))
  }
  fuzzy_div <- best_fuzzy(v, canonical_divs)
  if (!is.na(fuzzy_div))                         return(list(div = fuzzy_div, method = "fuzzy"))
  return(list(div = NA_character_, method = "no_match"))
}

# ── match a single (Dept, Div) pair ───────────────────────────────────────────
match_pair <- function(old_dept, old_div) {
  d <- trimws(old_dept)
  v <- trimws(old_div)

  # Special case: Dept == "Other" — keep as "Other", resolve Div only
  if (d == "Other") {
    div_result <- match_div(v)
    if (!is.na(div_result$div)) {
      return(list(new_dept = "Other", new_div = div_result$div,
                  method = paste0("other+div_", div_result$method), manual = FALSE))
    } else {
      return(list(new_dept = "Other", new_div = v,
                  method = "other+div_no_match", manual = TRUE))
    }
  }

  # 1. Exact match on both Dept and Div
  exact <- new_depts[trimws(new_depts$canonical_Dept) == d &
                     trimws(new_depts$canonical_Div)  == v, ]
  if (nrow(exact) > 0)
    return(list(new_dept = exact$canonical_Dept[1], new_div = exact$canonical_Div[1],
                method = "exact", manual = FALSE))

  # 2. Exact match on Dept only
  exact_dept <- new_depts[trimws(new_depts$canonical_Dept) == d, ]
  if (nrow(exact_dept) > 0)
    return(list(new_dept = exact_dept$canonical_Dept[1], new_div = exact_dept$canonical_Div[1],
                method = "exact_dept", manual = FALSE))

  # 3. Pattern match (apply each row's Pattern regex against old Dept)
  for (i in seq_len(nrow(new_depts))) {
    pattern <- trimws(new_depts$Pattern[i])
    if (is.na(pattern) || pattern == "") next
    matched <- tryCatch(
      grepl(pattern, d, ignore.case = TRUE, perl = TRUE),
      error = function(e) FALSE
    )
    if (matched)
      return(list(new_dept = new_depts$canonical_Dept[i],
                  new_div  = new_depts$canonical_Div[i],
                  method = "pattern", manual = FALSE))
  }

  # 4. Fuzzy match on Dept name
  fuzzy_dept <- best_fuzzy(d, canonical_depts)
  if (!is.na(fuzzy_dept)) {
    r <- new_depts[trimws(new_depts$canonical_Dept) == fuzzy_dept, ][1, ]
    return(list(new_dept = r$canonical_Dept, new_div = r$canonical_Div,
                method = "fuzzy", manual = FALSE))
  }

  # No match — copy old values as-is, flag for manual review
  return(list(new_dept = d, new_div = v, method = "no_match", manual = TRUE))
}

# ── load or initialise mapping ─────────────────────────────────────────────────
if (file.exists(MAPPING_FILE)) {
  mapping <- read.csv(MAPPING_FILE, stringsAsFactors = FALSE)
  mapping$manual_correction_required <-
    as.logical(toupper(trimws(mapping$manual_correction_required)))
  cat(sprintf("Loaded existing mapping: %d rows  (%s)\n", nrow(mapping), MAPPING_FILE))
} else {
  mapping <- data.frame(
    old_Div = character(), old_Dept = character(),
    new_Div = character(), new_Dept = character(),
    match_method = character(), manual_correction_required = logical(),
    stringsAsFactors = FALSE
  )
  cat(sprintf("No mapping file found — will create %s\n", MAPPING_FILE))
}

# ── find new pairs in author data ──────────────────────────────────────────────
authors <- read.csv(AUTHORS_FILE, stringsAsFactors = FALSE)

all_pairs <- authors %>%
  distinct(Dept, Div) %>%
  rename(old_Dept = Dept, old_Div = Div)

existing_keys <- paste(mapping$old_Dept, mapping$old_Div, sep = "|||")
new_pairs <- all_pairs %>%
  filter(!paste(old_Dept, old_Div, sep = "|||") %in% existing_keys)

cat(sprintf("New (Dept, Div) pairs to process: %d\n", nrow(new_pairs)))

# ── match new pairs and append ─────────────────────────────────────────────────
if (nrow(new_pairs) > 0) {
  new_rows <- do.call(rbind, lapply(seq_len(nrow(new_pairs)), function(i) {
    result <- match_pair(new_pairs$old_Dept[i], new_pairs$old_Div[i])
    data.frame(
      old_Div                    = new_pairs$old_Div[i],
      old_Dept                   = new_pairs$old_Dept[i],
      new_Div                    = result$new_div,
      new_Dept                   = result$new_dept,
      match_method               = result$method,
      manual_correction_required = result$manual,
      stringsAsFactors = FALSE
    )
  }))

  n_auto   <- sum(!new_rows$manual_correction_required)
  n_manual <- sum( new_rows$manual_correction_required)
  cat(sprintf("  Auto-matched: %d  |  Needs manual review: %d\n", n_auto, n_manual))

  if (n_manual > 0) {
    cat("  Flagged pairs:\n")
    flagged <- new_rows[new_rows$manual_correction_required, ]
    for (i in seq_len(nrow(flagged))) {
      cat(sprintf("    [%s]  Dept=%s  Div=%s\n",
                  flagged$match_method[i], flagged$old_Dept[i], flagged$old_Div[i]))
    }
  }

  mapping <- bind_rows(mapping, new_rows)
} else {
  cat("No new pairs — mapping is already up to date.\n")
}

# ── save updated mapping (manual rows always at the bottom) ───────────────────
mapping <- mapping %>%
  arrange(manual_correction_required)   # FALSE (0) before TRUE (1)
write.csv(mapping, MAPPING_FILE, row.names = FALSE)
cat(sprintf("Mapping saved -> %s  (%d rows total)\n", MAPPING_FILE, nrow(mapping)))

# ── apply corrections to author data ──────────────────────────────────────────
corrections <- mapping %>%
  filter(manual_correction_required == FALSE)

authors_corrected <- authors %>%
  left_join(corrections %>% select(old_Dept, old_Div, new_Dept, new_Div),
            by = c("Dept" = "old_Dept", "Div" = "old_Div")) %>%
  mutate(
    Dept = ifelse(!is.na(new_Dept), new_Dept, Dept),
    Div  = ifelse(!is.na(new_Div),  new_Div,  Div)
  ) %>%
  select(-new_Dept, -new_Div) %>%
  select(Div, Dept, everything())

# ── drop redundant "Other" dept rows ──────────────────────────────────────────
# If an author has a non-Other Dept under the same Div, remove the Other row.
# If Other is the only Dept for that author x Div, keep it.
n_before <- nrow(authors_corrected)
authors_corrected <- authors_corrected %>%
  group_by(authorID, Div) %>%
  mutate(n_depts = n(), has_real = any(Dept != "Other")) %>%
  filter(!(Dept == "Other" & n_depts > 1 & has_real)) %>%
  select(-n_depts, -has_real) %>%
  ungroup()
n_removed  <- n_before - nrow(authors_corrected)
n_kept_other <- sum(authors_corrected$Dept == "Other")
cat(sprintf("Redundant 'Other' dept rows removed: %d\n", n_removed))
cat(sprintf("'Other' dept rows kept (sole dept for that author x Div): %d\n", n_kept_other))

n_corrected <- sum(
  paste(authors$Dept, authors$Div) !=
  paste(authors_corrected$Dept[seq_len(nrow(authors))], authors_corrected$Div[seq_len(nrow(authors))])
)
write.csv(authors_corrected, AUTHORS_OUT, row.names = FALSE)
cat(sprintf("Author rows updated: %d / %d\n", n_corrected, nrow(authors)))
cat(sprintf("Author data saved  -> %s\n", AUTHORS_OUT))

# ── final summary ──────────────────────────────────────────────────────────────
n_pending <- sum(mapping$manual_correction_required)
if (n_pending > 0) {
  cat(sprintf(
    "\nWARNING: %d row(s) in the mapping still need manual correction.\n", n_pending))
  cat(sprintf(
    "  Open %s, fill new_Div/new_Dept for those rows,\n", MAPPING_FILE))
  cat(
    "  set manual_correction_required=FALSE, then re-run this script.\n")
} else {
  cat("\nAll mapping rows applied — no manual corrections pending.\n")
}
