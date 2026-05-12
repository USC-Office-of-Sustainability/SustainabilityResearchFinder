library(dplyr)

# ==============================================================================
# 02_identify_usc_authors.R
# For new publications (pubID > max of historical data), split each paper's
# semicolon-delimited author fields into one-row-per-author, determine which
# authors are USC-affiliated, and parse their names.
#
# Outputs:
#   - 02_usc_authors_identified.csv   (one row per author-publication pair)
#   - 02_bridge_pubid_authorid.csv     (pubID-Link-authorID mapping)
# ==============================================================================

# --- Config -------------------------------------------------------------------
HISTORICAL_MAX_PUBID <- 25915  # max pubID in all_usc_pubs_2020_2021_2022.csv

USC_REGEX <- paste0(
  "([uU]ni[versity\\.]{0,} ([oO]f )?[sS]ou?th[ernm]{0,}[,-]? ?[Cc]alifornia)",
  "|([kK]eck [Ss]chool [Oo]f [Mm]edicine)",
  "|([Kk]eck [Mm]edical [Cc]enter)",
  "|([Kk]eck [Mm]edical [Ss]chool)"
)

# Historical USC author IDs — used to rescue initials-only authors whose
# affiliation is missing in all new publications but are known USC authors.
historical_authorIDs <- read.csv("data_processed/author_lookup_table.csv") %>%
  pull(authorID) %>% as.character() %>% unique()

# --- Read and filter to new publications only ---------------------------------
pubs <- read.csv("data_processed/01_all_usc_pubs.csv")
focused_pubs <- pubs %>% filter(pubID > HISTORICAL_MAX_PUBID)
cat("New publications to process:", nrow(focused_pubs), "\n")

# --- Clean Authors.with.affiliations ------------------------------------------
focused_pubs$Authors.with.affiliations <- focused_pubs$Authors.with.affiliations %>%
  gsub("&amp;", "&", .) %>%
  gsub(";,", ",", .)

# --- Data integrity checks ----------------------------------------------------
cat("\n=== Data Integrity Checks ===\n")

# Check 1: Authors and Author.s..ID should have same count per row
#   Author.full.names may be shorter (Scopus 2025 skips initials-only authors)
check1 <- apply(focused_pubs, 1, function(x) {
  n_name <- length(strsplit(x["Authors"], ";")[[1]])
  n_full <- length(strsplit(x["Author.full.names"], ";")[[1]])
  n_id   <- length(strsplit(x["Author.s..ID"], ";")[[1]])
  c(name_id_match = (n_name == n_id), full_short = (n_full < n_name))
})
check1_df <- as.data.frame(t(check1))
cat("Check 1a - Authors/IDs count match:",
    sum(check1_df$name_id_match), "/", nrow(focused_pubs), "rows OK\n")
n_short <- sum(check1_df$full_short)
if (n_short > 0) {
  cat("Check 1b - Author.full.names shorter than Authors:", n_short,
      "rows (will be padded with empty strings)\n")
} else {
  cat("Check 1b - Author.full.names count: all match\n")
}

# Check 2: Author.full.names vs Authors.with.affiliations count match
same_len_name_aff <- apply(focused_pubs, 1, function(x) {
  length(strsplit(x["Author.full.names"], ";")[[1]]) ==
    length(strsplit(x["Authors.with.affiliations"], ";")[[1]])
})
cat("Check 2 - FullNames/Affiliations count match:",
    sum(same_len_name_aff), "/", nrow(focused_pubs), "rows OK\n")

# Check 3: Empty/NA fields
cat("Check 3 - Empty/NA fields:\n")
for (col in c("Authors.with.affiliations", "Author.full.names", "Authors", "Author.s..ID")) {
  n_na    <- sum(is.na(focused_pubs[[col]]))
  n_empty <- sum(focused_pubs[[col]] == "", na.rm = TRUE)
  if (n_na > 0 || n_empty > 0) {
    cat("  WARNING:", col, "- NA:", n_na, "/ empty:", n_empty, "\n")
  } else {
    cat("  ", col, "- OK\n")
  }
}
cat("=== End Checks ===\n\n")

# --- Skip publications with completely empty author IDs ----------------------
# Some Scopus records have completely empty author fields (e.g. pubID 31360).
# Only skip when Author.s..ID is empty — without IDs we can't process at all.
empty_ids <- is.na(focused_pubs$Author.s..ID) | focused_pubs$Author.s..ID == ""
if (sum(empty_ids) > 0) {
  cat("Skipping", sum(empty_ids), "publications with empty Author.s..ID:",
      paste(focused_pubs$pubID[empty_ids], collapse = ", "), "\n")
}
focused_pubs <- focused_pubs[!empty_ids, ]

# --- Identify publications where affiliations are unusable --------------------
# 1) Empty Authors.with.affiliations — use author names as placeholder
# 2) Multi-author papers where every author's affiliation entry is identical to
#    the shared Affiliations field — can't distinguish per-author affiliations.
empty_aff <- is.na(focused_pubs$Authors.with.affiliations) |
             focused_pubs$Authors.with.affiliations == ""
ignore_pubIDs_empty <- focused_pubs$pubID[empty_aff]

ignore_pubIDs_dup <- focused_pubs$pubID[apply(focused_pubs, 1, function(x) {
  if (is.na(x["Authors.with.affiliations"]) || x["Authors.with.affiliations"] == "") return(FALSE)
  affl_vec  <- strsplit(x["Authors.with.affiliations"], ";")[[1]]
  n_authors <- length(strsplit(x["Author.s..ID"], ";")[[1]])
  n_affls   <- length(strsplit(x["Affiliations"], ";")[[1]])
  if (n_authors == 1 || n_affls == 1) return(FALSE)
  all(grepl(gsub("[[:punct:]]", " ", x["Affiliations"]),
            gsub("[[:punct:]]", " ", affl_vec), fixed = TRUE))
})]

ignore_pubIDs <- unique(c(ignore_pubIDs_empty, ignore_pubIDs_dup))
cat("Publications with empty affiliations:", length(ignore_pubIDs_empty), "\n")
cat("Publications with unusable per-author affiliations:", length(ignore_pubIDs_dup), "\n")

# --- Split each publication into one row per author ---------------------------
split_authors <- function(x) {
  auth_name       <- trimws(strsplit(x["Authors"], ";")[[1]])
  auth_full_names <- trimws(strsplit(x["Author.full.names"], ";")[[1]])
  auth_affl_vec   <- trimws(strsplit(x["Authors.with.affiliations"], ";")[[1]])
  auth_ids        <- trimws(strsplit(x["Author.s..ID"], ";")[[1]])
  pub  <- x["pubID"]
  link <- x["Link"]
  n <- length(auth_ids)  # canonical count

  # 2025 Scopus data sometimes has fewer Author.full.names entries than IDs
  # (authors with only initials get skipped). Align by matching the embedded
  # Scopus ID in each full_name entry to the correct position in auth_ids.
  if (length(auth_full_names) < n) {
    aligned <- rep("", n)
    for (j in seq_along(auth_full_names)) {
      id_in_name <- regmatches(auth_full_names[j], regexpr("[0-9]+", auth_full_names[j]))
      if (length(id_in_name) > 0) {
        pos <- which(auth_ids == id_in_name)
        if (length(pos) == 1) aligned[pos] <- auth_full_names[j]
      }
    }
    auth_full_names <- aligned
  }

  if (pub %in% ignore_pubIDs) {
    # Affiliations are unusable — fill with author name as placeholder
    return(data.frame(auth_name, auth_full_names,
                      auth_affl_vec = auth_name, auth_ids,
                      pubID = pub, Link = link, stringsAsFactors = FALSE))
  }

  if (length(auth_name) == length(auth_affl_vec)) {
    # Normal case: authors and affiliations align 1:1
    return(data.frame(auth_name, auth_full_names, auth_affl_vec, auth_ids,
                      pubID = pub, Link = link, stringsAsFactors = FALSE))
  }

  # Mismatch: match each author to their affiliation by last name
  res <- data.frame(auth_name = character(0), auth_full_names = character(0),
                    auth_affl_vec = character(0), auth_ids = character(0),
                    pubID = character(0), Link = character(0),
                    stringsAsFactors = FALSE)
  for (i in seq_along(auth_name)) {
    lastname <- strsplit(auth_full_names[i], ",")[[1]][1]
    idx <- grep(paste0("^", lastname), auth_affl_vec)[1]
    affl <- if (!is.na(idx)) auth_affl_vec[idx] else NA_character_
    if (!is.na(idx)) auth_affl_vec <- auth_affl_vec[-idx]
    res[nrow(res) + 1, ] <- list(auth_name[i], auth_full_names[i], affl,
                                  auth_ids[i], pub, link)
  }
  res
}

all_authors_df <- do.call(rbind, apply(focused_pubs, 1, split_authors))

# --- Filter to USC-affiliated authors -----------------------------------------
# If an author is USC-affiliated in ANY publication, keep ALL their publications.
# Additionally, rescue authors whose authorID appears in the historical lookup
# table but have no detectable USC affiliation in new data (e.g. initials-only
# authors whose affiliation field is empty in all new publications).
all_authors_df$USC <- grepl(USC_REGEX, all_authors_df$auth_affl_vec)

usc_authors <- all_authors_df %>%
  group_by(auth_ids) %>%
  mutate(
    USCfinal          = any(USC),
    rescued_by_lookup = !any(USC) & any(as.character(auth_ids) %in% historical_authorIDs)
  ) %>%
  filter(USCfinal | rescued_by_lookup) %>%
  ungroup()

# --- Parse name fields from "Last, First (ID)" format ------------------------
# Extract authorID from auth_full_names; fall back to auth_ids for padded rows
# where auth_full_names is empty (2025 Scopus skips initials-only authors).
usc_authors$authorID <- sub(".*?(\\d+).*", "\\1", usc_authors$auth_full_names)
no_digit <- !grepl("\\d", usc_authors$auth_full_names)
usc_authors$authorID[no_digit] <- usc_authors$auth_ids[no_digit]
usc_authors$full_name  <- trimws(sub("\\(.*", "", usc_authors$auth_full_names))
usc_authors$last_name  <- sapply(usc_authors$full_name, function(x) {
  if (grepl(",", x)) return(strsplit(x, ",")[[1]][1])
  s <- strsplit(x, " ")[[1]]
  if (length(s) == 1) "" else s[1]
})
usc_authors$first_name <- sapply(usc_authors$full_name, function(x) {
  if (grepl(",", x)) return(trimws(strsplit(x, ",")[[1]][2]))
  s <- strsplit(x, " ")[[1]]
  if (length(s) == 1) s[1] else s[2]
})

# --- Save ---------------------------------------------------------------------
write.csv(usc_authors,
          here::here("data_processed/02_usc_authors_identified.csv"),
          row.names = FALSE)

bridge_table <- usc_authors %>% select(pubID, Link, authorID)
write.csv(bridge_table,
          here::here("data_processed/02_bridge_pubid_authorid.csv"),
          row.names = FALSE)

# --- Save rescued authors log -------------------------------------------------
rescued_log <- usc_authors %>%
  filter(rescued_by_lookup) %>%
  select(authorID = auth_ids, auth_name, pubID, Link) %>%
  distinct(authorID, pubID, .keep_all = TRUE)

write.csv(rescued_log,
          here::here("data_processed/02_rescued_authors_log.csv"),
          row.names = FALSE)

cat("\n=== Summary ===\n")
cat("Unique USC authors:", length(unique(usc_authors$authorID)), "\n")
cat("Author-pub pairs:", nrow(usc_authors), "\n")
cat("Publications covered:", length(unique(usc_authors$pubID)), "\n")
cat("Rescued from historical lookup (initials-only):",
    length(unique(rescued_log$authorID)), "authors /",
    nrow(rescued_log), "author-pub pairs\n")
