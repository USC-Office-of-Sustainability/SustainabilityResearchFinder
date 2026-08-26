"""
Adds a 'publications' column (semicolon-separated list of paper titles)
to the Category 1 and Category 2 duplicate-author CSVs, using the
authorID -> titles mapping found in the raw Scopus export.

USAGE:
1. Put this script in the same folder as:
   - 01_all_usc_pubs.csv
   - category1_same_division_duplicates.csv
   - category2_other_remaining_duplicates.csv
2. Run:  python build_pub_lists.py
3. Two new files will be created in the same folder:
   - category1_same_division_duplicates_with_pubs.csv
   - category2_other_remaining_duplicates_with_pubs.csv

Requires: pandas  (pip install pandas)
"""

import pandas as pd
from collections import defaultdict

# --- File paths (edit these if your files are in a different location) ---
PUBS_FILE = '01_all_usc_pubs.csv'
CAT1_FILE = 'category1_same_division_duplicates.csv'
CAT2_FILE = 'category2_other_remaining_duplicates.csv'

CAT1_OUT = 'category1_same_division_duplicates_with_pubs.csv'
CAT2_OUT = 'category2_other_remaining_duplicates_with_pubs.csv'

# --- Load publication-level data ---
pubs = pd.read_csv(PUBS_FILE)

# --- Build authorID -> list of titles mapping ---
author_titles = defaultdict(list)

for _, row in pubs.iterrows():
    ids_raw = row['Author.s..ID']
    title = row['Titles']
    if pd.isna(ids_raw) or pd.isna(title):
        continue
    ids = [i.strip() for i in str(ids_raw).split(';') if i.strip()]
    for aid in ids:
        author_titles[aid].append(str(title).strip())

print(f"Total unique author IDs with publication titles: {len(author_titles)}")


def get_pub_string(author_id):
    """Look up the semicolon-joined publication titles for a given author ID."""
    if pd.isna(author_id):
        return ''
    aid = str(int(author_id))
    titles = author_titles.get(aid, [])
    return '; '.join(titles)


def process_category(input_file, output_file, label):
    df = pd.read_csv(input_file)
    df['publications'] = df['authorID'].apply(get_pub_string)

    missing = df[df['publications'] == '']
    print(f"{label}: {len(df)} rows, {len(missing)} with no publications found")
    if len(missing) > 0:
        cols = [c for c in ['firstname', 'lastname', 'authorID', 'num_pubs'] if c in missing.columns]
        print(missing[cols].to_string(index=False))

    df.to_csv(output_file, index=False)
    print(f"  -> saved to {output_file}\n")


process_category(CAT1_FILE, CAT1_OUT, "Category 1")
process_category(CAT2_FILE, CAT2_OUT, "Category 2")
