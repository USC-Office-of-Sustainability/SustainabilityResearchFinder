import pandas as pd

# --- Load data ----------------------------------------------------------------
df = pd.read_csv('usc_authors_2023_25_combined_dept_data.csv')

# --- Normalize names ----------------------------------------------------------
df['firstname'] = df['firstname'].fillna('')
df['lastname'] = df['lastname'].fillna('')

# Strip to first word of first name only (handles middle initials like "Justin R." -> "justin")
df['first_token'] = df['firstname'].apply(
    lambda x: x.split()[0].rstrip('.').lower() if x.strip() else ''
)
df['lower_last'] = df['lastname'].str.lower()

# Remove rows with empty names
df = df[(df['first_token'] != '') & (df['lower_last'] != '')]

# --- Find duplicate name groups -----------------------------------------------
# Groups where the same first+last name has more than one authorID
multi_id = df.groupby(['first_token', 'lower_last']).filter(
    lambda g: g['authorID'].nunique() > 1
)

# Keep only groups that have at least one Other/Other AND at least one real dept
has_other = multi_id.groupby(['first_token', 'lower_last']).filter(
    lambda g: (
        ((g['Departments'] == 'Other') & (g['Divisions'] == 'Other')).any() and
        (~((g['Departments'] == 'Other') & (g['Divisions'] == 'Other'))).any()
    )
)

# --- Categorize into Category 1 and Category 2 --------------------------------
cat1_rows = []  # Same division for all real-dept IDs -> safe to auto-merge
cat2_rows = []  # Everything else -> needs manual verification

for (ft, ll), group in has_other.groupby(['first_token', 'lower_last']):
    real_rows = group[~((group['Departments'] == 'Other') & (group['Divisions'] == 'Other'))]
    real_ids = real_rows['authorID'].unique()
    real_divs = real_rows['Divisions'].unique()

    if len(real_ids) > 1 and len(real_divs) == 1:
        # Multiple real-dept IDs but all in same division -> Category 1
        cat1_rows.append(group)
    else:
        # Ambiguous -> Category 2
        cat2_rows.append(group)

# --- Export results -----------------------------------------------------------
output_cols = ['firstname', 'lastname', 'authorID', 'Departments', 'Divisions', 
               'num_pubs', 'sdgs', 'keywords']

if cat1_rows:
    cat1 = pd.concat(cat1_rows).drop(columns=['first_token', 'lower_last'])
    cat1 = cat1[output_cols]
    cat1.to_csv('category1_same_division_duplicates.csv', index=False)
    print(f'Category 1 (same division, safe to merge): {len(cat1)} rows written to category1_same_division_duplicates.csv')
else:
    print('No Category 1 cases found.')

if cat2_rows:
    cat2 = pd.concat(cat2_rows).drop(columns=['first_token', 'lower_last'])
    cat2 = cat2[output_cols]
    cat2.to_csv('category2_other_remaining_duplicates.csv', index=False)
    print(f'Category 2 (needs manual review): {len(cat2)} rows written to category2_other_remaining_duplicates.csv')
else:
    print('No Category 2 cases found.')
