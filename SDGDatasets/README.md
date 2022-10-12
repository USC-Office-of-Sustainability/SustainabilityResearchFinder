# functions.R
For publications:  
Input: downloaded CSV files (from Scopus or Google Drive) in /DownloadedData where the file name contains "SDG#" or the SDG is a column in the CSV  
Output: CSV files with the same name in /FormattedData with two columns: SDG and Text as "[TITLE]...[KEYWORDS]...[ABSTRACT]..."

For suggested keywords:  
Reads a CSV file of keywords and split the SDGs/goals such that each row only has 1 SDG that share the same keyword. Outputs a CSV file with two columns: SDG and keyword.