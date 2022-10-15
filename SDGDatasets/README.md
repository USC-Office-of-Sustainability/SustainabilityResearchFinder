# SDGDatasets

## functions.R
For publications:  
Input: downloaded CSV files (from Scopus or Google Drive) in /DownloadedData where the file name contains "SDG#" or the SDG is a column in the CSV  
Output: CSV files with the same name in /FormattedData with two columns: SDG and Text as "[TITLE]...[KEYWORDS]...[ABSTRACT]..."

For suggested keywords:  
Reads a CSV file of keywords and splits the SDGs/goals such that each row only has 1 SDG that shares the same keyword. Outputs a CSV file with two columns: SDG and keyword.

## DownloadedData/
Data downloaded from Scopus or Google Drive

## FormattedData/
Outputs created by code.

## TestData/
Datasets used when writing/debugging