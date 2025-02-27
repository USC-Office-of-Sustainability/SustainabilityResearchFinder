# Master Script to Run All Data Processing Scripts in Sequence

# Define the folder containing the scripts
script_folder <- "scripts_2023/"

# List of scripts to run in the desired sequence
scripts <- c(
  "01_read_data.R",
  "02_identify_usc_authors.R",
  "03_identify_usc_dept_div.R",
  "04_combine_with_2020.R",
  "05_add_law_pubs.R",
  "06_combine_authors.R",
  "07_manual_edit_authors.R",
  "08_run_text2sdg.R",
  "09_manual_category_fix.R",
  "10_dei.R",
  "11_wordcloud.R",
  "12_stacked_bar_chart_data.R",
  "13_correct_final_div_dept.R"
)

# Function to source each script
run_scripts <- function(folder, script_list) {
  for (script in script_list) {
    script_path <- file.path(folder, script)
    message("Running: ", script_path)

    # Check if the file exists before running
    if (file.exists(script_path)) {
      tryCatch(
        {
          source(script_path, echo = TRUE)
        },
        error = function(e) {
          stop("Error in script '", script, "': ", e$message)
        }
      )
    } else {
      stop("Script not found: ", script_path)
    }
  }
}

# Run all scripts in sequence
run_scripts(script_folder, scripts)
