devtools::document()
devtools::load_all()

library(metathresholds)

# Filepaths
samplesheet_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/samplesheet_metathresholds.csv"
taxonomizr_sql <- "C:/Users/Sarah Buddle/Documents/taxonomizr/nameNode.sqlite"
db_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/taxonomy/taxonomy_metathresholds.csv"
thresholds_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/thresholds_metathresholds.csv"

full_report <- makeFullReport(samplesheet_filepath, taxonomizr_sql, db_filepath, thresholds_filepath, collapse_species = TRUE)


