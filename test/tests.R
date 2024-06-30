devtools::document()
devtools::load_all()

library(metathresholds)

options(scipen = 999)

# Filepaths
samplesheet_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/samplesheet_metathresholds.csv"
taxonomizr_sql <- "C:/Users/Sarah Buddle/Documents/taxonomizr/nameNode.sqlite"
db_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/taxonomy/taxonomy_metathresholds.csv"
thresholds_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/thresholds_metathresholds.csv"
nothresholds_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/nothresholds_metathresholds.csv"
positive_species_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/positive_taxids.csv"

full_report_output_filepath <- "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/full_report_metathresholds_v6.csv"

exclude_taxids <- c("329852", "2681611") # taxids for internal controls

# Our thresholds applied
full_report_thres <- makeFullReport(samplesheet_filepath, taxonomizr_sql, db_filepath, thresholds_filepath, collapse_species = TRUE,
                                    keep_species_only = FALSE, positive_species_filepath = positive_species_filepath, virus_only = TRUE)

write.csv(full_report_thres, full_report_output_filepath, row.names = FALSE, quote = FALSE)

full_report_thres <- read.csv(full_report_output_filepath)

combined_dnarna_report_thres <- combineDNARNA(full_report_thres)

counts_thres <- countSpecies(combined_dnarna_report_thres, positive_species_filepath) %>%
  dplyr::mutate(threshold = "Thresholds")

counts_thres_bt <- countSpecies(combined_dnarna_report_thres, positive_species_filepath, by_type = TRUE) %>%
  dplyr::mutate(threshold = "Thresholds")

# No threshold applied
# full_report_nothres <- makeFullReport(samplesheet_filepath, taxonomizr_sql, db_filepath, nothresholds_filepath, collapse_species = TRUE,
#                               positive_species_filepath = positive_species_filepath)

full_report_nothres <- full_report_thres %>%
  dplyr::mutate(result = ifelse(is.na(result), NA, "positive")) %>%
  dplyr::mutate(result_category = ifelse(result_category %in% c("true_positive", "false_negative"), "true_positive", "false_positive")) %>%
  dplyr::mutate(result_category = ifelse(is.na(result), NA, result_category))

combined_dnarna_report_nothres <- combineDNARNA(full_report_nothres)

counts_nothres <- countSpecies(combined_dnarna_report_nothres, positive_species_filepath) %>%
  dplyr::mutate(threshold = "No thresholds", false_negative = 0, true_negative = 0)

counts_nothres_bt <- countSpecies(combined_dnarna_report_nothres, positive_species_filepath, by_type = TRUE) %>%
  dplyr::mutate(threshold = "No thresholds", false_negative = 0, true_negative = 0)




# Only species in mock community
mock_community <- combined_dnarna_report_thres %>%
  dplyr::filter(result_category %in% c("true_positive", "false_negative"))


controls <- read.csv("C:/Users/Sarah Buddle/OneDrive - University College London/PhD/Nanopore/corresponding_controls_metathresholds.csv") %>%
  dplyr::select(dnarna_pair, run, dilution, repeat.) %>%
  dplyr::distinct()

# Combine
counts <- rbind(counts_nothres, counts_thres) %>%
  dplyr::arrange(dnarna_pair, tool, threshold) %>%
  tidyr::separate(col = dnarna_pair, into = c("dnarna_pair", "run"), sep = "sub_") %>%
  dplyr::mutate(dnarna_pair = gsub("_$", "_sub", dnarna_pair)) %>%
  dplyr::left_join(controls)

write.csv(counts, "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/species_counts_metathresholds_v6.csv",
          row.names = FALSE, quote = FALSE)

counts_bt <- rbind(counts_nothres_bt, counts_thres_bt) %>%
  dplyr::arrange(dnarna_pair, tool, threshold) %>%
  tidyr::separate(col = dnarna_pair, into = c("dnarna_pair", "run"), sep = "sub_") %>%
  dplyr::mutate(dnarna_pair = gsub("_$", "_sub", dnarna_pair)) %>%
  dplyr::left_join(controls)

write.csv(counts_bt, "W:/home/sbuddle/mnt/BTRU-scratch/sarah/results/virus_methods/species_counts_metathresholds_bytype_v6.csv",
          row.names = FALSE, quote = FALSE)

proportion = "proportion_nonhuman_classified"
positive_species_filepath = NULL
reads_thres_bac = 0
reads_thres_vir = 0
reads_thres_fun = 0
reads_thres_euk = 0
rpm_ratio_thres_bac = 0
rpm_ratio_thres_vir = 0
rpm_ratio_thres_fun = 0
rpm_ratio_thres_euk = 0
proportion_thres_bac = 0
proportion_thres_vir = 0
proportion_thres_fun = 0
proportion_thres_euk = 0
low_level_bac = FALSE
low_level_vir = FALSE
low_level_fun = FALSE
low_level_euk = FALSE
collapse_species = TRUE


# full_report_thres2 <- full_report_thres %>%
#   dplyr::mutate(sample_id = ifelse(dnarna == "DNARNA", gsub("_sub", "_sub_twist_101123", sample_id), sample_id)) %>%
#   dplyr::mutate(sample_id = ifelse(grepl("barcode", sample_id), gsub("_sub", "_sub_nanopore_270923", sample_id), sample_id)) %>%
#   dplyr::mutate(dnarna_pair = ifelse(dnarna == "DNARNA", gsub("_sub", "_sub_twist_101123", dnarna_pair), dnarna_pair)) %>%
#   dplyr::mutate(dnarna_pair = ifelse(grepl("barcode", dnarna_pair), gsub("_sub", "_sub_nanopore_270923", dnarna_pair), dnarna_pair))


