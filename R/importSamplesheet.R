#' Import samplesheet
#'
#'
#'
#' @param samplesheet_filepath filepath to samplesheet
#' @return A named list containing fieds associated with the input taxon ID
#' @export


importSamplesheet <- function(samplesheet_filepath) {

  samplesheet_colnames <- c("sample_id", "corresponding_control_id", "dnarna", "raw_read_count_1", "raw_read_count_2", "kraken2_filepath")

  samplesheet <- read.csv(samplesheet_filepath, strip.white = TRUE)

  if (!setequal(colnames(samplesheet), samplesheet_colnames)) {

    stop("Error: incorrect filenames in samplesheet")

  }

 # Should add some more conditions here

  samplesheet_clean <- samplesheet %>%
    dplyr::mutate(total_raw_reads_sample = raw_read_count_1 + raw_read_count_2) %>%
    dplyr::select(-c(raw_read_count_1, raw_read_count_2)) %>%
    tidyr::pivot_longer(contains("_filepath"), names_to = "tool", values_to = "filepath") %>%
    dplyr::mutate(tool = gsub("_filepath", "", tool))

  return(samplesheet_clean)

}
