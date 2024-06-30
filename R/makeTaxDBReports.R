#' Create species-level database
#'
#'
#'
#' @param reports reports to create db for
#' @param taxonomizr_sql sqlite file for taxonomizr
#' @param db_filepath If provided, loads and saves existig db rather than making one from scratch
#' @return A named list containing fileds associated with the input taxon ID
#' @export

makeTaxDBReports <- function(reports, taxonomizr_sql, db_filepath = NULL) {

  # Handling taxids corresponding to unclassifies
  row0 <- c("unclassifed", 0, NA, "no rank", "unclassified", NA)
  row1 <- c("root", 1, NA, "no rank", "root", NA)

  # Create list of unique taxids
  taxids <- unique(reports$taxid)

  # If db filepath supplied, load db from that file and identify taxids from report that aren't already present
  if (!is.null(db_filepath)) {

    if (file.exists(db_filepath)) {

      db <- read.csv(db_filepath) %>%
        dplyr::distinct()

      taxids <- setdiff(taxids, db$taxid) %>%
        unique()

      # If no unique taxids, either there is an error or return the existing database unchanged
      if (as.numeric(length(taxids)) == 0) {

          print("All taxids already present in database")

          return(db)

          break()

        }

      # If no file already exists, make a new one
    } else {

      warning("Database file doesn't exist - creating new file")

      db <- data.frame(matrix(ncol = 6, nrow = 0)) %>%
        rbind(row0, row1)

      colnames(db) <- c("name", "taxid", "type", "rank", "name_speciesorhigher", "species_taxid")

      write.csv(db, db_filepath, quote = FALSE, row.names = FALSE)

    }

  }

  # Make db for the taxids
  taxonomy1 <- lapply(taxids, FUN = metathresholds::makeTaxDB, taxonomizr_sql = taxonomizr_sql) %>%
    do.call(rbind, .) %>%
    as.data.frame(.) %>%
    dplyr::filter(!is.na(taxid)) %>%
    dplyr::distinct()

  # Find details for species identified from strains
  extra_taxids <- na.omit(setdiff(taxonomy1$species_taxid, taxonomy1$taxid))

  taxonomy2 <- lapply(extra_taxids, FUN = metathresholds::makeTaxDB, taxonomizr_sql = taxonomizr_sql) %>%
    do.call(rbind, .) %>%
    as.data.frame(.) %>%
    dplyr::filter(!is.na(taxid)) %>%
    dplyr::distinct()

  taxonomy <- rbind(taxonomy1, taxonomy2)

  # If db filepath supplied, save new taxids to file and return the full db
  if (!is.null(db_filepath)) {

    write.table(taxonomy, db_filepath, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

    full_taxonomy <- rbind(db, taxonomy) %>%
      dplyr::distinct()

    return(full_taxonomy)

    # If not, just return the database we created
  } else {

    full_taxonomy <- rbind(taxonomy, row0, row1)

    return(taxonomy)

  }

}


