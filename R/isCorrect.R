#' Determines whether result predicted by thresholds is correct given list of true positive taxids. Only works at species level
#'
#'
#'
#' @param taxid Taxon ID
#' @param rank taxonomic rank e.g. species
#' @param result predicted result from thresholds
#' @param positive_species_taxids list of true positive taxids
#' @return A named list which will become the taxonomy database entry associated with the input taxon ID.
#' @export


isCorrect <- function(taxid, rank, result, positive_species_taxids) {

  if (!is.na(taxid) & !is.na(rank) & !is.na(result) & rank == "species") {

    if (taxid %in% positive_species_taxids & result == "positive") {

      return("true_positive")

    } else if (taxid %in% positive_species_taxids & result == "negative") {

      return("false_negative")

    } else if (!(taxid %in% positive_species_taxids) & result == "positive") {

      return("false_positive")

    } else if (!(taxid %in% positive_species_taxids) & result == "negative") {

      return("true_negative")

    }

  } else {

    return(NA)

  }

}

isCorrect <- Vectorize(isCorrect, vectorize.args = c("taxid", "rank", "result"))
