#' Create species-level database
#'
#'
#'
#' @param taxid Taxon ID
#' @param result
#' @param positive_taxids list of true positive taxids
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
