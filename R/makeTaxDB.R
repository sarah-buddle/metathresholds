#' Create species-level database
#'
#'
#'
#' @param taxid Taxon ID
#' @param taxonomizr_sql sql fro taxonomizr
#' @return A named list containing fileds associated with the input taxon ID
#' @export

makeTaxDB <- function(taxid, taxonomizr_sql) {

  suppressWarnings(
  rawTax <- taxonomizr::getRawTaxonomy(taxid, taxonomizr_sql)
  )

  superkingdom <- NA
  species <- NA
  type <- NA
  name <- NA
  rank <- NA
  species_taxid <- NA

  # Determine whether bacteria, virus, fungi etc

  if (!is.null(rawTax[[1]])) {

    if ("superkingdom" %in% names(rawTax[[1]])) {
      superkingdom <- rawTax[[1]][["superkingdom"]]
    }

    if ("species" %in% names(rawTax[[1]])) {
      species <- rawTax[[1]][["species"]]
    }

    if (!is.na(superkingdom) & superkingdom == "Archaea") {

      type <- "Archaea"

    } else if (!is.na(superkingdom) & superkingdom == "Viruses") {

      type <- "Virus"

    } else if (!is.na(superkingdom) & superkingdom == "Bacteria") {

      type <- "Bacteria"

    } else if (!is.na(superkingdom) & superkingdom == "Eukaryota") {

      if ("Fungi" %in% rawTax[[1]]) {
        type <- "Fungi"

      } else if (!is.na(species) & species == "Homo sapiens") {
        type <- "Human"

      } else {

        type <- "Other eukaryote"

      }

    } else {

      type <- NA
    }

    # Find taxonmic rak e.g. species, genus
    ranksRawTax <- names(rawTax[[1]][1])

    if (is.na(ranksRawTax)) {

      rank <- "unclassified"

    } else if (ranksRawTax == "no rank"){

      if ("strain" %in% names(rawTax[[1]]) | "species" %in% names(rawTax[[1]])){

        rank <- "strain"

      } else {

        rank <- ranksRawTax
      }

    } else {

      rank <- ranksRawTax

    }

    # Find species taxid
    if (!is.na(species)) {

      suppressWarnings(
      species_taxid <- taxonomizr::getId(species, taxonomizr_sql)
      )

    }

    # Find species name
    name <- rawTax[[1]][[1]]

  }

  if (is.na(species)) {

    species <- name

  }

  # Output

  output <- c(name, taxid, type, rank, species, species_taxid)

  names(output) <- c("name", "taxid", "type", "rank", "name_speciesorhigher", "species_taxid")

  return(output)

}
