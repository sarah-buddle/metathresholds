#' Use thresholds to decide whether a taxon is classed as positive or negative
#'
#'
#'
#' @param type options: bacteria, virus, fungi, other_eukaryote
#' @param rpm_ratio reads per million ratio
#' @param reads number ofreads
#' @param control_reads number of reads in negative control
#' @param proportion proportion
#' @param thresholds thresholds imported from thresholds_filepath
#' @param reads_thres_bac see readme
#' @param reads_thres_vir see readme
#' @param reads_thres_fun see readme
#' @param reads_thres_euk see readme
#' @param rpm_ratio_thres_bac see readme
#' @param rpm_ratio_thres_vir see readme
#' @param rpm_ratio_thres_fun see readme
#' @param rpm_ratio_thres_euk see readme
#' @param proportion_thres_bas see readme
#' @param proportion_thres_vir see readme
#' @param proportion_thres_fun see readme
#' @param proportion_thres_euk see readme
#' @param low_level_bac see readme
#' @param low_level_vir see readme
#' @param low_level_fun see readme
#' @param low_level_euk see readme
#' @return Positive or negative
#' @export

applyThresholds <- function(type, rpm_ratio, reads, control_reads, proportion,
                            thresholds,
                            reads_thres_bac = 0,
                            reads_thres_vir = 0,
                            reads_thres_fun = 0,
                            reads_thres_euk = 0,
                            rpm_ratio_thres_bac = 0,
                            rpm_ratio_thres_vir = 0,
                            rpm_ratio_thres_fun = 0,
                            rpm_ratio_thres_euk = 0,
                            proportion_thres_bac = 0,
                            proportion_thres_vir = 0,
                            proportion_thres_fun = 0,
                            proportion_thres_euk = 0,
                            low_level_bac = FALSE,
                            low_level_vir = FALSE,
                            low_level_fun = FALSE,
                            low_level_euk = FALSE) {


  if (!is.null(thresholds)) {

    reads_thres_bac <- extractThreshold(thresholds, "reads_bacteria")
    reads_thres_vir <- extractThreshold(thresholds, "reads_viruses")
    reads_thres_fun <- extractThreshold(thresholds, "reads_fungi")
    reads_thres_euk <- extractThreshold(thresholds, "reads_othereukaryotes")
    rpm_ratio_thres_bac <- extractThreshold(thresholds, "rpm_ratio_bacteria")
    rpm_ratio_thres_vir <- extractThreshold(thresholds, "rpm_ratio_viruses")
    rpm_ratio_thres_fun <- extractThreshold(thresholds, "rpm_ratio_fungi")
    rpm_ratio_thres_euk <- extractThreshold(thresholds, "rpm_ratio_othereukaryotes")
    proportion_thres_bac <- extractThreshold(thresholds, "proportion_bacteria")
    proportion_thres_vir <- extractThreshold(thresholds, "proportion_viruses")
    proportion_thres_fun <- extractThreshold(thresholds, "proportion_fungi")
    proportion_thres_euk <- extractThreshold(thresholds, "proportion_othereukaryotes")
    low_level_bac <- extractThreshold(thresholds, "low_level_bacteria")
    low_level_vir <- extractThreshold(thresholds, "low_level_viruses")
    low_level_fun <- extractThreshold(thresholds, "low_level_fungi")
    low_level_euk <- extractThreshold(thresholds, "low_level_othereukaryotes")

  }

  output <- NA

  if (!is.na(type) & !is.na(rpm_ratio) & !is.na(reads) & !is.na(proportion)) {

    if (type == "Bacteria") {

      if (reads >= reads_thres_bac &
          rpm_ratio >= rpm_ratio_thres_bac &
          proportion >= proportion_thres_bac
      ) {

        output <- "positive"

      } else if (low_level_bac == TRUE &
                 reads >= reads_thres_bac &
                 (control_reads == 0 | rpm_ratio >= rpm_ratio_thres_bac) &
                  proportion >= proportion_thres_bac
                 ) {

                   output <- "positive"
      } else {

      output <- "negative"

    }

    } else if (type == "Virus") {

      if (reads >= reads_thres_vir &
          rpm_ratio >= rpm_ratio_thres_vir &
          proportion >= proportion_thres_vir
      ) {

        output <- "positive"

       } else if (low_level_vir == TRUE &
                  reads >= reads_thres_vir &
                  (control_reads == 0 | rpm_ratio >= rpm_ratio_thres_vir) &
                  proportion >= proportion_thres_vir
         ) {

          output <- "positive"

        } else {

        output <- "negative"

        }

      } else if (type == "Fungi") {

        if (reads >= reads_thres_fun &
            rpm_ratio >= rpm_ratio_thres_fun &
            proportion >= proportion_thres_fun
        ) {

          output <- "positive"

        } else if (low_level_fun == TRUE &
                   reads >= reads_thres_fun &
                   (control_reads == 0 | rpm_ratio >= rpm_ratio_thres_fun) &
                    proportion >= proportion_thres_fun
                   ) {

                     output <- "positive"

      } else {

        output <- "negative"

      }

    } else if (type == "Other eukaryote") {

      if (reads >= reads_thres_euk &
          rpm_ratio >= rpm_ratio_thres_euk &
          proportion >= proportion_thres_euk
      ) {

        output <- "positive"

      } else if (low_level_euk == TRUE &
                 reads >= reads_thres_euk &
                 (control_reads == 0 | rpm_ratio >= rpm_ratio_thres_euk) &
                  proportion >= proportion_thres_euk
                 ) {

                   output <- "positive"

      } else {

        output <- "negative"

      }

    } else {

      output <- NA

    }

  } else {

    output <- NA

  }

  return(output)

}

applyThresholds <- Vectorize(applyThresholds, vectorize.args = c("type", "rpm_ratio", "reads", "control_reads", "proportion"))
