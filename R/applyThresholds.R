#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs
#' @return A report with control read counts linke dto their relevant sample
#' @export


applyThresholds <- function(type, rpm_ratio, reads, control_reads, proportion,
                            thresholds,
                            rpm_ratio_thres_bac = 0,
                            rpm_ratio_thres_vir = 0,
                            rpm_ratio_thres_fun = 0,
                            rpm_ratio_thres_euk = 0,
                            proportion_thres_bac = 0,
                            proportion_thres_vir = 0,
                            proportion_thres_fun = 0,
                            proportion_thres_euk = 0,
                            reads_thres_vir = 0,
                            reads_thres_fun = 0,
                            control_reads_thres_vir = 0) {

  if (!is.null(thresholds)) {

    rpm_ratio_thres_bac <- as.numeric(thresholds$rpm_ratio_thres_bac[[1]])
    rpm_ratio_thres_vir <- as.numeric(thresholds$rpm_ratio_thres_vir[[1]])
    rpm_ratio_thres_fun <- as.numeric(thresholds$rpm_ratio_thres_fun[[1]])
    rpm_ratio_thres_euk <- as.numeric(thresholds$rpm_ratio_thres_euk[[1]])
    proportion_thres_bac <- as.numeric(thresholds$proportion_thres_bac[[1]])
    proportion_thres_vir <- as.numeric(thresholds$proportion_thres_vir[[1]])
    proportion_thres_fun <- as.numeric(thresholds$proportion_thres_fun[[1]])
    proportion_thres_euk <- as.numeric(thresholds$proportion_thres_euk[[1]])
    reads_thres_vir <- as.numeric(thresholds$reads_thres_vir[[1]])
    reads_thres_fun <-  as.numeric(thresholds$reads_thres_vir[[1]])
    control_reads_thres_vir <- as.numeric(thresholds$control_reads_thres_vir[[1]])

  }

  output <- NA

  if (!is.na(type) & !is.na(rpm_ratio) & !is.na(reads) & !is.na(proportion)) {

    if (type == "Bacteria"){

      if (rpm_ratio >= rpm_ratio_thres_bac & proportion >= proportion_thres_bac) {

        output <- "positive"

      } else {

        output <- "negative"

      }

    } else if (type == "Virus") {

      if (rpm_ratio >= rpm_ratio_thres_vir & proportion >= proportion_thres_vir &
          reads >= reads_thres_vir &
          (control_reads <= control_reads_thres_vir | rpm_ratio >= rpm_ratio_thres_vir )) {

        output <- "positive"

      } else {

        output <- "negative"

      }

    } else if (type == "Fungi") {

      if (rpm_ratio >= rpm_ratio_thres_fun & reads >= reads_thres_fun &
          proportion >= proportion_thres_fun) {

        output <- "positive"

      } else {

        output <- "negative"

      }

    } else if (type == "Other eukaryote") {

      if (rpm_ratio >= rpm_ratio_thres_euk & proportion >= proportion_thres_euk) {

        output <- "positive"

      } else {

        output <- "negative"

      }

    }

  } else {

    output <- NA

  }

  return(output)

}

applyThresholds <- Vectorize(applyThresholds, vectorize.args = c("type", "rpm_ratio", "reads", "control_reads", "proportion"))
