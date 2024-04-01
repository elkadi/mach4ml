#' NormalizeBaseline
#'
#'  Normalize the baseline of the stress relaxation data to zero
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple MACH-1 files
#' @param  dpi number of initial datapoint to be considered as the baseline
#' @return a data frame (SRmergedfile_Clean) with all stress data subtracted from the baseline i.e. normalized to initial of Zero
#' @importFrom stats median
#' @export

NormalizeBaseline<-function(SRmergedfile,dpi=10){


  # Calculate the baseline for each row
  baseline <- apply(SRmergedfile[, 1:dpi], 1, median)

  # Subtract baseline from each row
  SRmergedfile_Clean <- sweep(SRmergedfile, 1, baseline, `-`)

  # Return the result
  SRmergedfile_Clean
  }
