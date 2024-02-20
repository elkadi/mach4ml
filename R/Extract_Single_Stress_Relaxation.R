#' Extract_Single_Stress_Relaxation
#'
#' Extract a Single Stress-Relaxation cycle
#'
#' @param x character vectors of data produced by biomomentum MACH-1
#' @param start_index the index of the beginning of the data of the Stress-Relaxation cycle ("<Stress Relaxation>" or "<divider>"); the extracted data start after z (the third argument of this function) + this index
#' @param end_index the index of the end of the data of the Stress-Relaxation cycle ("<divider>"); the extracted data end just before this index
#' @param z the number of the first index to be extracted after the start_index
#' @return a data frame with a single cycle of stress-relaxation data extracted
#' @export

Extract_Single_Stress_Relaxation<-function(x,start_index,end_index, z = 1) {
  # Extract the lines between the tags
  data_lines <- x[(start_index + z):(end_index - 1)]

  # Check the length of the split data
  split_data <- strsplit(data_lines, "\t")
  lengths <- sapply(split_data, length)

  if (!all(lengths == lengths[1])) {
    inconsistent_lines <- which(lengths != lengths[1])
    cat("Inconsistent number of elements in the split data at lines:", inconsistent_lines, "\n")
    cat("Lines with inconsistencies:\n")
    for (line in inconsistent_lines) {
      cat(paste("Line", line, ":", data_lines[line], "\n"))
    }
    stop("Please check the data for inconsistencies.")
  }

  matrix_data <- matrix(unlist(split_data), ncol = 5, byrow = TRUE)
  # Convert the matrix to a data frame
  as.data.frame(matrix_data)}
