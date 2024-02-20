#' Extract_Wait
#'
#' Extract the Wait period prior the stress relaxation cycles (if defined in the protocol)
#'
#' @param x path to the MACH-1 text file
#' @param y the number of lines after the tag "<Wait>" (default is 4)
#' @return a data frame with the extracted 3 stress-relaxation data cycles combined
#' @export



Extract_Wait <- function(x,y=4) {
  file_content <- readLines(x, warn = FALSE)
  # Find the indices of the lines containing the desired tags
  start_index <- grep("<Wait>", file_content)
  end_index <- grep("<END DATA>", file_content)[grep("<END DATA>", file_content)>start_index][1]
  # Extract the lines between the tags
  data_lines <- file_content[(start_index + y):(end_index - 1)]
  matrix_data <- matrix(unlist(strsplit(data_lines, "\t")), ncol = 5, byrow = TRUE)
  # Convert the matrix to a data frame
  Wait_df <- as.data.frame(matrix_data)
  Wait_df}
