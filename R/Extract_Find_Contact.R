#' Extract_Find_Contact
#'
#' Extract the "Find Contact" section from MACH-1 file
#'
#' @param x path to the MACH-1 text file
#' @param y the number of lines after the tag "<Find Contact>" (default is 11)
#' @return a data frame with the extracted Find Contact section
#' @export

Extract_Find_Contact<-function(x,y=11) {
  file_content <- readLines(x, warn = FALSE)
  # Find the indices of the lines containing the desired tags
  start_index <- grep("<Find Contact>", file_content)
  end_index <- grep("<END DATA>", file_content)[grep("<END DATA>", file_content)>start_index][1]
  # Extract the lines between the tags
  data_lines <- file_content[(start_index + y):(end_index - 1)]
  matrix_data <- matrix(unlist(strsplit(data_lines, "\t")), ncol = 5, byrow = TRUE)
  # Convert the matrix to a data frame
  Find_Contactdf <- as.data.frame(matrix_data)
  Find_Contactdf}
