#' Extract_Stress_Relaxation
#'
#' Extract the Stress-Relaxation cycles
#'
#' @param file path to the MACH-1 text file
#' @param y the number of lines after the tag "<Stress Relaxation>" (default is 12)
#' @return a data frame with the extracted 3 stress-relaxation data cycles combined
#' @export


Extract_Stress_Relaxation <- function(file, y = 12) {
  file_content <- readLines(file, warn = FALSE)
  if ("<Stress Relaxation>" %in% file_content){
  # Find the indices of the lines containing the desired tags
  SR1_start_index <- grep("<Stress Relaxation>", file_content)
  dividers<-grep("<divider>", file_content)
  SRdividers<-dividers[dividers>SR1_start_index]
  SR1_end_index <-SRdividers[1]
  SR2_start_index <- SR1_end_index
  SR2_end_index <- SRdividers[2]
  SR3_start_index <- SR2_end_index
  SR3_end_index <- SRdividers[3]
  SR1<-Extract_Single_Stress_Relaxation(x = file_content,start_index =SR1_start_index,end_index = SR1_end_index,z = y)
  SR2<-Extract_Single_Stress_Relaxation(x = file_content,start_index =SR2_start_index,end_index = SR2_end_index,z = 1)
  SR3<-Extract_Single_Stress_Relaxation(x = file_content,start_index =SR3_start_index,end_index = SR3_end_index,z = 1)
  colnames(SR1)<-c("Time",	"z",	"x",	"y",	"Fz")
  colnames(SR2)<-c("Time",	"z",	"x",	"y",	"Fz")
  colnames(SR3)<-c("Time",	"z",	"x",	"y",	"Fz")

  SR1$Step<-1
  SR2$Step<-2
  SR3$Step<-3
  rbind(SR1,SR2,SR3)
  }
  }
