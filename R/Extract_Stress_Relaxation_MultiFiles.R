#' Extract_Stress_Relaxation_MultiFiles
#'
#' Extract Stress-Relaxation sections from mutliple files in a folder and do essential preprocessing on them
#'
#' @param folder_path path to the folders with MACH-1 text files
#' @return A list containing; tdfn: A data frame with merged forces and time data extracted from the stress-relaxation section from all files; SRDisplacement: A data frame with merged displacement and time data extracted from the stress-relaxation section from all files; StepsIndex: A vector containing the indices of the 3 steps in the stress-relaxation section.
#' @export


Extract_Stress_Relaxation_MultiFiles<-function(folder_path){
  ####A.Importing and prepating data#######
  ##importing Files all files in a folder ending with .txt
  files <- Sys.glob(paste0(folder_path,"\\*.txt"))
  bnames<-basename(files)
  ##Extracting stress relaxation for all
  SRR<-lapply(files,Extract_Stress_Relaxation)
  ##extracting time and steps data
  T<-SRR[[1]][1]
  S<-SRR[[1]][6]
  ##Defining steps indices
  S1i <- which(S == 1)[1]
  S2i <- which(S == 2)[1]
  S3i <- which(S == 3)[1]
  StepsIndex<-c(S1i,S2i,S3i)
  ##Extract Displacement
  SRDisplacement<-SRR[[1]][2]
  for (i in 2:length(SRR)){
    zz<-SRR[[i]][2]
    SRDisplacement<-cbind(SRDisplacement,zz)
  }
  ##Removing non-target columns (time and force only for stress-time curve)
  for (i in 1:length(SRR)){
    SRR[[i]]<-SRR[[i]][-c(2,3,4,6)]
  }

  ##Appropriate naming
  ###Samples names
  names(SRR)<-bnames
  names(SRR)<-sub(".txt", "", names(SRR))
  ###Wavelength names
  colnames(T)<-"Time"
  SampleNames<-names(SRR)
  ###Adding the sample names
  colnames(SRDisplacement)<-SampleNames
  for (i in 1:length(SRR)){
    colnames(SRR[[i]])<-c("Time",SampleNames[i])
  }
  df<-T
  #For loop to merge the spectra
  for (i in 1:length(SRR)){
    Sample<-names(SRR)[i]
    colnames(SRR[[i]][2])<-Sample
    df<-cbind(df,SRR[[i]][2])
  }
  #binding displacement data to time
  SRDisplacement<-cbind(T,SRDisplacement)
  #transposing data
  tdf<-tnName(df)
  tdfn<- apply(tdf, c(1, 2), as.numeric)
  SRDisplacement<-tnName(SRDisplacement)

  list(tdfn,SRDisplacement,StepsIndex)
}
