#' Rmv_negative_peaks2
#'
#'  Remove negative peaks around the three/Four stress peaks of the stress relaxation protocol
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple MACH-1 files
#' @param S1i The index of the 1st step
#' @param S2i The index of the 2nd step
#' @param S3i The index of the 3rd step
#' @param S4i The index of the 4th step (default=0 i.e. just 3 steps)
#' @return a data frame (SRmergedfile_Clean) with the forces of Stress-Relaxation cleaned from negative peaks
#' @importFrom stats median
#' @export

Rmv_negative_peaks2<-function(SRmergedfile,S1i,S2i,S3i, S4i=0){
  ##Elimination of negative peaks
  for (n in 1:nrow(SRmergedfile)){
    ms0e<-median(SRmergedfile[n,(S1i):(S1i+10)])
    ms1e<-median(SRmergedfile[n,(S2i-20):(S2i-1)])
    ms2e<-median(SRmergedfile[n,(S3i-20):(S3i-1)])
    SRmergedfile[n,S1i:(S1i+50)][which(SRmergedfile[n,S1i:(S1i+50)] < ms0e)]<-ms0e
    SRmergedfile[n,(S2i-1):(S2i+50)][which(SRmergedfile[n,(S2i-1):(S2i+50)] < ms1e)]<-ms1e
    SRmergedfile[n,(S3i-1):(S3i+50)][which(SRmergedfile[n,(S3i-1):(S3i+50)] < ms2e)]<-ms2e
    if (S4i>0){
      ms3e<-median(SRmergedfile[n,(S4i-20):(S4i-1)])
      SRmergedfile[n,(S4i-1):(S4i+50)][which(SRmergedfile[n,(S4i-1):(S4i+50)] < ms3e)]<-ms3e
    }
    }
  SRmergedfile_Clean<-SRmergedfile
  SRmergedfile_Clean
  }
