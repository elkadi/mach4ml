#' Rmv_abnormal_peaks
#'
#'  Remove abnormally high peaks around the three/Four stress peaks of the stress relaxation protocol
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple MACH-1 files
#' @param S1i The index of the 1st step
#' @param S2i The index of the 2nd step
#' @param S3i The index of the 3rd step
#' @param S4i The index of the 4th step (default=0 i.e. just 3 steps)
#' @param dp The number of data points to consider around each peak
#' @param threshold How much higher is the maximum peak from the second higher stress that makes it abnormal
#' @return a modified data frame (SRmergedfile) with the forces of Stress-Relaxation cleaned from abnormally high peaks
#' @importFrom utils head
#' @export

Rmv_abnormal_peaks<-function(SRmergedfile,S1i,S2i,S3i, S4i=0,dp=200,threshold=1.4){
  for (n in 1:nrow(SRmergedfile)){
    #Get the top 2 stress for each peak
    peaksSR1<-head(sort(SRmergedfile[n,S1i:(S1i+dp)],decreasing = TRUE),2)
    peaksSR2<-head(sort(SRmergedfile[n,S2i:(S2i+dp)],decreasing = TRUE),2)
    peaksSR3<-head(sort(SRmergedfile[n,S3i:(S3i+dp)],decreasing = TRUE),2)
    # Clean abnormally high peaks
    if(peaksSR1[1]>peaksSR1[2]*threshold){
      SRmergedfile[n,S1i:(S1i+dp)][which(SRmergedfile[n,S1i:(S1i+dp)] == peaksSR1[1])]<-peaksSR1[2]
    }
    if(peaksSR2[1]>peaksSR2[2]*threshold){
      SRmergedfile[n,S2i:(S2i+dp)][which(SRmergedfile[n,S2i:(S2i+dp)] == peaksSR2[1])]<-peaksSR2[2]
    }
    if(peaksSR3[1]>peaksSR3[2]*threshold){
      SRmergedfile[n,S3i:(S3i+dp)][which(SRmergedfile[n,S3i:(S3i+dp)] == peaksSR3[1])]<-peaksSR3[2]
    }

    if (S4i>0){
      peaksSR4<-head(sort(SRmergedfile[n,(S4i):(S4i+dp)],decreasing = TRUE),2)
      SRmergedfile[n,S4i:(S4i+dp)][which(SRmergedfile[n,S4i:(S4i+dp)] == peaksSR4[1])]<-peaksSR4[2]
    }
    }
  return(SRmergedfile)
  }
