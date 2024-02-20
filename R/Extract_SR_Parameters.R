#' Extract_SR_Parameters
#'
#' Extract parameters for calculation of the moduli from extracted Stress-Relaxation data
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple files
#' @param Step2Index the index of the beginning of the second step of the Stress-Relaxation cycles
#' @param Step3Index the index of the beginning of the third step of the Stress-Relaxation cycles
#' @return a list of the equilibrium forces (Se1, Se2 & Se3), the peak forces (Sp1, Sp2 & Sp3), and the forces just before the peaks (Sm1, Sm2, & Sm3) of the three steps, respectively.
#' @export

Extract_SR_Parameters<-function(SRmergedfile,Step2Index,Step3Index){
  tdfn<-SRmergedfile; S2i<-Step2Index; S3i<-Step3Index
  #1.Extracting forces at equilibrium For Equilibrium Modulus of each step (last 2 seconds; 200 dp of each step)
  Se1<-mean(tdfn[1,(S2i-201):(S2i-1)]); Se2<-mean(tdfn[1,(S3i-201):(S3i-1)]); Se3<-mean(tdfn[1,(ncol(tdfn)-201):(ncol(tdfn)-1)])
  for (n in 2:nrow(tdfn)){
    Se1<-rbind(Se1,mean(tdfn[n,(S2i-201):(S2i-1)]))
    Se2<-rbind(Se2,mean(tdfn[n,(S3i-201):(S3i-1)]))
    Se3<-rbind(Se3,mean(tdfn[n,(ncol(tdfn)-201):(ncol(tdfn)-1)]))
  }
  #2.Extracting Maximum Forces for Instantaneous modulus (peak)
  Sp1<-max(tdfn[1,1:S2i-1]); Sp2<-max(tdfn[1,S2i:S3i-1]); Sp3<-max(tdfn[1,S3i:ncol(tdfn)])
  for (n in 2:nrow(tdfn)){
    Sp1<-rbind(Sp1, max(tdfn[n,1:S2i-1]))
    Sp2<-rbind(Sp2, max(tdfn[n,S2i:S3i-1]))
    Sp3<-rbind(Sp3,max(tdfn[n,S3i:ncol(tdfn)]))

  }
  #3.Extracting Minimum Forces before the peak for Instantaneous modulus
  Sm1<-median(tdfn[1,1:10]); Sm2<-median(tdfn[1,S2i:S2i+10]); Sm3<-median(tdfn[1,S3i:S3i+10])
  for (n in 2:nrow(tdfn)){
    Sm1<-rbind(Sm1, median(tdfn[n,1:10]))
    Sm2<-rbind(Sm2, median(tdfn[n,S2i:S2i+10]))
    Sm3<-rbind(Sm3,median(tdfn[n,S3i:S3i+10]))

  }
  list(Se1,Se2,Se3,Sp1,Sp2,Sp3,Sm1,Sm2,Sm3)
}
