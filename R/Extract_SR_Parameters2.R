#' Extract_SR_Parameters2
#'
#' Extract parameters for calculation of the moduli from extracted Stress-Relaxation data (3-4 steps), compared to the 1st version a new option added to adjust the data points to be considered, and allow adding a 4th step
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple files
#' @param Step2Index the index of the beginning of the second step of the Stress-Relaxation cycles
#' @param Step3Index the index of the beginning of the third step of the Stress-Relaxation cycles
#' @param Step4Index the index of the beginning of the fourth step of the Stress-Relaxation cycles (default is 0 ie ignored)
#' @param EqDataPoints the number of data points to be included in the measurements of the equilibrium modulus, add 1 to the desired values (default is 201)
#' @return a list of the equilibrium forces (Se1, Se2, Se3, and optionally Se4), the peak forces (Sp1, Sp2, Sp3, and optionally Sp4), and the forces just before the peaks (Sm1, Sm2, Sm3, and optionally Sm4) of the three/four steps, respectively.
#' @export

Extract_SR_Parameters2<-function(SRmergedfile,Step2Index,Step3Index,Step4Index=0,EqDataPoints=201){
  tdfn<-SRmergedfile; S2i<-Step2Index; S3i<-Step3Index; S4i<-Step4Index
  #1.Extracting forces at equilibrium For Equilibrium Modulus of each step (EqDataPoints)
  Se1<-mean(tdfn[1,(S2i-EqDataPoints):(S2i-1)]); Se2<-mean(tdfn[1,(S3i-EqDataPoints):(S3i-1)]); Se3<-mean(tdfn[1,(ncol(tdfn)-EqDataPoints):(ncol(tdfn)-1)])
  for (n in 2:nrow(tdfn)){
    Se1<-rbind(Se1,mean(tdfn[n,(S2i-EqDataPoints):(S2i-1)]))
    Se2<-rbind(Se2,mean(tdfn[n,(S3i-EqDataPoints):(S3i-1)]))
    Se3<-rbind(Se3,mean(tdfn[n,(ncol(tdfn)-EqDataPoints):(ncol(tdfn)-1)]))
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
  L1<-list(Se1,Se2,Se3,Sp1,Sp2,Sp3,Sm1,Sm2,Sm3)
  if (S4i>0){
    Se3<-mean(tdfn[1,(S4i-EqDataPoints):(S4i-1)]);Se4<-mean(tdfn[1,(ncol(tdfn)-EqDataPoints):(ncol(tdfn)-1)])
    for (n in 2:nrow(tdfn)){
      Se3<-rbind(Se3,mean(tdfn[n,(S4i-EqDataPoints):(S4i-1)]))
      Se4<-rbind(Se4,mean(tdfn[n,(ncol(tdfn)-EqDataPoints):(ncol(tdfn)-1)]))
    }
    Sp3<-max(tdfn[1,S3i:S4i-1]); Sp4<-max(tdfn[1,S4i:ncol(tdfn)])
    for (n in 2:nrow(tdfn)){
      Sp3<-rbind(Sp3, max(tdfn[n,S3i:S4i-1]))
      Sp4<-rbind(Sp4,max(tdfn[n,S4i:ncol(tdfn)]))
    }
    Sm4<-median(tdfn[1,S4i:S4i+10])
    for (n in 2:nrow(tdfn)){
      Sm4<-rbind(Sm4,median(tdfn[n,S4i:S4i+10]))
    }
    L1<-list(Se1,Se2,Se3,Se4,Sp1,Sp2,Sp3,Sp4,Sm1,Sm2,Sm3,Sm4)
  }
L1
  }
