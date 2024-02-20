#' calculate_moduli_multifiles
#'
#' Extract the Stress-Relaxation data from MACH-1 files with 3 steps Stress-Relaxation, perpocess the extracted data, and use them to calculate the elastic modulus
#'
#' @param folder path to a folder with the MACH-1 text files
#' @param Thicknesses a list with the thicknesses for of the sample for the three steps
#' @param Strain a list with the strains for of the sample for the three steps
#' @param indenterradius the radius of the indeter
#' @param poisson_eq the poisson ratio for the equilibrium modulus (default=0.5)
#' @param poisson_inst the poisson ratio for the instantaneous modulus (default=0.5)
#' @param smfl the length of the filter for SG smoothing (default=19)
#' @return A list containing; folderoutput:A data frame with calculated moduli for all files before and after smoothing, and before and after Hayes correction (1972; based on poisson ration of 0.5);tdfn: A data frame with merged forces and time data extracted from the stress-relaxation section from all files; tdfn_sm: A data frame with merged forces and time data extracted from the stress-relaxation section from all files after smoothing.
#' @export
calculate_moduli_multifiles<-function(folder,Thicknesses,Strain,indenterradius,poisson_eq=0.5,poisson_inst=0.5,smfl=19){
  #################A.Importing the files#############################
  #A.1.Specify the folder_path
  folder_path <- folder
  SRdata<-Extract_Stress_Relaxation_MultiFiles(folder_path)
  SRmergedfile<-SRdata[[1]]
  ############B.Preproccessing#######################################
  ##B.1. Convert to newton
  tdfn<-SRmergedfile*9.81*0.001
  #B.2. Smoothing data#
  tdfn_sm<-smooth_SR(tdfn,smfl=smfl)
  #B.3. Remove Negative peaks
  tdfn_sm<-Rmv_negative_peaks(tdfn_sm,S1i = SRdata[[3]][1],S2i = SRdata[[3]][2],S3i = SRdata[[3]][3])
  tdfn<-Rmv_negative_peaks(tdfn,S1i = SRdata[[3]][1],S2i = SRdata[[3]][2],S3i = SRdata[[3]][3])
  #################### C.Extracting parameters######################
  Stepsindecies<-SRdata[[3]]
  S1i<-Stepsindecies[1]; S2i<-Stepsindecies[2]; S3i<-Stepsindecies[3]
  SRParameters_Sm<-Extract_SR_Parameters(SRmergedfile=tdfn_sm,S2i,S3i)
  SRParameters<-Extract_SR_Parameters(tdfn,S2i,S3i)
  Se1_sm<-SRParameters_Sm[[1]];Se2_sm<-SRParameters_Sm[[2]];Se3_sm<-SRParameters_Sm[[3]]
  Sp1_sm<-SRParameters_Sm[[4]];Sp2_sm<-SRParameters_Sm[[5]];Sp3_sm<-SRParameters_Sm[[6]]
  Se1<-SRParameters[[1]];Se2<-SRParameters[[2]];Se3<-SRParameters[[3]]
  Sp1<-SRParameters[[4]];Sp2<-SRParameters[[5]];Sp3<-SRParameters[[6]]
  Sd1<-Sp1-SRParameters[[7]];Sd2<-Sp2-SRParameters[[8]];Sd3<-Sp3-SRParameters[[9]]
  Sd1_sm<-Sp1_sm-SRParameters_Sm[[7]];Sd2_sm<-Sp2_sm-SRParameters_Sm[[8]];Sd3_sm<-Sp3_sm-SRParameters_Sm[[9]]
  #################### D.Calculating the Moduli######################
  Iarea<-pi*indenterradius^2
  EqModulus<-calculate_Eq_Modulus(Se1,Se2,Se3,Indentation_area=Iarea,Strain)
  EqModulus_sm<-calculate_Eq_Modulus(Se1_sm,Se2_sm,Se3_sm,Indentation_area=Iarea,Strain)
  #InsModulus<-calculate_Ins_Modulus(Sd1,Sd2,Sd3,Indentation_area=Iarea,Strain)
  #InsModulus_sm<-calculate_Ins_Modulus(Sd1_sm,Sd2_sm,Sd3_sm,Indentation_area=Iarea,Strain)
  EqModulus_HC<-Hayes_correction(EqModulus,indenterradius=indenterradius,Thicknesses=Thicknesses)
  EqModulus_HC_Sm<-Hayes_correction(EqModulus_sm,indenterradius=indenterradius,Thicknesses=Thicknesses)
  folderoutput<-cbind(basename(folder_path),row.names(tdfn),EqModulus,EqModulus_HC,EqModulus_sm, EqModulus_HC_Sm)
  colnames(folderoutput)<-c("Folder","File","EqModulus","EqModulus_HC","EqModulus_sm","EqModulus_HC_Sm")
  row.names(tdfn)<-paste0(basename(folder_path),"_",row.names(tdfn))
  row.names(tdfn_sm)<-paste0(basename(folder_path),"_",row.names(tdfn_sm))
  list(folderoutput,tdfn,tdfn_sm)
}
