#' calculate_Ins_Modulus
#'
#' calculate Instantaneous elastic Modulus using the forces at equilibrium from the 3 stress-relaxation steps
#'
#' @param Sd1 the difference between the peak force and the force just prior the peak at the beginning of the 1st step
#' @param Sd2 the difference between the peak force and the force just prior the peak at the beginning of the 2nd step
#' @param Sd3 the difference between the peak force and the force just prior the peak at the beginning of the 3rd step
#' @param Indentation_area the indentation area (based on the radius of the indenter)
#' @param Strains list of the strains at the 3 steps
#' @return the calculated equilibrium elastic Modulus
#' @importFrom stats lm
#' @export
calculate_Ins_Modulus<-function(Sd1,Sd2,Sd3,Indentation_area,Strains){
  #Calculating the stresses
  DSt1<-Sd1/Indentation_area
  DSt2<-Sd2/Indentation_area
  DSt3<-Sd3/Indentation_area
  InsModulus<-NULL
  for (r in 1:nrow(DSt1)){
    InsModulus<-rbind(InsModulus,lm(c(DSt2[r],DSt3[r]) ~ Strain[c(2,3)])[[1]][2])}
  InsModulus
}
