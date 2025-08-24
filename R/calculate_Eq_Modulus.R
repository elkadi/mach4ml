#' calculate_Eq_Modulus
#'
#' calculate Equilibrium elastic Modulus using the forces at equilibrium from the 3 stress-relaxation steps
#'
#' @param Se1 forces at equilibrium at the end of the 1st step
#' @param Se2 forces at equilibrium at the end of the 2nd step
#' @param Se3 forces at equilibrium at the end of the 3rd step
#' @param Indentation_area the indentation area (based on the radius of the indenter)
#' @param Strains list of the strains at the 3 steps
#' @return the calculated equilibrium elastic Modulus
#' @importFrom stats lm
#' @export
calculate_Eq_Modulus<-function(Se1,Se2,Se3,Indentation_area,Strains){
  #Calculating the stresses
  Ste1<-Se1/Indentation_area
  Ste2<-Se2/Indentation_area
  Ste3<-Se3/Indentation_area
  EqModulus<-NULL
  for (r in 1:nrow(Ste1)){
    EqModulus<-rbind(EqModulus,lm(c(Ste1[r],Ste2[r],Ste3[r]) ~ Strains)[[1]][2])}
  EqModulus
}
