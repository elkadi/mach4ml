#' Hayes_correction
#'
#' Correct the modulus according to Hayes el al. (1972), only suitable for materials with Poisson’s ratio = 0.5 such as hydrogels
#'
#' @param Modulus the modulus to be corrected
#' @param indenterradius indenter radius in mm
#' @param Thicknesses the thickness of the sample or list of thicknesses from the 3 step stress relaxation protocol
#' @param v Poisson’s ratio (default 0.5; also data provided )
#' @return the corrected Modulus
#' @importFrom stats spline
#' @examples
#' Hayes_correction(0.03,1.5,c(2,1.9,1.8),v=0.5)
#' @export


Hayes_correction<-function(Modulus,indenterradius,Thicknesses,v=0.5){
  pt<-c(seq(0.2,1,by=0.2),seq(1.5,4,by=0.5),5,6,7,8)
  Kin<-c(1.281, 1.683, 2.211, 2.855, 3.609,5.970,9.069,13,17.86,23.74,30.75,48.47,71.75,101.27,137.7)
  points<-c(indenterradius[1]/Thicknesses[1],indenterradius/Thicknesses[2],indenterradius/Thicknesses[3])
  K_values <- spline(pt, Kin, xout = points, method = "natural")
  Hcfn<-((1-v^2))*pi*indenterradius/2
  Modulus_HC<-Hcfn*Modulus/(K_values[[2]][1]*Thicknesses[1])
  Modulus_HC
}
