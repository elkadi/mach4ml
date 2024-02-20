#' smooth_SR
#'
#'  Smooth the forces of Stress-Relaxation data by SG filters
#'
#' @param SRmergedfile A data frame with merged forces and time data extracted from the stress-relaxation section from multiple files
#' @param smfl The length of the filter (default=17)
#' @return a data frame (tdfn_sm) with the forces of Stress-Relaxation smoothed with less noises
#' @import pracma
#' @export

smooth_SR<-function(SRmergedfile,smfl=17){
  tdfn<-SRmergedfile
  tdfn_sm<-NULL
  for (r in 1:nrow(tdfn)){
    xx<-pracma::savgol(as.numeric(tdfn[r,]),fl=smfl, forder = 2, dorder = 0)
    tdfn_sm<-rbind(tdfn_sm,xx)
  }
  colnames(tdfn_sm)<-colnames(tdfn)
  row.names(tdfn_sm)<-row.names(tdfn)
  tdfn_sm<-tdfn_sm[,1:(ncol(tdfn_sm)-10)]
  tdfn_sm}
