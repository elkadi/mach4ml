#' tnName
#'
#' transpose a df and make the 1st row the new column names
#'
#' @param fx a dataframe
#' @return transposed dataframe with 1st row of the original dataframe as the new column names
#' @export

tnName<-function(fx){
  fxT<-t(fx)
  colnames(fxT)<-fxT[1,]
  fxT<-fxT[-1,]
  fxT}
