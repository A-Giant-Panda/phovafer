###
#' Get the daily load profiles.
#'
#' Transfer multiple days of load vector of one household into one matrix with
#' each row presents a daily observation.
#' @param loadsample A numeric vector that demonstrates load profile of a household.
#' It can be a yearly/seasonally/monthly or even several days' load vector.
#' @param load.date A vector of characters with the form 'yyyy-mm-dd', which presents
#' the corresponding date vectors of the "loadsample".
#' @param num.obs An integer which presents the number of observations on each day.
#' @keywords load
#' @return loadsample --- A numeric matrix with each row presents a daily observations.
#' @export
#' @examples
#' ### Import the load profiles.
#' #
#' data("loadsample")
#' #
#' ### Get the load profile matrix of the household X1.
#' #
#' loadsample.X1<-load.daily(loadsample$X1, load.date=loadsample$date, num.obs=48)
#'
#'
#'
load.daily<-function(loadsample, load.date, num.obs){
  uni.load.date<-unique(load.date)
  len.load.date<-length(uni.load.date)

  loadsample.new<-matrix(1:(len.load.date*num.obs), nrow = len.load.date, ncol=num.obs)
  for (i in 1:len.load.date){
    loadsample.new[i, ]<-loadsample[which(load.date==uni.load.date[i])]
  }
  return(loadsample=loadsample.new)
}
