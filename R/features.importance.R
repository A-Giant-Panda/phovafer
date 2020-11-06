###
#' Compute the significance of a feature.
#'
#' Compute the importance of each feature that identifies PV and non-PV consumers
#' based on the p-value of kruskal test.
#' @param feature.PV.NPV A numeric vector of feature values of a certain feature computed from all the PV and non-PV load profiles.
#' @param label.PV A 0-1 vector with 0 and 1 demonstrate PV and non-PV consumers, respectively.
#' @keywords features
#' @return p.value --- The p-value used to demonstrate the significance of the feature that
#' differs in PV and non-PV load profile. The smaller the p-value, the more significant the feature is.
#' Usually, those features with p-values < 0.05 should be regarded as significant features.
#' @import matrixStats
#' @export
#' @examples
#' # Load the required packages.
#'
#' rm(list=ls())
#'
#' library(matrixStats)
#'
#' ### Load the smart meter readings and the simulated PV generations.
#'
#' data("loadsample")
#' data("pvSample")
#'
#' ### Get the load profiles of both PV and non-PV consumers.
#'
#' N.house<-ncol(loadsample[,-c(1:6)]) # N.house is the total number of households.
#'
#' NPV.day<-lapply(loadsample[,-c(1:6)], load.daily, load.date=loadsample[,1], num.obs=48)
#'
#' ### Assuming 30% of the households use PV. Generate the PV samples.
#'
#' perc=0.3
#' pv.house<-floor(N.house*perc) # pv.house is the number of households which are PV users.
#' M<-ncol(pvSample[,-c(1:6)])
#' pv.rand<-sample(1:M, pv.house, replace = TRUE, prob = rep(1/M, times=M))
#' pvSample<-cbind(pvSample[ ,c(1:6)], pvSample[ ,-c(1:6)][ ,pv.rand])
#'
#' date.PV.NPV<-unique(pvSample$date)
#' PV.day<-lapply(pvSample[,-c(1:6)], load.daily, load.date=pvSample$date, num.obs=48)
#'
#' house_pv<-sample(N.house)[1:pv.house] # Randomly selected PV households.
#'
#' # Compute the real load demand of the PV consumers using the smart meter readings (loadsample),
#' # and the simulated PV samples (pvSample).
#' demand_pv<-vector(mode = "list", length = length(PV.day))
#'
#' for (k in 1:pv.house){
#'   demand_pv[[k]]<-NPV.day[[house_pv[k]]]-PV.day[[k]]
#' }
#' demand_npv<-NPV.day[-house_pv]
#'
#' # Label the PV consumers with 0, and non-PV consumers with 1.
#' label.PV<-c(rep(0, times=length(house_pv)),
#'            rep(1, times=length(demand_npv)))
#' label.PV<-factor(label.PV)
#'
#' ### Prepare the features used to identify PV and non-PV users.
#'
#' date.week<-weekdays(as.Date(date.PV.NPV))
#' x.week<-date.week
#' mor.be<-13 # It presents 06:00:00.
#' aft.be<-39 # It presents 19:00:00.
#'
#' feature.PV<-lapply(demand_pv, features.load, x.week, mor.be, aft.be)
#' feature.NPV<-lapply(demand_npv, features.load, x.week, mor.be, aft.be)
#'
#' ### Get the name of the features.
#'
#' features.name<-feature.PV[[1]]$features.name # Get the name of the computed features.
#'
#' ### Prepare the feature matrix.
#'
#' feature.PV.new<-feature.PV[[1]]$output.features
#' for (i in 2:length(feature.PV)){
#'   feature.PV.new<-rbind(feature.PV.new, feature.PV[[i]]$output.features)
#' }
#'
#' feature.NPV.new<-feature.NPV[[1]]$output.features
#' for (i in 2:length(feature.NPV)){
#'   feature.NPV.new<-rbind(feature.NPV.new, feature.NPV[[i]]$output.features)
#' }
#'
#' ### Compute the significance of each feature and select the 12 most significant features.
#'
#' feature.PV.NPV<-rbind(feature.PV.new, feature.NPV.new)
#'
#' p.value<-apply(feature.PV.NPV, 2, features.importance, label.PV)
#'
features.importance<-function(feature.PV.NPV, label.PV){
  peak.type<-data.frame(Feature=c(feature.PV.NPV),
                        Type=label.PV)
  peak.type$Type<-factor(peak.type$Type)
  p.value<-kruskal.test(Feature ~ Type, data = peak.type)$p.value
  return(p.value=p.value)
}

