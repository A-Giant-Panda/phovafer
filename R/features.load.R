###
#' Compute the features from the load profiles.
#'
#' Extract the features from the load profiles of each household. These features are prepared
#' for identifying the prosumers and regular consumers.
#' @param loadsample A numeric matrix that presents the load profile of a household.
#' It should be an 'm*48' matrix, in which 'm' demonstrates the number of days of the daily load profile
#' that can be obtained. There need to be 48 observations of one single day load profile.
#' @param x.week A vector of characters with 'm' elements that gives the day-of-the-week information
#' of the 'm' days' load profile. Each element should be one from
#' c('Monday',  'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday').
#' @param mor.be An integer presents the index of time stamp that PV generation has impact to the load profile.
#' Taking the 48 half-hourly observations of both daily load and PV profiles as an example,
#' if the PV generation has impact for the load profile at 06:00:00, then 'mor.be' should be chosen as 13 by counting the index number from 00:00:00.
#' @param aft.be An integer presents the index of time stamp that PV generation usually vanished in a day. For example,
#' if the PV generation vanished at 19:00:00, then 'aft.be' should be set as 39.
#' @keywords features
#' @return output.features --- A numeric feature vector with 63 element extracted from
#' the load profile of one household.
#' @return features.name --- A vector of characters that demonstrate the names of the features.
#' @import matrixStats
#' @export
#' @examples
#' ### Load the required packages.
#' #
#' library(matrixStats)
#' #
#' ### Import the load profiles.
#' #
#' data("loadsample")
#' #
#' ### Get the load profile matrix of the household X1.
#' #
#' loadsample.X1<-load.daily(loadsample$X1, load.date=loadsample$date, num.obs=48)
#' #
#' ### Compute the features.
#' #
#' features.X1<-features.load(loadsample.X1, unique(loadsample$week), mor.be=13, aft.be=39)

features.load<-function(loadsample, x.week, mor.be, aft.be){
  # loadsample should be a matrix of a certain season/month/year or several days of observations of one household.
  x<-as.matrix(loadsample)

  # Features extracted from all the data from 06:00:00 to 19:00:00.
  Mean.all<-mean(rowMeans(x[ complete.cases(x[ ,mor.be:aft.be]) , mor.be:aft.be]))
  Max.all<-mean(rowMaxs(x[ complete.cases(x[ ,mor.be:aft.be]), mor.be:aft.be]))
  Min.all<-mean(rowMins(x[ complete.cases(x[ ,mor.be:aft.be]), mor.be:aft.be]))
  Std.all<-mean(rowSds(x[ complete.cases(x[ ,mor.be:aft.be]), mor.be:aft.be]))
  Tot.all<-mean(rowSums(x[ complete.cases(x[ ,mor.be:aft.be]), mor.be:aft.be]))

  # Features extracted from all the data during the morning period 6:00 -- 10:00 (13 -- 21).
  Mor.peak<-mean(rowMaxs(x[ complete.cases(x[ ,mor.be:21]), mor.be:21]))
  Mor.mean<-mean(rowMeans(x[ complete.cases(x[ ,mor.be:21]), mor.be:21]))
  Mor.std<-mean(rowSds(x[ complete.cases(x[ ,mor.be:21]), mor.be:21]))
  Mor.low<-mean(rowMins(x[ complete.cases(x[ ,mor.be:21]), mor.be:21]))

  # Features extracted from all the data during the noon period 10:00 -- 14:00 (21 -- 29).
  Noo.peak<-mean(rowMaxs(x[ complete.cases(x[ ,21:29]), 21:29]))
  Noo.mean<-mean(rowMeans(x[ complete.cases(x[ ,21:29]), 21:29]))
  Noo.std<-mean(rowSds(x[ complete.cases(x[ ,21:29]), 21:29]))
  Noo.low<-mean(rowMins(x[ complete.cases(x[ ,21:29]), 21:29]))

  # Features extracted from all the data during the afternoon period 14:00 -- 17:00 (29 -- 35).
  Aft.peak<-mean(rowMaxs(x[ complete.cases(x[,29:35]), 29:35]))
  Aft.mean<-mean(rowMeans(x[ complete.cases(x[,29:35]), 29:35]))
  Aft.std<-mean(rowSds(x[ complete.cases(x[,29:35]), 29:35]))
  Aft.low<-mean(rowMins(x[ complete.cases(x[,29:35]), 29:35]))

  # Features extracted from all the data during the evening period 17:00 -- 19:00 (35 -- 39).
  Eve.peak<-mean(rowMaxs(x[ complete.cases(x[,35:aft.be]), 35:aft.be]))
  Eve.mean<-mean(rowMeans(x[ complete.cases(x[,35:aft.be]), 35:aft.be]))
  Eve.std<-mean(rowSds(x[ complete.cases(x[,35:aft.be]), 35:aft.be]))
  Eve.low<-mean(rowMins(x[ complete.cases(x[,35:aft.be]), 35:aft.be]))

  # Features extracted from all the weekdays' data from 06:00:00 to 19:00:00.
  weekdays.h<-which((x.week!="Sunday")&(x.week!="Saturday"))
  Mean.wd<-mean(rowMeans(x[complete.cases(x[weekdays.h, mor.be:aft.be]), mor.be:aft.be]))
  Max.wd<-mean(rowMaxs(x[complete.cases(x[weekdays.h, mor.be:aft.be]), mor.be:aft.be]))
  Min.wd<-mean(rowMins(x[complete.cases(x[weekdays.h, mor.be:aft.be]), mor.be:aft.be]))
  Std.wd<-mean(rowSds(x[complete.cases(x[weekdays.h, mor.be:aft.be]), mor.be:aft.be]))
  Tot.wd<-mean(rowSums(x[complete.cases(x[weekdays.h, mor.be:aft.be]), mor.be:aft.be]))

  # Features extracted from all the weekdays' data during the morning period 6:00 -- 10:00 (13 -- 21).
  Mor.peak.wd<-mean(rowMaxs(x[complete.cases(x[weekdays.h, mor.be:21]) ,mor.be:21]))
  Mor.mean.wd<-mean(rowMeans(x[complete.cases(x[weekdays.h, mor.be:21]) ,mor.be:21]))
  Mor.std.wd<-mean(rowSds(x[complete.cases(x[weekdays.h, mor.be:21]) ,mor.be:21]))
  Mor.low.wd<-mean(rowMins(x[complete.cases(x[weekdays.h, mor.be:21]) ,mor.be:21]))

  # Features extracted from all the weekdays' data during the noon period 10:00 -- 14:00 (21 -- 29).
  Noo.peak.wd<-mean(rowMaxs(x[complete.cases(x[weekdays.h, 21:29]) ,21:29]))
  Noo.mean.wd<-mean(rowMeans(x[complete.cases(x[weekdays.h, 21:29]) ,21:29]))
  Noo.std.wd<-mean(rowSds(x[complete.cases(x[weekdays.h, 21:29]) ,21:29]))
  Noo.low.wd<-mean(rowMins(x[complete.cases(x[weekdays.h, 21:29]) ,21:29]))

  # Features extracted from all the weekdays' data during the afternoon period 14:00 -- 17:00 (29 -- 35).
  Aft.peak.wd<-mean(rowMaxs(x[complete.cases(x[weekdays.h, 29:35]) ,29:35]))
  Aft.mean.wd<-mean(rowMeans(x[complete.cases(x[weekdays.h, 29:35]) ,29:35]))
  Aft.std.wd<-mean(rowSds(x[complete.cases(x[weekdays.h, 29:35]) ,29:35]))
  Aft.low.wd<-mean(rowMins(x[complete.cases(x[weekdays.h, 29:35]) ,29:35]))

  # Features extracted from all the weekdays' data during the evening period 17:00 -- 19:00 (35 -- 39).
  Eve.peak.wd<-mean(rowMaxs(x[complete.cases(x[weekdays.h, 35:aft.be]) ,35:aft.be]))
  Eve.mean.wd<-mean(rowMeans(x[complete.cases(x[weekdays.h, 35:aft.be]) ,35:aft.be]))
  Eve.std.wd<-mean(rowSds(x[complete.cases(x[weekdays.h, 35:aft.be]) ,35:aft.be]))
  Eve.low.wd<-mean(rowMins(x[complete.cases(x[weekdays.h, 35:aft.be]) ,35:aft.be]))

  # Features extracted from all the weekends' data from 06:00:00 to 19:00:00.
  weekends.h<-which((x.week=="Sunday")|(x.week=="Saturday"))
  Mean.wn<-mean(rowMeans(x[complete.cases(x[weekends.h, mor.be:aft.be]), mor.be:aft.be]))
  Max.wn<-mean(rowMaxs(x[complete.cases(x[weekends.h, mor.be:aft.be]), mor.be:aft.be]))
  Min.wn<-mean(rowMins(x[complete.cases(x[weekends.h, mor.be:aft.be]), mor.be:aft.be]))
  Std.wn<-mean(rowSds(x[complete.cases(x[weekends.h, mor.be:aft.be]), mor.be:aft.be]))
  Tot.wn<-mean(rowSums(x[complete.cases(x[weekends.h, mor.be:aft.be]), mor.be:aft.be]))

  # Features extracted from all the weekends' data during the morning period 6:00 -- 10:00 (13 -- 21).
  Mor.peak.wn<-mean(rowMaxs(x[complete.cases(x[weekends.h, mor.be:21]) ,mor.be:21]))
  Mor.mean.wn<-mean(rowMeans(x[complete.cases(x[weekends.h, mor.be:21]) ,mor.be:21]))
  Mor.std.wn<-mean(rowSds(x[complete.cases(x[weekends.h, mor.be:21]) ,mor.be:21]))
  Mor.low.wn<-mean(rowMins(x[complete.cases(x[weekends.h, mor.be:21]) ,mor.be:21]))

  # Features extracted from all the weekends' data during the noon period 10:00 -- 14:00 (21 -- 29).
  Noo.peak.wn<-mean(rowMaxs(x[complete.cases(x[weekends.h, 21:29]) ,21:29]))
  Noo.mean.wn<-mean(rowMeans(x[complete.cases(x[weekends.h, 21:29]) ,21:29]))
  Noo.std.wn<-mean(rowSds(x[complete.cases(x[weekends.h, 21:29]) ,21:29]))
  Noo.low.wn<-mean(rowMins(x[complete.cases(x[weekends.h, 21:29]) ,21:29]))

  # Features extracted from all the weekends' data during the afternoon period 14:00 -- 17:00 (29 -- 35).
  Aft.peak.wn<-mean(rowMaxs(x[complete.cases(x[weekends.h, 29:35]) ,29:35]))
  Aft.mean.wn<-mean(rowMeans(x[complete.cases(x[weekends.h, 29:35]) ,29:35]))
  Aft.std.wn<-mean(rowSds(x[complete.cases(x[weekends.h, 29:35]) ,29:35]))
  Aft.low.wn<-mean(rowMins(x[complete.cases(x[weekends.h, 29:35]) ,29:35]))

  # Features extracted from all the weekends' data during the evening period 17:00 -- 19:00 (35 -- 39).
  Eve.peak.wn<-mean(rowMaxs(x[complete.cases(x[weekends.h, 35:aft.be]) ,35:aft.be]))
  Eve.mean.wn<-mean(rowMeans(x[complete.cases(x[weekends.h, 35:aft.be]) ,35:aft.be]))
  Eve.std.wn<-mean(rowSds(x[complete.cases(x[weekends.h, 35:aft.be]) ,35:aft.be]))
  Eve.low.wn<-mean(rowMins(x[complete.cases(x[weekends.h, 35:aft.be]) ,35:aft.be]))

  output.features<-c(Mean.all, Max.all, Min.all, Std.all, Tot.all,
                 Mor.peak, Mor.mean, Mor.std, Mor.low,
                 Noo.peak, Noo.mean, Noo.std, Noo.low,
                 Aft.peak, Aft.mean, Aft.std, Aft.low,
                 Eve.peak, Eve.mean, Eve.std, Eve.low,
                 Mean.wd, Max.wd, Min.wd, Std.wd, Tot.wd,
                 Mor.peak.wd, Mor.mean.wd, Mor.std.wd, Mor.low.wd,
                 Noo.peak.wd, Noo.mean.wd, Noo.std.wd, Noo.low.wd,
                 Aft.peak.wd, Aft.mean.wd, Aft.std.wd, Aft.low.wd,
                 Eve.peak.wd, Eve.mean.wd, Eve.std.wd, Eve.low.wd,
                 Mean.wn, Max.wn, Min.wn, Std.wn, Tot.wn,
                 Mor.peak.wn, Mor.mean.wn, Mor.std.wn, Mor.low.wn,
                 Noo.peak.wn, Noo.mean.wn, Noo.std.wn, Noo.low.wn,
                 Aft.peak.wn, Aft.mean.wn, Aft.std.wn, Aft.low.wn,
                 Eve.peak.wn, Eve.mean.wn, Eve.std.wn, Eve.low.wn)

  features.name<-c("Mean of the daily average load (from 06:00:00 to 19:00:00)",
                   "Mean of the daily maximum load (from 06:00:00 to 19:00:00)",
                   "Mean of the daily minimum load (from 06:00:00 to 19:00:00)",
                   "Mean of the daily standard deviations (from 06:00:00 to 19:00:00)",
                   "Mean of the daily total load (from 06:00:00 to 19:00:00)",
                 "Mean of the daily morning peak load (from 06:00:00 to 10:00:00)",
                 "Mean of the daily morning average load (from 06:00:00 to 10:00:00)",
                 "Mean of the daily morning standard deviations (from 06:00:00 to 10:00:00)",
                 "Mean of the daily morning minimum load (from 06:00:00 to 10:00:00)",
                 "Mean of the daily noon peak load (from 10:00:00 to 14:00:00)",
                 "Mean of the daily noon average load (from 10:00:00 to 14:00:00)",
                 "Mean of the daily noon standard deviations (from 10:00:00 to 14:00:00)",
                 "Mean of the daily noon minimum load (from 10:00:00 to 14:00:00)",
                 "Mean of the daily afternoon peak load (from 14:00:00 to 17:00:00)",
                 "Mean of the daily afternoon average load (from 14:00:00 to 17:00:00)",
                 "Mean of the daily afternoon standard deviations (from 14:00:00 to 17:00:00)",
                 "Mean of the daily afternoon minimum load (from 14:00:00 to 17:00:00)",
                 "Mean of the daily evening peak load (from 17:00:00 to 19:00:00)",
                 "Mean of the daily evening average load (from 17:00:00 to 19:00:00)",
                 "Mean of the daily evening standard deviations (from 17:00:00 to 19:00:00)",
                 "Mean of the daily evening minimum load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily average load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily maximum load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily minimum load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily standard deviations (from 06:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily total load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily morning peak load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekdays' daily morning average load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekdays' daily morning standard deviations (from 06:00:00 to 10:00:00)",
                 "Mean of the weekdays' daily morning minimum load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekdays' daily noon peak load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekdays' daily noon average load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekdays' daily noon standard deviations (from 10:00:00 to 14:00:00)",
                 "Mean of the weekdays' daily noon minimum load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekdays' daily afternoon peak load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekdays' daily afternoon average load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekdays' daily afternoon standard deviations (from 14:00:00 to 17:00:00)",
                 "Mean of the weekdays' daily afternoon minimum load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekdays' daily evening peak load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily evening average load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily evening standard deviations (from 17:00:00 to 19:00:00)",
                 "Mean of the weekdays' daily evening minimum load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekends' daily average load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekends' daily maximum load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekends' daily minimum load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekends' daily standard deviations (from 06:00:00 to 19:00:00)",
                 "Mean of the weekends' daily total load (from 06:00:00 to 19:00:00)",
                 "Mean of the weekends' daily morning peak load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekends' daily morning average load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekends' daily morning standard deviations (from 06:00:00 to 10:00:00)",
                 "Mean of the weekends' daily morning minimum load (from 06:00:00 to 10:00:00)",
                 "Mean of the weekends' daily noon peak load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekends' daily noon average load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekends' daily noon standard deviations (from 10:00:00 to 14:00:00)",
                 "Mean of the weekends' daily noon minimum load (from 10:00:00 to 14:00:00)",
                 "Mean of the weekends' daily afternoon peak load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekends' daily afternoon average load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekends' daily afternoon standard deviations (from 14:00:00 to 17:00:00)",
                 "Mean of the weekends' daily afternoon minimum load (from 14:00:00 to 17:00:00)",
                 "Mean of the weekends' daily evening peak load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekends' daily evening average load (from 17:00:00 to 19:00:00)",
                 "Mean of the weekends' daily evening standard deviations (from 17:00:00 to 19:00:00)",
                 "Mean of the weekends' daily evening minimum load (from 17:00:00 to 19:00:00)")

  return(list(output.features=output.features, features.name=features.name))
}

