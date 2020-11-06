###
#' Identify PV and non-PV consumers using linear classifiers.
#'
#' It identifies PV and non-PV consumers using a set of linear classifiers, i.e., Naive Bayesian,
#' Support Vector Machine, Generalized Linear Model, Linear Discriminate Analysis and Perceptron.
#' @param data.train The training dataset presented as a dataframe. Each row of the dataframe demonstrates the features of one consumer, and
#' the row number is the number of consumers. The last column should be the class label.
#' @param data.test The testing dataset presented as a dataframe. Each row of the dataframe demonstrate the features of one consumer, and
#' the row number is the number of consumers. The last column should be the class label.
#' @param trControl A list of values that define how these linear classifiers been trained using the package 'caret'.
#' See trainControl and \url{http://topepo.github.io/caret/using-your-own-model-in-train.html}.
#' @keywords classify
#' @return model.name --- A character vector used to demonstrate the linear classifiers used in this function.
#' @return results --- A numeric vector of testing accuracy obtained from the correspondence classifiers listed in 'model.name'.
#' @import matrixStats
#' @import caret
#' @import stats
#' @export
#' @examples
#' ### Load the required packages.
#'
#' rm(list=ls())
#'
#' library(caret)
#'
#' library(matrixStats)
#'
#' library(GGally)
#'
#' ### Load the smart meter readings and the simulated PV generations.
#'
#' data("loadsample")
#' data("pvSample")
#'
#' #######################################################
#' ## Get the load profiles of both PV and non-PV consumers.
#'
#' N.house<-ncol(loadsample[,-c(1:6)]) # N.house is the total number of households.
#'
#' NPV.day<-lapply(loadsample[,-c(1:6)], load.daily, load.date=loadsample[,1], num.obs=48)
#'
#' # Assuming 30% of the households use PV. Generate the PV samples.
#' # Remark that in a real experiment there should be hundreds or thousands of simulation replication.
#' # Only one replication is shown below for the instant instruction.
#'
#' perc=0.3
#' pv.house<-floor(N.house*perc)
#' M<-ncol(pvSample[,-c(1:6)])
#' pv.rand<-sample(1:M, pv.house, replace = TRUE, prob = rep(1/M, times=M))
#' pvSample<-cbind(pvSample[ ,c(1:6)], pvSample[ ,-c(1:6)][ ,pv.rand])
#'
#' date.PV.NPV<-unique(pvSample$date)
#' PV.day<-lapply(pvSample[,-c(1:6)], load.daily, load.date=pvSample$date, num.obs=48)
#'
#' house_pv<-sample(N.house)[1:pv.house] # Randomly select the PV households.
#'
#' # Compute the real load demand of the PV consumers using the smart meter reading (loadsample),
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
#'             rep(1, times=length(demand_npv)))
#' label.PV<-factor(label.PV)
#'
#' ############ Prepare the features used to identify PV and non-PV users.
#' date.week<-weekdays(as.Date(date.PV.NPV))
#' x.week<-date.week
#' mor.be<-13
#' aft.be<-39
#'
#' feature.PV<-lapply(demand_pv, features.load, x.week, mor.be, aft.be)
#' feature.NPV<-lapply(demand_npv, features.load, x.week, mor.be, aft.be)
#'
#' features.name<-feature.PV[[1]]$features.name # Get the name of the computed features.
#'
#' # Prepare the feature matrix.
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
#' ##### Compute the significance of each feature and select the 12 most significant features.
#'
#' feature.PV.NPV<-rbind(feature.PV.new, feature.NPV.new)
#'
#' p.value<-apply(feature.PV.NPV, 2, features.importance, label.PV)
#'
#' p.value<-sort(p.value, decreasing = FALSE, index.return=TRUE)
#' features.order<-features.name[p.value$ix]
#'
#' PV_feature<-feature.PV.new[,p.value$ix[1:12]]
#' NPV_feature<-feature.NPV.new[ ,p.value$ix[1:12]]
#'
#' ##### Visualize the differences of between the features of PV and non-PV consumers.
#' ##### Top 5 features are selected for example.
#'
#' PV.NPV<-data.frame(rbind(PV_feature[,1:5], NPV_feature[,1:5]))
#' PV.NPV$cls<-label.PV
#'
#' PV.NPV<-PV.NPV[sample(nrow(PV.NPV)), ]
#'
#' PV.NPV$cls<- factor(PV.NPV$cls, levels = c(0,1), labels = c("PV", "non-PV"))
#'
#' options(repr.P.width=14,repr.P.height=10)
#' ggpairs(PV.NPV, title="The 5 most significant features",
#'         mapping=ggplot2::aes(colour = cls))
#'
#' ######### Use the above selected features to classify PV and non-PV consumers.
#'
#' #### Prepare the training and testing datasets.
#'
#' set.seed(123)
#'
#' # 70% of the data (PV and non-PV) are used for training, and the remainder are used for testing
#'
#'  train.data<-0.7
#'
#' # Prepare the training and testing datasets of PV consumers.
#'
#' n.pv<-sample(nrow(PV_feature))
#' data.train.pv<-as.matrix(PV_feature[n.pv[1:floor(train.data*length(n.pv))], ])
#' data.test.pv<-as.matrix(PV_feature[n.pv[-c(1:floor(train.data*length(n.pv)))], ])
#'
#' pv.cls.test<-rep(0, times=nrow(data.test.pv))
#' pv.cls.train<-rep(0, times=nrow(data.train.pv))
#'
#' # Prepare the training and testing datasets of non-PV consumers.
#'
#' n.npv<-sample(nrow(NPV_feature))
#' data.train.npv<-as.matrix(NPV_feature[n.npv[1:floor(train.data*length(n.npv))], ])
#' data.test.npv<-as.matrix(NPV_feature[n.npv[-c(1:floor(train.data*length(n.npv)))], ])
#'
#' npv.cls.test<-rep(1, times=nrow(data.test.npv))
#' npv.cls.train<-rep(1, times=nrow(data.train.npv))
#'
#' ### Composite the training dataset.
#'
#' data.train<-as.data.frame(rbind(data.train.pv, data.train.npv))
#' data.train$cls<-c(pv.cls.train, npv.cls.train)
#' data.train$cls<-factor(data.train$cls, levels = c(0,1), labels = c("False", "True"))
#'
#' ### Composite the testing dataset.
#'
#' data.test<-as.data.frame(rbind(data.test.pv, data.test.npv))
#' data.test$cls<-c(pv.cls.test, npv.cls.test)
#' data.test$cls<-factor(data.test$cls, levels = c(0,1), labels = c("False", "True"))
#'
#' # 5-Fold cross-validation is used for model selection.
#'
#' classify.results<-classify.pv.npv( data.train,
#'                                    data.test,
#'                                    trControl=trainControl(method='cv',number=5))
#'
#' # Print the testing results.
#'
#' classify.results
#'


classify.pv.npv<-function(data.train, data.test, trControl){

  options(warn=-1)

  # Compute the first two principle components.
  res.pca <- prcomp(data.train[,-ncol(data.train)], scale = FALSE)

  eigs<- res.pca$sdev^2
  eigs[1:2]/sum(eigs)

  train.pca<-res.pca$x

  feature.train<-data.frame(PC1=train.pca[ ,1], PC2=train.pca[ ,2], cls=data.train$cls)

  n<-sample(nrow(feature.train))
  feature.train<-feature.train[n, ]

  test.pca <- predict(res.pca, newdata = data.test)
  feature.test<-data.frame(PC1=test.pca[ ,1], PC2=test.pca[ ,2], cls=data.test$cls)
  n<-sample(nrow(feature.test))
  feature.test<-feature.test[n, ]


####### Naive Bayesian classifier
  model.nb = caret::train(feature.train[,-ncol(feature.train)],
                        feature.train[,ncol(feature.train)],'nb',
                        trControl=trControl)


  Predict.nb <- predict(model.nb,newdata =feature.test)
  NB.test.acc<-sum(Predict.nb ==feature.test$cls)/nrow(feature.test)

###### Support Vector Machine
  model.svm = caret::train(feature.train[,-ncol(feature.train)],
                         feature.train[,ncol(feature.train)],'svmLinear2',
                         trControl=trControl)

  Predict.svm <- predict(model.svm, newdata =feature.test)
  SVM.test.acc<-sum(Predict.svm ==feature.test$cls)/nrow(feature.test)

###### Generalized linear model
  model.glm = caret::train(feature.train[,-ncol(feature.train)],
                         feature.train[,ncol(feature.train)],'glm',
                         trControl=trControl)

  Predict.glm<- predict(model.glm, newdata =feature.test)
  GLM.test.acc<-sum(Predict.glm ==feature.test$cls)/nrow(feature.test)

##### Linear Discriminant Analysis
  model.lda = caret::train(feature.train[,-ncol(feature.train)],
                         feature.train[,ncol(feature.train)],'lda',
                         trControl=trControl)

  Predict.lda<- predict(model.lda, newdata =feature.test)
  LDA.test.acc<-sum(Predict.lda ==feature.test$cls)/nrow(feature.test)


##### Perceptron
  model.NN = caret::train(feature.train[,-ncol(feature.train)],
                        feature.train[,ncol(feature.train)],'mlp',size=1,
                        trControl=trainControl(method='cv',number=5))

  Predict.NN<- predict(model.NN, newdata =feature.test)
  NN.test.acc<-sum(Predict.NN ==feature.test$cls)/nrow(feature.test)


  results<-c(NB.test.acc,
           SVM.test.acc, GLM.test.acc,
           LDA.test.acc, NN.test.acc)
  model.name<-c('Naive Bayesian', 'Support Vector Machine', 'Generalized Linear Model',
                'Linear Discriminate Analysis', 'Perceptron')
return(list(model.name=model.name, results=results))
}



