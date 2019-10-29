
##Analysis of the study data

rm(list=ls(all=TRUE))

setwd('/home/dennis/Dokumente/Research/git/dynamicMoodModels/')

library("mco")
library("eaf")
library("emoa")

require('Rcpp')
require('RcppArmadillo')
Rcpp::sourceCpp('./moodModel.cpp')

source('./moodModels.R')

smodel = socialModel2()#

source('./helperAndPlotFile.R')
  

require('matrixStats')
  

options(error = recover)
options(warnings = recover)
options(stringsAsFactors = FALSE)
options(warn=1)
###


set.seed(1234)

#setup constants
N = 7*7
N_TRAIN =7*6
N_TEST=7

##set up parellel
library(foreach)
library(doMC)
registerDoMC(cores=4)


##load Data
valid_clientData_list= readRDS('./validClientsList.RDS')
####predicitve perforamnce

clientData_results = estimateModelPerformance(smodel,new_prams=NULL,valid_clientData_list,  pop_seq=c(80),nruns=5,niterations=100,numOfParams=32,select_features = NULL)
###
#do some plots and stuff
##
##estimtae evarages
clientData_results_avgs = estimateAverages(clientData_results)
#dhv =plotDHV(clientData_results_avgs)

##inspect convergence of the optimization
avgConcept = plotConceptError(clientData_results_avgs)
avgConcept

avgConceptPred = plotConceptError(clientData_results_avgs)
avgConceptPred

coneptErrorMin =  plotConceptErrorMin(clientData_results_avgs)
coneptErrorMin

conceptFitMin = plotConceptFitErrorMin(clientData_results_avgs)
conceptFitMin

conceptFitAvg = plotConceptFitError(clientData_results_avgs)
conceptFitAvg

fullModelPerfScore = estimateModelScore(clientData_results)


fullModelPredErrors = estimatePredictionErrors(clientData_results)

colMeans(fullModelPredErrors$minPredErrors)

colMeans(na.omit(fullModelPredErrors$predErrors)) #
predErrors = c()
for(i in 1:ncol(fullModelPredErrors$predErrors)){
  predErrors = c(predErrors,mean(na.omit(fullModelPredErrors$predErrors[,i])))
}

##mean prediction errors
meanPredList = getMeanModelPredictionList(valid_clientData_list,N)

t.test(rowMeans(meanPredList$predictionError),rowMeans(fullModelPredErrors$predErrors[seq(1, nrow(fullModelPredErrors$predErrors), 5),]),paired = T)



meanPrediction = meanPredList$predictionError
colMeans(meanPrediction)

mean(colMeans(meanPrediction))

###model fit err
colMeans(fullModelPredErrors$fittingError)
mean(colMeans(fullModelPredErrors$fittingError))
##mean fit err
colMeans(meanPredList$fittingError)
mean( colMeans(meanPredList$fittingError))


###estimate simulation errors 
realErrors = estimateSimulatedPredictionErrors(clientData_results,smodel,valid_clientData_list,n_train=42)
colMeans(na.omit(realErrors$rawErrorsList[[1]]) )
predErrorsRaw = c()
for(i in 1:ncol(realErrors$rawErrorsList[[1]])){
  predErrorsRaw = c(predErrorsRaw,mean(na.omit(realErrors$rawErrorsList[[1]][,i])))
}
predErrorsRaw

p <- plot_ly() 
allMeans = matrix(,nrow=0,ncol=7)
MeanModelallMeans = matrix(,nrow=0,ncol=7)


for(i in 1:4){
  #cmean = colMeans(na.omit(realErrors$rawErrorsList[[i]]) )
  cmean = c()
  cvar =c()
  meanModelmean = c()
  meanModelvar = c()
  for(k in 1:ncol(realErrors$rawErrorsList[[i]])){
    cmean = c(cmean,mean(na.omit(realErrors$rawErrorsList[[i]][,k])))
    cvar = c(cvar, var(na.omit(realErrors$rawErrorsList[[i]][,k]) ) )
    meanModelmean = c(meanModelmean,mean(na.omit(meanPredList$rawErrorsList[[i]][,k])))
    meanModelvar = c(meanModelvar,var(na.omit(meanPredList$rawErrorsList[[i]][,k])))
  }
  allMeans = rbind(allMeans,cmean )
  MeanModelallMeans =  rbind(allMeans,meanModelmean )
 # cvar = colVars(na.omit(realErrors$rawErrorsList[[i]]) )                 
  p<-add_trace(p,x = 1:7, y = sqrt(cmean), type = 'scatter', mode = 'lines+markers',
             name = colnames(realErrors$predErrors)[i],
             error_y = list(value = sqrt(cvar), width = 2,opacity=0.2),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
             line = list( width = 2), color=as.factor(i))
  ####mean model
  p<-add_trace(p,x = 1:7, y = sqrt(meanModelmean), type = 'scatter', mode = 'lines+markers',
               name = paste('Mean model',colnames(realErrors$predErrors)[i]) ,
               error_y = list(value = sqrt(meanModelvar), width = 2,opacity=0.2),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
               line = list( width = 2), color=as.factor(i), linetype=I("dot")  )
}
p

x <- list(
  title = "Prediction on day",
  ticktext = 1:7,
  titlefont = f,
  tickfont = f2
)
y <- list(
  title = "RMSE",
  titlefont = f,
  tickfont = f2
)
p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))

colMeans(allMeans)
p<-add_trace(p,x = 1:7, y = sqrt(colMeans(allMeans)), type = 'scatter', mode = 'lines+markers',
             name = 'Average',
             error_y = list(value = sqrt(colVars(allMeans)), width = 2,opacity=0.2),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
             line = list( width = 2) )
p


###append mean model

p<-add_trace(p,x = 1:7, y = sqrt(colMeans(MeanModelallMeans)), type = 'scatter', mode = 'lines+markers',
             name = 'Average',
             error_y = list(value = sqrt(colVars(MeanModelallMeans)), width = 2,opacity=0.2),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
             line = list( width = 2) )
p

plot(colMeans(na.omit(realErrors$rawErrorsList[[1]]) ),ylim=c(0,0.1) )
for(i in 2:4){
  points(colMeans(na.omit(realErrors$rawErrorsList[[i]]) ) ,col=i)
}
means = c()
for(i in 1:4){
  means = c(means, mean(colMeans(na.omit(realErrors$rawErrorsList[[i]]) ) ) )
}


estimateMeanModelPerformanceScores(meanPredList)

meanModelPerfScores =estimateMeanModelPerformanceScores(meanPredList)
