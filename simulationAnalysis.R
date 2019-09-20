##simulation analysis


rm(list=ls(all=TRUE))

require('Rcpp')
require('RcppArmadillo')
Rcpp::sourceCpp('./moodModel.cpp')

source('./moodModels.R')

#use c++ version for faster evaluation
smodel = socialModel2()#


source('./helperAndPlotFile.R')


require('matrixStats')

##set up parellel
library(foreach)
library(doMC)
registerDoMC(cores=4)

## we need random parameters
N = 7*7
N_TRAIN =7*6
N_TEST=7

set.seed(1234)
###generate paramters to be used
new_prams = list()
for(paramsetting in 1:100){
 new_prams[[paramsetting]] =runif(32, 0,1) ##letz save them
}
###

#create Data Once
AllSimData_list = list()
for(i in 1:length(new_prams)) {
  smodel$setParameters(new_prams[[paramsetting]])
  simdata = smodel$simulate(N)
  AllSimData_list[[i]] = simdata[,2:ncol(simdata)]
}


###run with noise..
features = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut','socialIntegration','networkStrength')

remList = list()
for(i in 1:length(features)){
  remList[[features[i] ]] = 6
}
remList$moodLevel = 2

set.seed(1234)

newDataList_removedDays = removeDaysWithNames(AllSimData_list,remList,N_TRAIN)

resultList_withNoise = list()
avgsList_withNoise = list()
avgPredictionErrors = c()
avgPredictionErrorsVar = c()
avgFitErrors=c()
avgFitErrorsVar=c()
meanModelPredictionsArrayNoise = c()
meanModelPredictionsArrayNoiseVar = c()
meanModelFitArrayNoise = c()
meanModelFitArrayNoiseVar= c()

meanModelDescScore = c()
meanModelDescScoreVar=c()
meanModelabsPredScore = c()
meanModelrelPredScore = c()

meanModelabsPredScoreVar = c()
meanModelrelPredScoreVar = c()


noise_seq = seq(0,0.5,0.05)
##create Data list First 


##run analysis
for(i in 1:length(noise_seq) ){
  start_time <- Sys.time()
  newDataList  = addNoise(newDataList_removedDays,noise_seq[i],N_TRAIN )
  
  resultList_withNoise[[i]] = estimateModelPerformance(smodel,new_prams,newDataList,  pop_seq=c(80),nruns=5,niterations=100,numOfParams=32,select_features = NULL)
  avgsList_withNoise[[i]] = estimateAverages(resultList_withNoise[[i]])
  
  predErrors = estimatePredictionErrors(resultList_withNoise[[i]])
  conceptMeanErrors = colMeans(na.omit(predErrors$predErrors)) 
  avgPredictionErrors=c(avgPredictionErrors, mean(conceptMeanErrors) )
  avgPredictionErrorsVar=c(avgPredictionErrorsVar, var(conceptMeanErrors) )
  
  conceptFitMeanErrors = colMeans(na.omit(predErrors$fittingError)) 
  avgFitErrors=c(avgFitErrors, mean(conceptFitMeanErrors) )
  avgFitErrorsVar=c(avgFitErrorsVar, var(conceptFitMeanErrors) )
  
  ##save mean model prediction
  meanModelPredictions = getMeanModelPredictionList(newDataList,N = N)
  meanModelPredictionsArrayNoise = c(meanModelPredictionsArrayNoise,mean( colMeans(meanModelPredictions$predictionError)))
  meanModelPredictionsArrayNoiseVar = c(meanModelPredictionsArrayNoiseVar,var( colMeans(meanModelPredictions$predictionError)))
 
  meanModelFitArrayNoise = c(meanModelFitArrayNoise,mean( colMeans(meanModelPredictions$fittingError)))
  meanModelFitArrayNoiseVar = c(meanModelFitArrayNoiseVar,var( colMeans(meanModelPredictions$fittingError)))
  
  
  
  meanModelPerfScores = estimateMeanModelPerformanceScores(meanModelPredictions)
  
  meanModelDescScore = c(meanModelDescScore,meanModelPerfScores$decriptive)
  meanModelDescScoreVar =c(meanModelDescScoreVar,meanModelPerfScores$decriptiveVar)
  meanModelabsPredScore = c(meanModelabsPredScore,meanModelPerfScores$absPredictive)
  meanModelabsPredScoreVar = c(meanModelabsPredScoreVar,meanModelPerfScores$absPredictiveVar)
  meanModelrelPredScore = c(meanModelrelPredScore,meanModelPerfScores$relPredictive)
  meanModelrelPredScoreVar = c(meanModelrelPredScoreVar,meanModelPerfScores$relPredictiveVar)
 
  
  end_time <- Sys.time()
  cat(c('i:',i,'/',length(noise_seq)  ,' elapsed with noise:',end_time-start_time,'\n'))
}

system("notify-send \"R script finished running\"")


createScoreTable = function(resultList){
  
  modelScores = list()
  for(i in 1:length(resultList)){
    modelScores[[i]]=estimateModelScore(resultList[[i]])
  }
  
  resTable = NULL
  for(i in 1:length(modelScores)){
    cm = modelScores[[i]]
    if(is.null(resTable) ){
      resTable = unlist(cm)
    }else{
      resTable = rbind(resTable , unlist(cm))
    }
  }
  rownames(resTable)= c(1:nrow(resTable))
  resTable= as.data.frame(resTable)
  
}

noiseScoreTable = createScoreTable(resultList_withNoise)
###
#estimate prediction errors
noise_plot = plotScoreTable(noiseScoreTable,noise_seq)
noise_plot<-add_trace(noise_plot,x = noise_seq, y = sqrt(avgPredictionErrorsVar) , type = 'scatter', mode = 'lines+markers',
                      name = 'Prediction error',
                      error_y = list(value = sqrt(avgPredictionErrorsVar), width = 2,opacity=0.1),
                      line = list( width = 2) )
noise_plot
###change nDays and noise

noise_plot<-add_trace(noise_plot,x = noise_seq, y = sqrt(meanModelPredictionsArrayNoise) , type = 'scatter', mode = 'lines+markers',
                      name = 'Mean prediction error',
                      error_y = list(value = sqrt(meanModelPredictionsArrayNoiseVar), width = 2,opacity=0.1),
                      line = list( width = 2) )
noise_plot


noise_plot<-add_trace(noise_plot,x = noise_seq, y = meanModelDescScore , type = 'scatter', mode = 'lines+markers',
                      name = 'Mean model descriptive score',
                      line = list( width = 2) )
noise_plot




noise_plot<-add_trace(noise_plot,x = noise_seq, y = (meanModelabsPredScore+meanModelrelPredScore)/2 , type = 'scatter', mode = 'lines+markers',
                      name = 'Mean model predictive score',
                      line = list( width = 2) )
noise_plot

#noise_plot<-add_trace(noise_plot,x = noise_seq, y = meanModelabsPredScore , type = 'scatter', mode = 'lines+markers',
#                      name = 'Mean model abd. predictive score',
#                      line = list( width = 2) )
#noise_plot

#noise_plot<-add_trace(noise_plot,x = noise_seq, y = meanModelrelPredScore , type = 'scatter', mode = 'lines+markers',
#                      name = 'Mean model rel. predictive score',
#                      line = list( width = 2) )
#noise_plot

###

##plot in same color
p <- plot_ly() 

opacityValue = 0.5

idx =noise_seq
p<-add_trace(p,x = idx, y = noiseScoreTable$descriptiveScore, type = 'scatter', mode = 'lines+markers',
             name = 'Descriptive performance',
             error_y = list(value = sqrt(noiseScoreTable$descriptiveScoreVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(1) )

p<-add_trace(p,x = idx, y = meanModelDescScore , type = 'scatter', mode = 'lines+markers',
             name = 'Mean model descriptive performance',
             error_y = list(value = sqrt(meanModelDescScoreVar), width = 2,opacity=opacityValue),
             line = list( width = 2) , color=as.factor(1), linetype=I("dot"),marker=list(symbol='diamond-open') )


p<-add_trace(p,x = idx, y = (noiseScoreTable$absolutePredScore+noiseScoreTable$relativePredScore)/2 , type = 'scatter', mode = 'lines+markers',
             name = 'Predictive performance',
             error_y = list(value = sqrt((noiseScoreTable$absolutePredScoreVar+noiseScoreTable$relativePredScoreVar)/2), width = 2,opacity=opacityValue),
             line = list( width = 2), color=as.factor(2)  )


p<-add_trace(p,x = idx, y = (meanModelabsPredScore+meanModelrelPredScore)/2 , type = 'scatter', mode = 'lines+markers',
             name = 'Mean model predictive performance',
             error_y = list(value = sqrt((meanModelabsPredScoreVar+meanModelrelPredScoreVar)/2), width = 2,opacity=opacityValue),
             line = list( width = 2), color=as.factor(2), linetype=I("dot") ,marker=list(symbol='diamond-open') )

p


p<-add_trace(p,x = idx, y = sqrt(avgPredictionErrors) , type = 'scatter', mode = 'lines+markers',
             name = 'Prediction error',
             error_y = list(value = sqrt(avgPredictionErrorsVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(3) )


p<-add_trace(p,x = idx, y = sqrt(meanModelPredictionsArrayNoise) , type = 'scatter', mode = 'lines+markers',
             name = 'Mean prediction error',
             error_y = list(value = sqrt(meanModelPredictionsArrayNoiseVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(3), linetype=I("dot") ,marker=list(symbol='diamond-open'))
p

p<-add_trace(p,x = idx, y = sqrt(avgFitErrors) , type = 'scatter', mode = 'lines+markers',
             name = 'Fit error',
             error_y = list(value = sqrt(avgFitErrorsVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(4) )


p<-add_trace(p,x = idx, y = sqrt(meanModelFitArrayNoise) , type = 'scatter', mode = 'lines+markers',
             name = 'Mean fit error',
             error_y = list(value = sqrt(meanModelFitArrayNoiseVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(4), linetype=I("dot") ,marker=list(symbol='diamond-open'))
p

####
y <- list(
  title = "Score",
  titlefont = f,
  tickfont = f2
)

x <- list(
  title = "Noise",
  ticktext = noise_seq,
  titlefont = f,
  tickfont = f2
)

p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
p



############



## This is the second simulation analysis. We estimated the noise from the data and simulate missing values.


set.seed(1234)

resultList_complete =list()
avgsList_complete = list()

meanModelFitNoiseArrayVar =c()
meanModelFitNoiseArray =c()
avgFitErrorsAndNa=c( )
avgFitErrorsAndNaVar =c( )



avgPredictionErrorsAndNa = c()
avgPredictionErrorsAndNaVar = c()
resultList_withNoiseAndNa =list()
avgsList_withNoiseAndNa = list()

meanModelDescScoreNA = c()
meanModelDescScoreNAVar = c()
meanModelabsPredScoreNA = c()
meanModelabsPredScoreNAVar = c()
meanModelrelPredScoreNA = c()
meanModelrelPredScoreNAVar = c()

meanModelPredictionsArray = c()
meanModelPredictionsArrayVar =c()
for(na in 0:6){
  start_time_na <- Sys.time()
  set.seed(1234)
  newDataList_removedDays = removeDays(AllSimData_list,na,N_TRAIN)
  noiseList = list('moodLevel'=0.137,'socialInteraction'=0.200,
                   'enjoyedActivites'= 0.173,'activitiesCarriedOut'= 0.181,
                   'networkStrength'=0,'socialIntegration'=0)
  start_time <- Sys.time()
  newDataList  = addNoise(newDataList_removedDays,noiseList,N_TRAIN )
  
  resultList_withNoiseAndNa[[na+1]] = estimateModelPerformance(smodel,new_prams,newDataList,  pop_seq=c(80),nruns=5,niterations=100,numOfParams=32,select_features = NULL)
  avgsList_withNoiseAndNa[[na+1]] = estimateAverages(resultList_withNoiseAndNa[[na+1]])
  
  predErrors = estimatePredictionErrors(resultList_withNoiseAndNa[[na+1]])
  conceptMeanErrors = colMeans(na.omit(predErrors$predErrors)) 
  conceptFitMeanErrors = colMeans(na.omit(predErrors$fittingError)) 
  
  avgPredictionErrorsAndNa=c(avgPredictionErrorsAndNa, mean(conceptMeanErrors) )
  avgPredictionErrorsAndNaVar =c(avgPredictionErrorsAndNaVar, var(conceptMeanErrors) )
  ###
  avgFitErrorsAndNa=c(avgFitErrorsAndNa, mean(conceptFitMeanErrors) )
  avgFitErrorsAndNaVar =c(avgFitErrorsAndNaVar, var(conceptFitMeanErrors) )
  
  
  ##save mean model prediction
  meanModelPredictions = getMeanModelPredictionList(newDataList,N = N)
  meanModelPredictionsArray = c(meanModelPredictionsArray,mean( colMeans(meanModelPredictions$predictionError)))
  meanModelPredictionsArrayVar = c(meanModelPredictionsArrayVar,var( colMeans(meanModelPredictions$predictionError)))
  
  meanModelFitNoiseArray = c(meanModelFitNoiseArray,mean( colMeans(meanModelPredictions$fittingError)))
  meanModelFitNoiseArrayVar = c(meanModelFitNoiseArray,var( colMeans(meanModelPredictions$fittingError)))
  
  
  meanModelPerfScores = estimateMeanModelPerformanceScores(meanModelPredictions)
  
  meanModelDescScoreNA = c(meanModelDescScoreNA,meanModelPerfScores$decriptive)
  meanModelDescScoreNAVar = c(meanModelDescScoreNAVar,meanModelPerfScores$decriptiveVar)
  meanModelabsPredScoreNA = c(meanModelabsPredScoreNA,meanModelPerfScores$absPredictive)
  meanModelabsPredScoreVar = c(meanModelabsPredScoreVar,meanModelPerfScores$absPredictiveVar)
  meanModelrelPredScoreNA = c(meanModelrelPredScoreNA,meanModelPerfScores$relPredictive)
  meanModelrelPredScoreNAVar = c(meanModelrelPredScoreNAVar,meanModelPerfScores$relPredictiveVar)
  
 
  end_time_na <- Sys.time()
  cat(c('na:',na,' elapsed for na:',end_time_na-start_time_na,'\n'))
  
}

system("notify-send \"R script finished running\"")

#naAvgPredErr = matrix(avgPredictionErrorsAndNa,nrow=6,byrow = T)

#####


noiseScoreTableNA = createScoreTable(resultList_withNoiseAndNa )
###
#estimate prediction errors
noise_plot_NA = plotScoreTable(noiseScoreTableNA,0:6)
noise_plot_NA<-add_trace(noise_plot_NA,x = 0:6, y = sqrt(avgPredictionErrorsAndNa) , type = 'scatter', mode = 'lines+markers',
                      name = 'Prediction error',
                      line = list( width = 2) )
noise_plot_NA


noise_plot_NA<-add_trace(noise_plot_NA,x = 0:6, y = sqrt(meanModelPredictionsArray) , type = 'scatter', mode = 'lines+markers',
                      name = 'Mean prediction error',
                      line = list( width = 2) )
noise_plot_NA


x <- list(
  title = "Missing values per week",
  ticktext = 0:6,
  titlefont = f,
  tickfont = f2
)
y <- list(
  title = "Score",
  titlefont = f,
  tickfont = f2
)
noise_plot_NA  = layout(noise_plot_NA,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
noise_plot_NA

####################

noiseScoreTable2 = createScoreTable(resultList_withNoiseAndNa )

p <- plot_ly() 

idx =0:6
p<-add_trace(p,x = idx, y = noiseScoreTable2$descriptiveScore, type = 'scatter', mode = 'lines+markers',
             name = 'Descriptive performance',
             error_y = list(value = sqrt(noiseScoreTable2$descriptiveScoreVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(1) )

p<-add_trace(p,x = idx, y = meanModelDescScoreNA , type = 'scatter', mode = 'lines+markers',
             name = 'Mean model descriptive performance',
             error_y = list(value = sqrt(meanModelDescScoreNAVar), width = 2,opacity=opacityValue),
             line = list( width = 2) , color=as.factor(1), linetype=I("dot"),marker=list(symbol='diamond-open') )


p<-add_trace(p,x = idx, y = (noiseScoreTable2$absolutePredScore+noiseScoreTable2$relativePredScore)/2 , type = 'scatter', mode = 'lines+markers',
             name = 'Predictive performance',
             error_y = list(value = sqrt((noiseScoreTable2$absolutePredScoreVar+noiseScoreTable2$relativePredScoreVar)/2), width = 2,opacity=opacityValue),
             line = list( width = 2), color=as.factor(2)  )


p<-add_trace(p,x = idx, y = (meanModelabsPredScoreNA+meanModelrelPredScoreNA)/2 , type = 'scatter', mode = 'lines+markers',
             name = 'Mean model predictive performance',
             error_y = list(value = sqrt((meanModelabsPredScoreNAVar+meanModelrelPredScoreNAVar)/2), width = 2,opacity=opacityValue),
             line = list( width = 2), color=as.factor(2), linetype=I("dot") ,marker=list(symbol='diamond-open') )

p


p<-add_trace(p,x = 0:6, y = sqrt(avgPredictionErrorsAndNa) , type = 'scatter', mode = 'lines+markers',
             name = 'Prediction error',
             error_y = list(value = sqrt(avgPredictionErrorsAndNaVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(3) )


p<-add_trace(p,x = 0:6, y = sqrt(meanModelPredictionsArray) , type = 'scatter', mode = 'lines+markers',
             name = 'Mean prediction error',
             error_y = list(value = sqrt(meanModelPredictionsArrayVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(3), linetype=I("dot"),marker=list(symbol='diamond-open') )
p

p<-add_trace(p,x = 0:6, y = sqrt(avgFitErrorsAndNa) , type = 'scatter', mode = 'lines+markers',
             name = 'Fit error',
             error_y = list(value = sqrt(avgFitErrorsAndNaVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(4) )


p<-add_trace(p,x = 0:6, y = sqrt(meanModelFitNoiseArray) , type = 'scatter', mode = 'lines+markers',
             name = 'Mean fit error',
             error_y = list(value = sqrt(meanModelFitNoiseArrayVar), width = 2,opacity=opacityValue),
             line = list( width = 2),color=as.factor(4), linetype=I("dot"),marker=list(symbol='diamond-open') )
p
####
p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
p

