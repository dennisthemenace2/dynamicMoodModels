
##Analysis of the study data

rm(list=ls(all=TRUE))

setwd('/home/dennis/Dokumente/Research/git/dynamicMoodModels/')

  
require('Rcpp')
require('RcppArmadillo')
Rcpp::sourceCpp('./moodModel.cpp')

source('./moodModels.R')

smodel = socialModel2()#

source('./helperAndPlotFile.R')
  

require('matrixStats')
  
  ## we need random parameters
  

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

colMeans(na.omit(fullModelPredErrors$predErrors)) ##this is not a good idea i think
predErrors = c()
for(i in 1:ncol(fullModelPredErrors$predErrors)){
  predErrors = c(predErrors,mean(na.omit(fullModelPredErrors$predErrors[,i])))
}

##mean prediction errors
meanPredList = getMeanModelPredictionList(valid_clientData_list,N)

#t.test(rowMeans(meanPredList$predictionError),rowMeans(fullModelPredErrors$predErrors[seq(1, nrow(fullModelPredErrors$predErrors), 5),]),paired = T)



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

#t.test(realErrors$rawErrorsList[[1]][seq(1, nrow(realErrors$rawErrorsList[[1]]), 5),4],meanPredList$rawErrorsList[[1]][,4],paired = T)


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
#colMeans(allMeans)
#[1] 0.04306457 0.03940711 0.02170839 0.03869736 0.03557977 0.03603749 0.03286829

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


##

#same z-test

x=seq(-1,5,by =  0.1)
curve(dnorm(x, mean=fullModelPerfScore$descriptiveScore,sd=sqrt(fullModelPerfScore$descriptiveScoreVar)) 
       , 0, 2,  col="blue")

curve(dnorm(x, mean=meanModelPerfScores$decriptive,sd=sqrt(meanModelPerfScores$decriptiveVar)) 
      , -1, 5,add=T,  col="red")


x=seq(-1,5,by =  0.1)
curve(dnorm(x, mean=(fullModelPerfScore$absolutePredScore+fullModelPerfScore$relativePredScore)/2,
            sd=sqrt((fullModelPerfScore$absolutePredScoreVar+fullModelPerfScore$relativePredScoreVar)/2)) 
      , -1, 2,  col="blue")

curve(dnorm(x, mean=(meanModelPerfScores$absPredictive+meanModelPerfScores$relPredictive)/2,
            sd=sqrt((meanModelPerfScores$absPredictiveVar+meanModelPerfScores$relPredictiveVar)/2)) 
      , -1, 5,add=T,  col="red")


###estimate variances in mood

allMoodMeans=c()
allMoodVars = c()
allEnjoyedMeans = c()

allEnjoyedVars = c()
allSocialVars = c()
allSocialMeans = c()

allActivitiesVars = c()
allActivitiesMeans = c()


MinORMax =0
N_MinORMax = 0

allMoodMeasures = c()
allEnjoyedMeasures = c()
allActivitiesMeasures = c()
allSocialMeasures = c()

for(i in 1:length(valid_clientData_list)){
  Smax =0
  
  ts = valid_clientData_list[[i]]
  mts = na.omit(ts$moodLevel[1:N])
  allMoodMeans=c(allMoodMeans,mean(mts))
  allMoodVars = c(allMoodVars,var(mts))
  allMoodMeasures = c(allMoodMeasures,mts)
  
  MinORMax = MinORMax+ sum(mts==1 | mts==0)
  N_MinORMax = N_MinORMax +length(mts)
  
  ets = na.omit(ts$enjoyedActivites[1:N])
  allEnjoyedVars = c(allEnjoyedVars, var(ets))
  allEnjoyedMeasures = c(allEnjoyedMeasures,ets)
  allEnjoyedMeans = c(allEnjoyedMeans, mean(ets))
  
  MinORMax = MinORMax+ sum(ets==1 | ets==0)
  N_MinORMax = N_MinORMax +length(ets)
  
  sts = na.omit(ts$socialInteraction[1:N])
  allSocialVars = c(allSocialVars, var(sts))
  allSocialMeasures = c(allSocialMeasures,sts)
  allSocialMeans = c(allSocialMeans, mean(sts))
  
  
   MinORMax = MinORMax+ sum(sts==1 | sts==0)
  N_MinORMax = N_MinORMax +length(sts)
  
  ats = na.omit(ts$activitiesCarriedOut[1:N])
  allActivitiesVars = c(allActivitiesVars, var(ats))
  allActivitiesMeans = c(allActivitiesMeans, mean(ats))
  allActivitiesMeasures = c(allActivitiesMeasures,ats)
  MinORMax = MinORMax+ sum(ats==1 | ats==0)
  N_MinORMax = N_MinORMax +length(ats)
  
}
sqrt(mean(allMoodVars))
sqrt(mean(allEnjoyedVars))
sqrt(mean(allSocialVars))
sqrt(mean(allActivitiesVars))
MinORMax / N_MinORMax



require('plotly')

p <- plot_ly(alpha = 0.7) %>%
  add_histogram(x = round(allMoodMeasures,1),name='Mood', histnorm = "probability") %>%
  add_histogram(x = round(allSocialMeasures,1),name='Social', histnorm = "probability",histfunc='sum') %>%
  add_histogram(x = round(allActivitiesMeasures,1),name='Activities', histnorm = "probability",histfunc='sum') %>%
  add_histogram(x = round(allEnjoyedMeasures,1),name='Enjoyed', histnorm = "probability",histfunc='sum') %>%
  layout(barmode = "overlay",
              xaxis = list(title = "Measure",
                             zeroline = FALSE),
                yaxis = list(title = "Frequency",
                             zeroline = FALSE))
         
p

ks.test(allMoodMeasures, "pnorm")

ks.test(allEnjoyedMeasures, "pnorm")


ks.test(mts, "pnorm")

x <- rnorm (100)
Box.test (allMoodMeasures, lag = 1)
Box.test (mts, lag = 1, type = "Ljung")

