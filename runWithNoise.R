##new Analysis to run on the server

rm(list=ls(all=TRUE))


##ADJUST THESE PARAMETERS
N_RUNS =5
N_POP =20 # 80
N_GEN = 10 #100


require('Rcpp')
require('RcppArmadillo')
Rcpp::sourceCpp('./moodModel.cpp')

source('./moodModels.R')

smodel = socialModel2()#

source('./newHelperAndPlotFile.R')


source('./dcmModelTest.R')

require('matrixStats')
## we need random parameters
N = 7*7
set.seed(1234)

library(foreach)
library(doMC)
registerDoMC(cores=8)

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
  AllSimData_list[[i]] = simdata
}


##letz put that in a loop and run with a concept removed


features = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut','socialIntegration','networkStrength')

resultList = list()
avgsList = list()
for(i in 1:length(features)){
  start_time <- Sys.time()
  resultList[[i]] = estimateModelPerformance(smodel,new_prams,AllSimData_list,  pop_seq=c(N_POP),nruns=N_RUNS,niterations=N_GEN,numOfParams=32,select_features = features[1:(length(features)-i+1)])
  avgsList[[i]] = estimateAverages(resultList[[i]])
  end_time <- Sys.time()
  cat(c('i',' elapsed:',end_time-start_time,'\n'))
}
## estimate some erros and so on.


modelScores = list()
for(i in 1:length(resultList)){
  modelScores[[i]]=estimateModelScore(resultList[[i]])
}


##make a function to create Summerizing Table

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

plot(resTable$descriptiveScore,ylim=c(0,1))
points(resTable$absolutePredScore,col='red')
points(resTable$relativePredScore,col='blue')
points(resTable$sensitivityScore,col='green')

require(xtable)
restab = xtable(resTable)
##absScore 

p2 = plotConceptPredErrorMinAVGList(avgsList,paste(1:6,'concept') )

absScore = rowSums(resTable[,1:4])

plot(absScore)


p_distList = plotDistanceList(avgs_list = avgsList,type = 'avg',names = paste(6:1,'Concepts'))
#export(p_distList, file='./distNoNoiseAvg.png')

p_distListmin = plotDistanceList(avgs_list = avgsList,type = 'min',names = paste(6:1,'Concepts'))
#export(p_distListmin, file='./distNoNoiseMin.png')

p_predErrListNoNoise = plotConceptPredErrorMinAVGList(avgs_list = avgsList,names = paste(6:1,'Concepts'))
#export(p_predErrListNoNoise, file='./predNoNoiseAvg.png')

p_dhvList  = plotDHVList(avgs_list = avgsList,names = paste(6:1,'Concepts'))
#export(p_dhvList, file='./dhvNoNoise.png')


p_FitConceptList  = plotConceptFitErrorMinAVGList(avgs_list = avgsList,names = paste(6:1,'Concepts'))
#export(p_FitConceptList, file='./fitConceptsNoNoise.png')


##create a function for easier table creation
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


features = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut','socialIntegration','networkStrength')

remList = list()
for(i in 1:length(features)){
  remList[[features[i] ]] = 6
}
remList$moodLevel = 2

set.seed(1234)
#newDataList = removeDays(AllSimData_list,2)
newDataList = removeDaysWithNames(AllSimData_list,remList)
newDataList  = addNoise(newDataList,0.05 )

resultList_withNoise = list()
avgsList_withNoise = list()
for(i in 1:length(features)){
  start_time <- Sys.time()
  resultList_withNoise[[i]] = estimateModelPerformance(smodel,new_prams,newDataList,  pop_seq=c(N_POP),nruns=N_RUNS,niterations=N_GEN,numOfParams=32,select_features = features[1:(length(features)-i+1)])
  avgsList_withNoise[[i]] = estimateAverages(resultList_withNoise[[i]])
  end_time <- Sys.time()
  cat(c('i',' elapsed with noise:',end_time-start_time,'\n'))
}

noiseScoreTable = createScoreTable(resultList_withNoise)


###make plots
p_distListNoise = plotDistanceList(avgsList_withNoise ,type = 'avg',names = paste(6:1,'Concepts'))
#export(p_distListNoise, file='./distWithNoiseAvg.png')

p_distListminNoise = plotDistanceList(avgsList_withNoise ,type = 'min',names = paste(6:1,'Concepts'))
#export(p_distListminNoise, file='./distWithNoiseMin.png')


p_predErrListWithNoise = plotConceptPredErrorMinAVGList(avgsList_withNoise ,names = paste(6:1,'Concepts'))
p_predErrListWithNoise
#export(p_predErrListWithNoise, file='./predWithNoiseAvg.png')

p_dhvListWithNoise  = plotDHVList(avgsList_withNoise, names = paste(6:1,'Concepts'))
#export(p_dhvListWithNoise, file='./dhvWithNoise.png')


p_FitConceptListWithNoise  = plotConceptFitErrorMinAVGList(avgsList_withNoise,names = paste(6:1,'Concepts'))
export(p_FitConceptListWithNoise, file='./fitConceptsWithNoise.png')

noisextab = xtable(noiseScoreTable, digits = 3)


##estimate best model

#estimte Modelstrucutore for best simple model

estimateBestSimpleModel = function(data_list,pop_size=c(160),generations=100, nruns=5){
  
  goOverWholeMatrix = function(bestScore,conMatrix){
    lastScore =bestScore
    change= F
    for( y in 1:nrow(conMatrix)){
      for(x in 1:ncol(conMatrix)){
        if(conMatrix[y,x]==1){
          ##remove
          conMatrix[y,x] =0
          dcmModel = dcm(conMatrix)
          result = estimateModelPerformanceSimplerModel(dcmModel,data_list,  pop_seq=pop_size,nruns=nruns,niterations=generations)
          scores = estimateModelScore(result)
          score = scores$descriptiveScore+scores$absolutePredScore+scores$relativePredScore+scores$sensitivityScore
          score = score +  ( 1 - ((scores$n_states + scores$n_params )/ MAX_STATES)) 
          if(score >lastScore){
            lastScore = score
            change = F
          }else{
            conMatrix[y,x] =1
          }
        }
      }
    }
    list('score'= lastScore,'conMatrix'=conMatrix,'changed'=change)
  }
  conMatrix = matrix(1,nrow=4,ncol=4)
  diag(conMatrix) = 0
  colnames(conMatrix) =c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut')
  
  dcmModel = dcm(conMatrix)
  result = estimateModelPerformanceSimplerModel(dcmModel,data_list,  pop_seq=pop_size,nruns=nruns,niterations=generations)
  scores = estimateModelScore(result)
  
  bestScore =scores$descriptiveScore+scores$absolutePredScore+scores$relativePredScore+scores$sensitivityScore
  
  ### THIS IS IMPORTANT, it defines to which this model is realted to.(max complexity)
  MAX_STATES = 6 + 32 #scores$n_states + scores$n_params
  bestScore = bestScore +  ( 1 - ((scores$n_states + scores$n_params )/ MAX_STATES)) 
  
  
  changed = T
  itCount = 1
  while(changed ==T){
    resList = goOverWholeMatrix(bestScore,conMatrix)
    changed = resList$changed
    bestScore = resList$score
    conMatrix = resList$conMatrix
    cat(c('iterations:',itCount,'\n'))
    itCount = itCount +1
  }
  
  
  resList
}

#bestModel = estimateBestSimpleModel(newDataList,pop_size = 160,generations = 50);
bestModel = estimateBestSimpleModel(AllSimData_list,pop_size = 160,generations = 100);


dcmModel = dcm(bestModel$conMatrix)
result = estimateModelPerformanceSimplerModel(dcmModel,newDataList,  pop_seq=N_POP,nruns=N_RUNS,niterations=N_GEN) ##with noisy data
#result = estimateModelPerformanceSimplerModel(dcmModel,AllSimData_list,  pop_seq=80,nruns=5,niterations=50) ##with no noisy 

scores = estimateModelScore(result)
search_avgs = estimateAverages(result)

p_searchPred = plotConceptPredErrorMinAVG(search_avgs) 
#export(p_searchPred, file='./p_searchPred.png')

p_searchFitErr = plotConceptFitErrorMinAVG(search_avgs)
#export(p_searchFitErr, file='./p_searchFitErr.png')

p_predErrors = plotConceptErrorMin(search_avgs)
#export(p_predErrors, file='./p_predErrors.png')

p_fitErrors = plotConceptFitErrorMin(search_avgs)
#export(p_fitErrors, file='./p_fitErrors.png')


