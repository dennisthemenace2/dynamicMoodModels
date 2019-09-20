#model

model <- setRefClass("model",
                     fields = list( ),
                     methods = list(
                       setParameters = function(parameters){
                         
                       },
                       train = function(x,y) {
                         #
                         cat("BASE MODEL", "\n")      
                       },
                       predict = function(x) {                      
                         cat("BASE MODEL", "\n")
                         1
                       },
                       getNumberOfParameters = function(){
                         cat("BASE MODEL", "\n")
                         0
                       },
                       getNumberOfConcepts = function(){
                         1
                       }
                       
                       
                     ))


library(deSolve)

######letz try to get altafs social support model
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
    sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}

# R implementation of the social integration model
socialModel <- setRefClass("socialModel",
                         fields = list(parameters = "numeric",params ="data.frame",true_parameters = "numeric"  ),
                         contains = "model", 
                         
                         methods = list(
                           simulate = function(length) {
                             
                             state <- c(socialInteraction = params$socialInteraction, 
                                        networkStrength =  params$networkStrength,
                                        moodLevel = params$moodLevel,
                                        socialIntegration = params$socialIntegration,
                                        activitiesCarriedOut = params$activitiesCarriedOut,
                                        enjoyedActivites = params$enjoyedActivites
                             )
                             
                             times <- seq(from = 0, to = length*0.1, by =0.1)
                             out <- ode (times = times, y = state, func = .self$rigidode, parms = .self, method = "ode45")
                             
                             ###check length for ode
                             out = out[1:length,]
                             
                             as.data.frame(out)
                           },
                           rigidode = function(t, state, parameters){
                             with(as.list(c(state, parameters)),{
                               # rate of change
                               
                               impact_socialInteraction=c(params$w_socialInteraction_mood*moodLevel,params$w_socialInteraction_network*networkStrength,params$w_socialInteraction_activitiesCarriedOut*activitiesCarriedOut )
                               
                               dsocialInteraction = params$eta_socialInteraction * ( .self$alogistic(impact_socialInteraction, params$sigma_socialInteraction ,params$tau_socialInteraction )  - socialInteraction)
                               
                               #dnetworkStrength =  params$networkStrength
                               impact_mood = c(params$w_mood_socialIntegration*socialIntegration,params$w_mood_socialInteraction * socialInteraction,params$w_mood_enjoyedActivites*enjoyedActivites)
                               dmoodLevel =  params$eta_mood *( .self$alogistic(impact_mood, params$sigma_mood ,params$tau_mood)  - moodLevel)
                               
                               impact_socialIntegration = c(params$w_socialIntegration_mood*moodLevel,params$w_socialIntegration_networkStrength * networkStrength,params$w_socialIntegration_activitiesCarriedOut*activitiesCarriedOut)
                               dsocialIntegration =  params$eta_socialIntegration *( .self$alogistic(impact_socialIntegration, params$sigma_socialIntegration ,params$tau_socialIntegration)  - socialIntegration)
                               
                               impact_activitiesCarriedOut = c(params$w_activitiesCarriedOut_socialIntegration*socialIntegration)
                               dactivitiesCarriedOut = params$eta_activitiesCarriedOut *( .self$alogistic(impact_activitiesCarriedOut, params$sigma_activitiesCarriedOut ,params$tau_activitiesCarriedOut)  - activitiesCarriedOut)
                               
                               impact_enjoyedActivites = c(params$w_enjoyedActivites_mood*moodLevel,params$w_enjoyedActivites_activitiesCarriedOut*activitiesCarriedOut)
                               denjoyedActivites =  params$eta_enjoyedActivites *( .self$alogistic(impact_enjoyedActivites, params$sigma_enjoyedActivites ,params$tau_enjoyedActivites)  - enjoyedActivites)
                               
                               dnetworkStrength = 0
                               # return the rate of change
                               list(c(dsocialInteraction,dnetworkStrength , dmoodLevel,dsocialIntegration,dactivitiesCarriedOut,denjoyedActivites))
                             }) 
                           },
                           alogistic = function(Vs ,sigma,tau){
                             
                             (1/(1+exp(-sigma*(sum(Vs) -tau))) - 1/(1+exp(tau*sigma)) ) *(1+exp(-tau*sigma))
                           }
                           ,
                           getParameters = function(){
                             ret = c(.self$params$socialInteraction,
                               .self$params$enjoyedActivites,
                               .self$params$activitiesCarriedOut, 
                               .self$params$socialIntegration,
                               .self$params$moodLevel,
                               .self$params$networkStrength, 
                               .self$params$eta_socialInteraction,
                               .self$params$eta_mood,
                               .self$params$eta_socialIntegration,
                               .self$params$eta_activitiesCarriedOut,
                               .self$params$eta_enjoyedActivites,
                               .self$params$sigma_enjoyedActivites,
                               .self$params$tau_enjoyedActivites,
                               .self$params$sigma_socialInteraction,
                               .self$params$tau_socialInteraction,
                               .self$params$sigma_mood,
                               .self$params$tau_mood,
                               .self$params$sigma_activitiesCarriedOut,
                               .self$params$tau_activitiesCarriedOut,
                               .self$params$sigma_socialIntegration,
                               .self$params$tau_socialIntegration,
                               .self$params$w_socialInteraction_mood,
                               .self$params$w_socialInteraction_network,
                               .self$params$w_socialInteraction_activitiesCarriedOut,
                               .self$params$w_mood_socialIntegration,
                               .self$params$w_mood_socialInteraction,
                               .self$params$w_mood_enjoyedActivites,
                               .self$params$w_socialIntegration_mood,
                               .self$params$w_socialIntegration_networkStrength,
                               .self$params$w_socialIntegration_activitiesCarriedOut,
                               .self$params$w_activitiesCarriedOut_socialIntegration,
                               .self$params$w_enjoyedActivites_mood,
                               .self$params$w_enjoyedActivites_activitiesCarriedOut
                             )
                               
                              return(ret)
                           },
                           setParameters = function(parameters){
                             
                             .self$params$socialInteraction= parameters[1]
                             .self$params$enjoyedActivites= parameters[2]
                             .self$params$activitiesCarriedOut=parameters[3] 
                             .self$params$socialIntegration=parameters[4] 
                             .self$params$moodLevel=parameters[5] 
                             .self$params$networkStrength=parameters[6] 
                             
                             if(length(parameters)>6){
                             
                               .self$params$sigma_socialIntegration = parameters[7]
                               .self$params$tau_socialIntegration= parameters[8]
                               
                               .self$params$w_socialInteraction_mood= parameters[9]
                               .self$params$w_socialInteraction_network= parameters[10]
                               .self$params$w_socialInteraction_activitiesCarriedOut= parameters[11]
                               
                               .self$params$w_mood_socialIntegration= parameters[12]
                               .self$params$w_mood_socialInteraction = parameters[13]
                               .self$params$w_mood_enjoyedActivites= parameters[14]
                               
                               .self$params$w_socialIntegration_mood= parameters[15]####is missing in paper!
                               .self$params$w_socialIntegration_networkStrength = parameters[16]
                               .self$params$w_socialIntegration_activitiesCarriedOut=parameters[17]###missing in paper
                               
                               .self$params$w_activitiesCarriedOut_socialIntegration= parameters[18]
                               
                               .self$params$w_enjoyedActivites_mood=parameters[19]
                               .self$params$w_enjoyedActivites_activitiesCarriedOut= parameters[20]
                               
                               ###
                               if(length(parameters)>20){
                               
                               .self$params$eta_socialInteraction = parameters[21]
                               .self$params$eta_mood =parameters[22] 
                               .self$params$eta_socialIntegration = parameters[23]
                               .self$params$eta_activitiesCarriedOut = parameters[24]
                               .self$params$eta_enjoyedActivites = parameters[25]
                               if(length(parameters)>25){
                                 .self$params$sigma_enjoyedActivites =parameters[26]
                                 .self$params$tau_enjoyedActivites= parameters[27]
                                 
                                 .self$params$sigma_socialInteraction= parameters[28]
                                 .self$params$tau_socialInteraction= parameters[29]
                                 
                                 .self$params$sigma_mood =parameters[30]
                                 .self$params$tau_mood= parameters[31]
                                 .self$params$sigma_activitiesCarriedOut = parameters[32]
                                 .self$params$tau_activitiesCarriedOut= parameters[33]
                               }
                              }
                             }
                             

                           },
                           
                           train = function(data) {
                              print('not implemented')
                           },
                           predict = function(x) {      
                             .self$simulate(length(x) )
                           },
                           getNumberOfParameters =function(){
                             c(6,20,25,33)
                           },
                           getNumberOfConcepts = function(){
                             6
                           },
                           multiOpti = function(data,iterations = 10, population = 20,parallel = FALSE,numOfParams=6,refPoint=NULL,testdata=NULL){
                             library("mco")
                             library("eaf")
                             library("emoa")
                             if(is.null(refPoint)){
                               refPoint= rep(1,ncol(data))
                             }
                             ##optimisation function
                             errorFn <- function(parameters) {
                               
                               .self$setParameters(parameters) ## init the model with the new parameters
                               
                               numPoints = dim(data)[1]
                               if(!is.null(testdata)){
                                 numPoints = numPoints +nrow(testdata)
                               }
                               simData = .self$simulate(numPoints)
                               
                               sim = data.matrix(simData);
                               sim[is.na(sim)] =  0
                               sim[sim==Inf] = 0
                               
                               sim= as.data.frame(sim)
                               
                               targetNames = colnames(data)
                               err = c()
                               testErr=c()
                               for(i in 1:length(targetNames)){
                                 idx= which(colnames(sim)==targetNames[i])
                                 if(length(idx)==0){ ## not found
                                   next;
                                 }
                                 tmp =  sim[1:nrow(data),idx] - data[,i]
                                 tmp[is.na(tmp)] =0
                                 if(!is.null(testdata)){
                                   dif = mean( (sim[(nrow(data)+1):numPoints,idx] - testdata[,i])^2 )
                                   testErr =c(testErr,dif)
                                   
                                   dif = mean( (tmp[1:nrow(data) ])^2 )
                                   err = c(err,dif)
                                 }else{
                                    dif = mean( (tmp)^2 )
                                    err = c(err,dif)
                                 }
                               }
                               if(!is.null(testdata)){
                                 #print(testErr)
                                 return(list(err,testErr))
                               }else{
                                 return(err)
                               }
                               
                             }
                             
                             ##vectorized
                             errorFnVec = function(param){
                               apply(param, 1,errorFn) 
                             }
                             
                             GASettings <- list()
                             GASettings$parameterNr = numOfParams
                             GASettings$objectiveNr = ncol(data)
                             
                             GASettings$generationNr =iterations# set 
                             GASettings$popSize = population #
                             
                             GASettings$cProb = 0.9  #crossover prob
                             GASettings$cDist = 5   #crossover distribution index
                             GASettings$mProb = 0.1  #mutation prob
                             GASettings$mDist = 10   #mutation distribution index
                             
                           
                             
                             if(parallel){
                               require(parallel) 
                               
                               if(.Platform$OS.type == "windows"){ # do windows stuff
                                 print('You use windows, sorry this is not implemented!')                                
                               } 
                               
                               fitmatrixMean = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                               fitmatrixVar  = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                               
                               distmatrix = matrix(,nrow =0,ncol = GASettings$popSize )
                              # distmatrixVar  = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                               
                               
                         
                               
                               optimVecDoPar= function(param){
                                 splitList <- split(param, 1:NROW(param))
                                 if(!is.null(testdata)){
                                   res = mclapply( splitList, errorFn,mc.cores= detectCores())
                                   fitlist = list()
                                   predlist =list()
                                  # distlist = list()
                                   for(i in 1:length(res)){
                                     fitlist[[i]]= res[[i]][[1]]
                                     predlist[[i]]= res[[i]][[2]]
                                   }
                                   fit = matrix(unlist(fitlist),nrow=GASettings$objectiveNr,byrow = F )
                                  
                                    
                                    pred = matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                                   
                                   
                                   fitmatrixMean<<-rbind(fitmatrixMean, colMeans(pred))
                                   fitmatrixVar<<- rbind(fitmatrixVar, colVars(pred))
                                   
                                   ##estimate distance to true parameters
                                   p_dist = c()
                                   for(i in 1:length(splitList)){
                                     p_dist = c(p_dist,sqrt( sum( (.self$true_parameters-splitList[[i]])^2 ) ))
                                   }
                                   
                                   #
                                   
                                   distmatrix<<-rbind(distmatrix, p_dist)
                                   
                                 }else{
                                   fit  = matrix(unlist(mclapply( splitList, errorFn,mc.cores= detectCores())),nrow=GASettings$objectiveNr,byrow = F )
                                 }
                                # print(fit)
                                 fit
                               }
                               
                               
                               r1 <- nsga2(optimVecDoPar, 
                                           GASettings$parameterNr, 
                                           GASettings$objectiveNr,
                                           generations=GASettings$generationNr,
                                           popsize=GASettings$popSize, 
                                           cprob=GASettings$cProb,  
                                           cdist=GASettings$cDist,   
                                           mprob=GASettings$mProb,  
                                           mdist=GASettings$mDist,   
                                           lower.bounds=rep(0, GASettings$parameterNr),
                                           upper.bounds=rep(1, GASettings$parameterNr),
                                           #    vectorized=T,refDHV =refPoint)
                                           vectorized=T)
                               
                               if(!is.null(testdata)){
                                 r1$fitmatrixMean = fitmatrixMean
                                 r1$fitmatrixVar = fitmatrixVar
                                 
                                 r1$distmatrix = distmatrix
                                 
                               }
                               r1
                               #  stopCluster(cl)
                             }else{
                               r1 <- nsga2(errorFnVec, 
                                           GASettings$parameterNr, 
                                           GASettings$objectiveNr,
                                           generations=GASettings$generationNr,
                                           popsize=GASettings$popSize, 
                                           cprob=GASettings$cProb,  
                                           cdist=GASettings$cDist,   
                                           mprob=GASettings$mProb,  
                                           mdist=GASettings$mDist,   
                                           lower.bounds=rep(0, GASettings$parameterNr),
                                           upper.bounds=rep(1, GASettings$parameterNr),
                                           #vectorized=T,refDHV =refPoint)
                                           vectorized=T)
                             }
                             
                             return(r1)
                             
                           },
                           
                           initialize =function(...) {
                             .self$params <<- data.frame( ##basic model
                               
                               w_socialInteraction_mood= 0.5,
                               w_socialInteraction_network= 0.4,
                               w_socialInteraction_activitiesCarriedOut= 0.6,
                               
                               eta_socialInteraction = 0.1,
                               sigma_socialInteraction= 3,
                               tau_socialInteraction= 0.44,
                               
                              
                               w_mood_socialIntegration= 0.3,
                               w_mood_socialInteraction = 0.5,
                               w_mood_enjoyedActivites= 0.5,
                               
                               eta_mood =0.1 ,
                               sigma_mood =4 ,
                               tau_mood= 0.5,
                               
                               w_socialIntegration_mood= 0.5,####is missing in paper!
                               w_socialIntegration_networkStrength = 0.5,
                               w_socialIntegration_activitiesCarriedOut=0.5 ,###missing in paper
                               
                               eta_socialIntegration = 0.1,
                               sigma_socialIntegration = 3,
                               tau_socialIntegration= 0.34,
                               
                               w_activitiesCarriedOut_socialIntegration= 0.6,
                               eta_activitiesCarriedOut = 0.1,
                               sigma_activitiesCarriedOut = 5,
                               tau_activitiesCarriedOut= 0.34,
                               
                               w_enjoyedActivites_mood=0.5 ,
                               w_enjoyedActivites_activitiesCarriedOut= 0.5,
                               eta_enjoyedActivites = 0.1,
                               sigma_enjoyedActivites =4 ,
                               tau_enjoyedActivites= 0.34,
                               
                               socialInteraction= 0.5,
                               enjoyedActivites= 0.5,
                               activitiesCarriedOut=0.5 ,
                               socialIntegration=0.4 ,
                               moodLevel=0.5 ,
                               networkStrength=0.5 
                               
                             )
                             
                           }
                         ))

##################### SOCIAL MODEL 2
require('matrixStats')


#this implementation used the C++ functions and is much faster than the R implementation
socialModel2 <- setRefClass("socialModel2",
                           fields = list(parameters = "numeric",true_parameters = "numeric"  ),
                           contains = "model", 
                           
                           methods = list(
                             simulate = function(length) {
                              out = simulateSM(parameters,length);
                             },
                             
                             getParameters = function(){
                               return(.self$parameters)
                             },
                             setParameters = function(parameters){
                               if(length(parameters)==32){
                                 .self$parameters = parameters
                               }else if(length(parameters)==17){
                                 
                                 .self$parameters = rep(1,32) ## set others to 1
                                 .self$parameters[1:6]= parameters[1:6]
                                 .self$parameters[22:33]= parameters[7:18]
                                 .self$parameters[c(13 ,15, 17 ,19 ,21)] =0;#set tau to 0
                                 
                               }else{
                                 print('number of parameters not valid')
                                 print(length(parameters))
                               }
                             },
                             train = function(data) {
                               print('not implemented')
                             },
                             predict = function(x) {      
                               .self$simulate(length(x) )
                             },
                             getNumberOfParameters =function(){
                               c(17,32)
                             },
                             getNumberOfConcepts = function(){
                               6
                             },
                             multiOpti = function(data,iterations = 10, population = 20,parallel = FALSE,numOfParams=6,refPoint=NULL,testdata=NULL){
                               library("mco")
                               library("eaf")
                               library("emoa")
                               if(is.null(refPoint)){
                                 refPoint= rep(1,ncol(data))
                               }
                               ##optimisation function
                             
                               fitmatrixMean = matrix(,nrow =iterations,ncol = ncol(data) )
                               fitmatrixMin = matrix(,nrow =iterations,ncol = ncol(data) )
                               
                                 errorMatrix  = matrix(,nrow =iterations,ncol = ncol(data) )
                               errorMatrixMin  = matrix(,nrow =iterations,ncol = ncol(data) )
                               
                               colnames(fitmatrixMean) = colnames(data)
                               distmatrix = matrix(,nrow =iterations,ncol =population )
                        
                               lastPreError = NULL
                               lastFitError = NULL
                               iterationCnt = 1;
                               ##vectorized
                               
                                 
                               errorFnOLD <- function(parameters) {
                                 
                                 
                                 numPoints = dim(data)[1]
                                 if(!is.null(testdata)){
                                   numPoints = numPoints +nrow(testdata)
                                 }
                                 sim =  simulateSM(parameters,numPoints)
                                 
                                 
                                 targetNames = colnames(data)
                                 err = c()
                                 testErr=c()
                                 for(i in 1:length(targetNames)){
                                   idx= which(colnames(sim)==targetNames[i])
                                   if(length(idx)==0){ ## not found
                                     next;
                                   }
                                   tmp =  sim[1:nrow(data),idx] - data[,i]
                                   notNAs =  sum(! is.na(tmp[1:nrow(data) ]) )
                                   tmp[is.na(tmp)] =0
                                   if(!is.null(testdata)){
                                     dif = mean( (sim[(nrow(data)+1):numPoints,idx] - testdata[,i])^2 )
                                     testErr =c(testErr,dif)
                                     
                                     dif = sum( (tmp[1:nrow(data) ])^2 )
                                     dif = dif /notNAs
                                    
                                     err = c(err,dif)
                                   }else{
                                     dif = mean( (tmp)^2 )
                                     err = c(err,dif)
                                   }
                                 }
                                 if(!is.null(testdata)){
                                   #print(testErr)
                                   return(list(err,testErr))
                                 }else{
                                   return(err)
                                 }
                                 
                               }
                              
                               ##gets called with the current population so we do bookkeeping here
                               reportFunction=function(param, ...){
                                 res = apply(param, 1,errorFnCpp,data=data,test=testdata) 
                                 
                                 fitlist = list()
                                 predlist =list()
                                 for(i in 1:length(res)){
                                   fitlist[[i]]= res[[i]][[1]]
                                   predlist[[i]]= res[[i]][[2]]
                                   
                                 }
                                 fit = matrix(unlist(fitlist),nrow=GASettings$objectiveNr,byrow = F )
                                 pred = matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                                 
                                 #    print(dim(rowMeans(fit)))
                                  fitmatrixMean[iterationCnt,]<<-colMeans(pred)
                                  fitmatrixMin[iterationCnt,] <<- colMins(pred)
                                  errorMatrix[iterationCnt,] <<-rowMeans(fit)
                                  errorMatrixMin[iterationCnt,] <<-rowMins(fit)
                                 
                                  lastPreError<<-pred
                                  lastFitError<<-fit
                                 #    fitmatrixVar[iterationCnt,]<<-  colVars(pred)
                                 
                                 ##estimate distance to true parameters
                                  p_dist = c()
                                  for(i in 1:nrow(param)){
                                 # p_dist = c(p_dist,sqrt( sum( (.self$true_parameters-splitList[[i]])^2 ) ))
                                    distmatrix[iterationCnt,i]<<- sqrt( sum( (.self$true_parameters-param[i,])^2 ) )
                                  }
                                  iterationCnt<<-iterationCnt+1  
                               }
                                
                               errorFnVec = function(param){
                                 res = apply(param, 1,errorFnCpp,data=data,test=testdata) 
                                 
                                 fitlist = list()
                                 predlist =list()
                                 for(i in 1:length(res)){
                                   fitlist[[i]]= res[[i]][[1]]
                                   predlist[[i]]= res[[i]][[2]]
                                 }
                                 fit = matrix(unlist(fitlist),nrow=GASettings$objectiveNr,byrow = F )
                                 
                                 fit
                               }
                               
                               GASettings <- list()
                               GASettings$parameterNr = numOfParams
                               GASettings$objectiveNr = ncol(data)
                               
                               GASettings$generationNr =iterations# set 
                               GASettings$popSize = population #
                               
                               GASettings$cProb = 0.9  #crossover prob
                               GASettings$cDist = 5   #crossover distribution index
                               GASettings$mProb = 0.1  #mutation prob
                               GASettings$mDist = 10   #mutation distribution index
                         
                              # ff <- function(x) reportFunction(x, ...)
                               
                               upper = rep(1, GASettings$parameterNr)
                               r1 <- nsga2(errorFnVec, 
                                             GASettings$parameterNr, 
                                             GASettings$objectiveNr,
                                             generations=GASettings$generationNr,
                                             popsize=GASettings$popSize, 
                                             cprob=GASettings$cProb,  
                                             cdist=GASettings$cDist,   
                                             mprob=GASettings$mProb,  
                                             mdist=GASettings$mDist,   
                                             lower.bounds=rep(0, GASettings$parameterNr),
                                             upper.bounds=upper,
                                             vectorized=T,refDHV =refPoint,reportfunction = reportFunction)
                                           #  vectorized=T)
                                 
                                 if(!is.null(testdata)){
                                   r1$fitmatrixMean = fitmatrixMean
                                   r1$fitmatrixMin = fitmatrixMin
                                   r1$errorMatrixMin = errorMatrixMin
                                   r1$errorMatrix = errorMatrix
                                   r1$distmatrix = distmatrix
                                   r1$lastPreError= lastPreError
                                   r1$lastFitError= lastFitError
                                 }
                                
                               
                               return(r1)
                               
                             },
                            multiOptiMulti = function(data_list,iterations = 10, population = 20,parallel = FALSE,numOfParams=6,refPoint=NULL,testdata=NULL){
                              library("mco")
                              library("eaf")
                              library("emoa")
                              if(is.null(refPoint)){
                                refPoint= rep(1,ncol(data_list[[1]]))
                              }
                              ##optimisation function
                              
                              errorFnOLD <- function(parameters) {
                                
                                errall = NULL
                                
                                for(x in 1:length(data_list)){
                                 # print('inloop')
                                  numPoints = dim(data_list[[x]])[1]
       
                                  sim =  simulateSM(parameters,numPoints)
                                   
                                  targetNames = colnames(data_list[[x]])
                                  err = c()
                                  testErr=c()
                                  for(i in 1:length(targetNames)){
                                    idx= which(colnames(sim)==targetNames[i])
                                    if(length(idx)==0){ ## not found
                                      print('not found')
                                      next;
                                    }
                                    tmp =  sim[1:nrow(data_list[[x]]),idx] - data_list[[x]][,i]
                                    tmp[is.na(tmp)] =0
                                    dif = mean( (tmp)^2 )
                                    err = c(err,dif)
                                   #print('append err')
                                  }
                                  if(is.null(errall)){
                                    errall = err
                                  #  print('set errall')
                                  }else{
                                    errall = errall +err
                                  }
                                  
                                }
                             #   print(errall)
                                return( errall/length(data_list))
                                  
                                
                              }
                              
                              errorFnVec = function(param){
                                  res = list()
                                
                                  for(i in 1:nrow(param)){
                                   res[[i]] = errorFnOLD(param[i,])
                                  }
                                
                                fit = matrix(unlist(res),nrow=GASettings$objectiveNr,byrow = F )
                           
                                fit
                              }
                              
                              GASettings <- list()
                              GASettings$parameterNr = numOfParams
                              GASettings$objectiveNr = ncol(data_list[[1]])
                              
                              GASettings$generationNr =iterations# set 
                              GASettings$popSize = population #
                              
                              GASettings$cProb = 0.9  #crossover prob
                              GASettings$cDist = 5   #crossover distribution index
                              GASettings$mProb = 0.1  #mutation prob
                              GASettings$mDist = 10   #mutation distribution index
                              
                              upper = rep(1, GASettings$parameterNr)
                              
                              r1 <- nsga2(errorFnVec, 
                                          GASettings$parameterNr, 
                                          GASettings$objectiveNr,
                                          generations=GASettings$generationNr,
                                          popsize=GASettings$popSize, 
                                          cprob=GASettings$cProb,  
                                          cdist=GASettings$cDist,   
                                          mprob=GASettings$mProb,  
                                          mdist=GASettings$mDist,   
                                          lower.bounds=rep(0, GASettings$parameterNr),
                                          upper.bounds=upper,
                                          vectorized=T,refDHV =refPoint)
                              #  vectorized=T)
                             
                              return(r1)
                              
                            },
                             
                             initialize =function(...) {
                               .self$parameters <<-rep(0.5,32)
                              
                                  .self$parameters[1]= 0.5#socialInteraction, #1
                                  .self$parameters[2]=0.5#         .self$params$enjoyedActivites,#2
                                  .self$parameters[3]=0.5#           .self$params$activitiesCarriedOut, #3
                                  .self$parameters[4]=0.4#4 socialIntegration
                                  .self$parameters[5]=0.5#moodLevel
                                  .self$parameters[6]=0.5 #6networkStrength
                                
                                  .self$parameters[7]= 0.1#       eta_socialInteraction = 0.1,,#7
                                  .self$parameters[8] = 0.1#8   eta_mood =0.1 ,
                                  .self$parameters[9] =0.1#eta_socialIntegration = 0.1,#9
                                  .self$parameters[10] =0.1#eta_activitiesCarriedOut = 0.1,#10
                                  .self$parameters[11] =0.1#eta_enjoyedActivites = 0.1,,#11
                                  .self$parameters[12] =4 #sigma_enjoyedActivites =4 ,,#12
                                  .self$parameters[13] =0.34#tau_enjoyedActivites= 0.34,,#13
                                  .self$parameters[14] = 3#sigma_socialInteraction= 3,,#14
                                  .self$parameters[15] =0.44#tau_socialInteraction= 0.44,,#15
                                  .self$parameters[16] = 4#sigma_mood =4 ,,#16
                                  .self$parameters[17] = 0.5 #tau_mood= 0.5,,#17
                                  .self$parameters[18] =5#sigma_activitiesCarriedOut = 5,,#18
                                  .self$parameters[19] = 0.34#tau_activitiesCarriedOut= 0.34,,#19
                                  .self$parameters[20] =3#sigma_socialIntegration = 3,,#20
                                  .self$parameters[21] = 0.34#tau_socialIntegration= 0.34,,#21
                                  .self$parameters[22] = 0.5#w_socialInteraction_mood= 0.5,,#22
                                  .self$parameters[23] = 0.4# w_socialInteraction_network= 0.4,,#23
                                  .self$parameters[24] =0.6#w_socialInteraction_activitiesCarriedOut= 0.6,,#24
                                  .self$parameters[25] =0.3#w_mood_socialIntegration= 0.3,,#25
                                  .self$parameters[26] =0.5#w_mood_socialInteraction = 0.5,,#26
                                  .self$parameters[27] =0.5#w_mood_enjoyedActivites= 0.5,,#27
                                  .self$parameters[28] = 0.5#w_socialIntegration_networkStrength = 0.5,,#28
                                  .self$parameters[29] =0.5#w_socialIntegration_activitiesCarriedOut=0.5 ,###missing in paper,#29
                                  .self$parameters[30] = 0.6#w_activitiesCarriedOut_socialIntegration= 0.6,,#30
                                  .self$parameters[31] =0.5#w_enjoyedActivites_mood=0.5 ,,#31
                                  .self$parameters[32] =0.5#w_enjoyedActivites_activitiesCarriedOut= 0.5,#32
                                     
                             }
                           ))
