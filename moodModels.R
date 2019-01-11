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




moodModel <- setRefClass("moodModel",
                         fields = list(parameters = "numeric",params ="data.frame"  ),
                         contains = "model", 
                         
                         methods = list(
                           simulate = function(length) {
                             
                             state <- c(oevs = params$oevs, 
                                        appraisal =  params$appraisal,
                                        moodLevel = params$moodLevel,
                                        thoughts = params$thoughts,
                                        sensitivity = params$sensitivity,
                                        beta = params$beta,
                                        STprospMoodLevel = params$STprospMoodLevel
                             )
                             
                             times <- seq(from = 0, to = length*0.1, by =0.1)
                             out <- ode (times = times, y = state, func = .self$rigidode, parms = .self$params, method = "ode45")
                             
                             ###check length for ode
                             out = out[1:length,]
                             
                             as.data.frame(out)
                           },
                           rigidode = function(t, state, parameters){
                             with(as.list(c(state, parameters)),{
                               # rate of change
                               
                               STprospMoodLevel = beta * params$LTprospMoodLevel
                               
                               deltaMood = moodLevel - beta* params$LTprospMoodLevel
                               
                               dOevs = -sensitivity*  ( (oevs *max(0,deltaMood) + ( 1- oevs)*min(0,deltaMood ) )) 
                               
                               psi = params$vulnerability * oevs * thoughts + params$coping * (1 - (1-oevs) * (1-thoughts));
                               dAppraisal <- (psi - appraisal) ;
                               
                               epsilon = appraisal * params$w_appraisal_mood + thoughts * params$w_thoughts_mood;  
                               dMoodLevel  <- params$coping * max(0,epsilon - moodLevel) + params$vulnerability * min(0,epsilon -moodLevel)
                               
                               phi = appraisal * params$w_appraisal_thoughts + moodLevel * params$w_mood_thoughts
                               dThoughts <- params$coping * max(0,phi - thoughts)+ params$vulnerability * min(0,phi- thoughts)
                               
                               eta = moodLevel * params$w_mood_sens + thoughts * params$w_thoughts_sens;
                               
                               dSensitivity <-  params$coping * max (0, eta - sensitivity) + params$vulnerability * min(0,eta - sensitivity)  
                               
                               dBeta <- params$vulnerability * (moodLevel/params$LTprospMoodLevel - beta  )+ params$coping *(1-beta)
                               
                               # return the rate of change
                               list(c(dOevs, dAppraisal, dMoodLevel,dThoughts,dSensitivity,dBeta,STprospMoodLevel))
                             }) 
                           },
                           setParameters = function(parameters){
                             
                             params$w_appraisal_mood <<- parameters[1];
                             params$w_thoughts_mood <<- 1- parameters[1];
                             
                             params$w_appraisal_thoughts <<- parameters[2];
                             params$w_mood_thoughts <<- 1 - parameters[2];
                             
                             params$w_mood_sens <<- parameters[3];
                             params$w_thoughts_sens <<- 1 - parameters[3];
                             
                             if(length(parameters)>3){
                               .self$params$oevs =  parameters[4];
                               .self$params$appraisal = parameters[5];
                               
                               .self$params$moodLevel =  parameters[10];
                               .self$params$thoughts  =   parameters[11];
                               
                               .self$params$sensitivity = parameters[6];
                               .self$params$beta = parameters[7];
                               .self$params$STprospMoodLevel = parameters[8] ;
                               .self$params$coping = parameters[9]
                               .self$params$vulnerability = 1.0 - parameters[9]
                             }
                           },
                           
                           train = function(data) {
                             
                             ###check data
                             back =data[1,]
                             for(i in 1:ncol(data)){
                               if(is.na(data[1,i]) ){
                                 ###take second.. ?
                                 data[1,i] = data[which(!is.na(data[,i]))[1],i]
                               }
                             }
                             ###if all are NA then dont know... I shoudl search this one too I guess
                             for(i in 1:ncol(data)){
                               if(is.na(data[1,i]) ){
                                 ###take second.. ?
                                 data[1,i] = 0.5
                               }
                             }
                             
                             
                             if(! is.null(data$oevs[1]) ){
                               .self$params$oevs = data$oevs[1];
                             }
                             if(! is.null(data$appraisal[1]) ){
                               .self$params$appraisal = data$appraisal[1];
                             }
                             if(! is.null(data$moodLevel[1]) ){
                               .self$params$moodLevel =  data$moodLevel[1];
                             }
                             if(! is.null(data$thoughts[1]) ){
                               .self$params$thoughts = data$thoughts[1];
                             }
                             if(! is.null(data$sensitivity[1]) ){
                               .self$params$sensitivity =data$sensitivity[1];
                             }
                             if(! is.null(data$beta[1]) ){
                               .self$params$beta = data$beta[1];
                             }
                             if(! is.null(data$STprospMoodLevel[1]) ){
                               .self$params$STprospMoodLevel = data$STprospMoodLevel[1] ;
                             }
                             if(! is.null(data$coping[1]) ){
                               .self$params$coping =data$coping[1]
                             }
                             .self$params$vulnerability = 1.0 - .self$params$coping 
                             
                             N_Points = nrow(data)
                             N_params = 11
                             
                             optimErrorFunction <- function(parameters) { 
                               .self$setParameters(parameters)
                               sim = .self$simulate(N_Points)
                               
                               targetNames = colnames(data)
                               err = c()
                               for(i in 1:length(targetNames)){
                                 idx= which(colnames(sim)==targetNames[i])
                                 if(length(idx)==0){ ## not found
                                   next;
                                 }
                                 tmp = sim[,idx] - data[,i]
                                 tmp[is.na(tmp)]=0
                                 dif = mean( (tmp)^2 )
                                 err = c(err,dif)
                               }
                               
                               sum(err)
                             }
                             
                             par = optim(par = rep(0.5,N_params), fn = optimErrorFunction, method = "L-BFGS-B", lower = 0, upper=1)
                             
                             .self$setParameters(par$par)
                             
                             if(par$convergence != 0){
                               cat('search not converged !!!\n')   
                             }
                             par$par
                           },
                           predict = function(x) {      
                             .self$simulate(length(x) )
                           },
                           getNumberOfParameters =function(){
                             3
                           },
                           getNumberOfConcepts = function(){
                             8
                           },
                           multiOpti = function(data,iterations = 10, population = 20,parallel = FALSE,refPoint=NULL){
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
                               simData = .self$simulate(numPoints)
                               
                               sim = data.matrix(simData);
                               sim[is.na(sim)] =  0
                               sim[sim==Inf] = 0
                               
                               sim= as.data.frame(sim)
                               
                               targetNames = colnames(data)
                               err = c()
                               for(i in 1:length(targetNames)){
                                 idx= which(colnames(sim)==targetNames[i])
                                 if(length(idx)==0){ ## not found
                                   next;
                                 }
                                 tmp =  sim[,idx] - data[,i]
                                 tmp[is.na(tmp)] =0
                                 dif = mean( (tmp)^2 )
                                 err = c(err,dif)
                               }
                               
                               return(err) # objectives error
                             }
                             
                             ##vectorized
                             errorFnVec = function(param){
                               apply(param, 1,errorFn) 
                             }
                             
                             GASettings <- list()
                             GASettings$parameterNr = 11
                             GASettings$objectiveNr = ncol(data)
                             
                             GASettings$generationNr =iterations# set 
                             GASettings$popSize = population #
                             
                             GASettings$cProb = 0.9  #crossover prob
                             GASettings$cDist = 5   #crossover distribution index
                             GASettings$mProb = 0.1  #mutation prob
                             GASettings$mDist = 10   #mutation distribution index
                             
                             ###############TEST
                        #     dhv =c()
                        #     reportFunction = function(error){
                               #  print('reported error')
                              #  print(error)
                               # print(dominatedHypervolume(error))
                        #       dhv<<- c(dhv,dominatedHypervolume(error))
                        #     }
                            
                             
                        #     optimVecDoParTMP= function(param){
                               #  print(param)
                        #       splitList <- split(param, 1:NROW(param))
                               #  print(splitList)
                        #       matrix(unlist(mclapply( splitList, errorFn,mc.cores= detectCores())),ncol=GASettings$objectiveNr,byrow = T )
                        #     }
                        #     require('parallel')
                        #     res = search(iterations, population,0.1,0.01,GASettings$parameterNr,errorFn,GASettings$objectiveNr ,0,1,T)
                        #     return(res)
                                  
                        #     result = my_nsga2Vec(optimVecDoParTMP,GASettings$parameterNr,GASettings$objectiveNr  ,
                        #                          GASettings$generationNr ,GASettings$popSize , GASettings$cProb,
                        #                          GASettings$mProb,0, 1,reportFunction)
                             
                         #    return(list(result,dhv) )
                             ################TEST
                             
                             if(parallel){
                               require(parallel) 
                               
                               if(.Platform$OS.type == "windows"){ # do windows stuff
                                 print('You use windows, sorry this is not implemented!')                                
                               } 

                               optimVecDoPar= function(param){
                                 splitList <- split(param, 1:NROW(param))
                                 matrix(unlist(mclapply( splitList, errorFn,mc.cores= 2)),nrow=GASettings$objectiveNr,byrow = F )
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
                             
                           }
                         ))


moodModel$methods( 
  
  initialize =
    function(...) {
      
      .self$params <<- data.frame( ##basic model
        ####### these have to be changed somehow
        oevs = 0.5, 
        appraisal = 0.5,
        moodLevel =1 , ###fellign great
        thoughts= 0.1,
        STprospMoodLevel=1,
        LTprospMoodLevel= 1 ,#
        sensitivity=0.5, 
        coping=  0.5,
        vulnerability = 1- 0.5,
        beta = 1.0,
        ######## parameters to estimate #####
        w_appraisal_mood = 0.7,  
        w_thoughts_mood = 0.3,
        w_appraisal_thoughts = 0.6,
        w_mood_thoughts=0.4,
        w_mood_sens = 0.5, #
        w_thoughts_sens = 1- 0.5#
      )
      
      callSuper(...)
    },
  finalize = function() {
  })




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
                             
                             ###############TEST
                             #     dhv =c()
                             #     reportFunction = function(error){
                             #  print('reported error')
                             #  print(error)
                             # print(dominatedHypervolume(error))
                             #       dhv<<- c(dhv,dominatedHypervolume(error))
                             #     }
                             
                             
                             #     optimVecDoParTMP= function(param){
                             #  print(param)
                             #       splitList <- split(param, 1:NROW(param))
                             #  print(splitList)
                             #       matrix(unlist(mclapply( splitList, errorFn,mc.cores= detectCores())),ncol=GASettings$objectiveNr,byrow = T )
                             #     }
                             #     require('parallel')
                             #     res = search(iterations, population,0.1,0.01,GASettings$parameterNr,errorFn,GASettings$objectiveNr ,0,1,T)
                             #     return(res)
                             
                             #     result = my_nsga2Vec(optimVecDoParTMP,GASettings$parameterNr,GASettings$objectiveNr  ,
                             #                          GASettings$generationNr ,GASettings$popSize , GASettings$cProb,
                             #                          GASettings$mProb,0, 1,reportFunction)
                             
                             #    return(list(result,dhv) )
                             ################TEST
                             
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
                                  
                                  # print( matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                                  # )
                                   
                                  # print(matrix(unlist(predlist),nrow=GASettings$objectiveNr,byrow = F) )
                                   
                                    pred = matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                                   
                                  #  dist = matrix(unlist(distlist),ncol=GASettings$objectiveNr,byrow = T )
                                    
                                   #print(dim(fitmatrixMean))
                                   #print(colMeans(pred))
                                   
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


socialModel2 <- setRefClass("socialModel2",
                           fields = list(parameters = "numeric",true_parameters = "numeric"  ),
                           contains = "model", 
                           
                           methods = list(
                             simulate = function(length) {
                              out = simulateSM(parameters,length);
                             },
                             
                             getParameters = function(){
                             #  ret = c(.self$params$socialInteraction, #1
                              #         .self$params$enjoyedActivites,#2
                            #           .self$params$activitiesCarriedOut, #3
                            #           .self$params$socialIntegration,#4
                            #           .self$params$moodLevel,#5
                            #           .self$params$networkStrength, #6
                            #           .self$params$eta_socialInteraction,#7
                            #           .self$params$eta_mood,#8
                            #           .self$params$eta_socialIntegration,#9
                            #           .self$params$eta_activitiesCarriedOut,#10
                            #           .self$params$eta_enjoyedActivites,#11
                            #           .self$params$sigma_enjoyedActivites,#12
                            #           .self$params$tau_enjoyedActivites,#13
                            #           .self$params$sigma_socialInteraction,#14
                            #           .self$params$tau_socialInteraction,#15
                            #           .self$params$sigma_mood,#16
                            #           .self$params$tau_mood,#17
                            #           .self$params$sigma_activitiesCarriedOut,#18
                            #           .self$params$tau_activitiesCarriedOut,#19
                            #           .self$params$sigma_socialIntegration,#20
                            #           .self$params$tau_socialIntegration,#21
                            #           .self$params$w_socialInteraction_mood,#22
                            #           .self$params$w_socialInteraction_network,#23
                            #           .self$params$w_socialInteraction_activitiesCarriedOut,#24
                            #           .self$params$w_mood_socialIntegration,#25
                            #           .self$params$w_mood_socialInteraction,#26
                            #           .self$params$w_mood_enjoyedActivites,#27
                            #           .self$params$w_socialIntegration_networkStrength,#28
                            #           .self$params$w_socialIntegration_activitiesCarriedOut,#29
                            #           .self$params$w_activitiesCarriedOut_socialIntegration,#30
                            #           .self$params$w_enjoyedActivites_mood,#31
                            #           .self$params$w_enjoyedActivites_activitiesCarriedOut#32
                            #   )
                               
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
                               
                              # fitmatrixMean = matrix(,nrow =iterations+1,ncol = ncol(data) )
                               
                               #fitmatrixVar  = matrix(,nrow =iterations+1,ncol = ncol(data) )
                               errorMatrix  = matrix(,nrow =iterations,ncol = ncol(data) )
                               errorMatrixMin  = matrix(,nrow =iterations,ncol = ncol(data) )
                               
                               colnames(fitmatrixMean) = colnames(data)
                               distmatrix = matrix(,nrow =iterations,ncol =population )
                               # distmatrixVar  = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                               lastPreError = NULL
                               lastFitError = NULL
                               iterationCnt = 1;
                               ##vectorized
                               
                              # dhvmatrix = matrix(0,nrow =iterations+1,ncol =1 )
                               
                               errorFnOLD <- function(parameters) {
                                 
                                # .self$setParameters(parameters) ## init the model with the new parameters
                                 
                                 numPoints = dim(data)[1]
                                 if(!is.null(testdata)){
                                   numPoints = numPoints +nrow(testdata)
                                 }
                                 #simData = .self$simulate(numPoints)
                                 sim =  simulateSM(parameters,numPoints)
                                 
                                 #sim = data.matrix(simData);
                             #    sim[is.na(sim)] =  0
                            #     sim[sim==Inf] = 0
                                 
                                 #sim= as.data.frame(sim)
                                 
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
                                   #  print('error and dif')
                                  #   print(dif)
                                     dif = dif /notNAs
                                    
                                  #   print(notNAs)
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
                              #   print('param')
                              #   param
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
                                 #errorFnCpp(NumericVector parameters,Rcpp::DataFrame data,Rcpp::DataFrame test){
                                   
                               #  
                                 #res = list()
                                # print(dim(param))
                                
                                # for(i in 1:nrow(param)){
                                 #  res[[i]] = errorFnCpp(param[i,],data,testdata);
                                  # print(length((param[i,])) )
                                #   res[[i]] = errorFnOLD(param[i,])
                                # }
                                 res = apply(param, 1,errorFnCpp,data=data,test=testdata) 
                               
                                 
                                 fitlist = list()
                                 predlist =list()
                                 # distlist = list()
                                 for(i in 1:length(res)){
                                   fitlist[[i]]= res[[i]][[1]]
                                  # print('error')
                                  # print(res[[i]][[1]])
                                 #  print(resCpp[[i]][[1]])
                                   predlist[[i]]= res[[i]][[2]]
                                 #  print('pred error')
                                   #print(res[[i]][[2]])
                                  # print(resCpp[[i]][[2]])
                                   
                                 }
                                 fit = matrix(unlist(fitlist),nrow=GASettings$objectiveNr,byrow = F )
                                    
                                # onedhv = dominated_hypervolume(points = (fit),ref = c(1,1,1,1) )
                                # dhvmatrix[iterationCnt,1]<<-onedhv
                                 
                                # pred = matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                                 
                             #    print(dim(rowMeans(fit)))
                                # fitmatrixMean[iterationCnt,]<<-colMeans(pred)
                                # fitmatrixMin[iterationCnt,] <<- colMins(pred)
                                # errorMatrix[iterationCnt,] <<-rowMeans(fit)
                                # errorMatrixMin[iterationCnt,] <<-rowMins(fit)

                                # lastPreError<<-pred
                                # lastFitError<<-fit
                             #    fitmatrixVar[iterationCnt,]<<-  colVars(pred)
                                 
                                 ##estimate distance to true parameters
                                 # p_dist = c()
                                # for(i in 1:nrow(param)){
                                   # p_dist = c(p_dist,sqrt( sum( (.self$true_parameters-splitList[[i]])^2 ) ))
                                #   distmatrix[iterationCnt,i]<<- sqrt( sum( (.self$true_parameters-param[i,])^2 ) )
                                # }
                                 
                                # iterationCnt<<-iterationCnt+1  
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
                                             vectorized=T,refDHV =refPoint,reportfunction = reportFunction)
                                           #  vectorized=T)
                                 
                                 if(!is.null(testdata)){
                                   r1$fitmatrixMean = fitmatrixMean
                                   r1$fitmatrixMin = fitmatrixMin
                                   r1$errorMatrixMin = errorMatrixMin
                            #       r1$fitmatrixVar = fitmatrixVar
                                   r1$errorMatrix = errorMatrix
                                   r1$distmatrix = distmatrix
                                   r1$lastPreError= lastPreError
                                  # if(is.null(lastFitError)){
                                  #   print('is null')
                                  # }
                                   r1$lastFitError= lastFitError
                                  # print('appened')
                                  # r1$dhvmatrix = dhvmatrix
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
                              
                              # dhvmatrix = matrix(0,nrow =iterations+1,ncol =1 )
                              
                              errorFnOLD <- function(parameters) {
                                
                                # .self$setParameters(parameters) ## init the model with the new parameters
                                errall = NULL
                                
                                for(x in 1:length(data_list)){
                                 # print('inloop')
                                  numPoints = dim(data_list[[x]])[1]
       
                                  #simData = .self$simulate(numPoints)
                                  sim =  simulateSM(parameters,numPoints)
                                   
                                  targetNames = colnames(data_list[[x]])
                                #  print(targetNames)
                               #   print('sim:')
                                 # print(colnames(sim))
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
                                #errorFnCpp(NumericVector parameters,Rcpp::DataFrame data,Rcpp::DataFrame test){
                                
                                #  
                                  res = list()
                                # print(dim(param))
                                
                                  for(i in 1:nrow(param)){
                                    #res[[i]] = errorFnCpp(param[i,],data,testdata);
                                # print(length((param[i,])) )
                                   res[[i]] = errorFnOLD(param[i,])
                                  }
                              #  res = apply(param, 1,errorFnCpp,data=data,test=testdata) 
                                #print(length(res))
                                
                              #  print(length(res[[1]]))
                                
                                fit = matrix(unlist(res),nrow=GASettings$objectiveNr,byrow = F )
                               # print(dim(fit))
 
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
                                          vectorized=T,refDHV =refPoint)
                              #  vectorized=T)
                             
                              return(r1)
                              
                            },
                             
                             initialize =function(...) {
                               .self$parameters <<-rep(0.5,32)
                             }
                           ))
