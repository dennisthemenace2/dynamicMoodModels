

#rm(list=ls(all=TRUE))


require('Rcpp')
require('RcppArmadillo')
Rcpp::sourceCpp('./moodModel.cpp')
require('matrixStats')


dcm <- setRefClass("dcm",fields = list(A = "matrix", X='matrix',time= 'matrix',conncetions='matrix',sigmas='matrix',taus='matrix',true_parameters = "numeric" ),
                   
                   methods = list(
                     modelStep = function(X,A,time,sigmas, taus){
                       StateTransition = A %*% X
                       transFormendStateTransition  =mapply(.self$alogistic,StateTransition,sigmas,taus)
                       innerSum = transFormendStateTransition -X 
                       newState =X+ innerSum * time 
                     },
                     simulate = function(N){
                       out = matrix(NA,nrow = N,ncol=ncol(A))
                       state = X
                       for(i in 1:N){
                         state = modelStep(state,.self$A,.self$time,.self$sigmas,.self$taus)
                         out[i,] = state
                       }
                       out = as.data.frame(out)
                       print(colnames(.self$conncetions))
                       if(!is.null(colnames(.self$conncetions)) ){
                         colnames(out) = colnames(.self$conncetions)
                       }
                       out
                     },
                     alogistic = function(Vs ,sigma,tau){   
                       (1/(1+exp(-sigma*(Vs -tau))) - 1/(1+exp(tau*sigma)) ) *(1+exp(-tau*sigma))
                     },
                     getParameters = function(){
                       ret = c()
                       for(y in 1:nrow(conncetions) ){
                         for(x in 1:ncol(conncetions) ){
                           if(conncetions[y,x]==1){
                             ret=c(ret, .self$A[y,x])
                           }
                         }
                       }
                       ret = c(ret,.self$sigmas[rowSums(conncetions ) >0])
                       ret = c(ret,.self$taus[rowSums(conncetions ) >0])
                       ret = c(ret,.self$time[rowSums(conncetions ) >0])
                       ret = c(ret,.self$X)
                       
                     },
                     setParameters = function(parameters){
                       nparams = ncol(.self$A)*4 + sum(.self$conncetions)
                       
                       if(any(rowSums(.self$conncetions ) ==0) ){##adjust for not connceted states
                         nparams =  nparams - sum(rowSums(.self$conncetions) ==0) *3
                       }
                       
           
                       if(length(parameters) !=nparams ){
                         print('unvalid number of paramters')
                       }
                       print(nparams)
                       
                       cnt=1
                       for(y in 1:nrow(.self$conncetions) ){
                         for(x in 1:ncol(.self$conncetions) ){
                           if(.self$conncetions[y,x]==1){
                             .self$A[y,x] = parameters[cnt]
                             cnt = cnt+1
                           }
                         }
                       }
                       active = sum(rowSums(.self$conncetions ) >0)
                       
                       .self$sigmas[rowSums(.self$conncetions ) >0] = parameters[cnt:(cnt+ active-1) ]
                       cnt = cnt+active
                       .self$taus[rowSums(.self$conncetions ) >0] = parameters[cnt:(cnt+active-1) ]
                       cnt = cnt+active
                       .self$time[rowSums(.self$conncetions ) >0] = parameters[cnt:(cnt+ active-1) ]
                       cnt = cnt+active
                       .self$X =matrix( parameters[cnt:(cnt+ ncol(A)-1) ])
                       
                     },
                     multiOpti = function(data,iterations = 10, population = 20,parallel = FALSE,refPoint=NULL,testdata=NULL){
                       library("mco")
                       library("eaf")
                       library("emoa")
                       if(is.null(refPoint)){
                         refPoint= rep(1,ncol(data))
                       }
                       
                       ###need to set up cols because i dindt do it in cpp
                       cnames = colnames(A)
                       ##do a quick check
                       for(i in 1:length(cnames)){
                         idx = which(cnames[i]==colnames(data))
                         idx2 = which(cnames[i]==colnames(testdata))
                         
                         if(is.null(idx) || is.null(idx2) ){
                           print('colname not found !')
                           print(ncnames[i])
                           return (NULL)
                         }
                       }
                       data = data[,cnames]
                       testdata = testdata[,cnames]
                       
                       ##optimisation function
                       errorFn <- function(parameters) {
                      #   .self$setParameters(parameters) ## init the model with the new parameters
                         
                       #  if(length(parameters) !=nparams ){
                      #     print('unvalid number of paramters')
                      #   }
                         #print(nparams)
                       #  if(F){
                         statesCount = sum(.self$conncetions)
                         A = .self$conncetions
                         A[A==1]=parameters[1:statesCount]
                         start = statesCount+1
                         active = sum(rowSums(.self$conncetions ) >0)
                         
                         end = statesCount + active
                         sigmas = rep(0,ncol(A))
                         sigmas[rowSums(.self$conncetions ) >0] =parameters[start:end]
                         start = end+1
                         end= end+ active
                         taus = rep(0,ncol(A))
                         taus[rowSums(.self$conncetions ) >0]=parameters[start:end]
                         start = end+1
                         end= end+ active
                         time = rep(0,ncol(A))
                         time[rowSums(.self$conncetions) >0]=parameters[start:end]
                         start = end+1
                         end= end+ ncol(A)
                         X = parameters[start:end]
                         #if(end != length(parameters)){
                        #   stop()
                        # }
                         ret = errorDcmFnCpp(X,A,time,sigmas,taus,data,testdata)
                         
                        # }
                        #ret = errorDcmFnCpp(.self$X,.self$A,.self$time,.self$sigmas,.self$taus,data,testdata)
                         
                        return(ret)
                     # if(F)   
                         numPoints = dim(data)[1]
                         if(!is.null(testdata)){
                           numPoints = numPoints +nrow(testdata)
                         }
                        # sim = .self$simulate(numPoints)
                         sim = .self$simCpp(numPoints)
                         
                         #    sim = data.matrix(simData);
                         #    sim[is.na(sim)] =  0
                         #     sim[sim==Inf] = 0
                         
                         #   sim= as.data.frame(sim)
                         
                         targetNames = colnames(data)
                         err = c()
                         testErr=c()
                         for(i in 1:length(targetNames)){
                           idx= which(colnames(sim)==targetNames[i])
                           if(length(idx)==0){ ## not found
                             cat(c('variable:',targetNames[i],' not found!\n'))
                             next;
                           }
                           tmp =  sim[1:nrow(data),idx] - data[,i]
                           # tmp[is.na(tmp)] =0
                           if(!is.null(testdata)){
                             dif = mean( na.omit(sim[(nrow(data)+1):numPoints,idx] - testdata[,i])^2 )
                             testErr =c(testErr,dif)
                             
                             dif = mean( na.omit(tmp[1:nrow(data) ])^2 )
                             err = c(err,dif)
                           }else{
                             dif = mean( na.omit(tmp)^2 )
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
                       GASettings$parameterNr = ncol(A)*4 + sum(conncetions)
                       
                       if(any(rowSums(.self$conncetions ) ==0) ){##adjust for not connceted states
                         GASettings$parameterNr =  GASettings$parameterNr - sum(rowSums(.self$conncetions ) ==0) *3
                       }
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
                         colnames(fitmatrixMean) = colnames(.self$A)
                         # fitmatrixVar  = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                         fitmatrixMin = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                         colnames(fitmatrixMin) = colnames(.self$A)
                         
                         errorMatrix  =  matrix(,nrow =0,ncol = GASettings$objectiveNr )
                         errorMatrixMin  =  matrix(,nrow =0,ncol = GASettings$objectiveNr )
                         colnames(errorMatrix) = colnames(.self$A)
                         colnames(errorMatrixMin) = colnames(.self$A)
                         
                         lastPreError = NULL
                         distmatrix = matrix(,nrow =0,ncol = GASettings$popSize )
                         # distmatrixVar  = matrix(,nrow =0,ncol = GASettings$objectiveNr )
                         
                         
                         
                    #     epsilon = sim$appraisal*mymoodModel$params$w_appraisal_mood +sim$thoughts*mymoodModel$params$w_thoughts_mood
                         reportFunction= function(param, ... ){
                           res = apply(param, 1,errorFnCppDCMWrap,conncetions=.self$conncetions ,data=data,test=testdata) 
                           
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

                           
                           fitmatrixMin <<-rbind(fitmatrixMin, colMins(pred))
                           lastPreError<<-pred
                           lastFitError<<- fit
                           errorMatrixMin  <<-rbind(errorMatrixMin, rowMins(fit))
                           errorMatrix <<-rbind(errorMatrix, rowMeans(fit))
                         }
                         
                         optimVecDoPar= function(param){
                           
                          # errorFnCppDCMWrap
                           
                          # splitList <- split(param, 1:NROW(param))
                          # if(!is.null(testdata)){
                          #   res = mclapply( splitList, errorFn,mc.cores= detectCores())
                           res = apply(param, 1,errorFnCppDCMWrap,conncetions=.self$conncetions ,data=data,test=testdata) 
                           
                             fitlist = list()
                         #    predlist =list()
                             # distlist = list()
                             for(i in 1:length(res)){
                               fitlist[[i]]= res[[i]][[1]]
                            #   predlist[[i]]= res[[i]][[2]]
                             }
                             fit = matrix(unlist(fitlist),nrow=GASettings$objectiveNr,byrow = F )
                             
                            # pred = matrix(unlist(predlist),ncol=GASettings$objectiveNr,byrow = T )
                             
                             #  dist = matrix(unlist(distlist),ncol=GASettings$objectiveNr,byrow = T )
                             
                             #print(dim(fitmatrixMean))
                             #print(colMeans(pred))
                             
                           #  fitmatrixMean<<-rbind(fitmatrixMean, colMeans(pred))
                             
                           #  fitmatrixMin <<-rbind(fitmatrixMin, colMins(pred))
                          #   lastPreError <<- pred
                          #   errorMatrix <<-rbind(errorMatrix, rowMeans(fit))
                          #   errorMatrixMin  <<-rbind(errorMatrixMin, rowMins(fit))
                             
                             # fitmatrixVar<<- rbind(fitmatrixVar, colVars(pred))
                             
                             ##estimate distance to true parameters
                             #    p_dist = c()
                             #    for(i in 1:length(splitList)){
                             #      p_dist = c(p_dist,sqrt( sum( (.self$true_parameters-splitList[[i]])^2 ) ))
                             #    }
                             
                             #
                             
                             #    distmatrix<<-rbind(distmatrix, p_dist)

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
                                     vectorized=T, refDHV= rep(1,GASettings$objectiveNr),reportfunction = reportFunction)
                         
                         if(!is.null(testdata)){
                           r1$fitmatrixMean = fitmatrixMean
                           
                           r1$fitmatrixMin= fitmatrixMin
                           
                           r1$errorMatrix = errorMatrix
                           r1$errorMatrixMin =errorMatrixMin
                           r1$lastPreError= lastPreError
                           r1$lastFitError= lastFitError
                           
                           #r1$fitmatrixVar = fitmatrixVar
                           # r1$distmatrix = distmatrix
                         }
                         r1
                       }
                       return(r1)
                       
                     },
                     simCpp= function(length){
                       ##set up data
                       state = .self$X
                       res = simulateDCM( state, .self$A,.self$time, .self$sigmas,.self$taus,length)
                       colnames(res)= colnames(.self$A)
                       res
                     },
                     initialize =function(conncetions) {
                       if(class(conncetions)!='matrix'){
                         print('conncetion must be type matrix')
                       }
                       if(ncol(conncetions)!=nrow(conncetions)){
                         print('conncetion matrix must be symetric')
                       }
                       params = ncol(conncetions)
                       .self$A <<- conncetions
                       .self$conncetions <<- conncetions
                       .self$X <<-matrix(1,ncol=1,nrow=params)
                       .self$time <<-matrix(0.1,ncol=1,nrow=params)
                       .self$taus <<-matrix(0.1,ncol=1,nrow=params)
                       .self$sigmas <<-matrix(0.1,ncol=1,nrow=params)
                       ##adjust in cast
                       if(any(rowSums(.self$conncetions ) ==0) ){##adjust for not connceted states
                         print('not connceted states')
                         .self$time[rowSums(.self$conncetions ) ==0] = 0
                         .self$taus[rowSums(.self$conncetions ) ==0] = 0
                         .self$sigmas[rowSums(.self$conncetions ) ==0] = 0
                         
                       }
                     }
                   ))
### TESTING STUFF FOR BUGS
if(F){
  conMatrix = diag(3)
  conMatrix[1,2]=1
  conMatrix[3,1]=1
  conMatrix[1,3]=1
  
  colnames(conMatrix) = c('mood','sleep','thinking')
  dcmModel = dcm(conMatrix)
  #p = 1:12
  #dcmModel$setParameters(p)10
  print(dcmModel$getParameters())
  params = dcmModel$getParameters()
  set.seed(1234)
  params = runif(length(params))
  dcmModel$setParameters(params)
  
  res = dcmModel$simulate(10)
  
  colnames(res)
  
  plot(res[,1],type='l',ylim=c(0,1))
  lines(res[,2],col='red')
  lines(res[,3],col='blue')
  microbenchmark( 
  rescpp = dcmModel$simCpp(20)
  )
  ###get parameters

  microbenchmark( 
    sss = smodel$simulate(20)
  )
  
  
  lines(rescpp[,1],col='green')
  lines(rescpp[,2],col='green')
  lines(rescpp[,3],col='green')
  microbenchmark(    
  ret = errorDcmFnCpp(dcmModel$X,dcmModel$A,dcmModel$time,dcmModel$sigmas,dcmModel$taus,rescpp[1:5,],rescpp[6:10,])
)
microbenchmark(    
  optimRes = dcmModel$multiOpti(data = rescpp[1:10,],iterations = 10,population = 20,parallel = T,testdata =rescpp[11:20,] )
)


sim = smodel$simulate(20)
microbenchmark(    
nsga_params = smodel$multiOpti(data =  sim[1:10,],iterations = 10, population = 20,parallel=T,numOfParams=32,testdata = sim[11:20,])
)

#Rcpp::List errorFnCppDCMWrap(NumericVector parameters,NumericMatrix conncetions,Rcpp::DataFrame data,Rcpp::DataFrame test){
conMatrix = matrix(1,nrow=4,ncol=4)
diag(conMatrix) = 0
colnames(conMatrix) =c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut')

for( i in 1:4 ){
  for(k in 1:4 ){
    print(i)
    print(k)
  #  if(i ==4 & k ==2){
  #    stop()
  #  }
  
  conMatrix[i,k] = 0;
  #conMatrix[1,] =0
  #conMatrix[2,] =0
  dcmModel = dcm(conMatrix)
  params = dcmModel$getParameters()
  params = rep(0.2,length(params))
  params[length(params)]=0.5
  dcmModel$setParameters(params)
  
  data = dcmModel$simCpp(20)
  
  testdata = data[11:20,]
  data = data[1:10,]
  
  res = errorFnCppDCMWrap(params, dcmModel$conncetions ,data=data,test=testdata) 
  
  nparameter = ncol(data)*4 + sum(conMatrix)
  
  if(any(rowSums(conMatrix ) ==0) ){##adjust for not connceted states
    nparameter =  nparameter - sum(rowSums(conMatrix ) ==0) *3
  }
  if(nparameter != length(params)){
    print('nparams missmatch')
  }
  nsga_params = dcmModel$multiOpti(data = data,iterations = 10, population = 40,parallel=T,refPoint = rep(1,ncol(data)),testdata)

  }
}
#nsga_params = smodel$multiOpti(data = simdata,iterations = niterations, population = pop_size,parallel=T,numOfParams=numOfParams ,refPoint = rep(1,ncol(simdata)),testdata)

}