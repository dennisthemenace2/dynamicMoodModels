##some plot and helpign functions


plotSocial=function(data){
  plot(data[,2],col=2,ylim=c(0,1))
  for(i in 3:ncol(data)){
    points(data[,i],col=i)
  }
  legend('topleft', legend = c(colnames(data)[2:ncol(data)]),col = c(2:(ncol(data)+1) ),lty = rep(1,ncol(data)))
}


selectParameters = function(simData,features=NULL){
  ### we need to find good parameters
  if(is.null(features)){
    features = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut')
  }
  idx = c()
  for(i in 1:length(features)){
    idx = c(idx,which(colnames(simData) ==features[i])  )
  }
  ###select concepts
  if(length(idx)==1){
    ret = as.data.frame(simData[ ,idx])
    colnames(ret) =  colnames(simData)[idx]
  }else{
    ret = simData[ ,idx]
  }
  ret
}

library(tictoc)


###generate paramters to be used

#timing_list = list()  

library(foreach)
library(doMC)
registerDoMC(cores=4)
### 

estimateModelPerformance = function(smodel,new_prams=NULL, data_list, pop_seq,nruns,niterations,numOfParams,select_features=NULL){
  
  #check for differetn parameter settings
  #for(paramsetting in 1:length(data_list)){
  ret = foreach(paramsetting= 1:length(data_list) ) %dopar%{
    param_objects = list()
    
    ##measure time
    #start_time <- Sys.time()
    
    if(!is.null(new_prams)){
      smodel$true_parameters = new_prams[[paramsetting]] ##hack for distantace to true params
    }
    
   # simdata = smodel$simulate(N)
    simdata = data_list[[paramsetting]]
    simdata = selectParameters(simdata,select_features) # strip of time and stuff
    
    if(ncol(simdata)==1){
      savedName = colnames(simdata)
      testdata =  simdata[(N-6):N,1]
      simdata =simdata[1:(N-7),1] 
      testdata = as.data.frame(testdata)
      colnames(testdata) =  savedName
      ##restore sim
      simdata = as.data.frame(simdata)
      colnames(simdata) =  savedName
    #  stop()
    }else{
    ##prepare learning and test
      testdata = simdata[(N-6):N,]
      simdata =simdata[1:(N-7),]
    }
    
    ### ok letz optimisze....   
    ##for different populations sizes
    nsga_params_list = list()
    for(pop_size in pop_seq){ 
      run_list=list()
      for(run in 1:nruns){
        nsga_params = smodel$multiOpti(data = simdata,iterations = niterations, population = pop_size,parallel=T,numOfParams=numOfParams ,refPoint = rep(1,ncol(simdata)),testdata) ### maybe ref point not used
        run_list[[run]] = nsga_params
        #run_list =c(run_list,nsga_params)
      }
      ##save params object
      nsga_params_list[[length(nsga_params_list)+1]]= run_list
      #nsga_params_list=c(nsga_params_list, run_list)
    }
    param_objects[[length(param_objects)+1]] = nsga_params_list ##save for later use
    
    #end_time <- Sys.time()
    #passed_time_loop = end_time - start_time
    #cat(c('elapsed time:',passed_time_loop,' ',attr(passed_time_loop,'units'),' iteration:',paramsetting,'\n') )
    
    param_objects ##return result
  }
  
  
  ret ###return results
}

###plot some things

estimateAverages = function(result){
  
  DHVs= NULL
  predErr = NULL
  predErrMin = NULL
  
  distance = NULL
  fitErr = NULL
  fitErrMin = NULL
  
  cnames = c()
  
  for(i in 1:length(result)){
    
    param_obj = result[[i]][[1]]
    
    for(k in 1:length(param_obj)){ # for each population
      run_list = param_obj[[k]]
      for(x in 1:length(run_list)){
        nsga_obj = run_list[[x]]
        
        ##add dhv 
        if(is.null(DHVs) ){
          DHVs = nsga_obj$DHV
          predErr = list() ## set up list
          for(pi in 1:ncol(nsga_obj$fitmatrixMean)){
            predErr[[pi]] = matrix(nsga_obj$fitmatrixMean[,pi],ncol=1)
            cnames =c(cnames,colnames(nsga_obj$fitmatrixMean)[pi])
          }
          predErrMin= list()
          for(pi in 1:ncol(nsga_obj$fitmatrixMin)){
            predErrMin[[pi]] = matrix(nsga_obj$fitmatrixMin[,pi],ncol=1)
            
         #   cnames =c(cnames,colnames(nsga_obj$fitmatrixMin)[pi])
          }
          
          fitErr = list()
          for(pi in 1:ncol(nsga_obj$errorMatrix)){
            fitErr[[pi]] = matrix(nsga_obj$errorMatrix[,pi],ncol=1)
          }
          fitErrMin = list()
          for(pi in 1:ncol(nsga_obj$errorMatrixMin)){
            fitErrMin[[pi]] = matrix(nsga_obj$errorMatrixMin[,pi],ncol=1)
          }
          ###set up distance list
          if(!is.null(nsga_obj$distmatrix) ){
            distance = matrix(rowMeans(nsga_obj$distmatrix),ncol=1)
          }
          
        }else{
          DHVs = cbind(DHVs, nsga_obj$DHV)
          for(pi in 1:ncol(nsga_obj$fitmatrixMean)){
            predErr[[pi]] = cbind(predErr[[pi]],matrix(nsga_obj$fitmatrixMean[,pi],ncol=1) )
          }
          for(pi in 1:ncol(nsga_obj$fitmatrixMin)){
            predErrMin[[pi]] = cbind(predErrMin[[pi]] ,matrix(nsga_obj$fitmatrixMin[,pi],ncol=1) )
           }
          for(pi in 1:ncol(nsga_obj$errorMatrix)){
            fitErr[[pi]] = cbind( fitErr[[pi]],matrix(nsga_obj$errorMatrix[,pi],ncol=1))
          }
          for(pi in 1:ncol(nsga_obj$errorMatrixMin)){
            fitErrMin[[pi]] = cbind( fitErrMin[[pi]],matrix(nsga_obj$errorMatrixMin[,pi],ncol=1) )
          }
          if(!is.null(nsga_obj$distmatrix) ){
            distance = cbind(distance,matrix(rowMeans(nsga_obj$distmatrix),ncol=1) )
          }
        }
      }##end run
      
    }## end pop list
    
  }#end params
  print(cnames)
  print(length(predErr))
  
  names(predErr) = cnames
  names(predErrMin) = cnames
  names(fitErr) = cnames
  names(fitErrMin) = cnames
  
  list('DHVs'=DHVs,'predErr'=predErr,'predErrMin'=predErrMin,'distance'=distance,'fitErr'=fitErr,'fitErrMin'=fitErrMin)
}

RowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}


require(plotly)

f <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "black"
)
f2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "black"
)


plotDHV = function(avgs,p=NULL,name=c('Full model')){
  if(is.null(p)){
    p <- plot_ly() 
  }
  
  idx = which(names(avgs) =='DHVs')
  if(is.null(avgs)){
    print('DHVs not found')  
  }
  DHVs = avgs[[idx]]
  
  DHVMeans = rowMeans(DHVs)
  DHVVars = RowVar(DHVs)
  
  idx =1:length(DHVMeans)
  p<-add_trace(p,x = idx, y = DHVMeans, type = 'scatter', mode = 'lines+markers',
               name = name,
               error_y = list(value = sqrt(DHVVars), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
               line = list( width = 2) )
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Dominated hypervolume",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
}


plotDistance = function(avgs,type='avg',p=NULL,name=c('Full model') ){
  if(is.null(p)){
    p <- plot_ly() 
  }
  
  idx = which(names(avgs) =='distance')
  if(is.null(avgs)){
    print('distance not found')  
  }
  distance = avgs[[idx]]
  
  if(type=='avg'){
    distanceMeans = rowMeans(distance)
    distanceVars = RowVar(distance)
  }else{
    distanceMeans = rowMins(distance)
    distanceVars = RowVar(distance)
  }
  
  idx =0:(length(distanceMeans)-1)
  p<-add_trace(p,x = idx, y = distanceMeans, type = 'scatter', mode = 'lines+markers',
               name = name,
               error_y = list(value = sqrt(distanceVars), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
               line = list( width = 2) )
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Distance to true parameters",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
}


plotConceptError = function(avgs){
  p <- plot_ly() 
  
  idx = which(names(avgs) =='predErr')
  if(is.null(avgs)){
    print('predErr not found')  
  }
  predErr = avgs[[idx]]
  
  for(i in 1:length(predErr)){
    conceptMean = rowMeans(na.omit(predErr[[i]]))
    conceptVar = rowVars(na.omit(predErr[[i]]))
    cname = names(predErr)[i]
    
    idx =0:(length(conceptMean)-1)
    p<-add_trace(p,x = idx, y = conceptMean, type = 'scatter', mode = 'lines+markers',
                 name = cname,
                 error_y = list(value = sqrt(conceptVar), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
                 line = list( width = 2) )
    
  }
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Prediction Error",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
  
}


plotConceptFitError = function(avgs){
  p <- plot_ly() 
  
  idx = which(names(avgs) =='fitErr')
  if(is.null(avgs)){
    print('fitErr not found')  
  }
  predErr = avgs[[idx]]
  
  for(i in 1:length(predErr)){
    col.has.na <- apply(predErr[[i]], 2, function(x){any(is.na(x))})
    if(sum(col.has.na)>0){
      cleanMatrix = predErr[[i]][,!col.has.na]
      conceptMean = rowMeans(cleanMatrix)
      conceptVar = RowVar(cleanMatrix)
    }else{
      conceptMean = rowMeans(predErr[[i]])
      conceptVar = RowVar(predErr[[i]])
    }
    
    
    
    cname = names(predErr)[i]
    
    idx =0:(length(conceptMean)-1)
    p<-add_trace(p,x = idx, y = conceptMean, type = 'scatter', mode = 'lines+markers',
                 name = cname,
                 error_y = list(value = sqrt(conceptVar), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
                 line = list( width = 2) )
    
  }
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Description Error",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  #p
  
}

plotConceptFitErrorMin = function(avgs){
  p <- plot_ly() 
  
  idx = which(names(avgs) =='fitErrMin')
  if(is.null(avgs)){
    print('fitErrMin not found')  
  }
  predErr = avgs[[idx]]
  
  for(i in 1:length(predErr)){
    
    col.has.na <- apply(predErr[[i]], 2, function(x){any(is.na(x))})
    if(sum(col.has.na)>0){
      cleanMatrix = predErr[[i]][,!col.has.na]
      conceptMean = rowMeans(cleanMatrix)
      conceptVar = RowVar(cleanMatrix)
    }else{
      conceptMean = rowMeans(predErr[[i]])
      conceptVar = RowVar(predErr[[i]])
    }
    
    cname = names(predErr)[i]
    
    idx =0:(length(conceptMean)-1)
    p<-add_trace(p,x = idx, y = conceptMean, type = 'scatter', mode = 'lines+markers',
                 name = cname,
                 error_y = list(value = sqrt(conceptVar), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
                 line = list( width = 2) )
    
  }
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Minimal Description Error",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
  
}

plotConceptErrorMin = function(avgs){
  p <- plot_ly() 
  
  idx = which(names(avgs) =='predErrMin')
  if(is.null(avgs)){
    print('predErrMin not found')  
  }
  predErr = avgs[[idx]]
  
  for(i in 1:length(predErr)){
    conceptMean = rowMeans(predErr[[i]])
    conceptVar = RowVar(predErr[[i]])
    cname = names(predErr)[i]
    
    idx =0:(length(conceptMean)-1)
    p<-add_trace(p,x = idx, y = conceptMean, type = 'scatter', mode = 'lines+markers',
                 name = cname,
                 error_y = list(value = sqrt(conceptVar), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
                 line = list( width = 2) )
  }
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Prediction Min Error",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
  
}
#########copy paste

estimateModelPerformanceSimplerModel = function(smodel,data_list, pop_seq,nruns,niterations,select_features=NULL,overWriteLength=F ){
  
  #check for differetn parameter settings
  #for(paramsetting in 1:length(new_prams)){
  ret = foreach(paramsetting= 1:length(data_list) ) %dopar%{
    param_objects = list()
    
    ##measure time
  #  start_time <- Sys.time()
    
 #   smodel$true_parameters = new_prams[[paramsetting]] ##hack for distantace to true params
   # genModel$setParameters(new_prams[[paramsetting]])
    
    #simdata = genModel$simulate(N)
    simdata = data_list[[paramsetting]]
    simdata = selectParameters(simdata,select_features) # strip of time and stuff
  #  print(dim(simdata))
    ##prepare learning and test
    if(overWriteLength){ ## used for learn the model from shorter ts
      N =  nrow(simdata)
    }
    
    
    if(ncol(simdata)==1){
      savedName = colnames(simdata)
      print('fix 1 feature only')
      testdata = simdata[(N-6):N]
      simdata =simdata[1:(N-7)]
      
      testdata = as.data.frame(testdata)
      simdata = as.data.frame(simdata)
      colnames(testdata)= select_features
      colnames(simdata)= select_features
    }else{
      
      testdata = simdata[(N-6):N,]
      simdata =simdata[1:(N-7),]
    }
    
    ### ok letz optimisze....   
    ##for different populations sizes
    nsga_params_list = list()
    for(pop_size in pop_seq){ 
      run_list=list()
      for(run in 1:nruns){
        nsga_params = smodel$multiOpti(data = simdata,iterations = niterations, population = pop_size,parallel=T,refPoint = rep(1,ncol(simdata)),testdata) ### maybe ref point not used
        run_list[[run]] = nsga_params
        #run_list =c(run_list,nsga_params)
      }
      ##save params object
      nsga_params_list[[length(nsga_params_list)+1]]= run_list
      #nsga_params_list=c(nsga_params_list, run_list)
    }
    param_objects[[length(param_objects)+1]] = nsga_params_list ##save for later use
    
   # end_time <- Sys.time()
  #  passed_time_loop = end_time - start_time
  #  cat(c('elapsed time:',passed_time_loop,' ',attr(passed_time_loop,'units'),' iteration:',paramsetting,'\n') )
    
    param_objects ##return result
  }
  
  
  ret ###return results
}




removeDays = function(datalist,NdaysToRemove=2){
  
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen = nrow(dataItem)
    for(k in 1:(Nlen/7) ){
      remidx = sample(1:7,NdaysToRemove) ##these will be removed
      dataItem[(remidx+ (k-1)*7)  ,] = NA## remove
    }
    datalist[[i]] = dataItem
  }
  datalist
}


removeDaysWithNames = function(datalist,NdaysToRemove){
  if(class(NdaysToRemove) !='list'){
    print('param has be of type list')
    return (NULL) ;
  }
  remnames = names(NdaysToRemove)
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen = nrow(dataItem)
    for(x in 1:length(remnames)){
      idx = which(remnames[x] == colnames(dataItem))  
      if(length(idx)>0 ){
        nrem = NdaysToRemove[[x]]
        for(k in 1:(Nlen/7) ){
          remidx = sample(1:7,nrem) ##these will be removed
          dataItem[(remidx+ (k-1)*7),idx] = NA## remove
        }
      }else{
        cat(c("not found:",remnames[x],'\n'))
      }
    }
    datalist[[i]] = dataItem
  }
  datalist
}


addNoise = function(datalist,NoiseLvl=0.01){
  
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen = nrow(dataItem)
    Rlen = ncol(dataItem)
    for(k in 1:Rlen){
      dataItem[,k] =dataItem[,k]+ rnorm(Nlen,0,NoiseLvl)
      ###bound between 0 and 1
      idx = which(dataItem[,k]<0)
      if(length(idx)>0){
        dataItem[idx,k] =0 ## bound  
      }
      idx = which(dataItem[,k]>1)
      if(length(idx)>0){
        dataItem[idx,k] =1 ## bound  
      }
      
    }
    datalist[[i]] = dataItem
  }
  datalist
}


##new modelScore
estimateModelScore = function(result){
  
  DHVs= NULL
  corScores = c()
  
  cnames = c()
  predErrors = NULL
  #  fitErrors = NULL
  corPramError = NULL
  corParamParam = NULL
  
  for(i in 1:length(result)){ ## each client
    
    param_obj = result[[i]][[1]]
    clientDHVs = c()
    clientFit = NULL
    clientPred = NULL
    
    corPramErrorClient = NULL
    
    for(k in 1:length(param_obj)){ # for each population
      run_list = param_obj[[k]]
      
      for(x in 1:length(run_list)){ ## for each run
        nsga_obj = run_list[[x]]
        clientDHVs= c(clientDHVs, nsga_obj$DHV[length(nsga_obj$DHV)] ) ##take last or max ??
        
        if(is.null(predErrors)){
          predErrors = matrix(,nrow=0,ncol=ncol(nsga_obj$fitmatrixMin))
          #          fitErrors = matrix(,nrow=0,ncol=ncol(nsga_obj$errorMatrixMin))
        }
        # predErrors = rbind(predErrors,nsga_obj$fitmatrixMin[nrow(nsga_obj$fitmatrixMin),] )
        predErrors = rbind(predErrors,nsga_obj$lastPreError )
        
        #        fitErrors  = rbind(fitErrors,nsga_obj$errorMatrixMin[nrow(nsga_obj$errorMatrixMin),] )
        
        ##clientIndividual
      #  if(is.null(clientFit)){
       #   clientFit = matrix(,nrow=0,ncol=ncol(nsga_obj$errorMatrixMin))
        #  clientPred= matrix(,nrow=0,ncol=ncol(nsga_obj$fitmatrixMin))
      #  }
        # clientFit =  rbind(clientFit,nsga_obj$errorMatrixMin[nrow(nsga_obj$errorMatrixMin),] )
        #  clientPred= rbind(clientPred,nsga_obj$fitmatrixMin[nrow(nsga_obj$fitmatrixMin),] )
        
        #clientFit =  rbind(clientFit,nsga_obj$value )
        #clientPred= rbind(clientPred,nsga_obj$lastPreError )
        
        for(z in 1:ncol(nsga_obj$value)){
          corScores= c(corScores,cor(nsga_obj$lastFitError[z,],nsga_obj$lastPreError[,z]))
        }
        
        
        cpe =abs( cor(nsga_obj$par,nsga_obj$value ))
        # if(any(is.na(cpe))){
        ##better skip
       # print('corretation between params and vlue contrains NAs')
        #  }else{
        if(is.null(corPramErrorClient)){
          corPramErrorClient = matrix(rowMaxs(cpe),ncol= nrow(cpe) )
        }else{
          corPramErrorClient = rbind(corPramErrorClient,rowMaxs(cpe) ) 
        }
        #  }
        cpp = cor(nsga_obj$par,nsga_obj$par )
        #  if(any(is.na(cpp))){
        ##better skip
      #  print('corretation between params contrains NAs')
        #  }else{
        if(is.null(corParamParam)){
          corParamParam =matrix(cpp[lower.tri(cpp, diag = FALSE)],nrow=1)
        }else{
          corParamParam = rbind(corParamParam,cpp[lower.tri(cpp, diag = FALSE)] )
        }
        #  }
        
      }##end run
      
    }## end pop list
    DHVs = c(DHVs,mean(clientDHVs) )
  #  print(DHVs)
  #  if(mean(clientDHVs)==0){
  #    stop()
  #  }
    ##calculate correlation scores
   if(is.null(corPramError)){
      corPramError = matrix(colMaxs(abs(corPramErrorClient) ),nrow=1)
    }else{
      corPramError = rbind(corPramError, colMaxs(abs(corPramErrorClient) ) )
    }
   
    
  }#end params
  
  ##clacualte DHV score
  #descriptive score= mu* (1-sigma)
  descriptiveScore = mean(DHVs)*(1- sqrt(var(DHVs)))
  # absolute pred score
  #  mean(predErrors)
  #corScores = abs(corScores)
  #print(corScores)
  
  if(sum(is.na(corScores)) >0 ){
    print('corScores contains NAs !')
    corScores = na.omit(corScores)
  }
  
  absolutePredScore= (1-mean(as.numeric(predErrors)))* (1-sqrt(var(as.numeric(predErrors))))
  relativePredScore = max(mean(corScores),0)*(1-sqrt(var(corScores)))
  ##estimate correlation for all in one.
  #overAllCorScores = c()
  #for(z in 1:ncol(predErrors)){
  #  overAllCorScores= c(overAllCorScores,cor(fitErrors[,z],predErrors[,z]))
  #}
  #relOverAll = max(mean(overAllCorScores),0) *(1-sqrt(var(overAllCorScores)))
  
  contributing = colMaxs(corPramError)>=0.35
  corParamParam = abs(corParamParam)
  collLinear = colMeans(corParamParam)>=0.35
  if(any(is.na(contributing))){
    contributing[ is.na(contributing)] = FALSE
  }
  if(any(is.na(collLinear))){
    collLinear[ is.na(collLinear)] = FALSE
  }
  
  ##calc sensitivity score
  sensitivityScore =(sum(contributing) + sum(1-collLinear)) / (length(contributing)+length(collLinear))
  
  #calclate complexity score
  n_states = ncol(nsga_obj$errorMatrixMin)#ncol(clientFit)
  n_params = length(contributing)
  ##paramter sensetivity
  list('descriptiveScore'=descriptiveScore,'absolutePredScore'=absolutePredScore,'relativePredScore'=relativePredScore,'sensitivityScore'=sensitivityScore,'n_states'=n_states,'n_params'=n_params)
}



###plot distance for the list

plotDistanceList = function(avgs_list,type='avg',names=c()){
  
  p <- plot_ly() 
  if(length(names)==0 ){
    names = paste(1:length(avgs_list),'Item')
  }
  for(i in 1:length(avgs_list)){
    avg = avgs_list[[i]]
    p = plotDistance( avg,type, p,name = names[i])
  }
  p
}

plotConceptFitErrorMinAVGList = function(avgs_list,names=c(),p=NULL){
  if(is.null(p)){
    p <- plot_ly() 
  }
  if(length(names)==0 ){
    names = paste(1:length(avgs_list),'Item')
  }
  for(i in 1:length(avgs_list)){
    avg = avgs_list[[i]]
    p = plotConceptFitErrorMinAVG( avg, p,name = names[i])
  }
  p
}

plotConceptPredErrorMinAVGList = function(avgs_list,names=c(),p=NULL){
  if(is.null(p)){
    p <- plot_ly() 
  }
  if(length(names)==0 ){
    names = paste(1:length(avgs_list),'Item')
  }
  for(i in 1:length(avgs_list)){
    avg = avgs_list[[i]]
    p = plotConceptPredErrorMinAVG( avg, p,name = names[i])
  }
  p
}


plotDHVList = function(avgs_list,names=c(),p=NULL){
  if(is.null(p)){
    p <- plot_ly() 
  }
  if(length(names)==0 ){
    names = paste(1:length(avgs_list),'Item')
  }
  for(i in 1:length(avgs_list)){
    avg = avgs_list[[i]]
    p = plotDHV( avg, p,name = names[i])
  }
  p
}
plotConceptFitErrorMinAVG = function(avgs,p=NULL,name=c('Full model'),ylab=c('Minimal Prediction Error')){
  if(is.null(p)){
    p <- plot_ly() 
  }
  
  idx = which(names(avgs) =='fitErrMin')
  if(is.null(avgs)){
    print('fitErrMin not found')  
  }
  predErr = avgs[[idx]]

  conceptMean = matrix(rowMeans(predErr[[1]]),nrow=1)
  if(length(length(predErr))>1){
    for(i in 2:length(predErr)){
      conceptMean = rbind( conceptMean,matrix(rowMeans(predErr[[i]]),nrow=1))
    }
  }
  conceptMean_m = colMeans(conceptMean)
  conceptMean_v = colVars(conceptMean)
  
  idx =0:(length(conceptMean_m)-1)
  p<-add_trace(p,x = idx, y = conceptMean_m, type = 'scatter', mode = 'lines+markers',
               name = name,
               error_y = list(value = sqrt(conceptMean_v), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
               line = list( width = 2) )
  
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = ylab,
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
  
}



plotConceptPredErrorMinAVG = function(avgs,p=NULL,name=c('Full model'),ylab=c('Minimal Prediction Error')){
  if(is.null(p)){
    p <- plot_ly() 
  }
  
  idx = which(names(avgs) =='predErrMin')
  if(is.null(avgs)){
    print('predErrMin not found')  
  }
  predErr = avgs[[idx]]
  
  conceptMean = matrix(rowMeans(predErr[[1]]),nrow=1)
  if(length(length(predErr))>1){
    for(i in 2:length(predErr)){
      conceptMean = rbind( conceptMean,matrix(rowMeans(predErr[[i]]),nrow=1))
    }
  }
  conceptMean_m = colMeans(conceptMean)
  conceptMean_v = colVars(conceptMean)
  
  idx =0:(length(conceptMean_m)-1)
  p<-add_trace(p,x = idx, y = conceptMean_m, type = 'scatter', mode = 'lines+markers',
               name = name,
               error_y = list(value = sqrt(conceptMean_v), width = 2,opacity=0.1),# list(value = sqrt(vars[showidx]), width = 2,opacity=0.1),
               line = list( width = 2) )
  
  
  x <- list(
    title = "Generation",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = ylab,
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
  p
  
}
####estimtae min error and predErr


estimatePredictionErrors = function(result){
  
  predErr = NULL
  predErrMin = NULL
  bestModelErr = NULL
  cnames = c()
  
  for(i in 1:length(result)){
    param_obj = result[[i]][[1]]
    
    for(k in 1:length(param_obj)){ # for each population
      run_list = param_obj[[k]]
      for(x in 1:length(run_list)){
        nsga_obj = run_list[[x]]
        
        ##add dhv 
        if(is.null(predErr) ){
          predErr =matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          predErrMin =matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          bestModelErr =matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          
          cnames = colnames(nsga_obj$fitmatrixMean)
          colnames(predErr) = cnames
          colnames(predErrMin) = cnames
          colnames(bestModelErr) = cnames
          
        }
        
        values = c()
        for(pi in 1:nrow(nsga_obj$lastFitError)){
          idx = which.min(nsga_obj$lastFitError[pi,])
          if(length(idx)>1){
            ###just take first
            idx = idx[1]
            print('same error just pick first')
          }
          if(length(idx)==0){
            print('need to append NA')
            values =  c(values,NA)
          }else{
            values =  c(values, nsga_obj$lastPreError[idx,pi]) ##apend
          }
        }
        predErr = rbind(predErr,values)
      
        values = c() ##rest and get min error
        for(pi in 1:ncol(nsga_obj$lastPreError)){
          values =  c(values, min(nsga_obj$lastPreError[,pi]) ) ##apend
          if(is.na(min(nsga_obj$lastPreError[,pi]))){
            print('min pred is NA')
          }
        }
        if(length(values)!= ncol(predErrMin)){
          print('this will reuslt into errors')
          stop()
        }
        predErrMin = rbind(predErrMin, values)
        ###best model
        values = c() ##rest and get min error
        idx = which.min(rowSums(nsga_obj$lastFitError))
        if(length(idx)>1){
          idx = idx[1]
        }else if(length(idx)==0){
          print('this will result into an error')
          stop()
        }
        values =  nsga_obj$lastPreError[idx,] ##apend
        if(length(values)!= ncol(bestModelErr)){
          print('this will reuslt into errors')
          stop()
        }
        bestModelErr = rbind(bestModelErr, values)
        
      }##end run
      
    }## end pop list
    
  }#end params

  list('predErrors'= predErr,'minPredErrors'= predErrMin,'bestModelErro'=bestModelErr)
}


###get mean predrttction erro

getMeanModelPredictionList = function(data_list){
  res = matrix(,nrow=0,ncol=4)
  colnames(res) = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut')

  for(paramsetting in 1:length(data_list)){
    simdata = data_list[[paramsetting]]
    simdata = selectParameters(simdata) # strip of time and stuff
    
    if(ncol(simdata)==1){
      savedName = colnames(simdata)
      testdata =  simdata[(N-6):N,1]
      simdata =simdata[1:(N-7),1] 
      testdata = as.data.frame(testdata)
      colnames(testdata) =  savedName
      ##restore sim
      simdata = as.data.frame(simdata)
      colnames(simdata) =  savedName
      #  stop()
    }else{
      ##prepare learning and test
      testdata = simdata[(N-6):N,]
      simdata =simdata[1:(N-7),]
    }
    
    ###simdata // test
    mat = matrix(NA,nrow=1,ncol=4)
    colnames(mat) = colnames(res)
    for(k in 1:ncol(simdata)){
      pred = mean(na.omit(simdata[,k]) )
      err = mean( na.omit( (testdata[,k]-pred)^2 ) )
      idx = which(colnames(simdata)[k] == colnames(mat))
      if(length(idx)<=0){
        print('error')
        stop()
      }
      mat[1,idx] = err
    }
    res = rbind(res,mat)
  }
  res
}
  

###create a a list of valid training examples



createListOfValidSamples = function(allClientsEmaRes,N){
  
  valid_clientData_list = list()
  not_valid_clientData  = list()

  for(i in 1:length(allClientsEmaRes)){
#  start_time <- Sys.time()
  
    obj = allClientsEmaRes[[i]]
    ret = createTSframeNEW(obj)
    if(is.null(ret) ){
      cat(c('list is empty:',i,'\n'))
      next;
    }
    simdata = replaceEmaNameWithSimNames(ret)
    ##prepare train and test set
    nr = nrow(simdata)
    if(nr<N){
      cat(c('too little data to create train and test:',i,'\n'))
      next;
    }
  
    simdata =selectParameters(simdata)
    ##do some moew checks... otherwise if will screw me over
    if(ncol(simdata)<4){
      cat(c('too little concepts:',i,'\n'))
      next;
    }
    ##check if there is test data
    valid = T
    for(t in 1:ncol(simdata)){
      test = simdata[(N-6): N,t]
      if(sum(!is.na(test))==0 ){
        valid = F
        break
      }
       train = simdata[1:(N-7),t]
        if(sum(!is.na(train))==0 ){
          valid = F
          break
        }
      
    }
    if(valid){
      valid_clientData_list[[length(valid_clientData_list)+1]] =simdata
    }else{
      cat(c('no test data available:',i,'\n'))
      not_valid_clientData[[length(not_valid_clientData)+1]]=obj #save for later use
    }
  
    cat(c('iteration:',i,':',length(allClientsEmaRes),'\n') )
  }
  list('valid'=valid_clientData_list,'invalid' =not_valid_clientData )  
}
  