##some plot and helper functions

##plot concepts in the data
plotSocial=function(data){
  plot(data[,2],col=2,ylim=c(0,1))
  for(i in 3:ncol(data)){
    points(data[,i],col=i)
  }
  legend('topleft', legend = c(colnames(data)[2:ncol(data)]),col = c(2:(ncol(data)+1) ),lty = rep(1,ncol(data)))
}

##helper function to select the analysizes features in the data
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

##helper function to estimate the performance on the data
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
      }
      ##save params object
      nsga_params_list[[length(nsga_params_list)+1]]= run_list
    }
    param_objects[[length(param_objects)+1]] = nsga_params_list ##save for later use
    
    #end_time <- Sys.time()
    #passed_time_loop = end_time - start_time
    #cat(c('elapsed time:',passed_time_loop,' ',attr(passed_time_loop,'units'),' iteration:',paramsetting,'\n') )
    
    param_objects ##return result
  }
  
  
  ret ###return results
}

###plots to assess the optimization performance

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

##load plotly
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

##plot the course of the DHV during optimization
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

##plot the distance to the true parameters during optimization
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

##plot the error in each concept during optimization
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

##plot the fitting error to the data during optimization
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

##plot the minimal fittig error over the course of optimization
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

###helper function to create missing values
removeDays = function(datalist,NdaysToRemove=2,ntrain=42){
  
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen = ntrain#nrow(dataItem)
    for(k in 1:(Nlen/7) ){
      remidx = sample(1:7,NdaysToRemove) ##these will be removed
      dataItem[(remidx+ (k-1)*7)  ,] = NA## remove
    }
    datalist[[i]] = dataItem
  }
  datalist
}

###helper function
removeDaysWithNames = function(datalist,NdaysToRemove,ntrain=42,offset = 0){
  if(class(NdaysToRemove) !='list'){
    print('param has be of type list')
    return (NULL) ;
  }
  remnames = names(NdaysToRemove)
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen = ntrain#nrow(dataItem)
    for(x in 1:length(remnames)){
      idx = which(remnames[x] == colnames(dataItem))  
      if(length(idx)>0 ){
        nrem = NdaysToRemove[[x]]
        for(k in (1+offset):(Nlen/7) ){
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

### function to add Gaussian noise to the data 
addNoise = function(datalist,NoiseLvl=0.01,train=42){
  
  for(i in 1:length(datalist) ){
    dataItem = datalist[[i]]
    Nlen =train#nrow(dataItem)
    Rlen = ncol(dataItem)
    for(k in 1:Rlen){
      if(is.list(NoiseLvl)){
        idx = which(colnames(dataItem)[k]==names(NoiseLvl))
        if(length(idx)==0){
          print('name not found!')
          stop()
        }
        noise =  rnorm(Nlen,0,NoiseLvl[[idx]])
      }else{
        noise =  rnorm(Nlen,0,NoiseLvl)
      }
      
      dataItem[1:Nlen,k] =dataItem[1:Nlen,k]+noise
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


##function to estimate the performance measures of a model
estimateModelScore = function(result){
  
  DHVs= NULL
  corScores = c()
  
  cnames = c()
  predErrors = NULL
  fitErrors = NULL
  corPramError = NULL
  corParamParam = NULL
  
  ##for my sample
  MeanCorOptims = c()
  MeanPredErrorsOption = c()
  
  for(i in 1:length(result)){ ## each client
    
    param_obj = result[[i]][[1]]
    clientDHVs = c()
    clientFit = NULL
    clientPred = NULL
    
    corPramErrorClient = NULL
    ###calculate individual scores
    
    
    for(k in 1:length(param_obj)){ # for each population
      run_list = param_obj[[k]]
      
      for(x in 1:length(run_list)){ ## for each run
        nsga_obj = run_list[[x]]
        clientDHVs= c(clientDHVs, nsga_obj$DHV[length(nsga_obj$DHV)] ) ##take last or max ??
        
        if(is.null(predErrors)){
          predErrors = matrix(,nrow=0,ncol=ncol(nsga_obj$fitmatrixMin))
          fitErrors = matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError))
          # print(nrow(nsga_obj$lastFitError))
        }
        predErrors = rbind(predErrors,nsga_obj$lastPreError )
        
        fitErrors  = rbind(fitErrors,t(nsga_obj$lastFitError) )

        cCorScore = c()
        for(z in 1:ncol(nsga_obj$value)){
          cscoreTmp = cor(nsga_obj$lastFitError[z,],nsga_obj$lastPreError[,z])
          cCorScore= c(cCorScore,cscoreTmp )
          corScores= c(corScores,cscoreTmp)
        }
        MeanCorOptims =c(MeanCorOptims,(cCorScore))

        MeanPredErrorsOption = c(MeanPredErrorsOption, colMeans(nsga_obj$lastPreError ) )
        
        cpe =abs( cor(nsga_obj$par,nsga_obj$value ))

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
  
  if(sum(is.na(corScores)) >0 ){
    print('corScores contains NAs!')
    corScores = na.omit(corScores)
  }
  
  absolutePredScore= (1-mean(as.numeric(sqrt(predErrors) )))* (1-sqrt(var(sqrt(as.numeric(predErrors)))))
  relativePredScore = max(mean(corScores),0)*(1-sqrt(var(corScores)))

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
  
  descSample = myboot(DHVs,y=NULL,getDescriptiveScore,5000) 
  
  if(any(is.na(MeanCorOptims))){
    sprintf('contains NA:%d/%d',sum(is.na(MeanCorOptims) ),length(MeanCorOptims))
    #  print(sum(is.na(MeanCorOptims) ))
    MeanCorOptims[is.na(MeanCorOptims)] = 0
  }
  mysample = myboot(MeanPredErrorsOption,MeanCorOptims,getPredictiveScore,2000);

  desc_low = as.numeric(quantile(descSample,c(0.025)) )#as.numeric(descbca[1])
  desc_high = as.numeric( quantile(descSample,c(0.975)) ) #as.numeric(descbca[2])
  
  if(any(is.na(mysample)) ){
    print('na')
    print(sum(is.na(mysample)))
  }
  pred_low= as.numeric(quantile(mysample ,c(0.025)))
  pred_high =as.numeric( quantile(mysample ,c(0.975)) )
  
  list('descriptiveScore'=mean(descSample),#mean(x.boot$data),#descriptiveScore,
       'descriptiveScoreVar'=var(descSample),
       'descriptiveScoreMean'=mean(DHVs),
       'absolutePredScore'=absolutePredScore,
       'absolutePredScoreVar'=var(sqrt(as.numeric(predErrors))),
       'relativePredScore'=relativePredScore,
       'relativePredScoreVar' =var(corScores),
       'sensitivityScore'=sensitivityScore,
       'descriptiveScore_boot'= c(desc_low,desc_high ),
       'predictiveScore_boot'= c(pred_low,pred_high),
       'predictiveScore_mean_boot'=  mean(mysample),
       'predictiveScore_var_boot'= var(mysample),
       'n_states'=n_states,'n_params'=n_params)
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

plotConceptPredErrorMinAVGList = function(avgs_list,names=c(),p=NULL,AVG=FALSE){
  if(is.null(p)){
    p <- plot_ly() 
  }
  if(length(names)==0 ){
    names = paste(1:length(avgs_list),'Item')
  }
  for(i in 1:length(avgs_list)){
    avg = avgs_list[[i]]
    p = plotConceptPredErrorMinAVG( avg, p,name = names[i],AVG=AVG)
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
plotConceptFitErrorMinAVG = function(avgs,p=NULL,name=c('Full model'),ylab=c('Minimal Prediction Error'),AVG = FALSE){
  if(is.null(p)){
    p <- plot_ly() 
  }
  
  searchStr = 'fitErrMin'
  if(AVG){
    searchStr = 'fitErr'
  }
  idx = which(names(avgs) ==searchStr )
  if(is.null(avgs)){
    cat(c(searchStr,' not found','\n') )  
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



plotConceptPredErrorMinAVG = function(avgs,p=NULL,name=c('Full model'),ylab=c('Minimal Prediction Error'),AVG = FALSE){
  if(is.null(p)){
    p <- plot_ly() 
  }
  searchStr = 'predErrMin'
  if(AVG){
    searchStr= 'predErr'
  }
  idx = which(names(avgs) ==searchStr)
  if(is.null(avgs)){
    cat(c(searchStr, ' not found\n'))  
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
# function can be used to calucalte the errors, however they are already calculated by the modified mco package
estimateSimulatedPredictionErrors = function(result,model,simData,n_train=42){
  fittingError= NULL
  predErr = NULL
  predErrMin = NULL
  bestModelErr = NULL
  cnames = c()
  ##im am intersted in the increase in error
  rawErrorsList = list()
  for(i in 1:4){
    rawErrorsList[[i]] =matrix(,nrow=0,ncol=N_TEST)
  }
  
  ###
  for(i in 1:length(result)){
    param_obj = result[[i]][[1]]
    
    
    for(k in 1:length(param_obj)){ # for each population
      run_list = param_obj[[k]]
      for(x in 1:length(run_list)){
        nsga_obj = run_list[[x]]
        
        if(is.null(predErr) ){
          predErr =matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          ##fiterror 
          fittingError = matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          cnames = colnames(nsga_obj$fitmatrixMean)
          colnames(predErr) = cnames
          colnames(fittingError) = cnames
        }
        
        values_fit = c()
        values_pred = c()
        for(pi in 1:nrow(nsga_obj$lastFitError)){
          idx = which.min(nsga_obj$lastFitError[pi,])
          if(length(idx)>1){
            ###just take first
            idx = idx[1]
            print('same error just pick first')
          }
          if(length(idx)==0){
            print('need to append NA')
            # values =  c(values,NA)
            stop()
          }
          ##simulate data
          model$setParameters(nsga_obj$par[idx,])
          sdata = model$simulate(nrow(simData[[i]]))
          colidx = which(colnames( simData[[i]]) ==colnames(nsga_obj$fitmatrixMean)[pi] )
          if(length(colidx)==0){
            print('not found')
            stop()
          }
          err = ( sdata[,colnames(nsga_obj$fitmatrixMean)[pi] ] - simData[[i]][,colidx])^2
          fiterr_tmp = mean(na.omit(err[1:n_train]) )
          prederr_tmp = mean(na.omit(err[(n_train+1):N ]) )
          
          ###RAW ERRORS
          rawErrors = rawErrorsList[[pi]]
          rawErrors = rbind(rawErrors,err[(n_train+1):N ])
          rawErrorsList[[pi]] = rawErrors 
          
          values_fit = c(values_fit,fiterr_tmp)
          values_pred = c(values_pred,prederr_tmp)
          
        }
        predErr = rbind(predErr,values_pred)
        
        fittingError = rbind(fittingError,values_fit)
      }##end run
      
    }## end pop list
    
  }#end params
  
  list('predErrors'= predErr,'fittingError'=fittingError,'rawErrorsList'=rawErrorsList)
  
}

estimatePredictionErrors = function(result){
  
  fittingError= NULL
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
          ##fiterror 
          fittingError = matrix(,nrow=0,ncol=nrow(nsga_obj$lastFitError) )
          
          cnames = colnames(nsga_obj$fitmatrixMean)
          colnames(predErr) = cnames
          colnames(predErrMin) = cnames
          colnames(bestModelErr) = cnames
          colnames(fittingError) = cnames
          
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
        
        ##append min fitting Error
        
        values = c()
        for(pi in 1:nrow(nsga_obj$lastFitError)){
          values =  c(values, min(nsga_obj$lastFitError[pi,])) ##apend
        }
        fittingError = rbind(fittingError,values)
      }##end run
      
    }## end pop list
    
  }#end params
  
  list('predErrors'= predErr,'minPredErrors'= predErrMin,'bestModelErro'=bestModelErr,'fittingError'=fittingError)
}


###get mean prediction error
getMeanModelPredictionList = function(data_list,N=49){
  res = matrix(,nrow=0,ncol=4)
  colnames(res) = c('moodLevel', 'enjoyedActivites','socialInteraction','activitiesCarriedOut')
  
  fitres = matrix(,nrow=0,ncol=4)
  colnames(fitres) = colnames(res)
  
  rawErrorsList = list()
  for(i in 1:4){
    rawErrorsList[[i]] =matrix(,nrow=0,ncol=N_TEST)
  }
  
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
    fitmat = matrix(NA,nrow=1,ncol=4)
    colnames(mat) = colnames(res)
    for(k in 1:ncol(simdata)){
      pred = mean(na.omit(simdata[,k]) )
      fitError = mean( na.omit( (simdata[,k]-pred)^2 ) )
      err = mean( na.omit( (testdata[,k]-pred)^2 ) )
      idx = which(colnames(simdata)[k] == colnames(mat))
      if(length(idx)<=0){
        print('error')
        stop()
      }
      mat[1,idx] = err
      fitmat[1,idx] = fitError
      rawErrors = rawErrorsList[[k]]
      rawErrors = rbind(rawErrors,(testdata[,k]-pred)^2)
      rawErrorsList[[k]] = rawErrors 
      
    }
    res = rbind(res,mat)
    
    fitres = rbind(fitres,fitmat)
  }
  list('predictionError'=res,'fittingError' =fitres,'rawErrorsList'=rawErrorsList)
}


###create a list of valid training examples


##removes columns by name in a data.frame
remstuff = function(data,remnames){
  ##remove cc.x , 
  savedColnames = colnames(data)
  for(i in 1:length(remnames)){
    cn = remnames[i]
    idx = which(colnames(data)==cn)
    if(length(idx)>0){
      data = data[,-idx]
      idx = which(savedColnames==cn)
      savedColnames = savedColnames[-idx]
    }
  }
  if(is.null(ncol(data))){
    data = as.data.frame(data)
    colnames(data)= savedColnames
  }
  data
}

##map to correct names
replaceEmaNameWithSimNames=function(df){
  simNames = c("moodLevel","enjoyedActivites","socialInteraction","activitiesCarriedOut")
  emaNames = c( "mood","enjoyed activities" ,"social contact","pleasant activities")
  for(i in 1:length(emaNames)){
    cn = emaNames[i]
    idx = which(colnames(df)==cn)
    if(length(idx)>0){
      colnames(df)[idx] = simNames[i] ##replace collname
    }
  }
  removeEma = c('sleep','worrying','self-esteem')
  df = remstuff(df, removeEma)
  df
}

createTSframeNEW = function(obj){
  
  if(length(obj)==0) {
    print('empty list')
    return (NULL);
  }
  ##get min and max
  alltimes = c()
  for(i in 1:length(obj)){
    alltimes =c(alltimes,as.character(time(obj[[i]])) ) 
  }
  alltimes =  as.Date(as.character(alltimes), format = '%Y-%m-%d')
  mints = min(alltimes)
  maxts = max(alltimes)
  
  days = as.numeric(maxts - mints)+1
  all = ceiling(days)
  #  lower = mints
  
  ##create ist
  resultList = list()
  for(i in 1:length(obj)){
    resultList[[i]]=rep(NA,all)
  }
  
  
  #tms = as.Date(as.character(tms), format = '%Y-%m-%d')
  lower = mints#as.Date(as.character(mints), format = '%Y-%m-%d')
  
  
  for(i in 1:all){
    for(k in 1:length(obj)){
      dates = as.Date(as.character(time(obj[[k]]) ), format = '%Y-%m-%d')
      idx =which( dates==lower )
      if(length(idx)>0){##found
        resultList[[k]][i]= mean(obj[[k]][idx])
      }
    }
    lower = lower +1
  }
  ##prepare dataframe
  ret = matrix(unlist(resultList),byrow = F,ncol=length(resultList) )
  colnames(ret)=names(obj)
  ret = ret /10
  ret[ret>1]=1
  as.data.frame(ret)
  #ret = data.frame(moodLevel = newMoodTs/10,  enjoyedActivites=  (newWorryTs/10)  )
}

createListOfValidSamples = function(clientData,N){
  
  valid_clientData_list = list()
  not_valid_clientData  = list()
  
  ids = unique(clientData$id)
  for(i in 1:length(ids)){
    #  start_time <- Sys.time()
    
    obj = clientData[clientData$id==ids[[i]],]
    ret = obj[,-c(1,2)]
    if(is.null(ret) ){
      cat(c('list is empty:',i,'\n'))
      next;
    }
    simdata = replaceEmaNameWithSimNames(ret)
    ##prepare train and test set
    nr = nrow(simdata)
    if(nr<N){
      cat(c('too few data to create train and test:',i,'\n'))
      next;
    }
    
    simdata =selectParameters(simdata)
    ##do some moew checks... otherwise if will screw me over
    if(ncol(simdata)<4){
      cat(c('too few concepts:',i,'\n'))
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
    
    cat(c('iteration:',i,':',length(ids),'\n') )
  }
  list('valid'=valid_clientData_list,'invalid' =not_valid_clientData )  
}


plotScoreTable=function(noiseScoreTable,noise_seq){
  p <- plot_ly() 
  
  idx =noise_seq
  p<-add_trace(p,x = idx, y = noiseScoreTable$descriptiveScore, type = 'scatter', mode = 'lines+markers',
               name = 'Descriptive performance',
               line = list( width = 2) )
  
  p<-add_trace(p,x = idx, y = (noiseScoreTable$absolutePredScore+noiseScoreTable$relativePredScore)/2 , type = 'scatter', mode = 'lines+markers',
               name = 'Predictive performance',
               line = list( width = 2) )
  
  
  # p<-add_trace(p,x = idx, y = noiseScoreTable$relativePredScore, type = 'scatter', mode = 'lines+markers',
  #               name = 'Relative predictive score',
  #               line = list( width = 2) )
  #  p<-add_trace(p,x = idx, y = noiseScoreTable$absolutePredScore, type = 'scatter', mode = 'lines+markers',
  #               name = 'Absolute predictive score',
  #               line = list( width = 2) )
  
  x <- list(
    title = "Noise",
    ticktext = noise_seq,
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "Score",
    titlefont = f,
    tickfont = f2
  )
  p  = layout(p,xaxis = x, yaxis = y,legend = list(font = list(size = 15)))
}

require(mco)


##bootstrap function
myboot= function(x,y=NULL,f,N= 10^4){
  if(class(x)=='numeric'){
    k = length(x)
  }else{
    k = nrow(x)
  }
  
  if(!is.null(y)){
    if(length(y)!= length(x)){
      print('x and y differ in length')
      print(length(x) )
      print(length(y))
    }
  }
  
  ###bayes
  #weights = matrix( rbeta(N*k,shape1 = 2,shape2 = 2 ), ncol=k,byrow = TRUE)
  # weights = matrix(  rexp(k * N, 1)  , ncol=k,byrow = TRUE)
  #  weights = weights / rowSums(weights)
  
  store = rep(NA,N)
  for(i in 1:N){  
    s =sample(1:k,k,replace = T)
    if(class(x)=='numeric'){
      if(is.null(y)){
        ret = f(x[s])
      }else{
        ret = f(x[s],y[s])
      }
    }else{
      if(is.null(y)){
        ret = f(x[s,])
      }else{
        ret = f(x[s,],y[s,]) 
      }
      
    }
    store[i]= ret
    
  }
  store 
}


#getDescriptiveScore =function(meanModelDHVs,idx){
#  dhvs = meanModelDHVs[idx]
getDescriptiveScore =function(dhvs){
  desc= mean(dhvs) * (1- sqrt(var(dhvs)))
  desc
}

#getPredictiveScore=function(predictionError,idx,fittingError){
getPredictiveScore=function(pred,fit){ 
  # pred = predictionError[idx,]
  #  fit = fittingError[idx,]
  if(any(is.na(pred)) ){
    print('pred contains NA')
  }
  
  if(any(is.na(fit)) ){
    print('fit contains NA')
  }
  
  
  abspred = (1-mean(sqrt(pred) )) * (1-sqrt(var(sqrt(as.numeric(pred)) )))
  corScores =c()
  
  if(class(pred)=='numeric'){
    corScores = fit
  }else{
    for(z in 1:ncol(pred)){
      corScores= c(corScores,cor(fit[,z],pred[,z]))
    }
  }
  #print(corScores)
  relativePredScore = max(mean(corScores),0)*(1-sqrt(var(na.omit(corScores)) )) 
  
  (relativePredScore +abspred)/2
}

estimateMeanModelPerformanceScores = function(meanPredList){
  
  meanModelDHVs = c()
  for(i in 1:nrow(meanPredList$fittingError)){
    meanModelDHVs=c(meanModelDHVs, dominatedHypervolume(matrix(meanPredList$fittingError[i,],ncol=4) ,c(1,1,1,1)) )
  }
  
  desc= mean(meanModelDHVs) * (1- sqrt(var(meanModelDHVs)))
  
  abspred = (1-mean(sqrt( meanPredList$predictionError) )) * (1-sqrt(var(as.numeric(meanPredList$predictionError))))
  corScores =c()
  for(z in 1:ncol(meanPredList$predictionError)){
    corScores= c(corScores,cor(meanPredList$fittingError[,z],meanPredList$predictionError[,z]))
  }
  relativePredScore = max(mean(corScores),0)*(1-sqrt(var(corScores)))
  
  descSample = myboot(meanModelDHVs,y=NULL,getDescriptiveScore,10000) 
  
  mysample = myboot(meanPredList$predictionError,meanPredList$fittingError,getPredictiveScore,10000);
  
  desc_low = quantile(descSample,c(0.025))#
  desc_high = quantile(descSample,c(0.975))#
  
  # print(mysample)
  pred_low= quantile(mysample,c(0.025))
  pred_high = quantile(mysample,c(0.975))
  
  list('decriptive'=mean(descSample),'decriptiveVar'=var(descSample),
       'decriptiveVar_low'= desc_low,'decriptiveVar_high'= desc_high,
       'absPredictive'=abspred,'absPredictiveVar'=var(as.numeric(meanPredList$predictionError)),
       'relPredictive'=mean(corScores),'relPredictiveVar'=var(corScores),
       'relPredictive_low'= pred_low,#
       'relPredictive_high'= pred_high,#
       'relPredictive_mean_boot'= mean(mysample)
  )
}
