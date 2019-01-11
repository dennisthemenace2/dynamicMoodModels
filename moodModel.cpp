
#include <iostream>

#include <string>
#include <iostream>
#include <algorithm>
#include <functional>


#include <stdio.h>
#include <pthread.h>



//#include <Rcpp.h>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

using namespace Rcpp;

Rcpp::DataFrame  simulateSM(NumericVector params,unsigned int length);


// [[Rcpp::export]]
Rcpp::List errorFnCpp(NumericVector parameters,Rcpp::DataFrame data,Rcpp::DataFrame test){
  
 // std::cout<< "start"<<std::endl;
  
  
  DataFrame sim = simulateSM(parameters,data.nrow()+ test.nrow());
  
  
  //NumericVector resid = as<NumericVector>(mod["residuals"]);
  CharacterVector datanames = data.names();
 // CharacterVector simnames = sim.names() ;
 // std::cout<< datanames<<simnames<<std::endl;
    
  NumericVector meanErrors(datanames.length()) ;
  NumericVector testErrors(datanames.length()) ;
  
  for(unsigned i =0;i<datanames.length();++i){
    double error =0.0;
  //  std::cout<< "get rows"<<std::endl;
    NumericVector datarow = as<NumericVector>(data[Rcpp::as<std::string>( datanames[i] )]);
    NumericVector simdata = as<NumericVector>(sim[Rcpp::as<std::string>( datanames[i] )]);
    int count = 0;
    for(unsigned int k=0;k<data.nrow();++k ){
      if(!NumericVector::is_na(datarow[k])){
        double dif = (simdata[k]-datarow[k]);
        error +=dif*dif;  
        ++count;
      }

    }
 //   std::cout<< "error:"<<error<<" count "<<count<<std::endl;
    error = error /count;
    meanErrors[i]=error;
    //calculate test error
    count = 0;
    error = 0.0;
    NumericVector testrow = as<NumericVector>(test[Rcpp::as<std::string>( datanames[i] )]);
    unsigned testcnt = 0;
    for(unsigned int k=data.nrow();k<data.nrow()+test.nrow();++k ){
      if(!NumericVector::is_na(testrow[testcnt])){
        
        double dif = (simdata[k]-testrow[testcnt]);
        error +=dif*dif; 
        ++count;
      }
      ++testcnt;
    }
    error = error /count;
    testErrors[i]=error;
  }
 // std::cout<< meanErrors<<std::endl;
  //"moodLevel"            "enjoyedActivites"     "socialInteraction"    "activitiesCarriedOut"
  //CharacterVector b = df["b"];
  return Rcpp::List::create(Rcpp::Named("error") = meanErrors,
                                 Rcpp::Named("test") = testErrors
  );
}

double alogistic(double Vsum ,double sigma, double tau){
  
  double result =  (1./(1.+exp(-sigma*(Vsum -tau))) - 1./(1.+exp(tau*sigma)) ) *(1.+exp(-tau*sigma));
  return result;
  
}


// [[Rcpp::export]]
Rcpp::DataFrame  simulateSM(NumericVector params,unsigned int length) {
  int n = params.size(); // should be 32
  
  if (n<32){
    if(n==17){
      NumericVector newparams(32);
      for(unsigned int i=0;i<n;++i){
        newparams[i]=1.0;
        //.self$parameters = rep(1,32) ## set others to 1
      }
      for(unsigned int i=0;i<6;++i){
        newparams[i] = params[i];
      }
      unsigned cnt =6;
      for(unsigned int i=21;i<32;++i){
        newparams[i] = params[cnt++];
      }
      
      newparams[12]=0;
      newparams[14]=0;
      newparams[16]=0;
      newparams[18]=0;
      newparams[20]=0;
      params = newparams;
      
    //  .self$parameters[1:6]= parameters[1:6]
    //  .self$parameters[22:33]= parameters[7:18]
    //  .self$parameters[c(13 ,15, 17 ,19 ,21)] =0;#set tau to 0
      n = params.size();
      if(n!=32){
        stop("setting up parameters didnt work"); 
      }
    
    }else{
      stop("Need at laest 32 or 17 parameters"); 
    }
  }
  
  //create space for saved states
  NumericVector time(length); //save time steps
  NumericVector socialInteraction(length); //save time steps
  NumericVector networkStrength(length); //save time steps
  NumericVector moodLevel(length); //save time steps
  NumericVector socialIntegration(length); //save time steps
  NumericVector activitiesCarriedOut(length); //save time steps
  NumericVector enjoyedActivites(length); //save time steps
  
//time socialInteraction networkStrength moodLevel socialIntegration activitiesCarriedOut enjoyedActivites
  socialInteraction[0]=params[0];
  enjoyedActivites[0]=params[1];
  activitiesCarriedOut[0]=params[2];
  socialIntegration[0]=params[3];
  moodLevel[0]=params[4];
  networkStrength[0]=params[5];
  time[0]=0.0;



//first 6 should be state
for(unsigned t =1;t<length;++t){
  double impact_socialInteraction=params[21]*moodLevel[t-1]+params[22]*networkStrength[t-1]+params[23]*activitiesCarriedOut[t-1];
  
  //  params$w_socialInteraction_mood*moodLevel,params$w_socialInteraction_network*networkStrength,params$w_socialInteraction_activitiesCarriedOut*activitiesCarriedOut 
  
  double dsocialInteraction =params[6]*(alogistic(impact_socialInteraction,params[13],params[14])-socialInteraction[t-1]);
  
//params$eta_socialInteraction * ( .self$alogistic(impact_socialInteraction, params$sigma_socialInteraction ,params$tau_socialInteraction )  - socialInteraction)
  
  double impact_mood = params[24]*socialIntegration[t-1]+params[25]*socialInteraction[t-1]+params[26]*enjoyedActivites[t-1];
    //c(params$w_mood_socialIntegration*socialIntegration,params$w_mood_socialInteraction * socialInteraction,params$w_mood_enjoyedActivites*enjoyedActivites)
   double dmoodLevel = params[7]*(alogistic(impact_mood,params[15],params[16]) - moodLevel[t-1]);
      //params$eta_mood *( .self$alogistic(impact_mood, params$sigma_mood ,params$tau_mood)  - moodLevel)
    
    double impact_socialIntegration = params[27]*networkStrength[t-1]+params[28]*activitiesCarriedOut[t-1];
    //  c(params$w_socialIntegration_mood*moodLevel,params$w_socialIntegration_networkStrength * networkStrength,params$w_socialIntegration_activitiesCarriedOut*activitiesCarriedOut)
    double dsocialIntegration =params[8]*(alogistic(impact_socialIntegration,params[19],params[20])-socialIntegration[t-1] );
      //params$eta_socialIntegration *( .self$alogistic(impact_socialIntegration, params$sigma_socialIntegration ,params$tau_socialIntegration)  - socialIntegration)
    
    double impact_activitiesCarriedOut = params[29]*socialIntegration[t-1];
 //c(params$w_activitiesCarriedOut_socialIntegration*socialIntegration)
    double dactivitiesCarriedOut = params[9]*(alogistic(impact_activitiesCarriedOut, params[17], params[18])-activitiesCarriedOut[t-1] );
      //params$eta_activitiesCarriedOut *( .self$alogistic(impact_activitiesCarriedOut, params$sigma_activitiesCarriedOut ,params$tau_activitiesCarriedOut)  - activitiesCarriedOut)
    
    double impact_enjoyedActivites =params[30]*moodLevel[t-1] +params[31]*activitiesCarriedOut[t-1] ;
      //c(params$w_enjoyedActivites_mood*moodLevel,params$w_enjoyedActivites_activitiesCarriedOut*activitiesCarriedOut)
    double denjoyedActivites = params[10]*(alogistic(impact_enjoyedActivites,params[11],params[12])-enjoyedActivites[t-1]);
      //params$eta_enjoyedActivites *( .self$alogistic(impact_enjoyedActivites, params$sigma_enjoyedActivites ,params$tau_enjoyedActivites)  - enjoyedActivites)
    
  //double  dnetworkStrength = 0;
  
  time[t]=t*0.1;
  socialInteraction[t]=socialInteraction[t-1]+dsocialInteraction*0.1;
  enjoyedActivites[t]=enjoyedActivites[t-1]+denjoyedActivites*0.1;
  activitiesCarriedOut[t]=activitiesCarriedOut[t-1]+dactivitiesCarriedOut*0.1;
  socialIntegration[t]=socialIntegration[t-1]+dsocialIntegration*0.1;
  moodLevel[t]=moodLevel[t-1]+dmoodLevel*0.1;
  networkStrength[t]=networkStrength[t-1]; // dont change this
  //list(c(dsocialInteraction,dnetworkStrength , dmoodLevel,dsocialIntegration,dactivitiesCarriedOut,denjoyedActivites))

}


//return
return Rcpp::DataFrame::create(Rcpp::Named("time") = time,
                          Rcpp::Named("socialInteraction") = socialInteraction,
                          Rcpp::Named("networkStrength") = networkStrength,
                          Rcpp::Named("moodLevel") = moodLevel,
                          Rcpp::Named("socialIntegration") =socialIntegration,
                          Rcpp::Named("activitiesCarriedOut") =activitiesCarriedOut,
                          Rcpp::Named("enjoyedActivites") =enjoyedActivites
);

}


// [[Rcpp::export]]
Rcpp::DataFrame  simulateDCM(arma::vec  X,arma::mat  A,NumericVector time,NumericVector sigmas,NumericVector taus,unsigned int length) {
  
  NumericMatrix ret(length,X.size());
// colnames(ret) = A. .attr("dimnames"); 
//ret.names() = A.names();
  //std::cout<<"A:"<<A<<std::endl;
  NumericVector newState(X.size());
  for(unsigned t =0;t<length;++t){
    arma::vec StateTransition= A * X;
  //  NumericVector StateTransition =as<NumericVector> atmp;
    for(unsigned k=0;k<StateTransition.size();++k){
      newState[k] =(alogistic(StateTransition[k],sigmas[k],taus[k])-X[k])*time[k]; //subtract state too
    }
    X+=newState;
    for(unsigned i=0;i<X.size();++i){
      ret(t,i) = X[i];
    }
  }
    
  return(ret);

}


// make sure they are set up correctly
// [[Rcpp::export]]
Rcpp::List errorDcmFnCpp(NumericVector X,NumericMatrix A,NumericVector time,NumericVector sigmas,NumericVector taus,Rcpp::DataFrame data,Rcpp::DataFrame test){
  
  DataFrame sim = simulateDCM(  as<arma::vec>(wrap(X)),  as<arma::mat>(wrap(A)), time,sigmas, taus, data.nrow()+ test.nrow());

 
// colnames(sim) =  colnames(A);
  //NumericVector resid = as<NumericVector>(mod["residuals"]);
//CharacterVector datanames = data.names();


  NumericVector meanErrors(data.ncol()) ;
  NumericVector testErrors(data.ncol()) ;
  
  for(unsigned i =0;i<data.ncol();++i){
    double error =0.0;
    //  std::cout<< "get rows"<<std::endl;
    NumericVector datarow = as<NumericVector>(data[i]);
    NumericVector simdata = as<NumericVector>(sim[i]);
   // std::cout<<simdata<<std::endl;
    int count = 0;
    for(unsigned int k=0;k<data.nrow();++k ){
      if(!NumericVector::is_na(datarow[k])){
        double dif = (simdata[k]-datarow[k]);
        error +=dif*dif;  
        ++count;
      }
      
    }
    //   std::cout<< "error:"<<error<<" count "<<count<<std::endl;
    error = error /count;
    meanErrors[i]=error;
    //calculate test error
    count = 0;
    error = 0.0;
    NumericVector testrow = as<NumericVector>(test[i]);
    unsigned testcnt = 0;
    for(unsigned int k=data.nrow();k<data.nrow()+test.nrow();++k ){
      if(!NumericVector::is_na(testrow[testcnt])){
        double dif = (simdata[k]-testrow[testcnt]);
        error +=dif*dif; 
        ++count;
      }
      ++testcnt;
    }
    error = error /count;
    testErrors[i]=error;
  }
  // std::cout<< meanErrors<<std::endl;
  //"moodLevel"            "enjoyedActivites"     "socialInteraction"    "activitiesCarriedOut"
  //CharacterVector b = df["b"];
  return Rcpp::List::create(Rcpp::Named("error") = meanErrors,
                            Rcpp::Named("test") = testErrors
  );
  
}
 // arma::vec  X,arma::mat  A,NumericVector time,NumericVector sigmas,NumericVector taus,unsigned int length
 // [[Rcpp::export]]
Rcpp::List errorFnCppDCMWrap(NumericVector parameters,NumericMatrix conncetions,Rcpp::DataFrame data,Rcpp::DataFrame test){
   
   NumericMatrix A( Rcpp::clone(conncetions) );
   unsigned cnt = 0;
   for(unsigned y=0;y<A.nrow();++y){
     for(unsigned x=0;x<A.ncol();++x){
       if(A(y,x)==1){
         A(y,x)= parameters[cnt++];
         //cnt +=1;
       }
     }
   }
   NumericVector sigmas(A.ncol());
   
   NumericVector consums( conncetions.ncol() );
   consums =  rowSums(conncetions); 
   
 // std::cout<<"consums"<<consums<<std::endl;
   
   for(unsigned x=0;x<sigmas.length();++x){
     if(consums[x]>0){
        sigmas[x] = parameters[cnt++];
     }
   }
   
   NumericVector taus(A.ncol());
   for(unsigned x=0;x<taus.length();++x){
     if(consums[x]>0){
       taus[x] = parameters[cnt++];
     }
   }
   
   NumericVector time(A.ncol());
   for(unsigned x=0;x<time.length();++x){
     if(consums[x]>0){
        time[x] = parameters[cnt++];
     }
   }
  // std::cout<<time<<std::endl;
   
   
   NumericVector X(A.ncol());
   for(unsigned x=0;x<X.length();++x){
     X[x] = parameters[cnt++];
  //   std::cout<< "setx"<<x<<std::endl;
   }

   if(cnt>parameters.length()){
     std::cout<< "accessing parameters that are not there"<<std::endl;
   }else if(cnt<parameters.length()){
     std::cout<< "too many parameters:"<<cnt <<"< "<<parameters.length() <<std::endl;   
     std::cout<<"X"<<X<<std::endl;
   }/*else{
    / std::cout<< "thats good"<<std::endl;
   }*/
    
   return(errorDcmFnCpp(X,A,time,sigmas, taus,data,test));
   
 }
  