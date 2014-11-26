#include <TMB.hpp>
//#include <fenv.h> // Extra line needed 
 
template<class Type> 
Type objective_function<Type>::operator() () { 

  //  feenableexcept(FE_INVALID | FE_OVERFLOW | FE_DIVBYZERO | FE_UNDERFLOW);	 
	 
  DATA_ARRAY(SI);            //survey indices from the spring survey 
  DATA_ARRAY(ldistSI);       //length distributions from the survey 
  DATA_ARRAY(ldistComm);     //length distributions from the commercial fleet 
  DATA_ARRAY(aldistSI);      //age-length distributions from the survey 
  DATA_ARRAY(aldistComm);    //age-length distributions from the commerical fleet 
  DATA_ARRAY(fleetCatches); //catches 
 
  DATA_VECTOR(SIlgroups);     //length group intervals for the survey idx  
 
  DATA_INTEGER(firstyear);    //first year of the simulation 
  DATA_INTEGER(lastyear);     //last year of the simulation 
  DATA_INTEGER(maxlgr);       //maximum length group growth 
  DATA_INTEGER(minage);       //minimum age of the stock 
  DATA_INTEGER(maxage);       //maximum age of the stock 
  DATA_INTEGER(minlength);    //maximum length of the stock 
  DATA_INTEGER(maxlength);    //maximum length of the stock 
  DATA_VECTOR(M);             //natural mortality 
  DATA_VECTOR(initSigma);     //variation in initial length at age  
  DATA_VECTOR(wa);            //weight length relationship 
  DATA_VECTOR(compW);         //likelihood component weights 
  DATA_INTEGER(dl);           //length group size
 
  PARAMETER_VECTOR(recruits); //number of minage year olds 
  PARAMETER(recl);               //average length at recruitment 
  PARAMETER(recsd);              //std.dev length at recruitment 
  PARAMETER_VECTOR(initial);     //initial number at age at year 0 
  PARAMETER(aComm);              //commerical suitability params 
  PARAMETER(bComm); 
  PARAMETER(aSurv);              //survey suitability params 
  PARAMETER(bSurv); 
  PARAMETER(k);                  //growth k 
  PARAMETER(linf);               //growth linf 
  PARAMETER(beta);               //growth dispersion parameter 
  PARAMETER_VECTOR(SIa);         //alpha in log(Idx) = a + log(N) 
  PARAMETER(meanrec);            //average num recruits
  PARAMETER(log_sigma);            //var num recruits
   
  Type lik_idx, lik_sldist, lik_saldist, 
    lik_cldist, lik_caldist,lik_rec;     // likelihood component scores 
  double numyears = lastyear - firstyear + 1; 
 
  vector<Type> len(maxlength); 
  vector<Type> wl(maxlength); 
   
  // follow the stock status through abundance by length and age at time 
  array<Type> stkArr(maxage,maxlength,numyears,4); 
  // catches by length and age at time 
  array<Type> survArr(maxage,maxlength,numyears,4);   
  array<Type> commArr(maxage,maxlength,numyears,4);   
  array<Type> survArrALP(maxage,maxlength,numyears,4);   
  array<Type> commArrALP(maxage,maxlength,numyears,4);   
  array<Type> survArrLP(maxlength,numyears,4);   
  array<Type> commArrLP(maxlength,numyears,4);   
  array<Type> predidx(SIlgroups.size(),numyears);  
  array<Type> tmpGrowth(maxage,maxlength);
  // Growth 
  matrix<Type> G(maxlength,maxlength); 
 
  
  // selection 
  vector<Type> suitSurv(maxlength);   
  vector<Type> suitComm(maxlength); 
  vector<Type> dmu(maxlength); 
  vector<Type> alpha(maxlength); 

  // mean length at age
  vector<Type> mu(maxage); 
  Type kk = exp(k);
  Type bbeta = exp(beta);
  Type scalar = Type(1e9);
  
  for(int l = 0; l < maxlength;l++){ 
    len(l) = Type(l+1)*dl; 
    wl(l) = exp(log(wa(0))+log(len(l))*wa(1)); 
  } 
  //  len(0) = 0;
  wl(0) = 0;
  

  for(int l = 0; l < maxlength;l++){ 
      suitSurv(l) = 1/(1+exp(-(aSurv+bSurv*l))); 
      suitComm(l) = 1/(1+exp(-(aComm+bComm*l))); 
  } 
   
 
  // Calculate average growth by lgroup 
  for(int i=1; i < maxlength; i++){ 
    dmu(i-1) = linf*(1-len(i)/linf)*(1-exp(-kk/4)); 
    /*   if(dmu(i-1)/(len(i) - len(i-1)) > maxlgr){ 
      dmu(i-1) = maxlgr-0.1; 
    } 
    if(dmu(i-1) < 0){ 
      dmu(i-1) = 0; 
      }*/ 
    alpha(i-1) = (bbeta*dmu(i-1)/(len(i) - len(i-1)))/(maxlgr-dmu(i-1)/(len(i) - len(i-1))); 
  } 
  // biggest lengtgroup doesn't grow 
  dmu(maxlength-1) = 0; 
  alpha(maxlength-1) = 0; 
   
  // growthprob i -> j 
  for(int i=0; i<maxlength-1; i++){ 
    for(int x=0; x < std::min(maxlgr+1,maxlength-i); x++){ 
      G(i,i+x) = exp(lgamma(maxlgr + Type(1.0))+ 
		     lgamma(alpha(i) + bbeta) + 
		     lgamma(maxlgr - Type(x) + bbeta) + 
		     lgamma(Type(x) + alpha(i)) - 
		     lgamma(maxlgr - Type(x) + Type(1.0)) - 
		     lgamma(Type(x) + Type(1.0)) - 
		     lgamma(maxlgr + alpha(i) + bbeta) - 
		     lgamma(bbeta) - 
		     lgamma(alpha(i))); 
    } 
  } 
  G(maxlength-1,maxlength-1) = 1; 
   
 
  // initial length at age 

  for(int a=0; a < maxage; a++){ 
    mu(a) = linf*(1-exp(-kk*(a))); 
  } 
   
  //looping variables 
  int year, step, l, a;  
 
  Type survBio, harvBio, survNum, harvNum; 
  

  for(year=0; year < numyears; year++){ 
    for(step=0; step < 4; step++){ 
      
      if(step == 0){ // recruitment 
	for(l = minlength; l < maxlength-1; l++){ 	  
	  stkArr(minage-1,l,year,step) = 
	    scalar*recruits(year)*(pnorm(len(l+1),recl,recsd) - 
				   pnorm(len(l),recl,recsd));	
	} 
	stkArr(minage-1,minlength-1,year,step) =  
	  scalar*recruits(year)*pnorm(len(minlength-1),recl,recsd); 
	stkArr(minage,maxlength-1,year,step) =  
	  scalar*recruits(year)*(1-pnorm(len(maxlength-1),recl,recsd)); 
      } 
      
      
      if(year == 0 && step == 0){ // initial conditions 
	for(a = minage; a < maxage; a++){ 
	  for(l = minlength; l < maxlength-1; l++){	   
	    stkArr(a,l,year,step) =  
	      scalar*initial(a-1)*(pnorm(len(l+1),mu(a),initSigma(a)) - 
				   pnorm(len(l),mu(a),initSigma(a))); 
	  }  
	  // fix the edges 
	  stkArr(a,minlength-1,year,step) =  
	    scalar*initial(a-1)*pnorm(len(minlength-1),mu(a),initSigma(a)); 
	  stkArr(a,maxlength-1,year,step) =  
	    scalar*initial(a-1)*(1-pnorm(len(maxlength-1),mu(a),initSigma(a))); 
	} 
      } else if(step == 0) { // update age 

	for(a = minage; a < maxage; a++){ 
	  for(l = minlength-1; l < maxlength; l++){ 
	    stkArr(a,l,year,0) = stkArr(a-1,l,year-1,3); 
	  } 
	} 
	// plus group 
	for(l = minlength-1; l < maxlength; l++){ 
	  stkArr(maxage-1,l,year,0) += stkArr(maxage-1,l,year-1,3); 
	} 
      } else { // move between timestep 
	for(a = minage-1; a < maxage; a++){ 
	  for(l = minlength-1; l < maxlength; l++){ 
	    stkArr(a,l,year,step) = stkArr(a,l,year,step-1); 
	  } 
	} 
      } 

      
      // calculate catches 
      harvBio = 0; 
      survBio = 0; 
      survNum = 0;
      harvNum = 0;
      for(a = minage-1; a<maxage;a++){ 
	for(l = minlength-1; l<maxlength;l++){
	  harvBio += suitComm(l)*wl(l)*stkArr(a,l,year,step);
	  survBio += suitSurv(l)*wl(l)*stkArr(a,l,year,step); 
	  survNum += suitSurv(l)*stkArr(a,l,year,step);
	  harvNum += 0.25*0.2*suitComm(l)*stkArr(a,l,year,step);
	} 
      } 

      // fleet operations 
      for(a = minage-1; a<maxage;a++){ 
	for(l = minlength-1; l<maxlength;l++){ 
	  if(step==1){ 
	    // assuming catch of 1 kg
	    survArr(a,l,year,step) = stkArr(a,l,year,step) *  
	      suitSurv(l)/(survNum); 
	    // reporting 
	    survArrALP(a,l,year,step) = survArr(a,l,year,step); 
	    survArrLP(l,year,step) += survArr(a,l,year,step); 
	  } 
	  
	  //	  commArr(a,l,year,step) = std::min(fleetCatches(year,step),0.95*harvBio)*
	  //	    stkArr(a,l,year,step) * suitComm(l)/(harvBio); 
	  commArr(a,l,year,step) = Type(0.25*0.2)*suitComm(l)*stkArr(a,l,year,step);
	  stkArr(a,l,year,step) -= commArr(a,l,year,step); 
	  //reporting 
	  commArrALP(a,l,year,step) = commArr(a,l,year,step)/(harvNum); 
	  commArrLP(l,year,step) += commArr(a,l,year,step)/(harvNum); 
	} 
      } 

      for(a = minage - 1; a<maxage; a++){ 
	for(l = minlength-1; l<maxlength;l++){ 
	  tmpGrowth(a,l) = 0;
	}
      }

      // growth  
      for(a = minage - 1; a<maxage; a++){ 
	for(l = minlength-1; l<maxlength;l++){ 
	  for(int x = 0; x < std::min(maxlgr+1,maxlength-l); x++){ 
	    /// check this
	    tmpGrowth(a,l+x) += G(l,l+x)*stkArr(a,l,year,step); 
	  } 
	} 
      } 

      for(a = minage - 1; a<maxage; a++){ 
	for(l = minlength-1; l<maxlength;l++){ 
	  stkArr(a,l,year,step) = tmpGrowth(a,l);
	}
      }
       
      for(a = minage - 1; a<maxage; a++){ 
	for(l = minlength-1; l<maxlength;l++){ 
	  stkArr(a,l,year,step) = exp(-M(a)*0.25)*stkArr(a,l,year,step); 
	} 
      } 
    } 
    // survey index 
    l = 0;
    for(int lg=0; lg < SIlgroups.size(); lg++){  
      predidx(lg,year) = 0; 
      while(l < std::min(SIlgroups(lg), Type(maxlength))){ 
	for(a=minage-1;a<maxage;a++){ 
	  predidx(lg,year) += stkArr(a,l,year,1); 
	} 
	l++; 
      } 
    } 

  }
 

  // likelihood functions 
  lik_idx = 0; 
  lik_sldist = 0; 
  lik_saldist = 0; 
  lik_cldist = 0; 
  lik_caldist = 0; 
  lik_rec = 0;

   
  for(year = 0; year < numyears; year++){ 
    // deal with the survey first 
    lik_rec -= dnorm(recruits(year),meanrec,exp(log_sigma),true);
    for(int lg=0; lg < SIlgroups.size(); lg++){ 

      lik_idx -= dnorm(log(SI(lg,year)),
		       (SIa(lg) + log(predidx(lg,year)+Type(0.00001))),Type(1.0),true);
		       
    }     
    for(l=minlength; l<maxlength; l++){ 
      // assumes that missing full observation at year/step combination is  
      // indicated by -1 
      if(ldistSI(l-minlength,year) != -1){ 
	lik_sldist -= dnorm(ldistSI(l-minlength,year),
			    survArrLP(l,year,1),Type(1.0),true);  
      } 
      
      for(a=minage-1; a < maxage; a++){ 
	if(aldistSI(a,l-minlength,year) != -1){ 
	  lik_saldist -= dnorm(aldistSI(a,l-minlength,year),
			       survArrALP(a,l,year,1),Type(1.0),true);  
	} 
      } 
       
    } 
    // now commercial 
    for(step = 0; step < 4; step++){ 
      for(l=minlength; l<maxlength; l++){ 
	// assumes that missing full observation at year/step combination is  
	// indicated by -1 
	if(ldistComm(l-minlength,year,step) != -1){ 
	  lik_cldist -= dnorm(ldistComm(l-minlength,year,step),
			      commArrLP(l,year,step), 
			      Type(1.0),true);  
	} 
	
	for(a=minage-1; a < maxage; a++){ 
	  if(aldistComm(a,l-minlength,year,step) != -1){ 
	    lik_caldist -= dnorm(aldistComm(a,l-minlength,year,step),
				 commArrALP(a,l,year,step), 
				 Type(1.0),true);  
	  } 
	}
	 
      } 
    } 
  } 
  


  Type nll = (compW(0)*lik_idx + compW(1)*lik_sldist + compW(2)*lik_saldist + 
	       compW(3)*lik_cldist + compW(4)*lik_caldist + lik_rec); 
  
  REPORT(G);
  REPORT(stkArr);
  REPORT(commArr);
  REPORT(predidx);
  return nll;
} 
