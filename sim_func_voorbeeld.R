  
library(bain)

  f.SeqBayes=function(HypSet,ES,BFtarget,fraction,testtype,Nmin) # function to perform the simulation. Between brackets are the arguments of the function
  {
  nr.trials=5000  ### number of replications (= number of trials. The more the better but the more computer time)
  
  ### create unique code (batch) for each combination of factor levels. For each of the factors in the simulation study a selected number of values can be chosen (see lines 8--13 for values)
  HypSet.levels=c(1,2,3) # see lines 59-73 for hypothesis sets
  ES.levels=c(0,0.2,0.5,0.8)
  BFtarget.levels=c(3,5,10,20)
  fraction.levels=c(1,2,3)
  testtype.levels=c(1,2)
  Nmin.levels=c(5,10,20)
  batch=100000*which(HypSet.levels==HypSet)+10000*which(ES.levels==ES)+1000*which(BFtarget.levels==BFtarget)+100*which(fraction.levels==fraction)+10*which(testtype.levels==testtype)+1*which(Nmin.levels==Nmin)
  
  set.seed(batch)
  
  ### create vectors to store output
  ### Inf is short for infinity. Here the sample size can increase without bounds
  BF.Inf.out=BF.200.out=BF.100.out=BF.50.out=rep(NA,nr.trials)
  N.Inf.out=N.200.out=N.100.out=N.50.out=rep(NA,nr.trials)
  
  start=date() # can be used to track how long simulation takes
  for(ii in 1:nr.trials)
    {
    print(ii) # print the replicaiton to the screen
    if(testtype==1)
      {
      pop.x=rnorm(100000,0,1) # vector with observations from the x population
      pop.y=rnorm(100000,ES,1) # vector with observations from the y population
    }
    if(testtype==2)
    {
      pop.x=rnorm(100000,0,sqrt(4/3))
      pop.y=rnorm(100000,ES,sqrt(2/3))
    }

    ### make certain to enter the while loop by using BF=1 (even though this may not be the true value)
	  N.x=Nmin-1
	  BF=1
	    
    ### increase sample size if needed. The step size with which you increase the sample size depends on the sample size thus far
    while(BF<BFtarget & BF>1/BFtarget&N.x<50000 )
      {
      if(N.x<50000) N.step=50
      if(N.x<5000) N.step=20
      if(N.x<2500) N.step=10
      if(N.x<1000) N.step=5
      if(N.x<100) N.step=1
      N.x=N.x+N.step

      x=pop.x[1:N.x]	
      y=pop.y[1:N.x]	

      if(testtype==1)
      ttest=t_test(x,y,paired=FALSE,var.equal=TRUE)
      if(testtype==2)
      ttest=t_test(x,y,paired=FALSE,var.equal=FALSE)
      
      if(HypSet==1)
      {
        results=bain(ttest,"x=y;x<y",fraction=fraction)
        BF=results$BFmatrix[2,1] # Bayes Factor
        # R does not print what is happening within the function to the screen by default
        # for instance, if you want to have the results of bain printed then use the following line (otherwise remove that line or add a # at the beginning)
        print(results)
      }
      if(HypSet==2)
      {
        results=bain(ttest,"x=y",fraction=fraction)
        BF=1/results$fit[1,7] # Bayes Factor
      }
      if(HypSet==3)
      {
        results=bain(ttest,"x>y;x<y",fraction=fraction)
        BF=results$BFmatrix[2,1] # Bayes Factor
      }

      if(N.x<=50)
      {
        BF.50.out[ii]=BF
        N.50.out[ii]=N.x
        }
      
      if(N.x<=100)
      {
        BF.100.out[ii]=BF
        N.100.out[ii]=N.x
        }
      
      if(N.x<=200)
      {
        BF.200.out[ii]=BF
        N.200.out[ii]=N.x
        }

      }

	  BF.Inf.out[ii]=BF
	  N.Inf.out[ii]=N.x

	    }
  stop=date() # can be used to track how long simulation takes
  output=list(nr.trials=nr.trials,HypSet=HypSet,ES=ES,BFtarget=BFtarget,fraction=fraction,testtype=testtype,Nmin=Nmin,batch=batch,
              BF.Inf.out=BF.Inf.out,
              BF.200.out=BF.200.out,
              BF.100.out=BF.100.out,
              BF.50.out=BF.50.out,
              
              N.Inf.out=N.Inf.out,
              N.200.out=N.200.out,
              N.100.out=N.100.out,
              N.50.out=N.50.out,
              
              start=start,stop=stop)
  return(output)
  }
  
  output=f.SeqBayes(HypSet=1,ES=0.2,BFtarget=5,fraction=1,testtype=1,Nmin=20) # call to function with a specific set of values of the factors in the simulation study
  output # print output to screen
  
  # the last line of code print an extensive list to the screen. You may also want to print certain elements of that list, such as
  output$BF.50.out # the Bayes factor in the case of at most 50 subjects per group
  # check duration of simulation using the next two lines:
  output$start
  output$stop
  

