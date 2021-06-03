/*This is a change from the ModelSpec.so by using faster random number generators*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>


/*
 z - starting point
 aL - lower bound
 aU - upper bound
 v  - drift rate
 response - response from the function
 rt - reaction time of the animal
 n - number of trials
 maxtimeStep - number of time steps
 dt = stepsize (for time usually 0.001 s, or 1 ms depending on model)
 s = standard deviation of noise usually fixed.
 */

int returnRandomNumber(int rangeLow, int rangeHigh)
{
  int index;
  // GetRNGstate();
  double randNum;
  randNum = rand()/(1.0 + RAND_MAX);
  index = (rangeHigh) - (rangeLow) + 1;
  index = (randNum * index) + (rangeLow);
  
  // PutRNGstate();
  return index;
}



// Classical model from DDM et al. 1960
int DDMs(double *z, double *v,double *aU, double *aL, double *s,double *dt,double *response,double *rt,
         double *n,double *maxTimeStep, int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x;
  int N,i,timeStep,MaxTimeStep;
  double *randValue;
  

  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) 
  {
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep = timeStep+1;
      
       x = x+(*dt)*(*v)+rhs*norm_rand();
      //x = x+(*dt)*(*v)+rhs*gsl_ran_gaussian_ziggurat(r, 1.0);
      if (x>=*aU) {
        response[i]=(double) 1.0; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
      
    } while (timeStep < MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}


// Classical model from DDM et al. 1960
int DDM(double *z, double *v,double *aU, double *aL, double *s,double *dt,double *response,
        double *rt,double *n,double *maxTimeStep, int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x;
  int N,i,timeStep,MaxTimeStep;
  int rangeL, rangeH;
  
  double randNum;
  int index;

  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) 
  {
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep = timeStep+1;
      
      randNum = rand()/(1.0 + RAND_MAX);
      index = (rangeH) - (rangeL) + 1;
      index = (randNum * index) + (rangeL);

      
      x = x+(*dt)*(*v)+rhs*randomTable[index];
      if (x>=*aU) {
        response[i]=(double) 1.0; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
      
    } while (timeStep < MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

/*
 z - starting point
 aL - lower bound
 aU - upper bound
 timecons - timeconstant to be used for low pass filtering
 of the urgency signals
 v  - drift rate
 resp - response from the function
 rt - reaction time of the animal
 n - number of trials
 maxTimeStep - number of time steps
 */

int UGM(double *z, double *v,double *aU, double *aL, double *timecons, double *usign, double *s,
           double *dt,double *response,double *rt,double *n,double *maxTimeStep,int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  /* Convert some double inputs to integer types. */
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  
  GetRNGstate();
  
  rhs=sqrt(*dt)*(*s);
  
  alpha=(*timecons)/((*timecons)+(*dt));
  
  for (i=0;i<N;i++) 
  {
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // filtered signal from previous step + input from current step
      //x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      
      
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * timeStep*(*dt)*(*usign);  // urgency is multiplicative
      
      // Done correctly with the dt multiplier!
      
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  
  PutRNGstate();
}



int DDMSv(double *z, double *v, double *eta, double *aU, double *aL, double *s,
           double *dt,double *response,double *rt,double *n,double *maxTimeStep,int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  int N,i,timeStep,MaxTimeStep;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) 
  {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

// step size here is 0.001 ms
int dDDM(double *z, double *v, double *sx, double *sy, double *delay, double *aU, double *aL,
           double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,timeStep,MaxTimeStep, currTime;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      currTime = timeStep*(*dt);
      term1 = exp((*sx)*(currTime-*delay));
      term2 = exp(-(*sx)*(*delay));
      
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      
      gamma = ((*sy)*term1)/(1+term1) + (1 + (1-*sy)*term2)/(1+term2);
      
      // This allows the specification of an increase in the accumulated evidence over time.
      // There is some lack of clarity in whether the urgency signal multiplies 
      // both the evidence and the noise or just the 
      // evidence, we shall now multiply by both.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}


// step size here is 0.001 ms
int dDDMSv(double *z, double *v, double *eta, double *sx, double *sy, double *delay, double *aU, double *aL,
                    double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
                    int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,timeStep,MaxTimeStep, currTime;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      currTime = timeStep*(*dt);
      term1 = exp((*sx)*(currTime-*delay));
      term2 = exp(-(*sx)*(*delay));
      
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      
      gamma = ((*sy)*term1)/(1+term1) + (1 + (1-*sy)*term2)/(1+term2);
      
      // This allows the specification of an increase in the accumulated evidence over time.
      // There is some lack of clarity in whether the urgency signal multiplies 
      // both the evidence and the noise or just the 
      // evidence, we shall now multiply by both.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int dDDMSvSz(double *zmin, double *zmax, double *v, double *eta, double *sx, double *sy, double *delay, double *aU, double *aL,
           double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,timeStep,MaxTimeStep, currTime;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*zmin + (*zmax-*zmin)*unif_rand();
    
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      currTime = timeStep*(*dt);
      term1 = exp((*sx)*(currTime-*delay));
      term2 = exp(-(*sx)*(*delay));
      
      
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      
      gamma = ((*sy)*term1)/(1+term1) + (1 + (1-*sy)*term2)/(1+term2);
      
      // This allows the specification of an increase in the accumulated evidence over time.
      // There is some lack of clarity in whether the urgency signal multiplies 
      // both the evidence and the noise or just the 
      // evidence, we shall now multiply by both.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}


// Like the UGM but no filtering and no time constants.
int uDDMSvSb(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
                  double *usign, double *intercept, double *ieta, double *usign_var,
                  double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep, 
                  int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,xu,samplev, sampleintercept, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  
  
  
  
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    sampleintercept = (*intercept) + (*ieta)*unif_rand();
    
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      
      // No filtering and just addition of this signal
      gamma = (sampleintercept + *usign_var*timeStep*(*dt));
      
      
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int nluDDM(double *z, double *v, double *aU, double *aL, double *timecons, 
         double *usign, double *intercept, double *usign_var,
         double *lambda, double *k,
         double *s,double *dt,double *response,double *rt,
         double *n,double *maxTimeStep,
         int *rangeLow, int *rangeHigh, double *randomTable)
{
  double rhs,x,xu,samplev, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  double tCurr;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  
  
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // Nothing fancy in this signal except a simple linearly increasing time varying signal with an intercept
      // and a slope term.
      // gamma = (*intercept + *usign_var*timeStep*(*dt));
      tCurr = (timeStep*(*dt))/1000.0;
      gamma = (*intercept + *usign_var*(1-exp(-pow(tCurr/(*lambda),*k))));
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int nluDDMSv(double *z, double *v, double *eta, double *aU, double *aL, double *timecons, 
           double *usign, double *intercept, double *usign_var,
           double *lambda, double *k,
           double *s,double *dt,double *response,double *rt,
           double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  double rhs,x,xu,samplev, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  double tCurr;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  
  
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // Nothing fancy in this signal except a simple linearly increasing time varying signal with an intercept
      // and a slope term.
      // gamma = (*intercept + *usign_var*timeStep*(*dt));
      tCurr = (timeStep*(*dt))/1000.0;
      gamma = (*intercept + *usign_var*(1-exp(-pow(tCurr/(*lambda),*k))));
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int nluDDMSvSb(double *z, double *v, double *eta, double *aU, double *aL, double *timecons, 
             double *usign, double *intercept, double *ieta, double *usign_var,
             double *lambda, double *k,
             double *s,double *dt,double *response,double *rt,
             double *n,double *maxTimeStep,
             int *rangeLow, int *rangeHigh, double *randomTable)
{
  double rhs,x,xu,samplev, sampleintercept, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  double tCurr;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  
  
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    sampleintercept = (*intercept) + (*ieta)*unif_rand();
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // Nothing fancy in this signal except a simple linearly increasing time varying signal with an intercept
      // and a slope term.
      // gamma = (*intercept + *usign_var*timeStep*(*dt));
      tCurr = (timeStep*(*dt))/1000.0;
      gamma = (sampleintercept + *usign_var*(1-exp(-pow(tCurr/(*lambda),*k))));
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}
// Like the UGM but no filtering and no time constants.
int uDDM(double *z, double *v, double *aU, double *aL, double *timecons, 
           double *usign, double *intercept, double *usign_var,
           double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,xu,samplev, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  
  
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // Nothing fancy in this signal except a simple linearly increasing time varying signal with an intercept
      // and a slope term.
      gamma = (*intercept + *usign_var*timeStep*(*dt));
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}




// Like the UGM but no filtering and no time constants.
int uDDMSv(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
                        double *usign, double *intercept, double *usign_var,
                        double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
                        int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,xu,samplev, gamma;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  
  
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // Nothing fancy in this signal except a simple linearly increasing time varying signal with an intercept
      // and a slope term.
      gamma = (*intercept + *usign_var*timeStep*(*dt));
      // This allows the specification of an increase in the momentary evidence over time.
      x = x+ ((*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)])*gamma;
      
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int cDDM(double *z, double *v, double *lambda, double *aU, double *aL, double *aprime, 
           double *k, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower;
  int N,i,timeStep,MaxTimeStep;
  double currTime;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      
      // Chand - 2018-08-17 noticed error in the implementation, used timeStep instead of dt, different timebases
      // Chand - 2018-08-28 - noticed that a' could sometimes be greater than aU which sucks, so 
      // Ended up making it a multiplier of aU and a max of 0.5 to ensure collapsing values
      
      currTime = timeStep*(*dt);
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),*k)))*(.5 - *aprime);
      upper = *aU - lower; 
      
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

// Fitted parameters are
// v, eta, lambda, aU, aprime, k, 
// these are free to vary!
// Noticed that aprime was giving fancy values.

int cDDMSv(double *z, double *v, double *eta, double *lambda, double *aU, double *aL, double *aprime, 
                   double *k, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
                   int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower;
  int N,i,timeStep,MaxTimeStep;
  double currTime;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;

      // Chand - 2018-08-17 noticed error in the implementation, used timeStep instead of dt, different timebases
      // Chand - 2018-08-28 - noticed that a' could sometimes be greater than aU which sucks, so 
      // Ended up making it a multiplier of aU and a max of 0.5 to ensure collapsing values
      
      currTime = timeStep*(*dt);
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),*k)))*(.5 - *aprime);
      upper = *aU - lower; 
      
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int cDDMSvSz(double *zmin, double *zmax, double *v, double *eta, double *lambda, double *aU, double *aL, double *aprime, 
           double *k, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep, 
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower, currTime;
  int N,i,timeStep,MaxTimeStep;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*zmin + (*zmax-*zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      currTime = timeStep*(*dt);
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),*k)))*(.5 - *aprime);
      upper = *aU - lower; 
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int cfkDDM(double *z, double *v,  double *lambda, double *aU, double *aL, double *aprime, 
             double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep, 
             int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower, currTime;
  int N,i,timeStep,MaxTimeStep;
  double kFixed=3.0;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      currTime = timeStep*(*dt);
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),kFixed)))*(.5 - *aprime);
      upper = *aU - lower; 
      
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}




int cfkDDMSv(double *z, double *v, double *eta, double *lambda, double *aU, double *aL, double *aprime, 
           double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep, 
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower, currTime;
  int N,i,timeStep,MaxTimeStep;
  double kFixed=3.0;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      currTime = timeStep*(*dt);
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),kFixed)))*(.5 - *aprime);
      upper = *aU - lower; 
      
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int cfkDDMSvSz(double *zmin, double *zmax, double *v, double *eta, double *lambda, double *aU, double *aL, double *aprime, 
             double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
             int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev;
  double upper, lower, currTime;
  int N,i,timeStep,MaxTimeStep;
  double kFixed=3.0;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*zmin + (*zmax-*zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();
      currTime = timeStep*(*dt);
      
      lower = *aU*(1 - exp(-pow((currTime)/(*lambda),kFixed)))*(.5 - *aprime);
      upper = *aU - lower; 
      
      // This allows the specification of an increase in the accumulated evidence over time.
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      
      if (x>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}




int DDMSvSz(double *zmin, double *zmax, double *v,double *aU, double *aL, double *eta,
                  double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
                  int *rangeLow, int *rangeHigh, double *randomTable)
{
  double rhs,x,samplev;
  int N,i,timeStep,MaxTimeStep;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*zmin + (*zmax-*zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      x = x+(*dt)*samplev+rhs*randomTable[returnRandomNumber(rangeL, rangeH)];
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}



int UGMSv(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
              double *usign, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
              int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  alpha=(*timecons)/((*timecons)+(*dt));
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*norm_rand();
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();   // DDM model
      // filtered signal from previous step + input from current step
      //       x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * timeStep*(*dt)*(*usign);  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}


int bUGM(double *z, double *v, double *aU, double *aL, double *timecons, 
           double *usign, double *intercept, double *s,double *dt,double *response,
           double *rt,double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  alpha=(*timecons)/((*timecons)+(*dt));
  for (i=0;i<N;i++) {
    samplev=(*v);
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();   // DDM model
      // filtered signal from previous step + input from current step
      //       x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * (*intercept + timeStep*(*dt)*(*usign));  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}





int bUGMSv(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
                       double *usign, double *intercept, double *s,double *dt,double *response,
                       double *rt,double *n,double *maxTimeStep,
                       int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  alpha=(*timecons)/((*timecons)+(*dt));
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();   // DDM model
      // filtered signal from previous step + input from current step
      //       x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * (*intercept + timeStep*(*dt)*(*usign));  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}



// ieta is a variable that adds variability to the urgency signal and multiplies everything.
int bUGMSvSb(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
                          double *usign, double *intercept, double *ieta,
                          double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
                          int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev, sampleintercept;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  alpha=(*timecons)/((*timecons)+(*dt));
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    sampleintercept = (*intercept) + (*ieta)*unif_rand();
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();   // DDM model
      // filtered signal from previous step + input from current step
      //       x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x*(sampleintercept + timeStep*(*dt)*(*usign));  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

// Not tested yet. 19th February 2019.
// For future use. Need to figure out parameter regimes in which cUGMSv would make sense.
int cUGMSv(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, double *aprime, 
           double *k, double *lambda, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep,
           int *rangeLow, int *rangeHigh, double *randomTable)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  double currTime, lower, upper;
  
  
  int rangeL, rangeH;
  rangeL = (int) *rangeLow;
  rangeH = (int) *rangeHigh;
  
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  alpha=(*timecons)/((*timecons)+(*dt));
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*randomTable[returnRandomNumber(rangeL, rangeH)];
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*randomTable[returnRandomNumber(rangeL, rangeH)]);
      xu = x;
      // Check if this filtered signal is above or below a threshold.file.
      currTime = timeStep*(*dt);
      lower = -*aU/2 + *aU*(1 - exp(-pow((currTime)/(*lambda),*k)))*(.5 - *aprime);
      upper = -lower; 
      
      if (xu>=upper) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=lower) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}

int UGMallVar(double *z, double *v,double *eta, double *aU, double *aL, double *timecons, 
                    double *usign, double *intercept, double *ieta, double *timecons_var, double *usign_var,
                    double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu,samplev, sampleintercept;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  // weight for exponentially-weighted moving average
  // alpha=(*dt)/((*dt)+(*timecons));
  
  alpha=(*timecons_var)/((*timecons_var)+(*dt));
  
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*norm_rand();
    sampleintercept = (*intercept) + (*ieta)*norm_rand();
    x=*z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      //       x = x+(*dt)*(*v)+rhs*norm_rand();   // DDM model
      // filtered signal from previous step + input from current step
      //       x = alpha*x + (1-alpha)*((*dt)*(*v) + rhs*norm_rand());
      x = alpha*x + (1-alpha)*((*dt)*samplev + rhs*norm_rand());
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x*(sampleintercept + timeStep*(*dt)*(*usign_var));  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}






int ratcliff(double *zmin, double *zmax, double *v,double *aU, double *aL, double *eta,
             double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep)
{
  double rhs,x,samplev;
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*norm_rand();
    x=*zmin + (*zmax-*zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      x = x+(*dt)*samplev+rhs*norm_rand();
      if (x>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}





int ratcliffUGM(double *zmin, double *zmax, double *v,double *aU, double *aL, double *eta,
                double *timecons, double *usign, double *s,double *dt,double *response,double *rt,double *n,double *maxTimeStep)
{
  double rhs,x,samplev,xu;
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  MaxTimeStep =(int) *maxTimeStep;
  GetRNGstate();
  rhs=sqrt(*dt)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*norm_rand();
    x=*zmin + (*zmax-*zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      x = x+(*dt)*samplev+rhs*norm_rand();
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * timeStep*(*dt)*(*usign);  // urgency is multiplicative
      if (xu>=*aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (xu<=*aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(*dt) - (*dt)/((double) 2.0);
  }
  PutRNGstate();
}
