/* Using Rcpp for DDM and other models. Rcpp is remarkably easy*/

#include <Rcpp.h>
using namespace Rcpp;
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>

// Using Rcpp instead of kludgy C based systems for fitting models for CHaRTr.
// Rcpp works easier than C based systems, because it removes the need for shared libraries

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


double evalDV(double x, double aU, double aL)
{
    double response=-1.0;
    if (x>=aU) {
        response=1.0; 
        
      }
      if (x<=aL) {
        response=2.0 ;     
      }
  return response;
}

double calcRT(double timeStep, double dt)
{  
  return timeStep*dt - dt/2.0;
}


// [[Rcpp::export]]
List DDM(double z, double v, double aU, double aL, double s, double dt, double n, double maxTimeStep)
{
  double rhs,x;
  int N,i,timeStep,MaxTimeStep;
  NumericVector response(N), rt(N);
  List data;

  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;

  GetRNGstate();
  rhs = sqrt(dt)*s;

  for(int i=0;i<N;++i)
  {
    x=z; 
    timeStep=0;
    response[i]=-1.0;

    do 
    {
      timeStep = timeStep+1;
      x = x + v*dt +rhs*norm_rand();

      response[i] = evalDV(x, aU, aL);
      if(response[i] > 0)
        break;

    } while (timeStep < MaxTimeStep) ; 
    rt[i]=calcRT(timeStep, dt);    
  }


  PutRNGstate(); 
  data["response"] = response;
  data["rt"] = rt;
  return data;
}

// Minor modification add a eta parameter
// [[Rcpp::export]]
List DDMSv(double z, double v, double eta, double aU, double aL, double s, double dt, double n, double maxTimeStep)
{
  double rhs,x;
  int N,i,timeStep,MaxTimeStep;
  double samplev;

  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;
  List data;
  NumericVector response(N);
  NumericVector rt(N);

  
  GetRNGstate();
  rhs = sqrt(dt)*s;

  for(int i=0;i<N;++i)
  {
    x=z; 
    timeStep=0;
    response[i]=-1.0;
    samplev=(v)+(eta)*norm_rand();

    do 
    {
      timeStep = timeStep+1;
      x = x + v*dt +rhs*norm_rand();
      if (x>=aU) {
        response[i]=1.0; 
        break ;
      }
      if (x<=aL) {
        response[i]=2.0 ; 
        break ;
      }

    } while (timeStep < MaxTimeStep) ; 

    rt[i]=(timeStep*dt)-dt/2.0;
  }

  data["response"] = response;
  data["rt"] = rt;
  PutRNGstate(); 

  return data;
}


// [[Rcpp::export]]
List DDMSvSz(double zmin, double zmax, double v, double eta, double aU, double aL, double s, double dt, double n, double maxTimeStep)
{
  double rhs,x;
  int N,i,timeStep,MaxTimeStep;
  double samplev;

  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;
  List data;
  NumericVector response(N);
  NumericVector rt(N);

  
  GetRNGstate();
  rhs = sqrt(dt)*s;

  for(int i=0;i<N;++i)
  {
    timeStep=0;
    response[i]=-1.0;
    samplev=(v)+(eta)*norm_rand();
    x=zmin + (zmax-zmin)*unif_rand();
    do 
    {
      timeStep = timeStep+1;
      x = x + v*dt +rhs*norm_rand();
      if (x>=aU) {
        response[i]=1.0; 
        break ;
      }
      if (x<=aL) {
        response[i]=2.0 ; 
        break ;
      }

    } while (timeStep < MaxTimeStep) ; 

    rt[i]=(timeStep*dt)-dt/2.0;
  }

  data["response"] = response;
  data["rt"] = rt;
  PutRNGstate(); 

  return data;
}

// step size here is 0.001 ms
// [[Rcpp::export]]
List dDDMSv(double z, double v, double eta, double sx, double sy, double delay, double aU, double aL,
  double s,double dt,double n,double maxTimeStep)
{
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,timeStep,MaxTimeStep, currTime;

/* Convert some double inputs to integer types. */
  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;
  GetRNGstate();
  rhs=sqrt(dt)*(s);


  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;
  List data;
  NumericVector response(N);
  NumericVector rt(N);


  for (i=0;i<N;i++) {
    samplev=(v)+(eta)*norm_rand();
    x=z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
    //       x = x+(*dt)*(*v)+rhs*norm_rand();
      currTime = timeStep*(dt);
      term1 = exp((sx)*(currTime-delay));
      term2 = exp(-(sx)*(delay));

    // Scaling term according to Ditterich 2006a
    // Using formulation from Ditterich 2006a, multiply by an urgency signal
    // samplev=(*v)*gamma + (*eta)*norm_rand();

      gamma = (sy*term1)/(1+term1) + (1 + (1-sy)*term2)/(1+term2);

    // This allows the specification of an increase in the accumulated evidence over time.
    // There is some lack of clarity in whether the urgency signal multiplies 
    // both the evidence and the noise or just the 
    // evidence, we shall now multiply by both.
      x = x+ (dt*samplev+rhs*norm_rand())*gamma;

      if (x>=aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(dt) - (dt)/((double) 2.0);
  }
  data["response"] = response;
  data["rt"] = rt;

  PutRNGstate();
  return data;
}


// [[Rcpp::export]]
List dDDMSvSz(double zmin, double zmax, double v, double eta, double sx, double sy, double delay, double aU, double aL,
  double s,double dt,double n,double maxTimeStep)
{
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,timeStep,MaxTimeStep, currTime;

/* Convert some double inputs to integer types. */
  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;

  List data;
  NumericVector response(N);
  NumericVector rt(N);


  GetRNGstate();
  rhs=sqrt(dt)*(s);
  for (i=0;i<N;i++) {
    samplev=(v)+(eta)*norm_rand();
    x=zmin + (zmax-zmin)*unif_rand();
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
    //       x = x+(*dt)*(*v)+rhs*norm_rand();
      currTime = timeStep*(dt);
      term1 = exp((sx)*(currTime-delay));
      term2 = exp(-(sx)*(delay));

    // Scaling term according to Ditterich 2006a
    // Using formulation from Ditterich 2006a, multiply by an urgency signal
    // samplev=(*v)*gamma + (*eta)*norm_rand();

      gamma = (sy*term1)/(1+term1) + (1 + (1-sy)*term2)/(1+term2);

    // This allows the specification of an increase in the accumulated evidence over time.
    // There is some lack of clarity in whether the urgency signal multiplies 
    // both the evidence and the noise or just the 
    // evidence, we shall now multiply by both.
      x = x+ (dt*samplev+rhs*norm_rand())*gamma;

      if (x>=aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(dt) - (dt)/((double) 2.0);
  }
  PutRNGstate();
  data["response"] = response;
  data["rt"] = rt;
  return data;
}

// [[Rcpp::export]]
List UGM(double z, double v,double aU, double aL, double timecons, double usign, double s,
           double dt,double n,double maxTimeStep)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,alpha,xu;   // xu stores x + urgency signal at each time point
  int N,i,timeStep,MaxTimeStep;
  
  /* Convert some double inputs to integer types. */
  
  N=(int) n;
  MaxTimeStep =(int) maxTimeStep;

  List data;
  NumericVector response(N);
  NumericVector rt(N);

  
  GetRNGstate();
  
  rhs=sqrt(dt)*(s);
  
  alpha=timecons/(timecons+dt);
  
  for (i=0;i<N;i++) 
  {
    x=z; 
    timeStep=0;
    response[i]=(double) -1.0 ;
    do 
    {
      timeStep=timeStep+1;
      // filtered signal from previous step + input from current step
      x = alpha*x + (1-alpha)*(dt*v + rhs*norm_rand());
      
      // multiply linear urgency signal. usign determines size of urgency signal (1 in Cisek, 2 in Thura)
      xu = x * timeStep*dt*usign;  // urgency is multiplicative
      
      // Done correctly with the dt multiplier!
      
      if (xu>=aU) {
        response[i]=(double) 1.0 ; 
        break ;
      }
      
      if (xu<=aL) {
        response[i]=(double) 2.0 ; 
        break ;
      }
    } while (timeStep<MaxTimeStep) ; 
    rt[i]=((double) timeStep)*(dt) - (dt)/((double) 2.0);
  }
  
  PutRNGstate();
  data["response"] = response;
  data["rt"] = rt;
  return data;
}
