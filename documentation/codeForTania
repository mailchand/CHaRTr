int stoneEtaDitterich(double *z, double *v, double *eta, double *sx, double *sy, double *delay, double *aU, double *aL,
                    double *s,double *h,double *resp,double *rt,double *n,double *maxiter)
{
  //   double t,rhs,x,hv,samplev;
  double rhs,x,samplev, gamma;
  double term1, term2;
  int N,i,iter,Maxiter;
  
  /* Convert some double inputs to integer types. */
  N=(int) *n;
  Maxiter =(int) *maxiter;
  GetRNGstate();
  rhs=sqrt(*h)*(*s);
  for (i=0;i<N;i++) {
    samplev=(*v)+(*eta)*norm_rand();
    
    x=*z; 
    iter=0;
    resp[i]=(double) -1.0 ;
    do 
    {
      iter = iter+1;
      //       x = x+(*h)*(*v)+rhs*norm_rand();
      term1 = exp((*sx)*(iter-*delay));
      term2 = exp(-(*sx)*(*delay));
      
      // Scaling term according to Ditterich 2006a
      // Using formulation from Ditterich 2006a, multiply by an urgency signal
      // samplev=(*v)*gamma + (*eta)*norm_rand();

      gamma = ((*sy)*term1)/(1+term1) + (1 + (1-*sy)*term2)/(1+term2);

      // This allows the specification of an increase in the accumulated evidence over time.
      // There is some lack of clarity in whether the urgency signal multiplies both the evidence and the noise or just the 
      // evidence, we shall now multiply by both.
      x = x+ ((*h)*samplev+rhs*norm_rand())*gamma;
      
      if (x>=*aU) {
        resp[i]=(double) 1.0 ; 
        break ;
      }
      if (x<=*aL) {
        resp[i]=(double) 2.0 ; 
        break ;
      }
    } while (iter<Maxiter) ; 
    rt[i]=((double) iter)*(*h) - (*h)/((double) 2.0);
  }
  PutRNGstate();
}