#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
gsl_rng * r;  /* global generator */

int main (void)
{
  const gsl_rng_type * T;
  
  gsl_rng_env_setup();
  
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  
  gsl_rng_set(r,2);
  
  printf ("generator type: %s\n", gsl_rng_name (r));
  printf ("seed = %lu\n", gsl_rng_default_seed);
  printf ("first value = %lu\n", gsl_rng_get (r));
  
  printf("gaussian random variable = %4.5f\n", gsl_ran_gaussian(r, 1.0));
  printf("gaussian random variable = %4.5f\n", gsl_ran_gaussian(r, 1.0));
  gsl_rng_free (r);
  return 0;
}