# CHaRTr: An R toolbox for modeling Choices and Response Times in decision-making tasks
## Chandramouli Chandrasekaran, Guy Hawkins

This toolbox is an attempt by us (Chand Chandrasekaran, Guy Hawkins) to provide researchers interested in various models of decision-making a simple, easily run and an extensible toolbox for analysis of RT and choice behavior in decision-making tasks. This toolbox or earlier less-documented and elaborated variants of it have currently been used in four papers ([Chandrasekaran et al. 2017](https://www.nature.com/articles/s41467-017-00715-0), [Hawkins et al. 2015](http://www.physiology.org/doi/10.1152/jn.00088.2015), [Hawkins et al. 2015](https://doi.org/10.1523/JNEUROSCI.2410-14.2015), [Evans et al. 2017](https://www.nature.com/articles/s41598-017-16694-7)) and we anticipate/expect/hope that many more are going to be published based on this toolbox. The motivation to release this toolbox is for a few reasons.

1. <b> Scientific </b>: Advance research into quantitative rather than descriptive models of psychophysics especially for two alternative forced choice decision-making and later for multi-alternative choice decision-making.

2. <b> Communication </b>: By releasing this toolbox we are outlining a specific implementation in code of the models that we think are currently being tested. We often find in journal papers that the methods section is still quite vague and we have scratched our heads to identify what exactly researchers chose as their model specification to perform their comparison between different models.

3. <b> Beyond the mean </b>: Remarkable amount of research into the diffusion decision model has led to the insight that simple fitting of mean RTs is perhaps not the best way to understand which candidate model of decision-making provides the best description of RT and choice behavior in discrimination tasks. The consensus is that we need to move beyond the mean and fitting the quantiles of RT distributions for both correct and error trials. However, fitting full quantiles of RT distributions is difficult and needs expertise in optimization (see also point 4 below). Fortunately, we are building on these insights and are now able to provide the ability to fit the quantiles of RT distributions.

4. <b> Ease of Use </b>: When trying to learn how to use these different models to understand decision-making, the brick wall that often comes up was implementing such a model in any language and deriving RT and choice outputs from it. Unfortunately, these are all stochastic differential equations and analytical solutions are rare except for some remarkably simple cases. Fortunately, R has powerful optimizers and we decided to leverage the powerful optimization technique such as Differential Evolution to fit the parameters of various models.

We outline further details in the accompanying biorxiv paper ([link](https://www.biorxiv.org/content/10.1101/570184v1)) that is currently under review. If you use the toolbox in your own research please cite the paper :)

### Using this toolbox. 

The architecture of the toolbox is very simple. The choosing of which model to run and the lower and upper parameter limits and the parameter names are performed in R. The models are specified in C which is then dynamically called by the DEoptim function from the differential evolution package in R called [DEoptim](https://cran.r-project.org/web/packages/DEoptim/index.html), This toolbox has several models available that can be easily run with minor modification.

We assume that there is a reasonable working knowledge of R and C to run this code. General understanding of programming is also quite helpful. There is a small learning curve to ensure that the parameters are properly specified.

## Compiling the shared object file

To run the model on your specific architecture you need to compile a shared object file that is dynamically loaded by R to perform the optimization. This is done very easily by typing the following code at a command line prompt. 

```
R CMD SHLIB chartr-ModelSpec.c
```

This command compiles chartr-ModelSpec.c and then link the output object file into a shared object (.so on unix systems such as a MAC and LINUX) which can be loaded into R using the function dyn.load.

If the compilation is successful you should see something like this on your terminal

```
gcc -std=gnu99 -I/usr/share/R/include -DNDEBUG -fpic -g -O2 -fstack-protector-strong -Wformat -Werror=format-security 
Wdate-time -D_FORTIFY_SOURCE=2  -g -c chartr-ModelSpec.c -o chartr-modelspec.o
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o chartr-ModelSpec.so chartr-ModelSpec.o 
L/usr/lib/R/lib -lR

```

The implementation has not been tested on windows.

## Packages

For smooth operation of ChaRTr, you will need to install the following packages -- devtools, reshape2, ggplot2, ggthemes, DEoptim, gridExtra, and tictoc.

Typically the install.packages command in R will help you install these packages. The packages often have dependencies on openssl and curl and this may need installation using either apt-get in Linux or homebrew in Mac.


