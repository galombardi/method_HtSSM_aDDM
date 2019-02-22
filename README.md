# Method for fitting HtSSM and HaDDM with Jags

## Before running 

install jags_4.3.0.orig.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run  
`tar --bzip2 -xf ~/jags_4.3.0.orig.tar.gz`  
`cd JAGS_4.3.0/`  
`./configure`  
`make -j 8`  
`sudo make install`  

install JAGS-WIENER-MODULE-1.1.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run  
`tar --bzip2 -xf ~/JAGS-WIENER-MODULE-1.1.tar.gz`  
`cd JAGS-WIENER-MODULE-1.1/`  
`./configure`  
`make -j 8`  
`sudo make install`  


## How to run the code
First change the path pathToFolder in the Rscript_tSSMjags_FIT_example.R with the path in which you saved all the scripts, for example:  
`pathToFolder <- "~/pathToFolder"`  
then run   
`source('~/method_HtSSM_aDDM/Rscript_tSSMjags_FIT_example.R')`



## NOTES
Make sure that the reaction time data you want to fit does not have missing values or NAs before running.

