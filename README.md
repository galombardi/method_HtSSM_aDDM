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
`source('~/method_HtSSM_aDDM/Rscript_tSSMjags_FIT_example.R')`

