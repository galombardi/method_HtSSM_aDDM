Method for fitting HtSSM and HaDDM with Jags
============================================

Before running
--------------

install jags_4.3.0.orig.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run  

.. code:: sh

  tar --bzip2 -xf ~/jags_4.3.0.orig.tar.gz
  cd JAGS_4.3.0/
  ./configure  
  make -j 8
  sudo make install  


install JAGS-WIENER-MODULE-1.1.tar.gz from library https://sourceforge.net/projects/mcmc-jags/ then run

.. code:: sh

  tar --bzip2 -xf ~/JAGS-WIENER-MODULE-1.1.tar.gz
  cd JAGS-WIENER-MODULE-1.1/ 
  ./configure
  make -j 8
  sudo make install

How to run the code
-------------------
First change the path pathToFolder in the Rscript_tSSMjags_FIT_example.R_ with the path in which you saved all the scripts, for example: 

.. code:: R

  pathToFolder <- "~/pathToFolder"  

then run  

.. code:: sh
  
  source('~/method_HtSSM_aDDM/Rscript_tSSMjags_FIT_example.R')



NOTES
-----
Make sure that the reaction time data you want to fit does not have missing values or NAs before running.  

The Recovery fitting can be found in the RecoveryFitting/ folder

