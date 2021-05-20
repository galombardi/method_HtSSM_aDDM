# Model simulations with 2ddm_r_cpp_22_noiseCorrected2.cpp
​
## Before running 
​
install boot boost_1_66_0.tar.bz2 from library https://www.boost.org/ then run  
`tar --bzip2 -xf ~/boost_1_66_0.tar.bz2`
`cd boost_1_66_0/`
`./bootstrap.sh`  
`sudo ./b2 install`
​
​
## How to use simulations code
`ddm2_parallel(d_v, d_h, thres,  nDT,  tIn,  bias,  vd,  hd,  1,  N)`
where 
- d_v    -> weight of drift first attribute (taste for example)
- d_h    -> weight of drift second attribute (health for example)
- thres    -> threshold or boundary of the accumulation
- tIn    -> time parameter at which the value 2 comes in the accumulation process
- bias      -> initial value (starting point) of the accumulation
- td      -> value difference in taste
- hd      -> value difference in health
- 1       -> noise value (to change sigma if you fix the threshold to 1)
- N         -> number of simulations

