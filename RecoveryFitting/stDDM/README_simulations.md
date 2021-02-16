# Model fitting for simple binary food choices in 3 conditions
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
## How to run the code
`source('~/tSSM_scRIB/fitTPDtSSM_free.R')`
​
​
## Conditions
condition 1 -> only rating bars for health and taste on the screen  
condition 2 -> only imgs of the food items on the screen  
condition 3 -> both rating bars and food imgs on the screen
​
​
## Variables
1. Choice outcome (coded as left = 1, right = 0)  
2. Signed value differences for taste and health (left - right)  
3. RTs in seconds  
4. Subject ID  
5. Condition ID  
