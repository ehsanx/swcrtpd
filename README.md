# swcrtpd
SW-CRT Power Distribution

We uploaded some basic code for our SW-CRT power distribution estimation in supply of our paper submitted to BMC medical research methodology. The code has not been well-documented. Please use at your own risk. Please note the computational time is massive on your local computer. You need about 2 hours to evaluate 84 allocations with 10,000 simulation runs. The authors finished all computation on cloud computing.

If you decide to try it anyways, you can first generate all possible allocation for your trial using genallocs.R, then generate all the data using Xmatrices.R. Last, the output Tx from genallocs.R and XYmtr from Xmatrices.R will be used in powercal.R for power calculation.

We delayed the documentation as we are developing a R-package for more efficient power distribution estimation. We also extend the outcomes to other types (binary, count). A well-documented version with reasonable computational time will be available in June/July, 2020. Please check back then. 
