
#!/bin/bash


R CMD BATCH 1-spsr-standardize.R 
R CMD BATCH spsr-figure-1-2-withinclus-sp.R 
R CMD BATCH spsr-figure-3-funnelplot.R 

R CMD BATCH spsr-table-1.R
R CMD BATCH spsr-table-2.R
R CMD BATCH spsr-table-3.R
R CMD BATCH spsr-table-4.R
R CMD BATCH spsr-table-5.R

R CMD BATCH spsr-supp-figure-2-vaccov-risk.R 
R CMD BATCH spsr-supp-table-group-data.R 
R CMD BATCH spsr-supp-table-rob.R 