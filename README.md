# VeganBugOrdination
This repository has the final code for VDEQ genus reference sites ordinations.  

R code using VEGAN package creates Non-metric multidimensional scaling (NMDS) and Multi Response Permutation Procedure (MRPP) at genus level 
reference macroinvertebrate sites. This code is useful in determining natural variability at reference sites to guide model development. 

Use NMDS.Rproj project and 'FinalSummaryMarkdown.Rmd' to look at final NMDS and MRPP results. Lots of different ordinations were reviewed and are
included in this folder (and were very important to the process), but the 'FinalSummaryMarkdown.Rmd' contains the most important results.

Code works in R4.0.3 and R4.1.2 with the following packages:
library(vegan)
library(dplyr)
library(lubridate)
library(purrr) 
library(ggplot2)
library(readxl)
library(tidyr)
library(DT)
library(data.table)
library(cluster)
library(car)
library(sp)
library(htmltools)
