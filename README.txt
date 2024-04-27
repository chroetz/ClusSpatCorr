Requirements:
- R version 4.1 or higher (with command line tool Rscript in search path)
- Python (tested with Python 3.11.5) with packages
  + pandas (version > 2.0)
  + numpy
  + scipy
  + matplotlib
  + palettable
- internet connection
- several GB free disk space (100 GB is enough)


Run
Rscript 000_RUN_ALL.R
to download required data, compute and plot.
This will take several hours (less than 24 on a modern laptop).



R Session Info:

> sessionInfo()
R version 4.2.3 (2023-03-15 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_Germany.utf8  LC_CTYPE=English_Germany.utf8   
[3] LC_MONETARY=English_Germany.utf8 LC_NUMERIC=C                    
[5] LC_TIME=English_Germany.utf8    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] sandwich_3.1-0    doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2     lubridate_1.9.2  
 [6] forcats_1.0.0     stringr_1.5.0     dplyr_1.1.0       purrr_1.0.1       readr_2.1.4      
[11] tidyr_1.3.0       tibble_3.2.0      ggplot2_3.4.1     tidyverse_2.0.0   fixest_0.12.0    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.12         dreamerr_1.4.0      compiler_4.2.3      pillar_1.9.0       
 [5] tools_4.2.3         timechange_0.2.0    lifecycle_1.0.4     nlme_3.1-162       
 [9] gtable_0.3.2        lattice_0.20-45     pkgconfig_2.0.3     rlang_1.1.0        
[13] cli_3.6.0           rstudioapi_0.15.0   withr_3.0.0         generics_0.1.3     
[17] vctrs_0.6.0         stringmagic_1.0.0   hms_1.1.3           grid_4.2.3         
[21] tidyselect_1.2.1    glue_1.6.2          R6_2.5.1            fansi_1.0.4        
[25] Formula_1.2-5       tzdb_0.3.0          magrittr_2.0.3      codetools_0.2-19   
[29] scales_1.3.0        colorspace_2.1-0    numDeriv_2016.8-1.1 utf8_1.2.3         
[33] stringi_1.7.12      munsell_0.5.0       zoo_1.8-12         

