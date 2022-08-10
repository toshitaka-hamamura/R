library(lme4)
library(simr)

# Toy model
fm = lmer(y ~ x + (x | g), data = simdata)

# Extend sample size of `g`
fm_extended_g = extend(fm, along = 'g', n = 12)

# Parallelize `breaks` by running each number of levels in a separate function.

# 4 levels of g
pwcurve_4g = powerCurve(fm_extended_g, fixed('x'), along = 'g', breaks = 4, 
                        nsim = 50, seed = 123, 
                        # No progress bar
                        progress = FALSE)

# 6 levels of g
pwcurve_6g = powerCurve(fm_extended_g, fixed('x'), along = 'g', breaks = 6, 
                        nsim = 50, seed = 123, 
                        # No progress bar
                        progress = FALSE)

# 8 levels of g
pwcurve_8g = powerCurve(fm_extended_g, fixed('x'), along = 'g', breaks = 8, 
                        nsim = 50, seed = 123, 
                        # No progress bar
                        progress = FALSE)

# 10 levels of g
pwcurve_10g = powerCurve(fm_extended_g, fixed('x'), along = 'g', breaks = 10, 
                         nsim = 50, seed = 123, 
                         # No progress bar
                         progress = FALSE)

# 12 levels of g
pwcurve_12g = powerCurve(fm_extended_g, fixed('x'), along = 'g', breaks = 12, 
                         nsim = 50, seed = 123, 
                         # No progress bar
                         progress = FALSE)

# Create a destination object using any of the power curves above.
all_pwcurve = pwcurve_4g

# Combine results
all_pwcurve$ps = c(pwcurve_4g$ps[1], pwcurve_6g$ps[1], pwcurve_8g$ps[1], 
                   pwcurve_10g$ps[1], pwcurve_12g$ps[1])

# Combine the different numbers of levels.
all_pwcurve$xval = c(pwcurve_4g$nlevels, pwcurve_6g$nlevels, pwcurve_8g$nlevels, 
                     pwcurve_10g$nlevels, pwcurve_12g$nlevels)

print(all_pwcurve)
## Power for predictor 'x', (95% confidence interval),
## by number of levels in g:
##       4: 46.00% (31.81, 60.68) - 40 rows
##       6: 74.00% (59.66, 85.37) - 60 rows
##       8: 92.00% (80.77, 97.78) - 80 rows
##      10: 98.00% (89.35, 99.95) - 100 rows
##      12: 100.0% (92.89, 100.0) - 120 rows
## 
## Time elapsed: 0 h 0 m 7 s
plot(all_pwcurve, xlab = 'Levels of g')

# For reproducibility purposes
sessionInfo()
## R version 4.1.1 (2021-08-10)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Big Sur 10.16
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] simr_1.0.5    lme4_1.1-27.1 Matrix_1.3-4 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7        lattice_0.20-44   tidyr_1.1.3       assertthat_0.2.1 
##  [5] digest_0.6.27     utf8_1.2.2        plyr_1.8.6        R6_2.5.1         
##  [9] cellranger_1.1.0  backports_1.2.1   evaluate_0.14     blogdown_1.5     
## [13] pillar_1.6.2      rlang_0.4.11      curl_4.3.2        readxl_1.3.1     
## [17] minqa_1.2.4       data.table_1.14.0 car_3.0-11        nloptr_1.2.2.2   
## [21] jquerylib_0.1.4   rmarkdown_2.11    splines_4.1.1     stringr_1.4.0    
## [25] foreign_0.8-81    broom_0.7.9       compiler_4.1.1    xfun_0.26        
## [29] pkgconfig_2.0.3   mgcv_1.8-36       htmltools_0.5.2   tidyselect_1.1.1 
## [33] tibble_3.1.4      binom_1.1-1       bookdown_0.24     rio_0.5.27       
## [37] fansi_0.5.0       crayon_1.4.1      dplyr_1.0.7       MASS_7.3-54      
## [41] grid_4.1.1        nlme_3.1-153      jsonlite_1.7.2    lifecycle_1.0.0  
## [45] DBI_1.1.1         magrittr_2.0.1    zip_2.2.0         stringi_1.7.4    
## [49] carData_3.0-4     bslib_0.3.0       ellipsis_0.3.2    vctrs_0.3.8      
## [53] generics_0.1.0    boot_1.3-28       openxlsx_4.2.4    iterators_1.0.13 
## [57] tools_4.1.1       forcats_0.5.1     glue_1.4.2        purrr_0.3.4      
## [61] hms_1.1.0         plotrix_3.8-2     parallel_4.1.1    abind_1.4-5      
## [65] pbkrtest_0.5.1    fastmap_1.1.0     yaml_2.2.1        RLRsim_3.1-6     
## [69] knitr_1.34        haven_2.4.3       sass_0.4.0