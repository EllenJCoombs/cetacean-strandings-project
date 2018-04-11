# What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years

:whale2: :chart_with_upwards_trend:



Author(s): 
[Ellen J. Coombs](mailto:ellen.coombs.14@ucl.ac.uk), [David L. Miller](https://github.com/dill), Rob Deaville, Richard C. Sabin, Richard C. Sabin, Louise Allan, Mick O’Connell, Simon Berrow, Andrew Brownlow, Mariel Ten Doeschate, Rod Penrose, Ruth Williams, Paul Jepson, and [Natalie Cooper](https://github.com/nhcooper123)


To cite the paper:  


To cite this repo: 


## Data :bar_chart: :chart_with_downwards_trend: 

These analyses use data from various studies. The data for possible environmental correlates of strandings can be found at these locations: 

- [HadISST data](https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html)
- [British Geological Survey](http://www.geomag.bgs.ac.uk/data_service/data/magnetic_indices/k_indices.html)
- [Storms](../blob/master/LICENSE) this will be a link to my own storms raw data and storm count
- [North Atlantic Oscillation](https://climatedataguide.ucar.edu/sites/default/files/nao_station_annual.txt)

Human population for the UK can be found here: 
[Human population data](../blob/master/LICENSE) this will be a link to my datasheet 


## Analysis :chart_with_upwards_trend: :whale2:
All code used to run analyses and make figures and tables is included in the analyses/ folder. Before starting remember to either set your working directory to the stranding poject folder on your computer, or open an RStudio project from that folder. All of these files are in the RMarkdown format. You can click the Knit button in RStudio and create HTML versions using the code. 

* **01-clean-up-data-for-analysis.Rmd** This script takes the raw data from the NHM and CSIP databases. This includes cleaning and standardising variables in the Natural History Museum (NHM), Cetacean Stranding Investigation Programme (CSIP) datasets for binding, cleaning and formatting dates in the NHM and CSIP dataset and binding the 2 datasets (NHM + CSIP first, followed by IWDG which was aquired at a later date; see **02-cleaning-and-adding-irish-data.Rmd**). If you're pushed for time, use the cleaned, merged final dataset of all 3 stranding datsets, which has also had rare species removed and is used in the GAMs. This can be found in the data folder and is called `UK_and_Irish_sp.csv`

* **02-cleaning-and-adding-irish-data.Rmd** The first part of this script imports and cleans the Irish dataset. The second part of this script binds the Irish dataset with the NHM + CSIP dataset (called `cleandatesnames.csv`)

* **03-final-data-clean.Rmd** This is the final stage of the data cleaning before analysis.
'UK_and_Irish_sp' is the final dataset used for analysis 

* **04-temporal-analysis-figure-1.Rmd** This code plots all cetacean strandings from 1913-2015 from the combined dataset (NHM + CSIP + IWDG) and produces Figure.1 from the paper. 

* **05-spatial-analysis-figure-2.Rmd** Produces Figure 2. 

If you are pushed for time, you can skip RMarkdown documents 06 to 11 and use the model dataset called `all_strandings.csv` to run the analysis in **12-generalised-additive-models.Rmd**. 

* **06-correlates-data-population.Rmd** This code reads in, and cleans population data which forms part of the dataset of possible correlates of strandings.

* **07-correlates-data-storms.Rmd** This code reads in and cleans storm data which will form part of the dataset of possible correlates of strandings. 

* **08-correlates-data-geomagnetic.Rmd** This code reads in and cleans geomagnetic data which will form part of the dataset of possible correlates of strandings. 

* **09-correlates-data-sst.Rmd** This code reads in and cleans sea surface temperature (SST) data which will form part of the dataset of possible correlates of strandings. 

* **10-correlates-data-nao.Rmd** This code reads in NOA data from University Corporation for Atmospheric Research.

* **11-correlates-data-for-model.Rmd** This code pulls together all of the correlates data e.g. Storms, and NAO, and puts them together along with 'Year', 'Species' and Total strandings' which is called `all_strandings.csv`. This dataset is fed into the Generalised Additive Model (`12-generalised-additive-models.Rmd`)

* **12-generalised-additive-models.Rmd** This code runs the GAM which looks at all strandings (species per year), with an offset of population.

* **13-extras-species-GAMs** This code runs additional GAMs which are not part of the paper or main analysis. These GAMs are species-specific, and run using the same covariates as in (`12-generalised-additive-models.Rmd`) should users wish to look at species-specifics 


## Other folders :file_folder:
`functions/` contains functions required by the code in the `analysis/` folder 
`figures/` contains figures found in the manuscript 
`raw data/` contains gathered raw data and sources on UK population and UK and Irish storms from 1913-2015
`cleaned data/` contains cleaned stranding dataset with the NHM, CSIP and IWDG datasets combined, and rare species removed. Also contains the cleaned correlates data; as well as the cleaned dataset (which is all of the correlates combined + year, species and total strandings) 


## Session Info :clipboard:
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.

Session info ---------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.4.0 (2017-04-21)
 system   x86_64, darwin15.6.0        
 ui       RStudio (1.0.153)           
 language (EN)                        
 collate  en_GB.UTF-8                 
 tz       <NA>                        
 date     2018-02-07                  

Packages -------------------------------------------------------------------------------------------
 package    * version date       source        
 base       * 3.4.0   2017-04-21 local  
 clipr      * 0.4.0   2017-11-03 CRAN (R 3.4.2)  
 colorspace   1.3-2   2016-12-14 CRAN (R 3.4.0)   
 compiler     3.4.0   2017-04-21 local  
 datasets   * 3.4.0   2017-04-21 local           
 devtools   * 1.13.4  2017-11-09 CRAN (R 3.4.2)  
 digest       0.6.12  2017-01-27 CRAN (R 3.4.0)   
 ggplot2      2.2.1   2016-12-30 CRAN (R 3.4.0)  
 graphics   * 3.4.0   2017-04-21 local           
 grDevices  * 3.4.0   2017-04-21 local           
 grid         3.4.0   2017-04-21 local           
 gtable       0.2.0   2016-02-26 CRAN (R 3.4.0)  
 knitr        1.17    2017-08-10 CRAN (R 3.4.0)  
 lattice      0.20-35 2017-03-25 CRAN (R 3.4.0)  
 lazyeval     0.2.0   2016-06-12 CRAN (R 3.4.0)  
 Matrix       1.2-9   2017-03-14 CRAN (R 3.4.0)  
 memoise      1.1.0   2017-04-21 CRAN (R 3.4.0)  
 methods    * 3.4.0   2017-04-21 local           
 mgcv         1.8-17  2017-02-08 CRAN (R 3.4.0)  
 munsell      0.4.3   2016-02-13 CRAN (R 3.4.0)  
 nlme         3.1-131 2017-02-06 CRAN (R 3.4.0)  
 plyr         1.8.4   2016-06-08 CRAN (R 3.4.0)  
 Rcpp         0.12.12 2017-07-15 CRAN (R 3.4.1)  
 rlang        0.1.1   2017-05-18 CRAN (R 3.4.0)  
 rstudioapi   0.7     2017-09-07 CRAN (R 3.4.1)  
 scales       0.4.1   2016-11-09 CRAN (R 3.4.0)  
 stats      * 3.4.0   2017-04-21 local           
 tibble       1.3.3   2017-05-28 CRAN (R 3.4.0)  
 tools        3.4.0   2017-04-21 local           
 utils      * 3.4.0   2017-04-21 local           
 withr        2.1.1   2017-12-19 CRAN (R 3.4.3)  