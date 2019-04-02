# What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years

:whale2: :chart_with_upwards_trend:



__Authors:__
[Ellen J. Coombs](mailto:ellen.coombs.14@ucl.ac.uk), Rob Deaville, Richard C. Sabin, Richard C. Sabin, Louise Allan, Mick O’Connell, Simon Berrow, Brian Smith, Andrew Brownlow, Mariel Ten Doeschate, Rod Penrose, Ruth Williams, Matthew Perkins, Paul Jepson, and [Natalie Cooper](https://github.com/nhcooper123)


__To cite the paper__: 
> Coombs, E.J., Deaville, R., Sabin, R. C., Allan, L., O’Connell, M., Berrow, S., Smith, B., Brownlow, A., Ten Doeschate, M., Penrose, R., Williams, R., Perkins, M., Jepson, P., Cooper, N. What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years. (2019). X. XXX-XXX 


__To cite this repo__: 
> Coombs, E.J., Deaville, R., Sabin, R. C., Allan, L., O’Connell, M., Berrow, S., Smith, B., Brownlow, A., Ten Doeschate, M., Penrose, R., Williams, R., Perkins, M., Jepson, P., Cooper, N. What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years. (2019). GitHub: github.com/EllenJCoombs/cetacean-strandings-project Published version v.1.0. Zenodo. http://doi.org/10.5281/zenodo.2612908

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2612908.svg)](https://doi.org/10.5281/zenodo.2612908)


## Data :bar_chart: :chart_with_downwards_trend: 

These analyses use data from various studies. The data for possible environmental correlates of strandings can be found at these locations: 

- [HadISST data](https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html)
- [British Geological Survey](http://www.geomag.bgs.ac.uk/data_service/data/magnetic_indices/k_indices.html)
- [Storms](../blob/master/LICENSE) this will be a link to my own storms raw data and storm count
- [North Atlantic Oscillation](https://climatedataguide.ucar.edu/sites/default/files/nao_station_annual.txt)
- [Fishing catch](https://www.ices.dk/marine-data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx) 
- [Shipping data](https://www.gov.uk/government/collections/maritime-and-shipping-statistics)

Human population for the UK can be found here: 
[Human population data](../blob/master/LICENSE) this will be a link to my datasheet 


## Analysis :chart_with_upwards_trend: :whale2:
All code used to run analyses and make figures and tables is included in the `analysis/` folder. Before starting remember to either set your working directory to the stranding poject folder on your computer, or open an RStudio project from that folder. All of these files are in the RMarkdown format. You can click the Knit button in RStudio and create HTML versions using the code. 

* **01-clean-up-data-for-analysis.Rmd** This script takes the raw data from the NHM and CSIP databases. This includes cleaning and standardising variables in the Natural History Museum (NHM), Cetacean Stranding Investigation Programme (CSIP) datasets for binding, cleaning and formatting dates in the NHM and CSIP dataset and binding the 2 datasets (NHM + CSIP first, followed by IWDG which was aquired at a later date; see **02-cleaning-and-adding-irish-data.Rmd**). Throughout the code  the cleaned, merged final dataset of all 3 stranding datsets, which has also had rare species removed is called: `UK_and_Irish_sp.csv`

If you're pushed for time, you can download a cleaned dataset for running the Generalised Additive Models from: [data.nhm](https://data.nhm.ac.uk/dataset/what-can-cetacean-stranding-records-tell-us)

* **02-cleaning-and-adding-irish-data.Rmd** The first part of this script shows where the Irish data can be accessed and also how to clean the Irish dataset. The second part of this script binds the Irish dataset with the NHM + CSIP dataset (called `cleandatesnames.csv`)

* **03-final-data-clean.Rmd** This is the final stage of the data cleaning before analysis.
'UK_and_Irish_sp' is the final dataset used for analysis in our code. 

* **04-temporal-analysis-figs-1-3.Rmd** This code plots all cetacean strandings from 1913-2015 from the combined dataset (NHM + CSIP + IWDG) and produces Figures 1, 2, and 3 from the paper. 

* **05-spatial-analysis-figs-4-5.Rmd** Produces Figures 4 and 5. 

If you are pushed for time, you can skip RMarkdown documents 06 to 11 and use the model dataset called `all_strandings.csv` to run the analysis in **12-generalised-additive-models.Rmd**. See 01. above for details on downloading the model input file. 

* **06-correlates-data-population.Rmd** This code reads in, and cleans population data which forms part of the dataset of possible correlates of strandings.

* **07-correlates-data-storms.Rmd** This code reads in and cleans storm data which will form part of the dataset of possible correlates of strandings. 

* **08-correlates-data-geomagnetic.Rmd** This code reads in and cleans geomagnetic data which will form part of the dataset of possible correlates of strandings. 

* **09-correlates-data-sst.Rmd** This code reads in and cleans sea surface temperature (SST) data which will form part of the dataset of possible correlates of strandings. 

* **10-correlates-data-nao.Rmd** This code reads in NOA data from University Corporation for Atmospheric Research.

* **11-correlates-data-fishing-catch** This code reads in the fishing catch data from the International Council for the Exploration of the Sea (ICES)

* **12-correlates-data-for-model.Rmd** This code pulls together all of the correlates data e.g. Storms, and NAO, and puts them together along with 'Year', 'Species' and Total strandings' which is called `all_strandings.csv` in our code. This dataset is fed into the Generalised Additive Model (`13-generalised-additive-models.Rmd`)

* **13-generalised-additive-models.Rmd** This code runs the GAM which looks at all strandings (species per year), with an offset of population.

## Other folders :file_folder:
`paper-plots/` contains figures found in the manuscript 
`raw data/` contains gathered raw data on UK population, North Atlantic Oscillation, and UK and Irish storms from 1913-2015
`cleaned data/` contains cleaned geomagnetic data, population data nd other correlates data
`modelling/` contains code for models run in the supplementary material. See folder `modelling/supplementary-models`

## Session Info :clipboard:
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.


`─ Session info ───────────────────────────────────────────────────────────
 setting  value                       
 version  R version 3.5.1 (2018-07-02)
 os       macOS High Sierra 10.13.4   
 system   x86_64, darwin15.6.0        
 ui       RStudio                     
 language (EN)                        
 collate  en_GB.UTF-8                 
 ctype    en_GB.UTF-8                 
 tz       Europe/London               
 date     2019-04-02                  

─ Packages ───────────────────────────────────────────────────────────────
 package      * version date       lib source        
 acepack        1.4.1   2016-10-29 [1] CRAN (R 3.5.0)
 assertthat     0.2.0   2017-04-11 [1] CRAN (R 3.5.0)
 backports      1.1.2   2017-12-13 [1] CRAN (R 3.5.0)
 base64enc      0.1-3   2015-07-28 [1] CRAN (R 3.5.0)
 bindr          0.1.1   2018-03-13 [1] CRAN (R 3.5.0)
 bindrcpp     * 0.2.2   2018-03-29 [1] CRAN (R 3.5.0)
 broom          0.5.0   2018-07-17 [1] CRAN (R 3.5.0)
 cellranger     1.1.0   2016-07-27 [1] CRAN (R 3.5.0)
 checkmate      1.9.1   2019-01-15 [1] CRAN (R 3.5.2)
 cli            1.0.0   2017-11-05 [1] CRAN (R 3.5.0)
 cluster        2.0.7-1 2018-04-13 [1] CRAN (R 3.5.1)
 colorspace     1.3-2   2016-12-14 [1] CRAN (R 3.5.0)
 crayon         1.3.4   2017-09-16 [1] CRAN (R 3.5.0)
 data.table     1.12.0  2019-01-13 [1] CRAN (R 3.5.2)
 devtools       1.13.6  2018-06-27 [1] CRAN (R 3.5.0)
 digest         0.6.18  2018-10-10 [1] CRAN (R 3.5.0)
 dplyr        * 0.7.8   2018-11-10 [1] CRAN (R 3.5.0)
 forcats      * 0.3.0   2018-02-19 [1] CRAN (R 3.5.0)
 foreign        0.8-70  2017-11-28 [1] CRAN (R 3.5.1)
 Formula        1.2-3   2018-05-03 [1] CRAN (R 3.5.0)
 geosphere      1.5-7   2017-11-05 [1] CRAN (R 3.5.0)
 ggmap        * 2.6.1   2016-01-23 [1] CRAN (R 3.5.0)
 ggplot2      * 3.1.0   2018-10-25 [1] CRAN (R 3.5.0)
 glue           1.3.0   2018-07-17 [1] CRAN (R 3.5.0)
 gridExtra    * 2.3     2017-09-09 [1] CRAN (R 3.5.0)
 gtable       * 0.2.0   2016-02-26 [1] CRAN (R 3.5.0)
 haven          1.1.2   2018-06-27 [1] CRAN (R 3.5.0)
 hexbin       * 1.27.2  2018-01-15 [1] CRAN (R 3.5.0)
 Hmisc          4.2-0   2019-01-26 [1] CRAN (R 3.5.2)
 hms            0.4.2   2018-03-10 [1] CRAN (R 3.5.0)
 htmlTable      1.13.1  2019-01-07 [1] CRAN (R 3.5.2)
 htmltools      0.3.6   2017-04-28 [1] CRAN (R 3.5.0)
 htmlwidgets    1.2     2018-04-19 [1] CRAN (R 3.5.0)
 httr           1.3.1   2017-08-20 [1] CRAN (R 3.5.0)
 jpeg           0.1-8   2014-01-23 [1] CRAN (R 3.5.0)
 jsonlite       1.5     2017-06-01 [1] CRAN (R 3.5.0)
 knitr          1.20    2018-02-20 [1] CRAN (R 3.5.0)
 labeling       0.3     2014-08-23 [1] CRAN (R 3.5.0)
 lattice        0.20-35 2017-03-25 [1] CRAN (R 3.5.1)
 latticeExtra   0.6-28  2016-02-09 [1] CRAN (R 3.5.0)
 lazyeval       0.2.1   2017-10-29 [1] CRAN (R 3.5.0)
 lubridate      1.7.4   2018-04-11 [1] CRAN (R 3.5.0)
 magrittr       1.5     2014-11-22 [1] CRAN (R 3.5.0)
 mapproj        1.2.6   2018-03-29 [1] CRAN (R 3.5.0)
 maps         * 3.3.0   2018-04-03 [1] CRAN (R 3.5.0)
 Matrix         1.2-14  2018-04-13 [1] CRAN (R 3.5.1)
 memoise        1.1.0   2017-04-21 [1] CRAN (R 3.5.0)
 mgcv           1.8-24  2018-06-23 [1] CRAN (R 3.5.1)
 modelr         0.1.2   2018-05-11 [1] CRAN (R 3.5.0)
 munsell        0.5.0   2018-06-12 [1] CRAN (R 3.5.0)
 nlme           3.1-137 2018-04-07 [1] CRAN (R 3.5.1)
 nnet           7.3-12  2016-02-02 [1] CRAN (R 3.5.1)
 pillar         1.2.2   2018-04-26 [1] CRAN (R 3.5.0)
 pkgconfig      2.0.2   2018-08-16 [1] CRAN (R 3.5.0)
 plyr         * 1.8.4   2016-06-08 [1] CRAN (R 3.5.0)
 png            0.1-7   2013-12-03 [1] CRAN (R 3.5.0)
 proto          1.0.0   2016-10-29 [1] CRAN (R 3.5.0)
 purrr        * 0.2.5   2018-05-29 [1] CRAN (R 3.5.0)
 R6             2.3.0   2018-10-04 [1] CRAN (R 3.5.0)
 RColorBrewer   1.1-2   2014-12-07 [1] CRAN (R 3.5.0)
 Rcpp           1.0.0   2018-11-07 [1] CRAN (R 3.5.0)
 readr        * 1.1.1   2017-05-16 [1] CRAN (R 3.5.0)
 readxl         1.1.0   2018-04-20 [1] CRAN (R 3.5.0)
 reshape      * 0.8.8   2018-10-23 [1] CRAN (R 3.5.0)
 reshape2       1.4.3   2017-12-11 [1] CRAN (R 3.5.0)
 RgoogleMaps    1.4.3   2018-11-07 [1] CRAN (R 3.5.0)
 rjson          0.2.20  2018-06-08 [1] CRAN (R 3.5.0)
 rlang          0.3.0.1 2018-10-25 [1] CRAN (R 3.5.0)
 rpart          4.1-13  2018-02-23 [1] CRAN (R 3.5.1)
 rstudioapi     0.7     2017-09-07 [1] CRAN (R 3.5.0)
 rvest          0.3.2   2016-06-17 [1] CRAN (R 3.5.0)
 scales         1.0.0   2018-08-09 [1] CRAN (R 3.5.0)
 sessioninfo    1.1.1   2018-11-05 [1] CRAN (R 3.5.0)
 sp             1.3-1   2018-06-05 [1] CRAN (R 3.5.0)
 stringi        1.1.7   2018-03-12 [1] CRAN (R 3.5.0)
 stringr      * 1.3.0   2018-02-19 [1] CRAN (R 3.5.0)
 survival       2.42-3  2018-04-16 [1] CRAN (R 3.5.1)
 tibble       * 1.4.2   2018-01-22 [1] CRAN (R 3.5.0)
 tidyr        * 0.8.2   2018-10-28 [1] CRAN (R 3.5.0)
 tidyselect     0.2.5   2018-10-11 [1] CRAN (R 3.5.0)
 tidyverse    * 1.2.1   2017-11-14 [1] CRAN (R 3.5.0)
 viridis      * 0.5.1   2018-03-29 [1] CRAN (R 3.5.0)
 viridisLite  * 0.3.0   2018-02-01 [1] CRAN (R 3.5.0)
 withr          2.1.2   2018-03-15 [1] CRAN (R 3.5.0)
 xml2           1.2.0   2018-01-24 [1] CRAN (R 3.5.0)
 yaml           2.1.18  2018-03-08 [1] CRAN (R 3.5.0)`


