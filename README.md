# What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years

:whale2: :chart_with_upwards_trend:



__Authors:__
[Ellen J. Coombs](mailto:ellen.coombs.14@ucl.ac.uk), Rob Deaville, Richard C. Sabin, Richard C. Sabin, Louise Allan, Mick O’Connell, Simon Berrow, Brian Smith, Andrew Brownlow, Mariel Ten Doeschate, Rod Penrose, Ruth Williams, Matthew Perkins, Paul Jepson, and [Natalie Cooper](https://github.com/nhcooper123)


__To cite the paper__: 
> Coombs, E.J., Deaville, R., Sabin, R. C., Allan, L., O’Connell, M., Berrow, S., Smith, B., Brownlow, A., Ten Doeschate, M., Penrose, R., Williams, R., Perkins, M., Jepson, P., Cooper, N. What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years. 2019. Marine Mammal Science, 1–29. 


Available at: https://onlinelibrary.wiley.com/doi/full/10.1111/mms.12610


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

* **05-spatial-analysis-figs-4-5.Rmd** Produces Figures 4 and 5 from the paper. 

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

## License :page_with_curl:
This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/EllenJCoombs/cetacean-strandings-project/blob/master/LICENSE) file for details

## Session Info :clipboard:
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication. 

```{r}
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

─ Packages ───────────────────────────────────────────────────────
 
 package     * version date       lib source        
 assertthat    0.2.0   2017-04-11 [1] CRAN (R 3.5.0)
 bindr         0.1.1   2018-03-13 [1] CRAN (R 3.5.0)
 bindrcpp      0.2.2   2018-03-29 [1] CRAN (R 3.5.0)
 cli           1.0.0   2017-11-05 [1] CRAN (R 3.5.0)
 clipr         0.4.1   2018-06-23 [1] CRAN (R 3.5.0)
 colorspace    1.3-2   2016-12-14 [1] CRAN (R 3.5.0)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.0)
 dplyr         0.7.8   2018-11-10 [1] CRAN (R 3.5.0)
 ggplot2       3.1.0   2018-10-25 [1] CRAN (R 3.5.0)
 glue          1.3.0   2018-07-17 [1] CRAN (R 3.5.0)
 gtable        0.2.0   2016-02-26 [1] CRAN (R 3.5.0)
 lattice       0.20-35 2017-03-25 [1] CRAN (R 3.5.1)
 lazyeval      0.2.1   2017-10-29 [1] CRAN (R 3.5.0)
 magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.0)
 Matrix        1.2-14  2018-04-13 [1] CRAN (R 3.5.1)
 mgcv          1.8-24  2018-06-23 [1] CRAN (R 3.5.1)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.0)
 nlme          3.1-137 2018-04-07 [1] CRAN (R 3.5.1)
 pillar        1.2.2   2018-04-26 [1] CRAN (R 3.5.0)
 pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.0)
 plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.0)
 purrr         0.2.5   2018-05-29 [1] CRAN (R 3.5.0)
 R6            2.3.0   2018-10-04 [1] CRAN (R 3.5.0)
 Rcpp          1.0.0   2018-11-07 [1] CRAN (R 3.5.0)
 rlang         0.3.0.1 2018-10-25 [1] CRAN (R 3.5.0)
 rstudioapi    0.7     2017-09-07 [1] CRAN (R 3.5.0)
 scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.0)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.0)
 tibble        1.4.2   2018-01-22 [1] CRAN (R 3.5.0)
 tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.0)
 withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.0)
 yaml          2.1.18  2018-03-08 [1] CRAN (R 3.5.0)

```
