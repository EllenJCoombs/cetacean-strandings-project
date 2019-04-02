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


**R version 3.5.1 (2018-07-02)**

**Platform:** x86_64-apple-darwin15.6.0 (64-bit) 

**locale:**
en_GB.UTF-8||en_GB.UTF-8||en_GB.UTF-8||C||en_GB.UTF-8||en_GB.UTF-8

**attached base packages:** 
_grid_, _stats_, _graphics_, _grDevices_, _utils_, _datasets_, _methods_ and _base_

**other attached packages:** 
_pander(v.0.6.3)_, _sessioninfo(v.1.1.1)_, _reshape(v.0.8.8)_, _bindrcpp(v.0.2.2)_, _forcats(v.0.3.0)_, _stringr(v.1.3.0)_, _purrr(v.0.2.5)_, _readr(v.1.1.1)_, _tibble(v.1.4.2)_, _tidyverse(v.1.2.1)_, _ggmap(v.2.6.1)_, _hexbin(v.1.27.2)_, _maps(v.3.3.0)_, _gtable(v.0.2.0)_, _gridExtra(v.2.3)_, _viridis(v.0.5.1)_, _viridisLite(v.0.3.0)_, _ggplot2(v.3.1.0)_, _plyr(v.1.8.4)_, _tidyr(v.0.8.2)_ and _dplyr(v.0.7.8)_

**loaded via a namespace (and not attached):** 
_httr(v.1.3.1)_, _jsonlite(v.1.5)_, _splines(v.3.5.1)_, _modelr(v.0.1.2)_, _Formula(v.1.2-3)_, _assertthat(v.0.2.0)_, _sp(v.1.3-1)_, _latticeExtra(v.0.6-28)_, _cellranger(v.1.1.0)_, _yaml(v.2.1.18)_, _pillar(v.1.2.2)_, _backports(v.1.1.2)_, _lattice(v.0.20-35)_, _glue(v.1.3.0)_, _digest(v.0.6.18)_, _RColorBrewer(v.1.1-2)_, _checkmate(v.1.9.1)_, _rvest(v.0.3.2)_, _colorspace(v.1.3-2)_, _htmltools(v.0.3.6)_, _Matrix(v.1.2-14)_, _clipr(v.0.4.1)_, _devtools(v.1.13.6)_, _pkgconfig(v.2.0.2)_, _broom(v.0.5.0)_, _haven(v.1.1.2)_, _scales(v.1.0.0)_, _jpeg(v.0.1-8)_, _htmlTable(v.1.13.1)_, _mgcv(v.1.8-24)_, _withr(v.2.1.2)_, _nnet(v.7.3-12)_, _lazyeval(v.0.2.1)_, _cli(v.1.0.0)_, _proto(v.1.0.0)_, _readxl(v.1.1.0)_, _survival(v.2.42-3)_, _magrittr(v.1.5)_, _crayon(v.1.3.4)_, _memoise(v.1.1.0)_, _nlme(v.3.1-137)_, _xml2(v.1.2.0)_, _foreign(v.0.8-70)_, _tools(v.3.5.1)_, _data.table(v.1.12.0)_, _hms(v.0.4.2)_, _geosphere(v.1.5-7)_, _RgoogleMaps(v.1.4.3)_, _munsell(v.0.5.0)_, _cluster(v.2.0.7-1)_, _compiler(v.3.5.1)_, _rlang(v.0.3.0.1)_, _rstudioapi(v.0.7)_, _rjson(v.0.2.20)_, _htmlwidgets(v.1.2)_, _base64enc(v.0.1-3)_, _labeling(v.0.3)_, _reshape2(v.1.4.3)_, _R6(v.2.3.0)_, _lubridate(v.1.7.4)_, _knitr(v.1.20)_, _bindr(v.0.1.1)_, _Hmisc(v.4.2-0)_, _stringi(v.1.1.7)_, _Rcpp(v.1.0.0)_, _mapproj(v.1.2.6)_, _rpart(v.4.1-13)_, _acepack(v.1.4.1)_, _png(v.0.1-7)_ and _tidyselect(v.0.2.5)_
