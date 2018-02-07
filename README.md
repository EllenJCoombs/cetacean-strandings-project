# What can cetacean stranding records tell us? A study of UK and Irish cetacean diversity over the past 100 years


Author(s): 


To cite the paper: 


To cite this repo: 


Image here 
![_whale_image_](web address or folder here)


##Data 

These analyses use data from various studies. The data for possible environmental correlates of strandings can be found at these locations: 

- [HadISST data](https://Metoffice.com)
- [British Geological Survey](link...)
- [Storms](../blob/master/LICENSE) this will be a link to my own storms raw data and storm count
- [North Atlantic Oscillation](https://climatedataguide.ucar.edu/sites/default/files/nao_station_annual.txt)

Human population for the UK can be found here: 
-[Human population data](../blob/master/LICENSE) this will be a link to my datasheet 


##Analysis
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


##Other folders 
`functions/` contains functions required by the code in the `analysis/` folder 
`figures/` contains figures found in the manuscript 
`raw data/` contains gathered raw data and sources on UK population and UK and Irish storms from 1913-2015
`cleaned data/` contains cleaned stranding dataset with the NHM, CSIP and IWDG datasets combined, and rare species removed. Also contains the cleaned correlates data; as well as the cleaned dataset (which is all of the correlates combined + year, species and total strandings) 















#Analyses



