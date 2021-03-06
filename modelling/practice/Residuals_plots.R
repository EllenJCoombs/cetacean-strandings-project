#Residual plots for covariates 


load("Model_for_Dave.Rdata") #Not sure I need this??

#From cleaned data 
all_strandings <- read.csv("all_strandings.csv")
par(mfrow=c(1,1))

#SST
sst_bins <- cut(all_strandings$Max_SST, c(14,15,16,17,18))
sst_resid_data <- data.frame(SST=sst_bins, resids=residuals(All_strandc1))
boxplot(resids~SST, data=sst_resid_data)


#Storms
storm_bins <- cut(all_strandings$Storms, c(0,1,2,3,4,5,6,7,8))
storm_resid_data <- data.frame(Storms=storm_bins, resids=residuals(All_strandc1))
boxplot(resids~Storms, data=storm_resid_data)


#K-index (Geomagnetic data)
geom_bins <- cut(all_strandings$Max_K_index, c(5,6,7,8,9))
geom_resid_data <- data.frame(K_index=geom_bins, resids=residuals(All_strandc1))
boxplot(resids~K_index, data=geom_resid_data)


#NAO_index 
NAO_bins <- cut(all_strandings$NAO_index, c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5))
NAO_resid_data <- data.frame(NAO=NAO_bins, resids=residuals(All_strandc1))
boxplot(resids~NAO, data=NAO_resid_data)



