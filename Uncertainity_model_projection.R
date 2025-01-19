#########################################################################
# Code to calculate uncertainty in model projections....................#
#########################################################################
rm(list = ls())

# Setting start -----------------------------------------------------------
#load required libraries
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)  
library(rstudioapi)
library(rgeos)

#locate the folder of the code
setwd(dirname(getActiveDocumentContext()$path))

#locate the main folder
setwd("D:\\G\\Data")

#set the main folder as input directory
indir <- getwd()

#set output folder
outdir <- paste0(getwd(),"\\Output\\Mean\\")

#read shapefile of Bangladesh basin
SHP_WGS  <- readOGR(paste0(indir,"\\Shapefile\\gandaki_WGS.shp"))
SHP_buffer <- gBuffer(SHP_WGS,width = 1)

#RCPs and variables
rcps <- c("RCP4.5","RCP8.5")
vars <- c("pr","tas")
#setting end 

#calculating delta change for all models
for (rcp in rcps)
{
  #setting filelist as per the RCP
  if(rcp=="RCP4.5"){r <- "rcp45"}else{r <- "rcp85"}
  filelist <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\pr\\"))
  #creating data frame to store seasonal data
  delta_change_all <- data.frame()
  for (i in 1:length(filelist))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\pr\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_1976"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    #removing NA values
    nc_ref[nc_ref>2000] <- NA
    #calculating annual sum 
    nc_ref_annual <- mean(stackApply(nc_ref,rep(seq(1,30),each=12),fun=sum))
    #extracting seasonal changes
    annual_ref <- round(extract(nc_ref_annual,SHP_WGS,fun=mean),2)
    #seperating future period (2070-2099)
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    #removing NA values
    nc_fut[nc_fut>2000] <- NA
    #calculating annual sum 
    nc_fut_annual <- mean(stackApply(nc_fut,rep(seq(1,30),each=12),fun=sum))
    #extracting future sum
    annual_fut <- round(extract(nc_fut_annual,SHP_WGS,fun=mean),2)
    
    #calculating changes in annual value
    fut_change <- round((annual_fut/annual_ref-1)*100,2)
    #setting rownames
    rownames(fut_change) <- model
    #saving the seasonal data to the data frame
    delta_change_all <- rbind(delta_change_all,fut_change)
    print(paste(i,"of 16 model done for pr of",rcp))
  }
  #setting column names
  colnames(delta_change_all) <- c("Annual")
  #saving the data to a csv file
  write.csv(delta_change_all,paste0(outdir,paste0(rcp,"_pr_delta_change_all_models.csv")))
}



#for temperature
for (rcp in rcps)
{
  #setting filelist as per the RCP
  if(rcp=="RCP4.5"){r <- "rcp45"}else{r <- "rcp85"}
  filelist <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\tas\\"))
  #creating data frame to store seasonal data
  delta_change_all <- data.frame()
  for (i in 1:length(filelist))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\tas\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_v"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    #removing NA values
    nc_ref[nc_ref>2000] <- NA
    #calculating annual average
    nc_ref_annual <- mean(stackApply(nc_ref,rep(seq(1,30),each=12),fun=mean))
    #extracting seasonal changes
    annual_ref <- round(extract(nc_ref_annual,SHP_WGS,fun=mean),2)
    #seperating future period (2075-2099)
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    #removing NA values
    nc_fut[nc_fut>2000] <- NA
    #calculating annual sum 
    nc_fut_annual <- mean(stackApply(nc_fut,rep(seq(1,30),each=12),fun=mean))
    #extracting future sum
    annual_fut <- round(extract(nc_fut_annual,SHP_WGS,fun=mean),2)
    
    #calculating changes in annual value
    fut_change <- round((annual_fut-annual_ref),2)
    #setting rownames
    rownames(fut_change) <- model
    #saving the seasonal data to the data frame
    delta_change_all <- rbind(delta_change_all,fut_change)
    print(paste(i,"of 16 model done for tas of",rcp))
  }
  #setting column names
  colnames(delta_change_all) <- c("Annual")
  #saving the data to a csv file
  write.csv(delta_change_all,paste0(outdir,paste0(rcp,"_tas_delta_change_all_models.csv")))
}


# Uncertainty analysis ----------------------------------------------------
#selected CORDEX models
select_pr_model <- read.csv(paste0(outdir,"selected_model_pr.csv"),row.names = 1)
select_tas_model <- read.csv(paste0(outdir,"selected_model_tas.csv"),row.names = 1)

#reading delta change for all models
RCP4.5_pr_delta_change <- read.csv(paste0(outdir,"RCP4.5_pr_delta_change_all_models.csv"),row.names = 1)
RCP4.5_tas_delta_change <- read.csv(paste0(outdir,"RCP4.5_tas_delta_change_all_models.csv"),row.names = 1)
RCP8.5_pr_delta_change <- read.csv(paste0(outdir,"RCP8.5_pr_delta_change_all_models.csv"),row.names = 1)
RCP8.5_tas_delta_change <- read.csv(paste0(outdir,"RCP8.5_tas_delta_change_all_models.csv"),row.names = 1)

#combining delta values for precipitation
pr_delta_change <- data.frame("RCP4.5"=RCP4.5_pr_delta_change,"RCP8.5"=RCP8.5_pr_delta_change)
rownames(pr_delta_change) <- rownames(RCP4.5_pr_delta_change) 

#seperating delta values for selected models
pr_delta_change_sel <- pr_delta_change[which(rownames(pr_delta_change) %in% rownames(select_pr_model)),]

#setting intervals in the plot
v3 <- c(seq(1.5,3.5,by=2))
v2 <- c("End-century")
v4 <- c("RCP4.5","RCP8.5")

#plotting scatter plot with max-min error bars
{
  png(paste0(outdir,"Precipitation_scatterplot.png"),width = 2000, height = 2000, units = "px", pointsize = 8,
      bg = "white", res = 600)
  #initiating the plot diagram
  boxplot((pr_delta_change),xlab=NA,ylab=NA,show.names=F,border=c(rgb(0,0,1,0)),boxwex=0,ylim=c(-20,60),range=4)
  #adding line between max and min delta change value using all models for RCP4.5
  segments(0.9, min((pr_delta_change)[,1]), 0.9, max((pr_delta_change)[,1]), col=rgb(0,0,0,.4))
  #adding line between max and min delta change value using all models for RCP8.5
  segments(1.9, min((pr_delta_change)[,2]), 1.9, max((pr_delta_change)[,2]), col=rgb(0,0,0,.4))
  #adding average delta change value
  points(c(0.9,1.9), c(mean((pr_delta_change)[,1]),mean((pr_delta_change)[,2])),pch=19,col=rgb(0,0,0,.4))
  #adding first quartile delta change value
  points(c(0.9,1.9), c(quantile(((pr_delta_change)[,1]),.25),quantile(((pr_delta_change)[,2]),.25)),pch=4,cex=.5,col=rgb(0,0,0,.4))
  #adding third quartile delta change value
  points(c(0.9,1.9), c(quantile(((pr_delta_change)[,1]),.75),quantile(((pr_delta_change)[,2]),.75)),pch=4,cex=.5,col=rgb(0,0,0,.4))
  par(new=T)
  #adding line between max and min delta change value using selected models for RCP4.5
  segments(1.1, min((pr_delta_change_sel)[,1]), 1.1, max((pr_delta_change_sel)[,1]), col="blue")
  #adding line between max and min dalta change value using selected models for RCP8.5
  segments(2.1, min((pr_delta_change_sel)[,2]), 2.1, max((pr_delta_change_sel)[,2]), col="red")
  #adding average delta change value
  points(c(1.1,2.1), c(mean((pr_delta_change_sel)[,1]),mean((pr_delta_change_sel)[,2])),pch=19)
  #adding first quartile delta change value
  points(c(1.1,2.1), c(quantile(((pr_delta_change_sel)[,1]),.25),quantile(((pr_delta_change_sel)[,2]),.25)),pch=4,cex=.5)
  #adding third quartile delta change value
  points(c(1.1,2.1), c(quantile(((pr_delta_change_sel)[,1]),.75),quantile(((pr_delta_change_sel)[,2]),.75)),pch=4,cex=.5)
  #adding title to plot
  title(main="Scatter plot with min-max error bars",font.main=2,cex.main=1)
  #y-axis label
  mtext("Change in precipitation (%)",side = 2,line=2)
  #x-axis label
  axis(1,at=c(1,2),labels=F)
  legend(0.75,-24, c("RCP4.5"),text.col = "blue",cex = .8,bty = "n",xpd = T)
  legend(1.75,-24, c("RCP8.5"),text.col = "red",cex = .8,bty = "n",xpd = T)
  legend(0.75,-30, c("Grey line shows range using all models"),text.col = rgb(0,0,0,.4),cex = .8,bty = "n",xpd = T)
  legend(0.6,-34, c("Colored line shows range using selected models"),text.col = "black",cex = .8,bty = "n",xpd = T)
  dev.off()
  
}

#combining delta values for temperature
tas_delta_change <- data.frame("RCP4.5"=RCP4.5_tas_delta_change,"RCP8.5"=RCP8.5_tas_delta_change)
rownames(tas_delta_change) <- rownames(RCP4.5_tas_delta_change) 

#seperating delta values for selected models
tas_delta_change_sel <- tas_delta_change[which(rownames(tas_delta_change) %in% rownames(select_tas_model)),]
{
  png(paste0(outdir,"Temperature_scatterplot.png"),width = 2000, height = 2000, units = "px", pointsize = 8,
      bg = "white", res = 600)
  boxplot((tas_delta_change),xlab=NA,ylab=NA,show.names=F,border=c(rgb(0,0,1,0)),boxwex=0,ylim=c(1,7),range=4)
  segments(0.9, min((tas_delta_change)[,1]), 0.9, max((tas_delta_change)[,1]), col=rgb(0,0,0,.4))
  segments(1.9, min((tas_delta_change)[,2]), 1.9, max((tas_delta_change)[,2]), col=rgb(0,0,0,.4))
  points(c(0.9,1.9), c(mean((tas_delta_change)[,1]),mean((tas_delta_change)[,2])),pch=19,col=rgb(0,0,0,.4))
  points(c(0.9,1.9), c(quantile(((tas_delta_change)[,1]),.25),quantile(((tas_delta_change)[,2]),.25)),pch=4,cex=.5,col=rgb(0,0,0,.4))
  points(c(0.9,1.9), c(quantile(((tas_delta_change)[,1]),.75),quantile(((tas_delta_change)[,2]),.75)),pch=4,cex=.5,col=rgb(0,0,0,.4))
  par(new=T)
  segments(1.1, min((tas_delta_change_sel)[,1]), 1.1, max((tas_delta_change_sel)[,1]), col="blue")
  segments(2.1, min((tas_delta_change_sel)[,2]), 2.1, max((tas_delta_change_sel)[,2]), col="red")
  points(c(1.1,2.1), c(mean((tas_delta_change_sel)[,1]),mean((tas_delta_change_sel)[,2])),pch=19)
  points(c(1.1,2.1), c(quantile(((tas_delta_change_sel)[,1]),.25),quantile(((tas_delta_change_sel)[,2]),.25)),pch=4,cex=.5)
  points(c(1.1,2.1), c(quantile(((tas_delta_change_sel)[,1]),.75),quantile(((tas_delta_change_sel)[,2]),.75)),pch=4,cex=.5)
  
  title(main="Scatter plot with min-max error bars",font.main=2,cex.main=1)
  mtext("Change in temperature (\u00B0C)",side = 2,line=2)
  axis(1,at=c(1,2),labels=F)
  legend(0.75,.4, c("RCP4.5"),text.col = "blue",cex = .8,bty = "n",xpd = T)
  legend(1.75,.4, c("RCP8.5"),text.col = "red",cex = .8,bty = "n",xpd = T)
  legend(0.75,0, c("Grey line shows range using all models"),text.col = rgb(0,0,0,.4),cex = .8,bty = "n",xpd = T)
  legend(0.6,-.4, c("Colored line shows range using selected models"),text.col = "black",cex = .8,bty = "n",xpd = T)
  dev.off()
}

#end of code..............#
