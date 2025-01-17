#########################################################################
# Extract and plot data for area of interest............................#
#########################################################################
rm(list = ls())

# Setting start
#install packages if not in the library
list.of.packages <- c("raster","rgdal","rgeos",'ggplot2','dplyr',"ncdf4","rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load required libraries
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)  
library(rstudioapi)

#locate the folder of the code
setwd(dirname(getActiveDocumentContext()$path))
#locate the main folder
setwd("D:\\G\\Data")
#get the main folder
getwd()
#set the main folder as input directory
indir <- getwd()
#set output folder
outdir <- paste0(getwd(),"\\Output\\Mean\\")


#read shapefile of area of interest
SHP_WGS  <- readOGR(paste0(indir,"\\Shapefile\\gandaki_WGS.shp"))
plot(SHP_WGS)
SHP_buffer <- gBuffer(SHP_WGS,width = 1)
plot(SHP_buffer)

#RCPs and variables
rcps <- c("RCP4.5","RCP8.5")
vars <- c("pr","tas")

#color palette for the plot
col_p <- colorRampPalette((c("yellow","#7fcdbb","#3182bd","#1D46A9","navyblue")))(255)
col_t <- colorRampPalette((c("green","yellow","red")))(255)
#setting end 

#list files in the folder
filelist_p <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\RCP4.5\\",vars[1]))

zmin <- 0;zmax <- 6000
#process for all files
{
  png(paste0(outdir,"Gandaki_",vars[1],"_historical_period_all_models.png"),width = 2500, height = 1500, units = "px", pointsize = 12, bg = "white", res = 300)
  #setting no. of rows and columns in the plot
  par(mfrow=c(3,6))
  #setting margins in the plot
  par(mgp=c(0.4,.4,0),mar=c(1,1,2,1))
  
  for (i in 1:length(filelist_p))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\RCP4.5\\",vars[1],"\\",filelist_p[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist_p[i],"WAS-44_|_v"))[2],"_rcp45_r1i1p1")),collapse="")
    nc_ref <- ncfile[[1:360]]
    nc_year <- stackApply(nc_ref,rep(seq(1:30),each=12),fun=sum)
    ref_avg <- mean(nc_year)
    ref_avg[ref_avg<zmin] <- zmin
    ref_avg[ref_avg>zmax] <- zmax
    plot(crop(ref_avg,SHP_buffer),col=col_p,main=model,legend=F,zlim=c(zmin,zmax),cex.main=.5,cex.axis=.5)
    plot(SHP_WGS,add=T)
    
  }
  #adding legend to the plot
  plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  par(mar=c(1,1,2,8))
  plot(ref_avg,col=col_p,zlim=c(zmin,zmax),legend.only=T,legend.width=2,
       legend.args = list(text = 'Precipitation (mm)', side = 4,font = 1, line = 2.5, cex = 0.4))
  dev.off()
}


### Now for the temperature datasets ####
#list files in the folder
filelist_t <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\RCP4.5\\",vars[2]))

#plot of all the models
zmin<- 10;zmax <- 30; 
{
  png(paste0(outdir,"Gandaki_",vars[2],"_historical_period_all_models.png"),width = 2500, height = 1500, units = "px", pointsize = 12, bg = "white", res = 300)
  par(mfrow=c(3,6))
  par(mgp=c(0.4,.4,0),mar=c(1,1,2,1))
  
  for (i in 1:length(filelist_t))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\RCP4.5\\",vars[2],"\\",filelist_t[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist_t[i],"WAS-44_|_v"))[2],"_rcp45_r1i1p1")),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    #converting units from kgm-1s-1 to mm/year
    nc_year <- stackApply(nc_ref,rep(seq(1:30),each=12),fun=mean)
    ref_avg <- mean(nc_year)
    ref_avg[ref_avg<zmin] <- zmin
    ref_avg[ref_avg>zmax] <- zmax
    plot(crop(ref_avg,SHP_buffer),col=col_t,main=model,legend=F,zlim=c(zmin,zmax),cex.main=.5,cex.axis=.5)
    plot(SHP_WGS,add=T)
    
  }
  plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  par(mar=c(1,1,2,8))
  plot(ref_avg,col=col_t,zlim=c(zmin,zmax),legend.only=T,legend.width=2, 
       legend.args = list(text = 'Mean temperature (\u00B0C)', side = 4,font = 1, line = 2.5, cex = 0.4))
  dev.off()
}


# Reference dataset (1976-2005) for Nepal  ---------------------------------------
### Precipitation ###
# read annual file
aphro_pr_year <- stack(paste0(indir,"\\Reference_data\\yearly_gandaki_pre25_V1101_1976-2005.nc"),varname="precip")

#plot the annual file
plot(crop(aphro_pr_year[[1]],SHP_buffer),interpolate=T)
plot(SHP_WGS,add=T)

#extract tha annnual values for Nepal
pr_year <- extract(aphro_pr_year,SHP_WGS,fun=mean)
colnames(pr_year) <- seq(1976,2005,1)
rownames(pr_year) <- "aphrodite"

#writing the annual data
write.csv(pr_year,paste0(outdir,"ref_pr_annual.csv"))

# read monthly file
aphro_pr_month_all <- stack(paste0(indir,"\\Reference_data\\monthly_gandaki_pre25_V1101_1976-2005.nc"),varname="precip")
tail(aphro_pr_month_all)
aphro_pr_month <- stackApply(aphro_pr_month_all,rep(seq(1:12),30),fun=mean)
names(aphro_pr_month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#plot monthly precipitation and save the plot
#saving the plot
png(paste0(outdir,"\\aphro_pr_month_sep.png"),width = 2400, height = 1600, units = "px", pointsize = 12, bg = "white", res = 300)
#specify no. of rows and columns in plot area
par(mfrow=c(3,4))
#specify margins
par(mar=c(2,2,2,4.8))
for (i in 1:12)
{
  #plotting each month seperately
  plot(crop(subset(aphro_pr_month,i),SHP_buffer),zlim=c(0,ceiling(cellStats(subset(aphro_pr_month,i),max)/50)*50),col=col_p,main=names(aphro_pr_month)[i]) 
  plot(SHP_WGS,add=T)
}
dev.off()

#extracting of the monthly data
pr_month <- data.frame(round(extract(aphro_pr_month,SHP_WGS,fun=mean),2))
colnames(pr_month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
rownames(pr_month) <- "aphrodite"

#writing the monthly data
write.csv(pr_month,paste0(outdir,"ref_pr_monthly.csv"))


### Temperature ###
# read annual file
aphro_tas_year <- stack(paste0(indir,"\\Reference_data\\yearly_gandaki_tem_25_V1808_1976-2005.nc"),varname="tave")

#plot the annual file
plot(crop(aphro_tas_year[[1]],SHP_buffer))
plot(SHP_WGS,add=T)
####
#Fun Exercise_6: Can you change the color of the plot. 
###

#extract tha annual values for Nepal
tas_year <- round(extract(aphro_tas_year,SHP_WGS,fun=mean),2)
colnames(tas_year) <- seq(1976,2005,1)
rownames(tas_year) <- "aphrodite"
#writing the annual data
write.csv(tas_year,paste0(outdir,"ref_tas_annual.csv"))

# read monthly file
aphro_tas_month_all <- stack(paste0(indir,"\\Reference_data\\monthly_gandaki_tem_25_V1808_1976-2005.nc"),varname="tave")
aphro_tas_month <- stackApply(aphro_tas_month_all,rep(seq(1:12),30),fun=mean)
names(aphro_tas_month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#plot monthly precipitation and save the plot
#saving the plot
png(paste0(outdir,"\\aphro_tas_month_sep.png"),width = 2400, height = 1600, units = "px", pointsize = 12, bg = "white", res = 300)
par(mfrow=c(3,4))
par(mar=c(2,2,2,4.8))
for (i in 1:12)
{
  plot(crop(subset(aphro_tas_month,i),SHP_buffer),zlim=c(-20,35),col=col_t,main=names(aphro_tas_month)[i]) 
  plot(SHP_WGS,add=T)
}
dev.off()

#extracting of the monthly data
tas_month <- data.frame(round(extract(aphro_tas_month,SHP_WGS,fun=mean),2))
colnames(tas_month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
rownames(tas_month) <- "aphrodite"

#writing the monthly data
write.csv(tas_month,paste0(outdir,"ref_tas_monthly.csv"))


# annual plot 
aphro_pr_annual <- mean(aphro_pr_year)
aphro_tas_annual <- mean(aphro_tas_year)
#save annual plot
png(paste0(outdir,"aphro_annual.png"),width = 2400, height = 1200, units = "px", pointsize = 12, bg = "white", res = 300)
par(mfrow=c(1,2))
par(mar=c(3.2,3,3,4.8))

plot(crop(aphro_pr_annual,SHP_buffer),zlim=c(0,8000),col=colorRampPalette((c("yellow","#7fcdbb","#3182bd","#1D46A9","navyblue")))(255),main="Annual Precipitation (mm)")
contour(aphro_pr_annual,col="white",lwd=0.5,add=TRUE) 
plot(SHP_WGS,add=T)

plot(crop(aphro_tas_annual,SHP_buffer),zlim=c(-10,30),col=colorRampPalette((c("green","yellow","red")))(255),main="Annual mean temperature (\u00B0C)")
contour(aphro_tas_annual,col="black",lwd=0.3,add=TRUE) 
plot(SHP_WGS,add=T)
dev.off()


#read all CORDEX file for your area of interest
for (rcp in rcps)
{
  if(rcp=="RCP4.5"){r <- "rcp45"}else{r <- "rcp85"}
  
  for (var in vars)
  {
    #list files in the folder
    filelist <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\",var))
    mon_12_all <- data.frame()
    year_data_all <- as.data.frame(matrix(0, ncol = 125))
    colnames(year_data_all) <- seq(1976,2100,1)
    #setting appropriate stat 
    if(var=="pr"){stat <- "sum"}else{stat <- "mean"}
    for (i in 1:length(filelist))
    {
      ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\",var,"\\",filelist[i]))
      # ncfile[ncfile>10000] <- NA
      model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_v"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
      nc_ref <- ncfile[[1:360]]
      nc_mon <- stackApply(nc_ref,rep(seq(1,12),30),fun=mean)
      mon_ref <- extract(nc_mon,SHP_WGS,fun=mean)
      row.names(mon_ref) <- model
      mon_12_all <- rbind(mon_12_all,mon_ref)
      nc_year <- stackApply(ncfile,rep(seq(1:(nlayers(ncfile)/12)),each=12),fun=stat)
      year_data <- data.frame(round(extract(nc_year,SHP_WGS,fun=mean),2))
      colnames(year_data) <- seq(1976,(1975+length(year_data)),1)
      year_data_all <- plyr::rbind.fill(year_data_all,year_data)
      print(paste(i,"of 16 model done for",var,"of",rcp))
    }
    year_data_all <- year_data_all[-1,]
    colnames(mon_12_all) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    row.names(year_data_all) <- row.names(mon_12_all)
    write.csv(mon_12_all,paste0(outdir,var,"_CORDEX_monthly.csv"))
    write.csv(year_data_all,paste0(outdir,rcp,"_",var,"_CORDEX_all_year.csv"))
    
  }
}

# Bias check --------------------------------------------------------------
#reading reference dataset for bias calculation
ref_pr <- read.csv(paste0(outdir,"ref_pr_monthly.csv"),row.names = 1)
ref_tas <- read.csv(paste0(outdir,"ref_tas_monthly.csv"),row.names = 1)

#reading CORDEX dataset for bias calculation
CORDEX_pr <- read.csv(paste0(outdir,"pr_CORDEX_monthly.csv"),row.names = 1)
CORDEX_tas <- read.csv(paste0(outdir,"tas_CORDEX_monthly.csv"),row.names = 1)

#combining ref and CORDEX dataset for bias calculation
all_pr <- rbind(ref_pr,CORDEX_pr)
all_tas <- rbind(ref_tas,CORDEX_tas) 

#checking plot for all models for precipitation
matplot(t(all_pr),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1)
#adding reference dataset
lines(as.numeric(ref_pr),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"pr_all_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(all_pr),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly precipitation (mm)")
  lines(as.numeric(ref_pr),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 16 CORDEX models")
  dev.off()
  
}

#checking plot for all models for temperature
matplot(t(all_tas),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1)
lines(as.numeric(ref_tas),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"tas_all_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(all_tas),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly temperature (\u00B0C)")
  lines(as.numeric(ref_tas),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 16 CORDEX models")
  dev.off() 
}

# seasonal bias calculation-----------------------------------------------------------
#calculate seasonal values for all model 
all_pr_seasonal  <- all_pr %>% summarise(Winter = rowSums(select(., c(Jan,Feb,Dec))),
                                         Premon = rowSums(select(., c(Mar,Apr,May))),
                                         Monson = rowSums(select(., c(Jun,Jul,Aug,Sep))),
                                         Posmon = rowSums(select(., c(Oct,Nov))))

all_tas_seasonal  <- all_tas %>% summarise(Winter = rowMeans(select(., c(Jan,Feb,Dec))),
                                           Premon = rowMeans(select(., c(Mar,Apr,May))),
                                           Monson = rowMeans(select(., c(Jun,Jul,Aug,Sep))),
                                           Posmon = rowMeans(select(., c(Oct,Nov))))
#calculate seasonal contribution to total precipitation
all_pr_seasonal_per <- round(all_pr_seasonal/rowSums(all_pr_seasonal)*100,2)
#calculate difference from the reference value
all_pr_seasonal_change <- all_pr_seasonal_per %>% summarise(diff_winter = round(abs(`Winter`-Winter[1]),2),
                                                            diff_premon = round(abs(`Premon`-Premon[1]),2),
                                                            diff_monson = round(abs(`Monson`-Monson[1]),2),
                                                            diff_posmon = round(abs(`Posmon`-Posmon[1]),2))


all_tas_seasonal_change <- all_tas_seasonal %>% summarise(diff_winter = round(abs(`Winter`-Winter[1]),2),
                                                          diff_premon = round(abs(`Premon`-Premon[1]),2),
                                                          diff_monson = round(abs(`Monson`-Monson[1]),2),
                                                          diff_posmon = round(abs(`Posmon`-Posmon[1]),2))

#compiling seasonal change
all_seasonal_change <- data.frame(Average=matrix(nrow = 17))
#calculate seasonal bias for precipitation using monsoon precipitation
all_seasonal_change$pr_bias <- all_pr_seasonal_change$diff_monson 
#calculate seasonal bias for temperature using all season
all_seasonal_change$tas_bias <- rowMeans(all_tas_seasonal_change)

#ranking the model based on seasonal bias
all_seasonal_change$pr_rank <- rank(all_seasonal_change$pr_bias)-1
all_seasonal_change$tas_rank <- rank(all_seasonal_change$tas_bias)-1

#averaging the rank for precipitation and temperature
all_seasonal_change$Average <- (all_seasonal_change$pr_rank+all_seasonal_change$tas_rank)/2
rownames(all_seasonal_change) <- rownames(all_pr)

#ordering models according to the average rank
all_seasonal_change <- all_seasonal_change[order(all_seasonal_change$Average),]

#selecting models based on seasonal bias
seasonal_select <- all_seasonal_change[1:9,]
seasonal_select
write.csv(seasonal_select, paste0(outdir,"Seasonal_select_model_with_rank.csv"))
# annual_bias calculation-------------------------------------------------------------
#models selected for annual bias calculation 
annual_pr_model <- all_pr[which(rownames(all_pr) %in% rownames(seasonal_select)),]
annual_tas_model <- all_tas[which(rownames(all_tas) %in% rownames(seasonal_select)),]

#plotting selected model for precipitation
matplot(t(annual_pr_model),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1)
lines(as.numeric(ref_pr),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"pr_step_1_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(annual_pr_model),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly precipitation (mm)")
  lines(as.numeric(ref_pr),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 8 CORDEX models")
  dev.off()
  
}

#plotting selected model for temperature
matplot(t(annual_tas_model),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1)
lines(as.numeric(ref_tas),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"tas_step_1_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(annual_tas_model),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly temperature (\u00B0C)")
  lines(as.numeric(ref_tas),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 8 CORDEX models")
  dev.off()
  
}

#calculating annual values
annual_pr <- rowSums(annual_pr_model)
annual_tas <- rowMeans(annual_tas_model)

#calculating annual bias
annual_pr_bias <- round(abs(1-annual_pr/annual_pr[1])*100,1)
annual_tas_bias <- round(abs(annual_tas-annual_tas[1]),2)

#compiling annual change
all_annual_change <- data.frame(Average=matrix(nrow = 9))
all_annual_change$pr_bias <- annual_pr_bias
all_annual_change$tas_bias <- annual_tas_bias
#ranking biases for precipitation and temperature
all_annual_change$pr_rank <- rank(all_annual_change$pr_bias)-1
all_annual_change$tas_rank <- rank(all_annual_change$tas_bias)-1
#averaging the rank for precipitation and temperature
all_annual_change$Average <- (all_annual_change$pr_rank+all_annual_change$tas_rank)/2
rownames(all_annual_change) <- rownames(annual_pr_model)

#ordering models according to the average rank
all_annual_change <- all_annual_change[order(all_annual_change$Average),]

#selecting models based on annual bias
final_selection <- all_annual_change[1:5,]
final_selection
write.csv(final_selection, paste0(outdir,"final_selction_with_rank_annual_bias.csv"))
#final model selection
select_pr_model <- all_pr[which(rownames(all_pr) %in% rownames(final_selection)),]
select_tas_model <- all_tas[which(rownames(all_tas) %in% rownames(final_selection)),]

#save selected models
write.csv(select_pr_model[-1,],paste0(outdir,"selected_model_pr.csv"))
write.csv(select_tas_model[-1,],paste0(outdir,"selected_model_tas.csv"))

#plotting selected model for precipitation
matplot(t(select_pr_model),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1)
lines(as.numeric(ref_pr),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"pr_final_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(select_pr_model),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly precipitation (mm)")
  lines(as.numeric(ref_pr),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 4 selected CORDEX models")
  dev.off()
  
}

#plotting selected model for temperature
matplot(t(select_tas_model),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1)
lines(as.numeric(ref_tas),col="black",lwd=2)

#saving the plot
{
  png(paste0(outdir,"tas_final_model_clim.png"),width = 2000, height = 1600, 
      units = "px", pointsize = 12, bg = "white", res = 300)
  matplot(t(select_tas_model),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1,xaxt="n",ylab="Monthly temperature (\u00B0C)")
  lines(as.numeric(ref_tas),col="black",lwd=2)
  axis(1,at=seq(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  title("Climatology of 4 selected CORDEX models")
  dev.off()
  
}
#end of code..............#

