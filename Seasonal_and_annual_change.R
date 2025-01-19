#########################################################################
# Code to calculate seasonal and annual future change...................#
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

#read shapefile of Bangladesh
SHP_WGS  <- readOGR(paste0(indir,"\\Shapefile\\gandaki_WGS.shp"))
SHP_buffer <- gBuffer(SHP_WGS,width = 1)

#RCPs and variables
rcps <- c("RCP4.5","RCP8.5")
vars <- c("pr","tas")

#color palette for the plot
col_p <- colorRampPalette((c("yellow","#7fcdbb","#3182bd","#1D46A9","navyblue")))(255)
col_t <- colorRampPalette((c("green","yellow","red")))(255)
#setting end 

# selected models analysis ---------------------------------------------------------
#reference annual data
ref_pr_annual <- read.csv(paste0(outdir,"ref_pr_annual.csv"),row.names = 1)
ref_tas_annual <- read.csv(paste0(outdir,"ref_tas_annual.csv"),row.names = 1)

#formatting data for plot
ref_pr_annual <- data.frame(Year=seq(1976,2005,1),"Aprhodite"=t(ref_pr_annual))
ref_tas_annual <- data.frame(Year=seq(1976,2005,1),"Aprhodite"=t(ref_tas_annual))

#selected CORDEX models
select_pr_model <- read.csv(paste0(outdir,"selected_model_pr.csv"),row.names = 1)
select_tas_model <- read.csv(paste0(outdir,"selected_model_tas.csv"),row.names = 1)

#annual data for all CORDEX models
RCP4.5_pr_CORDEX_yearly <- read.csv(paste0(outdir,"RCP4.5_pr_CORDEX_all_year.csv"),row.names = 1)
RCP4.5_tas_CORDEX_yearly <- read.csv(paste0(outdir,"RCP4.5_tas_CORDEX_all_year.csv"),row.names = 1)
RCP8.5_pr_CORDEX_yearly <- read.csv(paste0(outdir,"RCP8.5_pr_CORDEX_all_year.csv"),row.names = 1)
RCP8.5_tas_CORDEX_yearly <- read.csv(paste0(outdir,"RCP8.5_tas_CORDEX_all_year.csv"),row.names = 1)

#annual data for selected CORDEX models
select_pr_year_RCP4.5 <- RCP4.5_pr_CORDEX_yearly[which(rownames(RCP4.5_pr_CORDEX_yearly) %in% rownames(select_pr_model)),]
select_tas_year_RCP4.5 <- RCP4.5_tas_CORDEX_yearly[which(rownames(RCP4.5_tas_CORDEX_yearly) %in% rownames(select_tas_model)),]
select_pr_year_RCP8.5 <- RCP8.5_pr_CORDEX_yearly[which(rownames(RCP8.5_pr_CORDEX_yearly) %in% rownames(select_pr_model)),]
select_tas_year_RCP8.5 <- RCP8.5_tas_CORDEX_yearly[which(rownames(RCP8.5_tas_CORDEX_yearly) %in% rownames(select_tas_model)),]

#plotting the annual data for selected models
matplot(t(select_pr_year_RCP4.5),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1)
matplot(t(select_tas_year_RCP4.5),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1)
matplot(t(select_pr_year_RCP8.5),type="l",col = rgb(0,0,1,.2),lwd=2,lty=1)
matplot(t(select_tas_year_RCP8.5),type="l",col = rgb(1,0,0,.2),lwd=2,lty=1)

#formatting the data to data frame
pr_year_RCP4.5_t <- data.frame(t(select_pr_year_RCP4.5))
tas_year_RCP4.5_t <- data.frame(t(select_tas_year_RCP4.5))

pr_year_RCP8.5_t <- data.frame(t(select_pr_year_RCP8.5))
tas_year_RCP8.5_t <- data.frame(t(select_tas_year_RCP8.5)) 

#calculating statistics for precipitation
pr_average <- data.frame(Year = seq(1976,2100,1))  
#calculating average
pr_average$mean_RCP4.5 <- round(rowMeans(pr_year_RCP4.5_t),1)
pr_average$mean_RCP8.5 <- round(rowMeans(pr_year_RCP8.5_t),1)
#calculating standard deviation
pr_average$sd_RCP4.5 <- round(apply(pr_year_RCP4.5_t,1,sd),1)
pr_average$sd_RCP8.5 <- round(apply(pr_year_RCP8.5_t,1,sd),1)

#removing standard deviation for reference period
pr_average$sd_RCP4.5[pr_average$Year<2006] <- 0
pr_average$sd_RCP8.5[pr_average$Year<2006] <- 0
write.csv(pr_average, paste0(outdir,"Ensemble_mean_value.csv"))
#giving appropriate colors to the plot
colors <- c("Aphrodite" = "black","mean_RCP4.5" = "darkblue", "mean_RCP8.5" = "darkred",
            "RCM_ref"="grey")
colors_SD <- c("sd_RCP4.5" = "blue", "sd_RCP8.5" = "red")

#plotting the ensemble values for precipitation
ggplot(pr_average,aes(x=Year))+
  #adds error bands
  geom_ribbon(aes(ymin=mean_RCP4.5-sd_RCP4.5,ymax=mean_RCP4.5+sd_RCP4.5,fill="sd_RCP4.5"),alpha=.3)+
  #adds average line
  geom_line(aes(y=mean_RCP4.5,col="mean_RCP4.5"),size=1.5)+
  geom_ribbon(aes(ymin=mean_RCP8.5-sd_RCP8.5,ymax=mean_RCP8.5+sd_RCP8.5,fill="sd_RCP8.5"),alpha=.3)+
  geom_line(aes(y=mean_RCP8.5,col="mean_RCP8.5"),size=1.5)+
  #adds average of 4 selected RCM data to the plot
  geom_line(aes(y=mean_RCP4.5,col="RCM_ref"),subset(pr_average, Year %in% seq(1976,2005,1)),size=1.5)+
  #adds reference data to the plot
  geom_line(data = ref_pr_annual,aes(x=Year,y=aphrodite,color="Aphrodite"),size=1.5)+
  #gives color to the plot
  scale_color_manual("",values = colors,labels=c("APHRODITE","Average of RCP4.5","Average of RCP8.5","Historical average of RCM"))+
  scale_fill_manual("",values = colors_SD,labels=c("+/- SD 4.5","+/- SD 8.5"))+
  theme_bw()+
  #x-axis and y-axis labelling
  scale_x_continuous(breaks = seq(1976,2100,by=12),labels= seq(1976,2100,by=12),expand = c(0,0))+
  scale_y_continuous(expand = c(.01,.01))+
  xlab("Year") + ylab("Annual precipitation sum (mm)")+ ggtitle("Ensemble band plot for precipitation")+
  #resizing the elements of plot
  theme(plot.title = element_text(hjust = 0.5,size=rel(4)),axis.title=element_text(size=rel(2)),
        axis.text = element_text(size=rel(2)),
        legend.key.width = unit(1, "cm"), legend.key.height = unit(1.2, "cm"),
        legend.text = element_text(size = rel(2)),legend.position = "bottom") 

#saving the plot
ggsave("pr_yearly_plot_with_SD.png",path=outdir,width=1400,height=700,units="mm",dpi=300,scale = .4)


#calculating statistics for temperature
tas_average <- data.frame(Year = seq(1976,2100,1))  
tas_average$mean_RCP4.5 <- round(rowMeans(tas_year_RCP4.5_t),1)
tas_average$sd_RCP4.5 <- round(apply(tas_year_RCP4.5_t,1,sd),1)
tas_average$mean_RCP8.5 <- round(rowMeans(tas_year_RCP8.5_t),1)
tas_average$sd_RCP8.5 <- round(apply(tas_year_RCP8.5_t,1,sd),1)
tas_average$sd_RCP4.5[tas_average$Year<2006] <- 0
tas_average$sd_RCP8.5[tas_average$Year<2006] <- 0
write.csv(tas_average, paste0(outdir,"ensemble_mean_value_tas.csv"))
#plotting the ensemble values for temperature
ggplot(tas_average,aes(Year))+
  geom_ribbon(aes(ymin=mean_RCP4.5-sd_RCP4.5,ymax=mean_RCP4.5+sd_RCP4.5,fill="sd_RCP4.5"),alpha=.3)+
  geom_line(aes(y=mean_RCP4.5,col="mean_RCP4.5"),size=1.5)+
  geom_ribbon(aes(ymin=mean_RCP8.5-sd_RCP8.5,ymax=mean_RCP8.5+sd_RCP8.5,fill="sd_RCP8.5"),alpha=.3)+
  geom_line(aes(y=mean_RCP8.5,col="mean_RCP8.5"),size=1.5)+
  geom_line(aes(y=mean_RCP4.5,col="RCM_ref"),subset(tas_average, Year %in% seq(1976,2005,1)),size=1.5)+
  geom_line(data = ref_tas_annual,aes(x=Year,y=aphrodite,color="Aphrodite"),size=1.5)+
  scale_color_manual("",values = colors,labels=c("APHRODITE temperature","Average of RCP4.5","Average of RCP8.5","Historical average of RCM"))+
  scale_fill_manual("",values = colors_SD,labels=c("+/- SD 4.5","+/- SD 8.5"))+
  theme_bw()+
  scale_x_continuous(breaks = seq(1976,2100,by=12),labels= seq(1976,2100,by=12),expand = c(0,0))+
  scale_y_continuous(expand = c(.01,.01))+
  xlab("Year") + ylab("Average annual mean temperature (\u00B0C)")+ ggtitle("Ensemble band plot for mean temperature")+
  #resizing the elements of plot
  theme(plot.title = element_text(hjust = 0.5,size=rel(4)),axis.title=element_text(size=rel(2)),
        axis.text = element_text(size=rel(2)),
        legend.key.width = unit(1, "cm"), legend.key.height = unit(1.2, "cm"),
        legend.text = element_text(size = rel(2)),legend.position = "bottom")  

ggsave("tas_yearly_plot_with_SD.png",path=outdir,width=1400,height=700,units="mm",dpi=300,scale = .4)

# future climate change  --------------------------------------------------
#calculate delta change between future period(2070-2099) and reference period (1976-2005)
delta_pr_RCP4.5 <- round((colMeans(pr_year_RCP4.5_t[95:124,],na.rm=T)/colMeans(pr_year_RCP4.5_t[1:30,],na.rm=T)-1)*100,2)
delta_tas_RCP4.5 <- round(colMeans(tas_year_RCP4.5_t[95:124,],na.rm=T)-colMeans(tas_year_RCP4.5_t[1:30,],na.rm=T),2)

delta_pr_RCP8.5 <- round((colMeans(pr_year_RCP8.5_t[95:124,],na.rm=T)/colMeans(pr_year_RCP8.5_t[1:30,],na.rm=T)-1)*100,2)
delta_tas_RCP8.5 <- round(colMeans(tas_year_RCP8.5_t[95:124,],na.rm=T)-colMeans(tas_year_RCP8.5_t[1:30,],na.rm=T),2)

#compiling the delta values
delta_value <- rbind(delta_pr_RCP4.5,delta_tas_RCP4.5,delta_pr_RCP8.5,delta_tas_RCP8.5)

#writing the delta value to a csv file
write.csv(delta_value,paste0(outdir,"delta_value.csv"))


#spatial plot for selected model ----------------------------------------
all_model <- rownames(read.csv(paste0(outdir,"RCP4.5_pr_CORDEX_all_year.csv"),row.names = 1))
index_selected <- match(rownames(select_pr_model),all_model)

#list selected files in the precipitation folder for RCP4.5
filelist_selected_pr_rcp45 <- list.files(paste0(indir,"\\Gandaki_monthly_arranged\\RCP4.5\\pr"))[index_selected]

#list selected files in the precipitation folder for RCP8.5
filelist_selected_pr_rcp85 <- gsub("rcp45","rcp85",filelist_selected_pr_rcp45)

#list selected files in the temperature folder for RCP4.5
filelist_selected_tas_rcp45 <- gsub("pr_","tas_",filelist_selected_pr_rcp45)

#list selected files in the temperature folder for RCP8.5
filelist_selected_tas_rcp85 <- gsub("pr_","tas_",filelist_selected_pr_rcp85)

#plotting spatial change maps for precipitation
for (rcp in rcps)
{
  #setting filelist as per the RCP
  if(rcp=="RCP4.5"){r <- "rcp45";filelist <- filelist_selected_pr_rcp45}else{r <- "rcp85";filelist <- filelist_selected_pr_rcp85}
  #setting a empty raster layer
  spatial_change <- raster()
  for (i in 1:length(filelist))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\pr\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_1976"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    nc_ref_year <- stackApply(nc_ref,rep(seq(1:30),each=12),fun=mean)
    #seperating future period (2070-2099)
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    nc_fut_year <- stackApply(nc_fut,rep(seq(1:30),each=12),fun=mean)
    #calculating spatial future change
    fut_change <- (mean(nc_fut_year)/mean(nc_ref_year)-1)*100
    #stacking the layers from all four models
    spatial_change <- stack(spatial_change,fut_change)
    
  }
  #calculating ensemble mean
  spatial_change_mean <- mean(spatial_change)
  #setting the z limits 
  zmin <- -10
  zmax <- 30
  spatial_change_mean[spatial_change_mean<zmin] <- zmin
  spatial_change_mean[spatial_change_mean>zmax] <- zmax
  
  #saving the plot
  png(paste0(outdir,"Gandaki_",rcp,"_pr_change.png"),width = 1450, height = 1500, units = "px", pointsize = 12, bg = "white", res = 300)
  plot(crop(spatial_change_mean,SHP_buffer),col=colorRampPalette((c("blue","white","red")))(255),zlim=c(zmin,zmax),legend=F,cex.axis=.5,main=paste0("Change in annual precipitation for ",rcp))
  plot(SHP_WGS,add=T)
  plot(spatial_change_mean,col=colorRampPalette((c("blue","white","red")))(255),zlim=c(zmin,zmax),legend.only=T,legend.width=1, 
       legend.args = list(text = 'Change (%)', side = 4,font = 1, line = 2, cex = 1))
  dev.off()
}

#plotting spatial change maps for temperature
for (rcp in rcps)
{
  if(rcp=="RCP4.5"){r <- "rcp45";filelist <- filelist_selected_tas_rcp45}else{r <- "rcp85";filelist <- filelist_selected_tas_rcp85}
  spatial_change <- raster()
  for (i in 1:length(filelist))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\tas\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_1976"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    nc_ref_year <- stackApply(nc_ref,rep(seq(1:30),each=12),fun=mean)
    #seperating future period (2070-2099)
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    nc_fut_year <- stackApply(nc_fut,rep(seq(1:30),each=12),fun=mean)
    fut_change <- mean(nc_fut_year)-mean(nc_ref_year)
    spatial_change <- stack(spatial_change,fut_change)
    
  }
  spatial_change_mean <- mean(spatial_change)
  zmin <- 1
  zmax <- 5
  spatial_change_mean[spatial_change_mean<zmin] <- zmin
  spatial_change_mean[spatial_change_mean>zmax] <- zmax
  png(paste0(outdir,"BNG_",rcp,"_tas_change.png"),width = 1450, height = 1500, units = "px", pointsize = 12, bg = "white", res = 300)
  plot(crop(spatial_change_mean,SHP_buffer),col=colorRampPalette((c("blue","white","red")))(255),zlim=c(zmin,zmax),legend=F,cex.axis=.5,main=paste0("Change in annual temperature for ",rcp))
  plot(SHP_WGS,add=T)
  plot(spatial_change_mean,col=colorRampPalette((c("blue","white","red")))(255),zlim=c(zmin,zmax),legend.only=T,legend.width=1, 
       legend.args = list(text = 'Change  (\u00B0C)', side = 4,font = 1, line = 2, cex = 1))
  dev.off()
}

# seasonal changes --------------------------------------------------------
# for precipitation
for (rcp in rcps)
{
  #setting filelist as per the RCP
  if(rcp=="RCP4.5"){r <- "rcp45";filelist <- filelist_selected_pr_rcp45}else{r <- "rcp85";filelist <- filelist_selected_pr_rcp85}
  #creating data frame to store seasonal data
  season_change_all <- data.frame()
  for (i in 1:length(filelist))
  {
    #read the ncfile
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\pr\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_1976"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    #seperating reference period (1976-2005)
    nc_ref <- ncfile[[1:360]]
    #calculating monthly average value
    nc_ref_mon <- stackApply(nc_ref,rep(seq(1,12),30),fun=mean)
    #calculating seasonal sum (Winter, Pre-monsoon, "Monsoon", "Post-monsoon")
    nc_ref_season <- stackApply(nc_ref_mon,c(1,1,2,2,2,3,3,3,3,4,4,1),fun=sum)
    #extracting seasonal changes
    season_ref <- round(extract(nc_ref_season,SHP_WGS,fun=mean),2)
    #seperating future period (2070-2099)
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    #calculating monthly average value
    nc_fut_mon <- stackApply(nc_fut,rep(seq(1,12),30),fun=mean)
    #calculating seasonal sum 
    nc_fut_season <- stackApply(nc_fut_mon,c(1,1,2,2,2,3,3,3,3,4,4,1),fun=sum)
    #extracting seasonal changes
    season_fut <- round(extract(nc_fut_season,SHP_WGS,fun=mean),2)
    
    #calculating changes in seasonal value
    fut_change <- round((season_fut/season_ref-1)*100,2)
    #setting rownames
    rownames(fut_change) <- model
    #saving the seasonal data to the data frame
    season_change_all <- rbind(season_change_all,fut_change)
  }
  #setting column names
  colnames(season_change_all) <- c("Winter","Pre-monsoon","Monsoon","Post-monsoon")
  #saving the data to a csv file
  write.csv(season_change_all,paste0(outdir,paste0(rcp,"pr_seasonal_change_all_models.csv")))
}

#for temperature
for (rcp in rcps)
{
  if(rcp=="RCP4.5"){r <- "rcp45";filelist <- filelist_selected_tas_rcp45}else{r <- "rcp85";filelist <- filelist_selected_tas_rcp85}
  season_change_all <- data.frame()
  for (i in 1:length(filelist))
  {
    ncfile <- stack(paste0(indir,"\\Gandaki_monthly_arranged\\",rcp,"\\tas\\",filelist[i]))
    model <- paste0(unlist(strsplit(unlist(strsplit(filelist[i],"WAS-44_|_1976"))[2],paste0("_",r,"_r1i1p1"))),collapse="")
    
    nc_ref <- ncfile[[1:360]]
    nc_ref_mon <- stackApply(nc_ref,rep(seq(1,12),30),fun=mean)
    nc_ref_season <- stackApply(nc_ref_mon,c(1,1,2,2,2,3,3,3,3,4,4,1),fun=mean)
    season_ref <- round(extract(nc_ref_season,SHP_WGS,fun=mean),2)
    
    nc_fut <- ncfile[[(nlayers(ncfile)-359):nlayers(ncfile)]]
    nc_fut_mon <- stackApply(nc_fut,rep(seq(1,12),30),fun=mean)
    nc_fut_season <- stackApply(nc_fut_mon,c(1,1,2,2,2,3,3,3,3,4,4,1),fun=mean)
    season_fut <- round(extract(nc_fut_season,SHP_WGS,fun=mean),2)
    
    fut_change <- round((season_fut-season_ref),2)
    rownames(fut_change) <- model
    season_change_all <- rbind(season_change_all,fut_change)
  }
  colnames(season_change_all) <- c("Winter","Pre-monsoon","Monsoon","Post-monsoon")
  write.csv(season_change_all,paste0(outdir,paste0(rcp,"_tas_seasonal_change_all_models.csv")))
}

#end of code..............#
