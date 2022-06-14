#
# load libraries and  TC_track data from the selected year 
library("plyr")
library("fields")
library("rgdal")
library("maptools")
library("raster")

# load forest cover type 
# ESA land cover map from 2015, the domain shold be also matched the LAI map over WP region
load(paste("/lfs/home/ychen/LAI_STUDY_EAsia/LANDCOVER_DATA/2015.esa.landcover.east.asia.rda",sep=""))
# variable name: esa.lc
# group the types to croplands(type 1), forests(type 2), others(type 3)
# see http://http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf   # Page-30
# set water(210) to NA
esa.lc[ (esa.lc == 210 ) ] <- NA
esa.lc[ (esa.lc >= 10)  & (esa.lc <= 40 ) ] <- 1
esa.lc[ (esa.lc >= 50)  & (esa.lc <= 120) ] <- 2
#esa.lc[ (esa.lc == 160) | (esa.lc == 170) ] <- 2
esa.lc[ (esa.lc > 2) ]  <- 3
# create the LC mask
lc.mask <- esa.lc
# agricuture mask
lc.agr.mask <- lc.mask
lc.agr.mask[lc.mask!=1] <- NA
lc.agr.mask[lc.mask==1] <- 1
# forest mask
lc.for.mask <- lc.mask
lc.for.mask[lc.mask!=2] <- NA
lc.for.mask[lc.mask==2] <- 1
# other mask
lc.oth.mask <- lc.mask
lc.oth.mask[lc.mask!=3] <- NA
lc.oth.mask[lc.mask==3] <- 1


#load the coastlines data by readOGR function from sp package 
#coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_110m_coastline/ne_110m_coastline.shp")
coastlines <- readOGR("/lfs/home/ychen/GIS/Coastline/ne_10m_coastline/ne_10m_coastline.shp")

fname = c("era5_monthly_data_1999_2019.nc")

# load the function 
library("raster")
source("./read_ncdf4.R")

#mask    <- fun_read_nc("./mask/land_frac.nc")
input_data  <- fun_read_nc(  paste(fname, sep="")) 
#create time stamp 
date.id   <- format(seq(as.POSIXct("1999-01-15 00:00:00", tz="UTC"), length.out=252, by='1 mon'),'%Y-%m-%d')

#load table
track.data <- read.csv("all_tc_track_table.txt")
table.neutral <- read.csv("table_neutral_3b_run.txt")
table.comb <- read.csv("table_comb_3b_run.txt")

#import all_event_tables.csv
all.table <- read.csv("all.table.csv")
#w_0_p_100_4D w_0_p_60_2D w_0_p_80_3D w_10_p_0_3D w_10_p_80_3D w_12_p_0_4D w_12_p_100_4D w_8_p_0_2D w_8_p_60_2D

all.table <- subset( all.table, all.table$run.case == "w_10_p_80_3D")


#neutral.table  <-  subset(all.table, abs(all.table$eff.size.for) <= 0.25 )
#negative.table <-  subset(all.table,  (all.table$eff.size.for) < -0.25 )
#positive.table <-  subset(all.table,  (all.table$eff.size.for) > 0.25 )

neutral.table  <-  subset(all.table,  all.table$group == "Neutral" )
negative.table <-  subset(all.table,  all.table$group == "Negative" )
positive.table <-  subset(all.table,  all.table$group == "Positive" )


neu_gp <- neutral.table$date.time
neg_gp <- negative.table$date.time
pos_gp <- positive.table$date.time


neu_date <- neu_gp
neg_date <- neg_gp
pos_gate <- pos_gp



ld_go <- FALSE

if (ld_go) {
#load_ini_lai_file
input_lai <- fun_read_nc("/lfs/home/ychen/LAI_STUDY_EAsia/LAI_DATA/c_gls_LAI_199901100000_VGT_V2_EAST_ASIA.nc",var_st=2)

nx=6722
ny=6722
#
#allocate array
neu_lai <- array(0, dim=c(nx,ny) )
neg_lai <- array(0, dim=c(nx,ny) )
pos_lai <- array(0, dim=c(nx,ny) )


neu_n=0
neg_n=0
pos_n=0

lai_dir <- c("/lfs/home/ychen/LAI_STUDY_EAsia/LAI_DATA/")

for (ieve in 1:length(all.table$date.time)) {
  

  tmp_date <-  all.table$date.time[ieve]
  iyr  = substr(all.table$date.time[ieve],start=1,stop=4)

   if (iyr <= 2013) {

     lai_fname = paste("c_gls_LAI_",all.table$date.time[ieve],"0000_VGT_V2_EAST_ASIA.nc",sep="") 
   }else{

     lai_fname = paste("c_gls_LAI_",all.table$date.time[ieve],"0000_PROBAV_V2_EAST_ASIA.nc",sep="")

   }

lai_fname=paste(lai_dir,lai_fname,sep="") 

print(lai_fname)

# neu     
      for (ineu in 1:length(neu_gp) ) {
         if( neu_gp[ineu] == tmp_date ) {
         tmp1_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI
         neu_lai <- tmp1_arr + neu_lai 
         neu_n=neu_n+1
         print("neutral event.")
         }
      } 
# neg
      for (ineg in 1:length(neg_gp) ) {
         if( neg_gp[ineg] == tmp_date ) {
         tmp2_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI 
         neg_lai <- tmp2_arr + neg_lai
         neg_n=neg_n+1
         print("negative event.")
         }
      } 
# pos
      for (ipos in 1:length(pos_gp) ) {
         if( pos_gp[ipos] == tmp_date ) {
         tmp3_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI 
         pos_lai <- tmp3_arr + pos_lai 
         pos_n=pos_n+1
         print("positive event.")
         }

      } 
} #end_for
#take average
neu_lai <- (neu_lai*lc.for.mask)/ (as.numeric(neu_n) )
neg_lai <- (neg_lai*lc.for.mask)/ (as.numeric(neg_n) )
pos_lai <- (pos_lai*lc.for.mask)/ (as.numeric(pos_n) )

 #end for 
#convert arr to raster


neu.raster.lai <-  raster( x=t(neu_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

neg.raster.lai <-  raster( x=t(neg_lai-neu_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

pos.raster.lai <-  raster( x=t(pos_lai-neu_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))
} #end ld_go


ld_go<-TRUE

if (ld_go) {

#positive case  
#neu_gp <- c("20041120","20050430","20050810","20050920","20061130","20071010",
#"20080520","20080920","20081010","20090920","20100720","20120410","20131120",
#"20140810","20140930","20150630") 
#
#neg_gp <- c("19991110","20000720","20010810","20011120","20011231","20031120",
#"20031210","20040910","20041010","20041020","20060520","20060710","20061120",
#"20061210","20061220","20070920","20071120","20081220","20090620","20091110",
#"20130710","20141020","20141210","20160731","20161010","20171031","20180820",
#"20180930","20181010")
#
#pos_gp <- c("19990731","19990820","19990910","20000810","20001010","20010720",
#"20010731","20020810","20020920","20030430","20030531","20030731","20030910",
#"20030930","20040420","20040820","20040920","20051020","20051120","20060731",
#"20061010","20061020","20070720","20070820","20080810","20080831","20090820",
#"20090831","20100731","20100810","20100820","20100910","20100920","20110720",
#"20110810","20110910","20110920","20120720","20140710","20140910","20141010",
#"20150910","20151010","20160820","20160910","20161031","20170810","20171120",
#"20180710","20180731","20180910","20181031","20181110")
# 
# convert to monthly
neu_gp <- paste(substr(neu_gp,start=1,stop=6),"15",sep="")
neg_gp <- paste(substr(neg_gp,start=1,stop=6),"15",sep="")
pos_gp <- paste(substr(pos_gp,start=1,stop=6),"15",sep="")
#
neu_gp <- as.Date(neu_gp,format="%Y%m%d")
neg_gp <- as.Date(neg_gp,format="%Y%m%d")
pos_gp <- as.Date(pos_gp,format="%Y%m%d")
#
nx=length(input_data$latitude)
ny=length(input_data$longitude)
#
#allocate array
neu_arr <- array(0, dim=c(nx,ny) )
neg_arr <- array(0, dim=c(nx,ny) )
pos_arr <- array(0, dim=c(nx,ny) )


neu_n=0
neg_n=0
pos_n=0


for (imon in 1:length(date.id)) {
      tmp_date <-  date.id[imon]
# neu     
      for (ineu in 1:length(neu_gp) ) {
         if( neu_gp[ineu] == tmp_date ) {
         tmp1_arr <- input_data$msl[,,imon] 
         neu_arr <- tmp1_arr + neu_arr 
         neu_n=neu_n+1
         }
      } 
# neg
      for (ineg in 1:length(neg_gp) ) {
         if( neg_gp[ineg] == tmp_date ) {
         tmp2_arr <- input_data$msl[,,imon] 
         neg_arr <- tmp2_arr + neg_arr 
         neg_n=neg_n+1
         }
      } 
# pos
      for (ipos in 1:length(pos_gp) ) {
         if( pos_gp[ipos] == tmp_date ) {
         tmp3_arr <- input_data$msl[,,imon] 
         pos_arr <- tmp3_arr + pos_arr 
         pos_n=pos_n+1
         }

      } 




} 

#take average 

neu_arr <- neu_arr/ (as.numeric(neu_n) )
neg_arr <- neg_arr/ (as.numeric(neg_n) )
pos_arr <- pos_arr/ (as.numeric(pos_n) )


neu_arr[is.nan(neu_arr)] <- 100500 
neg_arr[is.nan(neu_arr)] <- 100500 
pos_arr[is.nan(neu_arr)] <- 100500 
#
#convert array to raster obj
neu.raster <-  raster( x=t(neu_arr), 
                              xmn=input_data$longitude[1],  xmx=input_data$longitude[nx],
                              ymn=input_data$latitude[ny],  ymx=input_data$latitude[1], 
                              crs=CRS("+proj=longlat +datum=WGS84"))
 
neg.neu.raster <-  raster( x=t(neg_arr-neu_arr), 
                              xmn=input_data$longitude[1],  xmx=input_data$longitude[nx],
                              ymn=input_data$latitude[ny],  ymx=input_data$latitude[1], 
                              crs=CRS("+proj=longlat +datum=WGS84"))

pos.neu.raster <-  raster( x=t(pos_arr-neu_arr), 
                              xmn=input_data$longitude[1],  xmx=input_data$longitude[nx],
                              ymn=input_data$latitude[ny],  ymx=input_data$latitude[1], 
                              crs=CRS("+proj=longlat +datum=WGS84"))
} # end of lg_go




#par(oma = c(0.05, 0.05, 0.05, 0.05),mgp=c(2.5,1,0))
#color palette
pdf(file= "Neutral.pdf" , width = 10, height = 9 ) 
#
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"),alpha=1)(11)
  my.breaks.b2r<- round(seq(100900, 102000, length.out = 11), digits=1)
 
  my.color.w2g <- colorRampPalette(
    c("ivory","wheat","greenyellow","yellowgreen","limegreen"
      ,"forestgreen","darkgreen","darkgreen"))(36)
  my.breaks.w2g<- seq(0,7,length.out=36)

  leg.at.lai  <- my.breaks.w2g 
  leg.txt.lai <- my.breaks.w2g 
  # set subplot margin
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180))
  #### 
  par(fig=c(0.02,.96,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neu.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(0,7),
         col=my.color.w2g,breaks=my.breaks.w2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  # 
 #plot lagend 
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neu.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180), col=my.color.w2g,breaks=my.breaks.w2g,
       smallplot=c(0.5,.88, 0.15, 0.20), add=T, horizontal=T,
       axis.args=list(at=my.breaks.w2g, labels=my.breaks.w2g, cex.axis=0.8),
       legend.args=list(text= expression("LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )
  #add coastaline
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)
#  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,cex=1.5,lwd=1.0 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)
  #  mtext("Longitude" , side=1, line=2.0, cex=1.0) 
  dev.off()

  pdf(file= "Negative_Neutral.pdf" , width = 10, height = 9 ) 
  #positive - neutral cases 
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
  my.breaks.b2r<- round(seq(-400, 700, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("400","-300","-200","-100","0","100","200","300","400","500","600","700") 
  leg.txt <-  c("-400","-300","-200","-100","0","100","200","300","400","500","600","700") 

  my.color.b2g <- colorRampPalette(
    c("brown","wheat","ivory","limegreen", "forestgreen"))(25)
  my.breaks.b2g<- seq(-1.2,1.2,length.out=25)
  # set subplot margin
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180))
  #### 
  par(fig=c(0.02,.96,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neg.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot lagend 
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neg.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.5,.88, 0.15, 0.20), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=0.8),
       legend.args=list(text= expression("LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )
  #add coastaline
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)
  # add contour
  contour(neg.neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,cex=1.5,lwd=1.0 )
  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
  # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)
  dev.off()


 pdf(file= "Positive_Neutral.pdf" , width =10, height = 9 ) 
 #
 #positive - neutral cases 
 my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
 my.breaks.b2r<- round(seq(-300, 200, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("-300","-200", "-100","0","100","200") 
  leg.txt <-  c("-300","-200", "-100","0","100","200") 
  # set subplot margin
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180))
  #### 
  par(fig=c(0.02,.96,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(pos.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot lagend 
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(pos.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.5,.88, 0.15, 0.20), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=0.8),
       legend.args=list(text= expression("LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )

  #add coastaline
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
#  box()
  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(pos.neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,cex=1.5,lwd=1.0 )
  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)
  dev.off()
#######################################################################
#
#   all phase plot  
#
#######################################################################
pdf(file= "all_phases.pdf" , width = 27, height = 9 ) 
#
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"),alpha=1)(11)
  my.breaks.b2r<- round(seq(100900, 102000, length.out = 11), digits=1)
 
  my.color.w2g <- colorRampPalette(
    c("ivory","wheat","greenyellow","yellowgreen","limegreen"
      ,"forestgreen","darkgreen","darkgreen"))(36)
  my.breaks.w2g<- seq(0,7,length.out=36)

  leg.at.lai  <- my.breaks.w2g 
  leg.txt.lai <- my.breaks.w2g 

  # set subplot margin
#Neutral cases 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180))
  #### 
  par(fig=c(0.05,.35,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neu.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(0,7),
         col=my.color.w2g,breaks=my.breaks.w2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  # 
 #plot lagend 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neu.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180), col=my.color.w2g,breaks=my.breaks.w2g,
       smallplot=c(0.65,.95, 0.16, 0.19), add=T, horizontal=T,
       axis.args=list(at=my.breaks.w2g, labels=my.breaks.w2g, cex.axis=0.8),
       legend.args=list(text= expression("LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )
  #add coastaline
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)
#  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,labcex=1.5,col.axis="black",lwd=1.0 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)

 #  mtext("Longitude" , side=1, line=2.0, cex=1.0) 
 #
 #######postive - neutral cases ##########
 my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
 my.breaks.b2r<- round(seq(-300, 200, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("-300","-200", "-100","0","100","200") 
  leg.txt <-  c("-300","-200", "-100","0","100","200") 
  # set subplot margin
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180),add=T)
  #### 
  par(fig=c(0.35,.65,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(pos.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot lagend 
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(pos.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.65, 0.95, 0.15, 0.18), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=0.8),
       legend.args=list(text= expression(delta*"LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )
  #add coastaline
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(pos.neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,labcex=1.5,lwd=1.0 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)

##############  negative - neutral cases ########################### 
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
  my.breaks.b2r<- round(seq(-400, 700, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("400","-300","-200","-100","0","100","200","300","400","500","600","700") 
  leg.txt <-  c("-400","-300","-200","-100","0","100","200","300","400","500","600","700") 

  my.color.b2g <- colorRampPalette(
    c("brown","wheat","ivory","limegreen", "forestgreen"))(25)
  my.breaks.b2g<- seq(-1.2,1.2,length.out=25)

  # set subplot margin
 
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180),add=T)
  #### 
  par(fig=c(0.65,.95,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neg.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )

  #plot lagend 
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neg.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.2,1.2), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.65,.95, 0.16, 0.19), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=0.8),
       legend.args=list(text= expression(delta*"LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=0.8) )


   #add coastaline
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=0.5,ylim=c(0,60),xlim=c(100,180),add=T  )
  box()
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)

  # add contour
  contour(neg.neu.raster, lev=my.breaks.b2r, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2r,labcex=1.5,lwd=1.0 )


#  par(fig=c(0.0,0.95,0.1,1.0), mar = c(4, 4.5, 1.5 ,0.1), usr=c(90,180,0,60), xpd=TRUE)  
#  plot(coastlines, lwd=0.8,ylim=c(0,60),xlim=c(90,180),add=T  )
#  box()
 

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = -0.02)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = -0.02)


dev.off()



ld_go <- FALSE
if(ld_go) { 
# add TC track lines
   par(fig=c(0.0,0.95,0.1,1.0), mar = c(4, 4.5, 1.5 ,0.1), usr=c(90,180,0,60), xpd=FALSE)  
 #plot QCQA-track lines
   for (ieve in 1:length(table.comb$date.time) ) {
      #for Neutral ES condition
      if (  (table.comb$eff.size.for[ieve] >= -0.2) & (table.comb$eff.size.for[ieve]) <= 0.45) {  

      #subset track.data to event table 
      eve.table  <-  subset (track.data, ((substr(track.data$YYYYMMDDHH,start=1,stop=4) == table.comb$date.yr[ieve]) &
                                          (track.data$CY == table.comb$eve.id[ieve]) ))
      eve.table  <- subset (eve.table, ((eve.table$VMAX) >=30 & (eve.table$LonEW <= 180 )& (eve.table$LonEW >= 102 ))  ) 

      eve.stp <- length(eve.table$VMAX)
      eve.rad <- max(eve.table$RAD)
      print(paste("TC max RAD:",eve.rad,", ","TC_stps:",eve.stp,sep=""))
      
     # plot the tracks
      lines( x=eve.table$LonEW, y=eve.table$LatNS, lty="dotdash",
             col="purple",
             lwd= 1.0 )
      #add TC genesis points with different Color for positive/neutral/negative ES
      #if (abs(table.comb$eff.size.for[ieve]) <= 0.35) points(x=eve.table$LonEW[1], y=eve.table$LatNS[1], pch=21, col="black",bg="lightblue",cex=0.8)
      #if ( (table.comb$eff.size.for[ieve]) > 0.35)  points(x=eve.table$LonEW[1], y=eve.table$LatNS[1], pch=24, col="black",bg="forestgreen",cex=0.8)
      #if ( (table.comb$eff.size.for[ieve]) < -0.25) points(x=eve.table$LonEW[1], y=eve.table$LatNS[1], pch=25, col="black",bg="orange",cex=0.8)
      }
   } 
} #end ld_gp 



