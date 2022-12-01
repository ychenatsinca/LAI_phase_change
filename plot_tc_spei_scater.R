#
# load libraries and  TC_track data from the selected year 
library("plyr")
library("fields")
library("rgdal")
library("maptools")
library("raster")
library("hexbin")



ld_load <- T
if (ld_load) {

#load_ini_lai_file
source("./read_ncdf4.R")
input_lai <- fun_read_nc("/lfs/home/ychen/LAI_STUDY_EAsia/LAI_DATA/c_gls_LAI_199901100000_VGT_V2_EAST_ASIA.nc",var_st=2)
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

#fname = c("era5_monthly_data_1999_2019.nc")

# load the function 
library("raster")
source("./read_ncdf4.R")

#mask    <- fun_read_nc("./mask/land_frac.nc")
#input_data  <- fun_read_nc(  paste(fname, sep="")) 
#create time stamp 
#date.id   <- format(seq(as.POSIXct("1999-01-15 00:00:00", tz="UTC"), length.out=252, by='1 mon'),'%Y-%m-%d')

#load table
track.data <- read.csv("all_tc_track_table.txt")
table.neutral <- read.csv("table_neutral_3a_run.txt")
table.comb <- read.csv("table_comb_3a_run.txt")

all.table <- table.comb
#import all_event_tables.csv
#all.table <- read.csv("all.table.csv")
#w_0_p_100_4D w_0_p_60_2D w_0_p_80_3D w_10_p_0_3D w_10_p_80_3D w_12_p_0_4D w_12_p_100_4D w_8_p_0_2D w_8_p_60_2D

nx=6722
ny=6722


#load return frequence of tc and SPEI
#2D case
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/tc.occ.1.JJA.ann.1999to2018.2D.Rda")
occ.avg.2d <- tc.spei.occ.1.JJA.ann[20,,]
occ.avg.2d =occ.avg.2d/(as.numeric(20))
#count annual occurance of SPEI 
spei.frq <- occ.avg.2d*lc.for.mask

#2a
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/2D_8_0_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.2d.a <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.2d.a$tc.frq[ table.2d.a$tc.frq==0] <- NA
table.2d.a$spei.frq[ table.2d.a$spei.frq==0] <- NA
table.2d.a <- na.omit(table.2d.a) 

#2b
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/2D_0_60_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.2d.b <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.2d.b$tc.frq[ table.2d.b$tc.frq==0] <- NA
table.2d.b$spei.frq[ table.2d.b$spei.frq==0] <- NA
table.2d.b <- na.omit(table.2d.b) 

#2c
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/2D_8_60_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.2d.c <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.2d.c$tc.frq[ table.2d.c$tc.frq==0] <- NA
table.2d.c$spei.frq[ table.2d.c$spei.frq==0] <- NA
table.2d.c <- na.omit(table.2d.c) 






#3D cases
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/tc.occ.1.JJA.ann.1999to2018.3D.Rda")
occ.avg.3d <- tc.spei.occ.1.JJA.ann[20,,]
occ.avg.3d =occ.avg.3d/(as.numeric(20))
#count annual occurance of SPEI 
spei.frq <- occ.avg.3d*lc.for.mask

#3a
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/3D_10_0_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.3d.a <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.3d.a$tc.frq[ table.3d.a$tc.frq==0] <- NA
table.3d.a$spei.frq[ table.3d.a$spei.frq==0] <- NA
table.3d.a <- na.omit(table.3d.a) 


# filter extreme to show 3D cases
tc.frq[tc.frq > 4.0] <- 4.0
spei.frq[spei.frq > 2.0] <- 2.0 

raster.tc.frq <-  raster( x=t(tc.frq),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

raster.spei.frq <-  raster( x=t(spei.frq),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

#3b
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/3D_0_80_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.3d.b <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.3d.b$tc.frq[ table.3d.b$tc.frq==0] <- NA
table.3d.b$spei.frq[ table.3d.b$spei.frq==0] <- NA
table.3d.b <- na.omit(table.3d.b) 

#3c
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/3D_10_80_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.3d.c <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.3d.c$tc.frq[ table.3d.c$tc.frq==0] <- NA
table.3d.c$spei.frq[ table.3d.c$spei.frq==0] <- NA
table.3d.c <- na.omit(table.3d.c) 



#4D cases
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/tc.occ.1.JJA.ann.1999to2018.4D.Rda")
occ.avg.4d <- tc.spei.occ.1.JJA.ann[20,,]
occ.avg.4d =occ.avg.4d/(as.numeric(20))
#count annual occurance of SPEI 
spei.frq <- occ.avg.4d*lc.for.mask

#4a
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/4D_12_0_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up TC occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of TC (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.4d.a <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.4d.a$tc.frq[ table.4d.a$tc.frq==0] <- NA
table.4d.a$spei.frq[ table.4d.a$spei.frq==0] <- NA
table.4d.a <- na.omit(table.4d.a) 

#4b
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/4D_0_100_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up tc occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of tc (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.4d.b <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.4d.b$tc.frq[ table.4d.b$tc.frq==0] <- NA
table.4d.b$spei.frq[ table.4d.b$spei.frq==0] <- NA
table.4d.b <- na.omit(table.4d.b) 

#4c
load("/lfs/home/ychen/scripts/R/Rscripts/TC_track_mask/AllExtremes_rda/4D_12_100_1999_2018_.rda")
count.1999.to.2018 <- array( 0., dim=c(6722,6722))
#sum up tc occurence
for ( it in 1:20 ) {
      print(paste("working on year:",it+1999-1) )
      #tc.occ.avg[,,it][ tc.occ.avg[,,it]>=1] <- 1.0
      count.1999.to.2018 <- count.1999.to.2018 + tc.occ.avg[,,it]
}
#count annual occurance of tc (divided by average event numbers from 1999 to 2018, 29 )
tc.frq <- (count.1999.to.2018 / 20)*lc.for.mask

table.4d.c <- data.frame(tc.frq=as.vector(tc.frq) , spei.frq=as.vector(spei.frq))
table.4d.c$tc.frq[ table.4d.c$tc.frq==0] <- NA
table.4d.c$spei.frq[ table.4d.c$spei.frq==0] <- NA
table.4d.c <- na.omit(table.4d.c) 


#combine 2D, 3D, and 4D

table.pt <- rbind(table.2d.a, table.2d.b, table.2d.c, table.3d.a, table.3d.b, table.3d.c, table.4d.a, table.4d.b, table.4d.c)

} #end ld_load



#######################################################################
#
#   all phase plot  
#
#######################################################################

library("viridis")

pdf(file= "tc_spei_scatter.pdf" , width = 27, height = 9 ) 

  my.color.tc <- viridis(n=12, alpha=1, direction=1, option="H") # D for viridus  H for turbo/rainbow
  my.color.tc[1] <- "gray"
#  my.breaks<- round(seq(0, 6.0, length.out = 13), digits=1)
  my.breaks.tc<- c(-.5,0.001,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)
  my.breaks.txt.tc<- c("Nan","0.01","0.5","1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5",">5")
  #

  # set subplot color palette 
  my.color.spei <- viridis(n=9, alpha=1, direction=1, option="H") # D for viridus  H for turbo/rainbow
  my.color.spei[1] <- "gray"
  my.color.spei[9] <- "brown"

  my.breaks.spei<- c(-0.2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6)
  my.breaks.txt.spei <- c("Nan","0.005","0.2","0.4","0.6","0.8","1.0","1.2","1.4",">1.5")

#Neutral cases 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,160))
  #### 
  par(fig=c(0.05,.35,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis 
  plot(raster.tc.frq,  legend=FALSE,ylim=c(0,60),xlim=c(0,160),zlim=c(0,5),
         col=my.color.tc,breaks=my.breaks.tc, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
 #plot legend 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis
  plot(raster.tc.frq, legend.only=TRUE,ylim=c(0,60),xlim=c(100,160), col=my.color.tc,breaks=my.breaks.tc,
       smallplot=c(0.65,.95, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.tc, labels=my.breaks.txt.tc, cex.axis=1),
       legend.args=list(text= expression("Cyclones frequency") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,160),add=T  )
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)

  #par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #contour(raster.tc.frq,  add=TRUE, 
  #        ylim=c(0,60),xlim=c(100,180), col="NA",labcex=1.5,col.axis="black",lwd=0.6 )
  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160")
  axis(side = 1, at = seq(100,160,length.out=7), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 #  mtext("Longitude" , side=1, line=2.0, cex=1.0) 
 #
 #######postive - neutral cases ##########
 # set subplot margin
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,160),add=T)
  #### 
  par(fig=c(0.35,.65,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis 
  plot(raster.spei.frq,  legend=FALSE,ylim=c(0,60),xlim=c(100,160),zlim=c(0,1.6),
         col=my.color.spei,breaks=my.breaks.spei, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot lagend 
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60)) #no buffer in x-y axis
  plot(raster.spei.frq, legend.only=TRUE,ylim=c(0,60),xlim=c(100,160),zlim=c(0,1.6),
        col=my.color.spei,breaks=my.breaks.spei,
       smallplot=c(0.6, 0.98, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.spei, labels=my.breaks.txt.spei, cex.axis=1),
       legend.args=list(text= expression("SPEI < -1.0 frequency") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,160),add=T  )
  par(xpd=TRUE)
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,160,0,60), xpd=FALSE)
  #add contour
  #contour(pos.neu.raster, lev=my.breaks.b2b, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2b,labcex=1.5,lwd=0.6 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160")
  axis(side = 1, at = seq(100,160,length.out=7), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 
##############  negative - neutral cases ########################### 
 # set subplot margin
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1)) #no buffer in x-y axis

  par(new=T) 

# create a color palette to use in smoothed scatterplot
library(RColorBrewer)
buylrd = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
           "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 
myColRamp = colorRampPalette(c(buylrd))


# filter extreme to show 3D cases
table.pt$tc.frq[table.pt$tc.frq > 2.0] <- NA
table.pt$spei.frq[table.pt$spei.frq > 2.0] <- NA 

table.pt <- na.omit(table.pt) 



cor(x=table.pt$tc.frq, y=table.pt$spei.frq)

#hex <- hexbin(table.pt$tc.frq,  table.pt$spei.frq)
#plot(hex, legend = TRUE,  colramp = myColRamp,  xlab="Cyclones frequency (1/yr)", ylab="Dry spells (SPEI < -1.0) frquency (1/yr)")

#smoothe scatterplot or hexplot

smoothScatter(x=table.pt$tc.frq ,y=table.pt$spei.frq, colramp=myColRamp, main="", bandwidth = 0.05,
              xlab="Tropical cyclones frequency (1/yr)",ylab="Dry spells frequency, (SEPI< -1.0) (1/yr)", xlim=c(0,1.2),ylim=c(0,1.2), cex=1.5)

table.pt1<- table.pt 
table.pt1$tc.frq[table.pt1$tc.frq > 1.0] <- NA
table.pt1$spei.frq[table.pt1$spei.frq > 0.6] <- NA 

table.pt1 <- na.omit(table.pt1) 

cor(x=table.pt1$tc.frq, y=table.pt1$spei.frq)


dev.off()



