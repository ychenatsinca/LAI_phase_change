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
table.neutral <- read.csv("table_neutral_3a_run.txt")
table.comb <- read.csv("table_comb_3a_run.txt")

all.table <- table.comb
#import all_event_tables.csv
#all.table <- read.csv("all.table.csv")
#w_0_p_100_4D w_0_p_60_2D w_0_p_80_3D w_10_p_0_3D w_10_p_80_3D w_12_p_0_4D w_12_p_100_4D w_8_p_0_2D w_8_p_60_2D

nx=6722
ny=6722

ld_go <- F

if (ld_go) {
##
#allocate array
neu_lai <- array(0, dim=c(nx,ny) )
neg_lai <- array(0, dim=c(nx,ny) )
pos_lai <- array(0, dim=c(nx,ny) )

neu_spei <- array(0, dim=c(nx,ny) )
neg_spei <- array(0, dim=c(nx,ny) )
pos_spei <- array(0, dim=c(nx,ny) )


neu_n <- array(0,dim=c(nx,ny))
neg_n <- array(0,dim=c(nx,ny))
pos_n <- array(0,dim=c(nx,ny))



for (icase in 1:1) { 

#run.case  <- c("w_8_p_60_2D","w_10_p_80_3D", "w_12_p_100_4D") 
#track.dir.case <- c("/TRACK_DATA_2D/","/TRACK_DATA_3D/","/TRACK_DATA_4D/") 
 
run.case  <- c("w_10_p_0_3D") 
track.dir.case <- c("/TRACK_DATA_3D/") 
 
track.dir <- track.dir.case[icase] 

#all.table <- subset( all.table, (all.table$run.case == run.case[icase])   )

neutral.table  <-  subset(all.table, abs(all.table$eff.size.for) <= 0.25 )
negative.table <-  subset(all.table,  (all.table$eff.size.for) < -0.25 )
positive.table <-  subset(all.table,  (all.table$eff.size.for) > 0.25 )

#neutral.table  <-  subset(all.table,  all.table$group == "Neutral"  )
#negative.table <-  subset(all.table,  all.table$group == "Negative" )
#positive.table <-  subset(all.table,  all.table$group == "Positive" )

# only for JJA
#neutral.table  <-  subset(all.table,  ((all.table$date.mon >= as.integer(6)) & (all.table$date.mon <= as.integer(8))) )
#negative.table <-  subset(all.table,  ((all.table$date.mon >= as.integer(6)) & (all.table$date.mon <= as.integer(8))) ) 
#positive.table <-  subset(all.table,  ((all.table$date.mon >= as.integer(6)) & (all.table$date.mon <= as.integer(8))) ) 


neu_gp <- neutral.table$date.time
neg_gp <- negative.table$date.time
pos_gp <- positive.table$date.time


neu_date <- neu_gp
neg_date <- neg_gp
pos_gate <- pos_gp



#load_ini_lai_file
input_lai <- fun_read_nc("/lfs/home/ychen/LAI_STUDY_EAsia/LAI_DATA/c_gls_LAI_199901100000_VGT_V2_EAST_ASIA.nc",var_st=2)

lai_dir <- c("/lfs/home/ychen/LAI_STUDY_EAsia/LAI_DATA/")

#get file list from the folder 
lai.fnames <- list.files(path=paste(lai_dir,sep=""), pattern="*.nc") 

   wrk.date <- substr( lai.fnames, start=11, stop=18)
   wrk.yr  <- as.integer(substr(wrk.date, start=1, stop=4) )
   #wrk.mon <- as.integer(substr(wrk.date, start=5, stop=6) ) 


   wrk.mon <- formatC(as.integer(substr(wrk.date, start=5, stop=6) ),format="d",width=2,flag="0")
   print( paste("working date:", wrk.date, sep="") )

   #SPEI data
   spei_fname = paste("/lfs/home/ychen/LAI_STUDY_EAsia/SPEI_DATA/","spei02_", wrk.yr,"_",wrk.mon,"_EA.nc",sep="")



#
for (ieve in 1:length(all.table$date.time)) {
  
  #print( paste("working date:", wrk.date, sep="") )
 


  iyr  = substr(all.table$date.time[ieve],start=1,stop=4)

   if (iyr <= 2013) {

     lai_fname = paste("c_gls_LAI_",all.table$date.time[ieve],"0000_VGT_V2_EAST_ASIA.nc",sep="") 
   }else{

     lai_fname = paste("c_gls_LAI_",all.table$date.time[ieve],"0000_PROBAV_V2_EAST_ASIA.nc",sep="")

   }


#  lai_fname=paste(lai_dir,lai_fname,sep="") 

  print(lai_fname)

  tmp_date <-  all.table$date.time[ieve]
  ipt = as.integer(60/10) 
  file.id <- which(wrk.date==tmp_date) + ipt
  print(paste("wrk file id :", file.id, sep=""))
  print(paste("wrk file_post_two_month:", lai.fnames[file.id], sep=""))



    tc.pot <- fun_read_nc(arg1=paste("/lfs/home/ychen/LAI_STUDY_EAsia/",track.dir,"/","tc_mask_pot_",tmp_date,"00.nc",sep=""), var_st=1)
#
       tc.aff.mask <- tc.pot$tc_pot
   #    tc.aff.mask[tc.aff.mask==0] <- NA 
    
# set lai file as two months later 
lai_fname = paste(lai_dir,lai.fnames[file.id],sep="")

  
# neu     
      for (ineu in 1:length(neu_gp) ) {
         if( neu_gp[ineu] == tmp_date ) {
         tmp1_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI*tc.aff.mask
         neu_lai <- tmp1_arr + neu_lai
 
         #count tc times in the tc_aff_mask  
         neu_n <- neu_n + tc.aff.mask
         
         tmp1_arr <- fun_read_nc(arg1=spei_fname,var_st=1)$SPEI*tc.aff.mask 
         neu_spei <- tmp1_arr + neu_spei 

         print("neutral event.")
         }
      } 
# neg
      for (ineg in 1:length(neg_gp) ) {
         if( neg_gp[ineg] == tmp_date ) {
         #tmp2_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI*tc.aff.mask
         # eff_size * std_lai
#         tmp2_arr <- tc.aff.mask * ((negative.table$std.for.lai.aff[ineg] + negative.table$std.for.lai.ref[ineg])/2. ) * negative.table$eff.size.for[ineg] 
         tmp2_arr <- tc.aff.mask * (negative.table$std.for.lai.aff[ineg]  ) * negative.table$eff.size.for[ineg]  

#         image.plot(tmp2_arr)

         neg_lai <- tmp2_arr + neg_lai
         neg_n <- neg_n + tc.aff.mask

         # for delta spei
         tmp2_arr <- tc.aff.mask * (negative.table$eve.for.spei.aff[ineg]- negative.table$eve.for.spei.ref[ineg])
         neg_spei <- tmp2_arr + neg_spei 

         print("negative event.")
         }
      } 
# pos
      for (ipos in 1:length(pos_gp) ) {
         if( pos_gp[ipos] == tmp_date ) {
         #tmp3_arr <- fun_read_nc(arg1=lai_fname,var_st=2)$LAI*tc.aff.mask 
         # eff_size * std_lai
        # tmp3_arr <- tc.aff.mask * ((positive.table$std.for.lai.aff[ipos] + positive.table$std.for.lai.ref[ipos])/2. ) * positive.table$eff.size.for[ipos] 
          tmp3_arr <- tc.aff.mask * (positive.table$std.for.lai.aff[ipos] ) * positive.table$eff.size.for[ipos] 
         pos_lai <- tmp3_arr + pos_lai 
         pos_n <- pos_n + tc.aff.mask
 
         # for delta spei
         tmp3_arr <- tc.aff.mask * (positive.table$eve.for.spei.aff[ipos]-  positive.table$eve.for.spei.ref[ipos])
         pos_spei <- tmp3_arr + pos_spei 

        print("positive event.")
         }

      } 
} #end_for



} # end of run icase


#take average
neu_lai <- (neu_lai*lc.for.mask)/ (neu_n)
neg_lai <- (neg_lai*lc.for.mask)/ (neg_n) 
pos_lai <- (pos_lai*lc.for.mask)/ (pos_n)


#take average

neu_spei <- (neu_spei*lc.for.mask)/ (neu_n)
neg_spei <- (neg_spei*lc.for.mask)/ (neg_n)
pos_spei <- (pos_spei*lc.for.mask)/ (pos_n)




 #end for 
#convert arr to raster


neu.raster.lai <-  raster( x=t(neu_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

neg.raster.lai <-  raster( x=t(neg_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

pos.raster.lai <-  raster( x=t(pos_lai),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))
# for spei
neu.raster.spei <-  raster( x=t(neu_spei),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

neg.raster.spei <-  raster( x=t(neg_spei),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))

pos.raster.spei <-  raster( x=t(pos_spei),
                              xmn=min(input_lai$lon),  xmx=max(input_lai$lon),
                              ymn=min(input_lai$lat),  ymx=max(input_lai$lat),
                              crs=CRS("+proj=longlat +datum=WGS84"))



ld_go<-T

if (ld_go) {

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


} #end ld_go




#######################################################################
#
#   all phase plot  
#
#######################################################################

library("viridis")

pdf(file= "all_phases_3a_eff_60days.pdf" , width = 27, height = 9 ) 
  my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(11)
  my.breaks.b2b<- round(seq(100900, 101900, length.out = 11), digits=0)
 # my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"),alpha=1)(11)
 # my.breaks.b2r<- round(seq(100900, 102000, length.out = 11), digits=1)
#  my.color.w2g <- colorRampPalette(
#    c("ivory","wheat","greenyellow","yellowgreen","limegreen"
#      ,"forestgreen","darkgreen","darkgreen"))(36)
   my.color.w2g <- colorRampPalette(
    c("ivory","yellowgreen","darkgreen"))(15)
  my.breaks.w2g<- seq(0,7,length.out=15)
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
 #plot legend 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neu.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180), col=my.color.w2g,breaks=my.breaks.w2g,
       smallplot=c(0.65,.95, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.w2g, labels=my.breaks.w2g, cex.axis=1),
       legend.args=list(text= expression("LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)
#  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  contour(neu.raster, lev=my.breaks.b2b, add=TRUE, 
          ylim=c(0,60),xlim=c(100,180), col="black",labcex=1.5,col.axis="black",lwd=0.6 )
  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 #  mtext("Longitude" , side=1, line=2.0, cex=1.0) 
 #
 #######postive - neutral cases ##########

 my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(11)
 my.breaks.b2b<- round(seq(-250, 250, length.out = 11), digits=0)
 
 my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
 my.breaks.b2r<- round(seq(-300, 200, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)
  my.color.b2g <- colorRampPalette(
    c("ivory","forestgreen"))(11)
 # my.color.b2g <- gray.colors(11,start = 1.0, end = 0.0)
  my.breaks.b2g<- seq(0,1.0,length.out=11)
  leg.at  <-  c("-200", "-100","0","100","200") 
  leg.txt <-  c("-200", "-100","0","100","200") 
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
  plot(pos.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(0,1.0), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.6, 0.98, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=1),
       legend.args=list(text= expression(delta*"LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(pos.neu.raster, lev=my.breaks.b2b, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2b,labcex=1.5,lwd=0.6 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 
##############  negative - neutral cases ########################### 
  my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(33)
  my.breaks.b2b<- round(seq(-800, 800, length.out = 33), digits=0)
  
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
  my.breaks.b2r<- round(seq(-400, 400, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("-300","-200","-100","0","100","200","300") 
  leg.txt <-  c("-300","-200","-100","0","100","200","300") 
  my.color.b2g <- colorRampPalette(
   c("brown","wheat","ivory"))(11)
#  my.color.b2g <- gray.colors(11,start = 0, end = 1.0)
  my.breaks.b2g<- seq(-1,0,length.out=11)
  # set subplot margin
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180),add=T)
  #### 
  par(fig=c(0.65,.95,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neg.raster.lai,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.0,0),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot legend 
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neg.raster.lai, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-1.0,0), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.60,.98, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=1),
       legend.args=list(text= expression(delta*"LAI("*"m"^2*"/m"^2*")") ,side=3, line=.1, cex=1) )
   #add coastaline
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)

  # add contour
  contour(neg.neu.raster, lev=my.breaks.b2b, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2b,labcex=1.5,lwd=0.6 )
  box()

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
dev.off()

ld_go <- T

if (ld_go) {
pdf(file= "all_phases_3a_spei_60days.pdf" , width = 27, height = 9 ) 
  my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(11)
  my.breaks.b2b<- round(seq(100900, 101900, length.out = 11), digits=0)
 # my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"),alpha=1)(11)
 # my.breaks.b2r<- round(seq(100900, 102000, length.out = 11), digits=1)
#  my.color.w2g <- colorRampPalette(
#    c("ivory","wheat","greenyellow","yellowgreen","limegreen"
#      ,"forestgreen","darkgreen","darkgreen"))(36)
   my.color.w2g <- colorRampPalette(
    c("red","orange","ivory","lightgreen","forestgreen"))(13)
  my.breaks.w2g<- seq(-3.,3.,length.out=13)
  leg.at.spei  <- my.breaks.w2g 
  leg.txt.spei <- my.breaks.w2g 
  # set subplot margin
#Neutral cases 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180))
  #### 
  par(fig=c(0.05,.35,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neu.raster.spei,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-3,3),
         col=my.color.w2g,breaks=my.breaks.w2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
 #plot legend 
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neu.raster.spei, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180), col=my.color.w2g,breaks=my.breaks.w2g,
       smallplot=c(0.65,.95, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.w2g, labels=my.breaks.w2g, cex.axis=1),
       legend.args=list(text= expression("SPEI[-]") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.05,.35,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)
#  par(fig=c(0.02,.96,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  contour(neu.raster, lev=my.breaks.b2b, add=TRUE, 
          ylim=c(0,60),xlim=c(100,180), col="black",labcex=1.5,col.axis="black",lwd=0.6 )
  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 #  mtext("Longitude" , side=1, line=2.0, cex=1.0) 
 #
 #######postive - neutral cases ##########

 my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(11)
 my.breaks.b2b<- round(seq(-250, 250, length.out = 11), digits=0)
 
 my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
 my.breaks.b2r<- round(seq(-300, 200, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)
  my.color.b2g <- colorRampPalette(
    c("brown","ivory","forestgreen"))(11)
 # my.color.b2g <- gray.colors(11,start = 1.0, end = 0.0)
  my.breaks.b2g<- seq(-.5,.5,length.out=11)
  leg.at  <-  c("-.5", "-.25","0",".25",".5") 
  leg.txt <-  c("-.5", "-.25","0",".25",".5") 
  # set subplot margin
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180),add=T)
  #### 
  par(fig=c(0.35,.65,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(pos.raster.spei,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-.5,.5),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot lagend 
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(pos.raster.spei, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-.5,.5), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.6, 0.98, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=1),
       legend.args=list(text= expression(delta*"SPEI[-]") ,side=3, line=.1, cex=1) )
  #add coastaline
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  par(fig=c(0.35,.65,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  #add contour
  contour(pos.neu.raster, lev=my.breaks.b2b, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2b,labcex=1.5,lwd=0.6 )

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
  box()
 
##############  negative - neutral cases ########################### 
  my.color.b2b<- colorRampPalette(c("lightgray","gray","black"),alpha=1)(33)
  my.breaks.b2b<- round(seq(-800, 800, length.out = 33), digits=0)
  
  my.color.b2r<- colorRampPalette(c("blue","cyan","yellow","red"))(21)
  my.breaks.b2r<- round(seq(-400, 400, length.out = 21), digits=0)
  #my.breaks<- c(-.5,0.05,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)

  leg.at  <-  c("-300","-200","-100","0","100","200","300") 
  leg.txt <-  c("-300","-200","-100","0","100","200","300") 
  my.color.b2g <- colorRampPalette(
   c("brown","ivory","forestgreen"))(11)
#  my.color.b2g <- gray.colors(11,start = 0, end = 1.0)
  my.breaks.b2g<- seq(-.5,.5,length.out=11)
  # set subplot margin
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(coastlines,col="white",lwd=0,ylim=c(0,60),xlim=c(100,180),add=T)
  #### 
  par(fig=c(0.65,.95,0.02,0.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis 
  plot(neg.raster.spei,  legend=FALSE,ylim=c(0,60),xlim=c(100,180),zlim=c(-.5,.5),
         col=my.color.b2g,breaks=my.breaks.b2g, box=T, 
       xlab="", ylab="", cex.lab=1., cex.main=2.0,
       main=paste("",sep=""),add=T )
  #plot legend 
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60)) #no buffer in x-y axis
  plot(neg.raster.spei, legend.only=TRUE,ylim=c(0,60),xlim=c(100,180),zlim=c(-.5,.5), col=my.color.b2g,breaks=my.breaks.b2g,
       smallplot=c(0.60,.98, 0.17, 0.2), add=T, horizontal=T,
       axis.args=list(at=my.breaks.b2g, labels=my.breaks.b2g, cex.axis=1),
       legend.args=list(text= expression(delta*"SPEI[-]") ,side=3, line=.1, cex=1) )
   #add coastaline
  par(fig=c(0.65,.95,0.02,.96), mar = c(4, 4.5, 1.5 ,0.1), usr=c(100,180,0,60), xpd=FALSE)
  plot(coastlines, lwd=1,ylim=c(0,60),xlim=c(100,180),add=T  )
  par(xpd=TRUE)
  #text(x=92,y=60, label="a",cex=1.2, font=2)
  par(xpd=FALSE)

  # add contour
  contour(neg.neu.raster, lev=my.breaks.b2b, add=TRUE, ylim=c(0,60),xlim=c(100,180), col=my.color.b2b,labcex=1.5,lwd=0.6 )
  box()

  degs.N <- seq(0,60, length=7)
  #degs.lab = sapply(degs, function(a) bquote(.(a) * degree))
  axis(side = 2, at = degs.N, srt=0, las=1, 
       labels = paste0(degs.N,"°","N") , tck = 0.02,cex.axis=1.5)
 # mtext("Latitude" , side=2, line=2.0, cex=1.0) 
  deg.E <- c("100","110","120","130","140","150","160","170","180")
  axis(side = 1, at = seq(100,180,length.out=9), 
       labels = paste0(deg.E,"°","E"), tck = 0.02,cex.axis=1.5)
dev.off()

} #end ld_go if 

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



