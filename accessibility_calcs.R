
# main
# include all six counties

#setwd("D:/summer, 2011/R/R_Calculations")

#========== begin public =============
CBD<-t(as.matrix(c(-118.446305110092,34.0615632358611)))
geodist<-function(center_coor,coordinates)
  {
    a<-57.2958
   dist_center<-3963*acos( outer(sin(center_coor[,2]/a),sin(coordinates[,2]/a))+
   outer(cos(center_coor[,2]/a),cos(coordinates[,2]/a))*cos(outer(-        center_coor[,1]/a,coordinates[,1]/a,"+")))
  return(dist_center)
  }
con<-1609.344


library(rgdal)
library(foreign)

coor<-read.csv(file="subcoord.csv")
subid<-as.numeric(as.matrix(as.data.frame(table(coor$subcenterid))$Var1))[-1]
subcoor<-matrix(0,nrow=51,ncol=3)
for (k in 1:length(subid))
{  
   for(i in 1:dim(coor)[1])
     {
       if (coor$subcenterid[i]==subid[k])
         subcoor[k,]<-subcoor[k,]+  c(coor$EMP03[i]*coor$longitude[i],coor$EMP03[i]*coor$latitud[i],coor$EMP03[i]) 
        }
}
subcoor1<-cbind(subcoor[,1]/subcoor[,3],subcoor[,2]/subcoor[,3])
#========== end public =============

#======== San Bernardino county ================
parcel_sb<-read.dbf("SBparcelMZ.dbf")
parcel_sb1<-subset(parcel_sb,LU_08>=1700 & LU_08<3000 & LU_08!=1900)

xy<-cbind(parcel_sb1$X,parcel_sb1$Y)
xy1<-project(xy, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist<-geodist(subcoor1,xy1)
parcel_sb1$fsub<- apply(subdist,2,min)
parcel_sb1$CBD<-as.vector(geodist(CBD,xy1))

write.table(parcel_sb1,"parcel_sb1.csv",row.names = FALSE,sep = "\t")

#======== Imperial county ==============

parcel_im<-read.dbf("IMparcelMZ.dbf")
parcel_im1<-subset(parcel_im,LU_08>=1700 & LU_08< 3000 & LU_08!=1900)

xy_im<-cbind(parcel_im1$X,parcel_im1$Y)
xy_im1<-project(xy_im, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist_im<-geodist(subcoor1,xy_im1)
parcel_im1$fsub<- apply(subdist_im,2,min)
parcel_im1$CBD<-as.vector(geodist(CBD,xy_im1))

#write.table(parcel_im1,"parcel_im1.csv",row.names = FALSE,sep = "\t")
write.csv(parcel_im1,"parcel_im2.csv")




#========== Los Angeles county ==============

parcel_la<-read.dbf("LAparcelMZ.dbf")
parcel_la1<-subset(parcel_la,LU_08>=1700 & LU_08< 3000 & LU_08!=1900)

xy_la<-cbind(parcel_la1$X,parcel_la1$Y)
xy_la1<-project(xy_la, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist_la<-geodist(subcoor1,xy_la1)
parcel_la1$fsub<- apply(subdist_la,2,min)
parcel_la1$CBD<-as.vector(geodist(CBD,xy_la1))

#write.table(parcel_la1,"parcel_la1.csv",row.names = FALSE,sep = "\t")
write.csv(parcel_la1,"parcel_la2.csv")

#============ Ventura county ==============


parcel_vt<-read.dbf("VTparcelMZ.dbf")
parcel_vt1<-subset(parcel_vt,LU_08>=1700 & LU_08< 3000 & LU_08!=1900)

xy_vt<-cbind(parcel_vt1$X,parcel_vt1$Y)
xy_vt1<-project(xy_vt, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist_vt<-geodist(subcoor1,xy_vt1)
parcel_vt1$fsub<- apply(subdist_vt,2,min)
parcel_vt1$CBD<-as.vector(geodist(CBD,xy_vt1))

#write.table(parcel_vt1,"parcel_vt1.csv",row.names = FALSE,sep = "\t")
write.csv(parcel_vt1,"parcel_vt2.csv")


#=========== orange county ================
parcel_or<-read.dbf("ORparcelMZ.dbf")
parcel_or1<-subset(parcel_or,LU_08>=1700 & LU_08< 3000 & LU_08!=1900)

xy_or<-cbind(parcel_or1$X,parcel_or1$Y)
xy_or1<-project(xy_or, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist_or<-geodist(subcoor1,xy_or1)
parcel_or1$fsub<- apply(subdist_or,2,min)
parcel_or1$CBD<-as.vector(geodist(CBD,xy_or1))

#write.table(parcel_or1,"parcel_or1.csv",row.names = FALSE,sep = "\t")
write.csv(parcel_or1,"parcel_or2.csv")

#============ Riverside county ============

parcel_ri<-read.dbf("RVparcelMZ.dbf")
parcel_ri1<-subset(parcel_ri,LU_08>=1700 & LU_08< 3000 & LU_08!=1900)

xy_ri<-cbind(parcel_ri1$X,parcel_ri1$Y)
xy_ri1<-project(xy_ri, "+proj=utm +zone=11 ellps=WGS84",inv=T) 



subdist_ri<-geodist(subcoor1,xy_ri1)
parcel_ri1$fsub<- apply(subdist_ri,2,min)
parcel_ri1$CBD<-as.vector(geodist(CBD,xy_ri1))

#write.table(parcel_ri1,"parcel_ri1.csv",row.names = FALSE,sep = "\t")

## ArcGIS can only read comma separated file.
write.csv(parcel_ri1,"parcel_ri2.csv")



