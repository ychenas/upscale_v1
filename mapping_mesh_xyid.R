# Author name: Yi-Ying Chen 
# Email: yiyingchen@gate.sinica.edu.tw
# 
# load R library 

#

library("sp")
library("raster")
library("rgdal") #readOGR
library("rgeos") #gCentroid
library("viridis") 
library("proj4")
#library("snow")
#library("tidyverse")

# load finish net at 500m by 500m spacing 

mesh.500m = readOGR(verbose = FALSE, 
            "/home/u9197633/R_scripts/SPOT_CLASS/fishnet/mesh/500m/taiwan_raster_t97.shp")
#mesh.500m = readOGR(verbose = FALSE, 
#            "/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/fishnet/mesh/500m/taiwan_raster_WGS84.shp")
#convert the projection into twd97
#mesh.500m =  spTransform(mesh.500m, sp::CRS("+init=epsg:3826"))  

mesh.12km = readOGR(verbose = FALSE,
             "/home/u9197633/R_scripts/SPOT_CLASS/fishnet/mesh/12km/SPOT_12km_TW_MESH_260_images.shp")
#convert the projection into twd97
mesh.12km =  spTransform(mesh.12km, sp::CRS("+init=epsg:3826"))  

# get gelocation of center point  of the grid.
cent.xy.500m = gCentroid(mesh.500m, byid=TRUE)
# create dataframe for xy coordinate
df.500m <- as.data.frame(cent.xy.500m)
# Using add_column()
df.500m.share <- data.frame(x=df.500m$x, y=df.500m$y,forest = 0, agri=0, water=0, built=0, other=0  )
cent.xy.12km = gCentroid(mesh.12km, byid=TRUE)


# 
columns = c("mesh","xid","yid") 
df.imesh.xyid = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df.imesh.xyid) = columns

# set a tempotory table 
df.meshxy.tmp <- df.imesh.xid



for (imesh in 1:length(mesh.12km@data$XID)) {

print(paste("Working on imesh:",imesh,".", sep=" "))
xid=mesh.12km@data$XID[imesh]
yid=mesh.12km@data$YID[imesh]

print( paste("mesh_index:",imesh, "XID:", xid, " YID:", yid, sep="") )

  # assign mapping information  
   df.tmp <- data.frame(mesh=imesh, xid=xid, yid=yid)
   #update the data.frame 
   df.imesh.xyid <- rbind(df.imesh.xyid, df.tmp)
   
}

#write out mapping information between mesh, xid and yid information 
write.csv(x=df.imesh.xyid, file = "./mesh_xid_yid.csv", sep = ",",
                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                 col.names = TRUE, qmethod = c("escape", "double"))



