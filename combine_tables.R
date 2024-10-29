
library("sp")
library("raster")
library("rgdal") #readOGR
library("rgeos") #gCentroid
library("viridis") 
library("proj4")
#
# search table in the folder of df_table
wrk_yr=2023

df.list <- list.files(path = paste("./df_table_",wrk_yr,"/",sep=""), pattern = "*.csv", all.files = FALSE,
                full.names = TRUE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE)

#combine tables

all.table <- data.frame()

for (it  in 1:length(df.list)){  
  tmp.table <- read.csv(file=df.list[[it]]) 
  all.table <- rbind(all.table, tmp.table)
}

# output the all.table and convert to the raster and nc file
#write.csv(all.table, file=paste("./df_table_",wrk_yr,"/combined_table.csv",sep=""), row.names=FALSE)


#use the majority of land use type at 500m resolution

#load the 500m grid data template

mesh.500m = readOGR(verbose = FALSE, 
            "/lfs/home/ychen/lfs_dir/GIS/Taiwan_Fishnet/500m/taiwan_raster_t97.shp")
#mesh.500m = readOGR(verbose = FALSE, 
#            "/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/fishnet/mesh/500m/taiwan_raster_WGS84.shp")
#convert the projection into twd97
#mesh.500m =  spTransform(mesh.500m, sp::CRS("+init=epsg:3826"))  

# get gelocation of center point  of the grid.
cent.xy.500m = gCentroid(mesh.500m, byid=TRUE)
# create dataframe for xy coordinate
df.500m <- as.data.frame(cent.xy.500m)
# Using add_column()
df.500m <- data.frame(x=df.500m$x, y=df.500m$y,lu_type=0, forest = 0, agri=0, water=0, built=0, other=0, grass=0  )


#set a tolerance of error by 2.0 meter 

epsilon=2.0 

for (ipt in 1: length(all.table$x)) {
#for (ipt in 1: 100) {
 print(paste("ipt:",ipt,sep=""))
# tmp_id <- which ( (abs(all.table$x[ipt] - df.500m$x)<= epsilon) & (abs(all.table$y[ipt]-df.500m$y) <= epsilon) )  
 tmp_id <- which ( (all.table$x[ipt] == df.500m$x) & (all.table$y[ipt]==df.500m$y)  )  

 df.500m$forest[tmp_id] <- all.table$forest[ipt] 
 df.500m$agri[tmp_id]  <- all.table$agri[ipt] + all.table$other[ipt]*0.2
 df.500m$water[tmp_id] <- all.table$water[ipt] 
 # merge built-up and other
 df.500m$built[tmp_id] <- all.table$built[ipt] + all.table$other[ipt]*0.8  
 df.500m$grass[tmp_id] <- all.table$grass[ipt]
 df.500m$other[tmp_id] <- 0. 
# df.500m$other[tmp_id] <- 0.

# formt of aa tbale #       3            4     5         6          7
#            x       y    forest        agri water      built      other
#7170 173773.4 2521366 0.8812626 0.001835199     0 0.03468526 0.08221692
 aa <- c(df.500m$forest[tmp_id], df.500m$agri[tmp_id] , df.500m$water[tmp_id] , df.500m$built[tmp_id], df.500m$grass[tmp_id], df.500m$other[tmp_id]) 
 df.500m$lu_type[tmp_id] <- which(aa==max(aa)) 
 # check sum 
 aa_sum <- sum(aa)
 residual_aa <- 1.0 - aa_sum
 #redistribute the residual fraction at 500m resolution 
 if ( abs(residual_aa) >=1E-5) {
    df.500m$forest[tmp_id] = df.500m$forest[tmp_id] + residual_aa/5.
    df.500m$agri[tmp_id] = df.500m$agri[tmp_id] + residual_aa/5.
    df.500m$water[tmp_id] = df.500m$water[tmp_id] + residual_aa/5.
    df.500m$built[tmp_id] = df.500m$built[tmp_id] + residual_aa/5.
    df.500m$grass[tmp_id] = df.500m$grass[tmp_id] + residual_aa/5.
    } 
#print(df.500m[tmp_id,]) 
#<-  all.table[ (abs(all.table$x - df.500m$x)<= epsilon) & (abs(all.table$y-df.500m$y) <= epsilon ),]     
}





ld_go <- TRUE

if(ld_go) {

#conver dataframe to raster object 
raster.lulcc <- rasterFromXYZ(all.table, crs=sp::CRS("+init=epsg:3826"))
#save the ratser as netCDf file
writeRaster(raster.lulcc, paste("./df_table_",wrk_yr,"/",wrk_yr,"_all_table_combined_lulcc_500m_twd97.nc",sep=""), 
        overwrite=TRUE, format="CDF", varname="LU", varunit="fraction", 
	longname="Landuse/cover type derived from SPOT images 6m and upscale to 500m grid, Forest=1, Builtup=2, Water=3, Agri=4, Unkn.=5, Grassland=6 ",
	xname="x", yname="y", zname="Coverage fraction")

names(raster.lulcc) <- c("forest", "agri", "water", "builtup", "unkn", "grass")
#save the ratser as GeoTIF file
writeRaster(raster.lulcc*10000, paste("./df_table_",wrk_yr,"/",wrk_yr,"_combined_lulcc_500m_twd97_",names(raster.lulcc),".tif",sep=""), 
        format = "GTiff", datatype = "INT2U", options = c("COMPRESS=NONE", "TFW=YES"),
        overwrite = TRUE, bylayer = TRUE)


#conver dataframe to raster object 
raster.lulcc.500m <- rasterFromXYZ(df.500m[,,1], crs=sp::CRS("+init=epsg:3826"))
#
raster.lulcc.500m.wgs84 <- projectRaster(raster.lulcc.500m,  res=c(0.00506,0.004598),  crs=as.character(CRS("+init=epsg:4326")))
#write raster file
writeRaster(raster.lulcc.500m.wgs84, paste("./",wrk_yr,"_lulcc_500m_wgs84.nc",sep=""), overwrite=TRUE, format="CDF", varname="LU_TYPE", varunit="majority_lu_type",
        longname="Landuse/cover type derived from SPOT images 6m and upscale to 500m grid, Forest=1, Builtup=2, Water=3, Agri=4, Unkn.=5, Grassland=6 ",
        xname="Longitude", yname="Latitude", zname="Coverage fraction")



}





