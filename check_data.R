# import the function

source("src_function_ychen.R")
#source("/home/u9197633/R_scripts/function/src_function_ncdf4.R")
# get nc data
wrk_yr=2023
land_input <- fun_read_nc(arg1=paste(wrk_yr,"_lulcc_500m_wgs84.nc",sep=""))
out_filename <- paste(wrk_yr,"_lulcc_500m_wgs84_cor.nc",sep="") 



lon <-  land_input$Longitude
nx=length(lon) 
#lon <- c(lon[(1536/2+1):1536]-360,lon[1:(1536/2)])
#lon[lon>180] <- lon - 360 
lat <-  land_input$Latitude
ny=length(lat)
#mask <- land_input$land_mask
#temp<-mask
#mask[1:(1536/2),]  <- temp[(1536/2+1):1536,]
#mask[(1536/2+1):1536,] <- temp[1:(1536/2),] 

print("load libraries...")
library("ncdf")

nav_lon <- array ( NA, dim = c(nx, ny) )
nav_lat <- array ( NA, dim = c(nx, ny) )




# $ Logitude         : num [1:747(1d)] 120 120 120 120 120 ...
# $ Latitude         : num [1:900(1d)] 26 26 26 26 26 ...
# $ Coverage fraction: int [1:7(1d)] 1 2 3 4 5 6 7
# $ LU_TYPE          : num [1:747, 1:900, 1:7] NA NA NA NA NA NA NA NA NA NA ...
df <- data.frame()

for (ix in 1:nx) {
for (iy in 1:ny) {
    nav_lon[ix,iy] <- lon[ix]
    nav_lat[ix,iy] <- lat[iy]

    if ( is.na(land_input$LU_TYPE[ix,iy,1]) != TRUE ) {
       aa_sum <- sum(land_input$LU_TYPE[ix,iy,2:7])
       epsilon <- abs(1.0 - aa_sum)  
       if ( ( epsilon >= 1E-4)) {
       tmp <- data.frame(lon=lon[ix], lat=lat[iy])
       df <- rbind(df,tmp)
       print(paste("residule land share percentage:","lon:",lon[ix],"lat:",lat[iy],sep="")) 
       # reallocate the problematic pixels\
       for (iz in 2:7) {
           land_input$LU_TYPE[ix,iy,iz]= land_input$LU_TYPE[ix,iy,iz] + epsilon/(7-2+1)
       } 
      
       }
    } 
}
}


nx=length(lon)
ny=length(lat)

#plot(x=df$lon, y=df$lat, cex=0.2, pch=24) 
	dimX <- dim.def.ncdf("lon","",  1:nx, create_dimvar=FALSE    )
	dimY <- dim.def.ncdf("lat","",  1:ny, create_dimvar=FALSE    )
        dimZ <- dim.def.ncdf("lu","",   1:7,  create_dimvar=FALSE    )
 	mv <- 1e+20
	set1 <- var.def.ncdf( "nav_lon", "degrees_east",list(dimX,dimY),longname="Longitude"  , mv  )
	set2 <- var.def.ncdf( "nav_lat", "degrees_north",list(dimX,dimY),longname="Latitude"  , mv  )
	lu_type <- var.def.ncdf( "lu_type", "[class];[%]", 
                                 list(dimX,dimY,dimZ), longname="land use type [1] and cover fraction of each class [2:7]" ,mv)


	#==== creat the nc file based on definition ========================== 
	names<-list(set1,set2,lu_type)
	#setting the input_information from "out_filename"  
	output_nc <- create.ncdf( out_filename, names)
	#=====================================================================

	#===== set attributes of variables in nc files for various types =====
	#longitude
	att.put.ncdf( output_nc, set1, "title","lon" ,               prec="text"   ) 
	att.put.ncdf( output_nc, set1, "axis", "X"       ,           prec="text"   )
	#att.put.ncdf( output_nc, set1, "valid_max", 180.0  ,         prec="single" )
	#att.put.ncdf( output_nc, set1, "valid_min",-180.0  ,         prec="single" )
	#att.put.ncdf( output_nc, set1, "modolo", 360.0,              prec="single" )
	#att.put.ncdf( output_nc, set1, "topology","circular",        prec="text"   )
	#att.put.ncdf( output_nc, set1, "_CoordinateAxisType", "Lon", prec="text"   )
	#latitude
	att.put.ncdf( output_nc, set2, "title","lat"  , prec="text"   )
	att.put.ncdf( output_nc, set2, "axis", "Y"       , prec="text"   ) 
	#att.put.ncdf( output_nc, set2, "valid_max", 90.0   , prec="single" )
	#att.put.ncdf( output_nc, set2, "valid_min",-90.0   , prec="single" )
	#att.put.ncdf( output_nc, set2, "_CoordinateAxisType", "Lat", prec="text") 

        att.put.ncdf( output_nc, lu_type, "title", "LU_TYPE" , prec="text" )
        att.put.ncdf( output_nc, 0, "description", "lu_type[1]:the marjority of the land-use/land-cover type, lu_type[2]:forest, lu_type[3]:agri, lu_type[4]:water, lu_type[5]:built-up/bare soil,lu_type[6]:unkn, lu_type[7]:rgrass/shrub, unit in share of land (fraction), the fraction was originally derived from SPOT images at a spatial resoultion of 6m " , prec="text" )
        att.put.ncdf( output_nc, 0, "version", "this dataset is only used for the reseach purpose." , prec="text" )
       # att.put.ncdf( output_nc, 0, "licence", "This file is distributed under the terms of the GNU General Public License, either Version 2, June 1991 or Version 3, June 2007.", prec="text")  
        att.put.ncdf( output_nc, 0, "contact", "Yi-Ying Chen, Email:yiyingchen@gate.sinica.edu.tw", prec="text")  


	print(paste("Writing the dataset to nc file."))
	#put coordinate & time  
	put.var.ncdf(output_nc, set1, nav_lon[,ny:1]      )
	put.var.ncdf(output_nc, set2, nav_lat[,ny:1]      )
	#put compressed land index 
	#put forcing data
	put.var.ncdf(output_nc, lu_type, land_input$LU_TYPE[,ny:1,]   )
	
        close.ncdf(output_nc)
	#print(paste("Finished zoom grid io transfering for ",in_filename))
	#print(paste("Produced ouput file:", out_filename) )
	#} # skip mark end

	#finished
	print(paste("create ", out_filename, "!", sep="") )
	#rm(list=ls()) 


