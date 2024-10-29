#======Local functions define==========


#======================================================== 
#get lm() p- value
#
#Use the lmp function
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
    }
#=========================================================

#=========================================================
# set Font for the ploting in PDF
#
# Author: Yiying CHEN
#
#First Date: 2014-11-27
#
#Purpose:
#        
#=========================================================

# First install
#load local library extrafont
#library(extrafont, lib.loc="/home/orchidee01/ychen/R/library/")
# import the fonts
#font_import()
# show fonts
#fonts()
# This will show more detailed information about fonts
#fonttable()

#library(extrafont, lib.loc="/home/orchidee01/ychen/R/library/")
#load fonts 
#loadfonts()

pdf.options(family='Helvetica')


# Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
require(grid)
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
dots <- list(...)
n <- length(dots)
if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
if(is.null(nrow)) { nrow = ceiling(n/ncol)}
if(is.null(ncol)) { ncol = ceiling(n/nrow)}
## NOTE see n2mfrow in grDevices for possible alternative
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
ii.p <- 1
for(ii.row in seq(1, nrow)){
ii.table.row <- ii.row
if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
for(ii.col in seq(1, ncol)){
ii.table <- ii.p
if(ii.p > n) break
print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
ii.p <- ii.p + 1
}
}
}


#=========================================================
# make Radar plot Radar_Plot 
#
# Author: 
#
#First Date: 
#
#Purpose: use ggplot library to plot radar plot
#         
#=========================================================
#Funtion RadarPlot source code
Radar_Plot <- function(plot.data,
                             axis.labels=colnames(plot.data)[-1],                             
                             grid.min= -0.2,  #10,
                             grid.mid=  0.5,  #50,
                             grid.max=  1.0,  #100,
                             centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                             plot.extent.x.sf=1.2,
                             plot.extent.y.sf=1.2,
                             x.centre.range=0.02*(grid.max-centre.y),
                             label.centre.y=FALSE,
                             grid.line.width=0.5,
                             gridline.min.linetype="longdash",
                             gridline.mid.linetype="longdash",
                             gridline.max.linetype="longdash",
                             gridline.min.colour="lightgrey",
                             gridline.mid.colour="lightgray",
                             gridline.max.colour="lightgrey",
                             grid.label.size=5.0,
                             gridline.label.offset=-0.02*(grid.max-centre.y),
                             label.gridline.min=TRUE,
                             axis.label.offset=1.15,
                             axis.label.size=4,
                             axis.line.colour="lightgrey",
                             group.line.width=0.5,
                             group.point.size=3,
                             background.circle.colour="lightgray",
                             background.circle.transparency=0.1,
                             plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                             legend.title=NULL,
                             legend.text.size=16,
                             plot.title=NULL,
                             legend.position=c(0.5, 1.2 ) ) {
#  library(ggplot2)
#  library(labeling)
# filter out NA  data and set to zero
plot.data[is.na(plot.data)] <- 0.
#library(ggplot2,  lib.loc="/home/orchidee01/ychen/R/library/")
#library(labeling, lib.loc="/home/orchidee01/ychen/R/library/")

  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n

  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf

  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  
#Declare required internal functions

CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

  path <- as.factor(as.character(df[,1]))
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]
  
  for(i in levels(path)){
    
    pathData = subset(df, df[,1]==i)
    
    for(j in c(2:ncol(df))){
      
      #pathData[,j]= pathData[,j]
      
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  
  graphData #data frame returned by function
  
}
  
CaclulateAxisPath = function(var.names,min,max) {
  #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
  
  #Args:
  #var.names - list of variables to be plotted on radar plot
  #min - MININUM value required for the plotted axes (same value will be applied to all axes)
  #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    
  #var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  
  #Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i,min.x[i],min.y[i])
    b <- c(i,max.x[i],max.y[i])
    axisData <- rbind(axisData,a,b)
  }
  
  #Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no","x","y")
  rownames(axisData) <- seq(1:nrow(axisData))
  
  #Return calculated axis paths
  as.data.frame(axisData)
}


funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

### Convert supplied data into plottable format
   
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  print(group$path)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  
  # (d) Create file containing axis labels + associated plotting coordinates
  
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  
  # (e) Create Circular grid-lines + labels
  
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(formatC(grid.min,width=3,digits=1,format="f")) )
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(formatC(grid.max,width=3,digits=1,format="f")) )
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(formatC(grid.mid,width=3,digits=1,format="f")) )
  print(gridline$min$label)
  print(gridline$max$label)
  print(gridline$mid$label)

  
### Start building up the radar plot

# Delcare 'theme_clear', with or without a plot legend as required by user
#[default = no legend if only 1 group [path] being plotted]
theme_clear <- theme_bw() + 
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        legend.key=element_rect(linetype="blank"))

if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")

#Base-layer = axis labels + plot extent
# [need to declare plot extent as well, since the axis labels don't always
# fit within the plot area automatically calculated by ggplot, even if all
# included in first plot; and in any case the strategy followed here is to first
# plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
# then centred labels for axis labels almost immediately above/below x= 0 
# [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
# This building up the plot in layers doesn't allow ggplot to correctly 
# identify plot extent when plotting first (base) layer]

#base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
  geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
            aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
  scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
  scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
  
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)

  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)

  
#  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group,shape=group)
                            ,size=group.point.size)
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,face="bold",size=grid.label.size*0.5, hjust=0.5) }
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,face="bold",size=grid.label.size*0.5, hjust=0.5)
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,face="bold",size=grid.label.size*0.5, hjust=0.5)
  
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,face="bold",size=grid.label.size, hjust=0.5) }
  #... + plot title 
  base <- base + ggtitle( paste(plot.title, sep="")  )

  #... + modified legend title
  if (plot.legend==TRUE) base <- base + 
                                 scale_colour_discrete(name  = legend.title) +
                                 scale_shape_discrete(name   = legend.title) +
                                 theme( legend.position = legend.position  ) + #change legend position
                                 theme( legend.text  = element_text(size= legend.text.size) )+
                                 theme( legend.title = element_text(size= legend.text.size) )
  base
  
}

#=========================================================
# make point observation nc-file according the input list
#
# Author: Yiying CHEN
#
#First Date: 2014-08-07
#
#Purpose: preparing the observation data for the nc file 
#         & doing the unit convertion 
#=========================================================
fun_make_observation <- function (input_list1, input_list2) 
{
#===load ncdf library ====
library(ncdf)
#======= Start loading the variables in the list1 ===============
print(names(input_list1) )
print(input_list2)

#=== unit conservation

#=== inital variables
steps_since   <- NULL
seconds_since <- NULL
#=== get the tatal datatable lenth 
n_tsteps      <- length(input_list1[[1]])
#=== create time step indexs with seconds
for (i in 1:n_tsteps) {
  steps_since[i]   <- 1.0*(i-1)
  seconds_since[i] <- 1800.0*(i-1)  # for half-hourly
}
#=== set some constant and genrate time index 
missing_r = 1.0e+20
#=== arrange a datatable(observation) for the observation data
lon_arr           <- input_list2[[1]]
lat_arr           <- input_list2[[2]]
output_fname      <- input_list2[[3]]
time_start_origin <- input_list2[[4]]


#=== setup a NCDF file "define dimensions and  variables"
#=== the latitude
dimX <- dim.def.ncdf("lon", "",  1:length(lon_arr), create_dimvar=FALSE )
# the longitude
dimY <- dim.def.ncdf('lat' ,"",  1:length(lat_arr), create_dimvar=FALSE)
# the timestep
dimT <- dim.def.ncdf("time_counter", "", 1:n_tsteps,  create_dimvar=FALSE)
mv <- 1e+30
d1 <- var.def.ncdf( "lon", "degrees_east",  dimX,longname="Longitude", mv)
d2 <- var.def.ncdf( "lat", "degrees_north", dimY,longname="Latitude", mv)
d3 <- var.def.ncdf( "time_counter",  paste("seconds since ",time_start_origin ,sep=""), dimT,longname="Time step", mv)
mv <- 1e+20


netcdf.namelist<-list()
netcdf.namelist[["d1"]]<-d1
netcdf.namelist[["d2"]]<-d2
netcdf.namelist[["d3"]]<-d3
#using input list1 variable name to define variables
for (i in 1: length(input_list1)) {
    name_text <- names(input_list1)[i]
    #print(name_text) 
    assign( "temp", var.def.ncdf( paste(names(input_list1)[i] ,sep="")  ,   "not avariable", list(dimX,dimY,dimT), longname= names(input_list1)[i], mv) )
    netcdf.namelist[[name_text]] <- temp 
    assign(paste(names(input_list1)[i] ,sep=""), temp) 
}

#====
#creat the nc file based on definition 
new_nc <- create.ncdf( paste(output_fname, sep=""), netcdf.namelist)
#====
#set attributes of variables in nc files for various types
#longitude
#latitude
#time
#att.put.ncdf( new_nc, d3, "calendar","gregorian" , prec="text" )
att.put.ncdf( new_nc, d3, "calendar","gregorian" , prec="text" )

#put coordinate & time  
put.var.ncdf(new_nc, d1, lon_arr)
put.var.ncdf(new_nc, d2, lat_arr)
put.var.ncdf(new_nc, d3, seconds_since)

#put observation data
for ( i in 1: length(input_list1) ) {
    put.var.ncdf(new_nc, netcdf.namelist[[i+3]], input_list1[[i]] )
    att.put.ncdf(new_nc, netcdf.namelist[[i+3]], "associate", 'time(lat lon)'   , prec="text" )
#
}

close.ncdf(new_nc)
print(paste("Create observation file: ", list2[[3]],sep='')) 

} #end fun_make_observation FUNCTION


#=========================================================
# Creating point forcing file for ORCHIDEE 
#
# Author: Yiying CHEN
#
#First Date: 2014-05-18
#
#Purpose:preparing the forcing data for the nc file 
#        & doing the unit convertion 
#=========================================================
fun_make_forcing <- function (input_list1, input_list2) 
{
print(names(input_list1) )
print(input_list2)
#===load ncdf library ====
library(ncdf)
#library(ncdf,lib.loc="/home/orchidee01/ychen/R/library/")
#======= Start loading the variables in the list1 ===============
swdown   <- input_list1[[1]]    #short wave rad.   [W/m2]
lwdown   <- input_list1[[2]]    #long wave rad.    [W/m2]
rainfall <- input_list1[[3]]    #rainfall rate     [kg/m2/s]  if (mm/30mins)  data*(1/1800.0) for convertion 
snowfall <- input_list1[[4]]    #snowfall rate     [kg/m2/s] 
#indata$--    #specific humidity [kg/kg]  
psurf    <- input_list1[[5]]    #air pressure      [Pa]       if (kPa)  data*1000.0
tair     <- input_list1[[6]]    #air temperature   [K]        if (oC)   data+273.15
qair     <- input_list1[[7]]    #air humidity      [%]
wind     <- input_list1[[8]]    #wind speed        [m/s]
#=== calculated the specific humidity from relative humidity and air temperarute 
#=== saturate water vapor pressure calculation
#esat <- 611.2*exp((17.67 * tair)/(tair+243.5)) # Bolton (1980) tair in (oC); qstat in(Pa)
#eair <- esat* (rhair/100.)
#qair<- ( (0.622*eair) / ((psurf*1000.) - (0.378*eair)) )

#=== unit conservation
psurf    <- psurf*1000.
tair     <- tair + 273.15
rainfall <- rainfall * (1./1800.0) 


#=== get the tatal datatable lenth 
n_tsteps       <- length(input_list1[[1]])

#=== inital variables
steps_since   <- NULL
seconds_since <- as.double(NA, length=n_tsteps)

#=== create time step indexs with seconds
for (i in 1:n_tsteps) {
  steps_since[i]   <- 1.0*(i-1)
  seconds_since[i] <- 1800.0*(i-1)  # for half-hourly
}
#=== set some constant and genrate time index 
missing_r = 1.0e+20
#=== arrange a datatable(forcing) for the forcing data
forcing <- data.frame (
  swdown=swdown,
  lwdown=lwdown,
  rainfall=rainfall,
  snowfall=snowfall,
  qair=qair,
  psurf=psurf,
  tair=tair,
  wind=wind,
  tsteps_since=steps_since,
  seconds_since=seconds_since)	

lon_arr           <- input_list2[[1]]
lat_arr           <- input_list2[[2]]
output_fname      <- input_list2[[3]]
time_start_origin <- input_list2[[4]]

#=== setup a NCDF file "define dimensions and  variables"
#=== the latitude
dimX <- dim.def.ncdf("lon", "",  1:length(lon_arr), create_dimvar=FALSE )
# the longitude
dimY <- dim.def.ncdf('lat' ,"",  1:length(lat_arr), create_dimvar=FALSE)
# the timestep
dimT <- dim.def.ncdf("tstep", "", 1:n_tsteps,  create_dimvar=FALSE)
mv <- 1e+30
d1 <- var.def.ncdf( "lon", "degrees_east",  dimX,longname="Longitude", mv, prec="double")
d2 <- var.def.ncdf( "lat", "degrees_north", dimY,longname="Latitude", mv, prec="double")
d3 <- var.def.ncdf( "tstep",  paste("seconds since ",time_start_origin ,sep=""), dimT, mv, prec="double")
mv <- 1e+20
v1 <- var.def.ncdf( "SWdown", "W/m^2", list(dimX,dimY,dimT),longname="Surface incident SW radiation OR", mv, prec="double")
v2 <- var.def.ncdf( "LWdown", "W/m^2", list(dimX,dimY,dimT),longname="Surface incident LW radiation OR", mv, prec="double")
v3 <- var.def.ncdf( "Rainf", "kg/m^2/s", list(dimX,dimY,dimT),longname="Rainfall rate" ,mv, prec="double")
v4 <- var.def.ncdf( "Snowf", "kg/m^2/s", list(dimX,dimY,dimT),longname="Snowfall rate" ,mv, prec="double")
v5 <- var.def.ncdf( "Qair", "kg/kg", list(dimX,dimY,dimT),longname="Specific humidity 2m" ,mv, prec="double")
v6 <- var.def.ncdf( "PSurf", "Pa", list(dimX,dimY,dimT),longname="Surfce pressure 2m" ,mv, prec="double")
v7 <- var.def.ncdf( "Tair", "K", list(dimX,dimY,dimT),longname="Temperature 2m" ,mv, prec="double")
v8 <- var.def.ncdf( "Wind", "m/s", list(dimX,dimY,dimT),longname="Surf wind speed 2m" ,mv, prec="double")
#====
#creat the nc file based on definition 
names<-list(d1,d2,d3,v1,v2,v3,v4,v5,v6,v7,v8)
new_nc <- create.ncdf( paste(output_fname, sep=""), names)
#====
#set attributes of variables in nc files for various types
#longitude
#latitude
#time
#att.put.ncdf( new_nc, d3, "calendar","noleap" , prec="text" )
att.put.ncdf( new_nc, d3, "calendar","gregorian" , prec="text" )
att.put.ncdf( new_nc, d3, "origin",time_start_origin , prec="text" )
#land sea mask

#short wave radiation
att.put.ncdf( new_nc, v1, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v1, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#long wave radiation
att.put.ncdf( new_nc, v2, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v2, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#rainfall
att.put.ncdf( new_nc, v3, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v3, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#snowfall
att.put.ncdf( new_nc, v4, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v4, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#specific humidity
att.put.ncdf( new_nc, v5, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v5, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#air pressure
att.put.ncdf( new_nc, v6, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v6, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#air temperature
att.put.ncdf( new_nc, v7, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v7, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#wind 
att.put.ncdf( new_nc, v8, "axis", "TYX" , prec="text" )
att.put.ncdf( new_nc, v8, "associate", 'time (nav_lat nav_lon)'   , prec="text" )
#put coordinate & time  
put.var.ncdf(new_nc, d1,lon_arr)
put.var.ncdf(new_nc, d2,lat_arr)
put.var.ncdf(new_nc, d3, forcing$seconds_since)

#put forcing data
put.var.ncdf(new_nc, v1, forcing$swdown   )
put.var.ncdf(new_nc, v2, forcing$lwdown   )
put.var.ncdf(new_nc, v3, forcing$rainfall )
put.var.ncdf(new_nc, v4, forcing$snowfall )
put.var.ncdf(new_nc, v5, forcing$qair     )
put.var.ncdf(new_nc, v6, forcing$psurf    )
put.var.ncdf(new_nc, v7, forcing$tair     )
put.var.ncdf(new_nc, v8, forcing$wind     )
close.ncdf(new_nc)
print(paste("Create forcing file: ", input_list2[[3]],sep='')) 
} #End Fun_make_forcing


#=========================================================
# gap-filling function using mean diurnal average method
#
# Author: Yiying CHEN
#
#First Date: 2014-07-26
#=========================================================
fun_gapfill_md <- function (md_days=7, wrkdata) {
  
  #source("src_function_ychen.R")
  nstep   <- 48
  #ndays   <- 30
  ndays  <- length(wrkdata)/nstep 
  #md_days <- 3
  winsize <- nstep*ndays
  #print(sum( is.na(wrkdata)))
  
  #=== calculate the diuranl mean array ===
  for (i in 1:ndays ){
    
    #=== calculate the gaps in a single day ::ngaps
    ngaps <- sum (is.na(wrkdata[((i-1)*nstep+1) : (i*nstep)   ])) 
    if (ngaps == 0) {
       next
    }
    #print(paste("gaps in day",i,":", ngaps,sep=""))  
     print(paste("Gapfilling for day: ",formatC(i,width=3,digits=0,flag="0"),"~", sep="")) 
    if ( ngaps <= 24 ) {
      #using moving window average to fill the gaps    
      wrkdata[1:(i*nstep) ] =fun_gapfill_avg (winsize=nstep/8, wrkdata=wrkdata[1:(i*nstep) ])   
    } else {
      #using mean diurnal to fill the gaps  
      
      if (i <= md_days ) {
        id1 <- 1
        id2 <- i*nstep
        temp <- wrkdata[id1: id2]
        md_array <-   fun_diurnal_mean (winsize=nstep, rawdata=temp)[["avg"]]
        
      } else {
        id1 <- (i - md_days)*nstep + 1
        id2 <- i*nstep
        temp <- wrkdata[id1: id2]
        md_array <-   fun_diurnal_mean (winsize=nstep, rawdata=temp)[["avg"]]
      } 
      #=== using diuranl mean array to fill the gaps===
      for (j in 1: nstep ){
        id3 <- (i-1)*nstep + j
        if (is.na (wrkdata[id3]) == TRUE) wrkdata[id3] <- md_array[j]
      }
      
    } #if else for gapfilling approach  
    
  } # i days loop
  
  wrkdata
  #plot(wrkdata, typ="l",col="green")
} #end fun_gapfilling_md FUNCTION
   
#==============================================================
#Function: fun_gapfill_avg
#
#First date: 2014-July-??
#
#
#==============================================================
fun_gapfill_avg <- function(winsize, wrkdata)
{
  #initial the std/avg array for each moving windows backward looking
  datasize <- length(wrkdata)
  #wstd  <-  array ( 0., dim = c(datasize) )
  wavg  <- c(0)
  
  for ( i in  1 : (datasize-1) )  {
    n2 <-  i-1 
    n1 <- (n2 - winsize )
    if ( n1 <= 1 ) n1 <- 1 
    if ( n2 <= 1 ) n2 <- 1    
    #   wstd[i] <- sd(wrkdata[n1:n2])
    wavg <- mean(wrkdata[n1:n2], na.rm=TRUE)
    if (is.na(wrkdata[i]) == TRUE ) {
      wrkdata[i] <- wavg
      #print(paste("i:",i, "n1:",n1,"n2:","avg:",wavg[i],"data_gapfilled:", wrkdata[i],sep="" ))
    } 
  }
  #export the data 
  wrkdata
}
#

#==============================================================
#Function: fun_get_taylor_skill
#
#First date: 2014-July-??
#
#
#==============================================================
fun_get_taylor_skill <- function(df)
{
  #df:: a dataframe contains simlation & observation data
  # calcualte root mean squart error ::RMSE
  sim <- df[,1] # first  column
  obs <- df[,2] # second column
  RMSE <- sqrt( mean( (sim-obs)^2. , na.rm = TRUE ) )
  # calculate linear correlation coefficient ::R
  #R    <- sum( (df$sim-mean(df$sim))*(df$obs-mean(df$obs))) /
  #        (sqrt(sum (df$sim-mean(df$sim)^2.)) * sqrt(sum (df$obs-mean(df$obs)^2.)) )         
  R <- var(sim, obs, na.rm=TRUE)/ (sd(sim, na.rm=TRUE)*sd(obs, na.rm=TRUE))          
  # calculate normalized standard deviation ::SIGMA_N
  SIGMA_N <- sd( sim, na.rm=TRUE)/sd( obs, na.rm=TRUE) 
  # Talyor Skill :: TAYLOR_S
  TAYLOR_S <- (2.*(1 + R)) / ((SIGMA_N + (1./SIGMA_N))^2.)
  # Mean values
  SIM_AVE <- mean( sim, na.rm=T)
  OBS_AVE <- mean( obs, na.rm=T)
  #return a list
  list(RMSE=RMSE, R=R, SIGMA_N=SIGMA_N, TAYLOR_S=TAYLOR_S, SIM_AVE=SIM_AVE, OBS_AVE=OBS_AVE)
}
#


#==============================================================
#Function: fun_read_nc
#
#First date: 2014-08-01
#
#Purpose: get the variable for any nc file 
#         and output a list for every varibles in nc file 
#==============================================================
fun_read_nc<- function(arg1) {
#load  ncdf library
#library(ncdf,lib.loc="/home/orchidee01/ychen/R/library/")
library(ncdf)
#arg1: filepath fo the nc file from James multilayer output
print(paste("arg1: for reading file path ;", arg1)) 
# open the read in file and copy the variables to the dataframe for analysis
input_nc <- open.ncdf(arg1)
#str(input_nc)
# creat a result list for storage varibales as results list for output
result<-list() 
for (i in 1:length(input_nc$dim) ) {
     # store each variable with respect of the var_name 
     result[[input_nc$dim[[i]]$name]] <- get.var.ncdf(input_nc,input_nc$dim[[i]]$name)
}
for (i in 1:length(input_nc$var) ) {
     # store each variable with respect of the var_name 
     result[[input_nc$var[[i]]$name]] <- get.var.ncdf(input_nc,input_nc$var[[i]]$name)
}
close.ncdf(input_nc)
# export the datatable		
result
} #end of fun_read_nc FUNCTION


#==============================================================
#Function: fun_read_nc_var
#
#First date: 2014-08-01
#
#Purpose: get the variable for any nc file 
#         and output a list for every varibles in nc file 
#==============================================================
fun_read_nc_var <- function(arg1, arg2) {
#load  ncdf library
library(ncdf)
#arg1: filepath fo the nc file from James multilayer output
print(paste("arg1: for reading file path ;", arg1,sep='')) 
print(paste("search variable :",arg2," in file: ",arg1, sep='' )) 
print(paste("please wait for a moment~",sep='' )) 
# open the read in file and copy the variables to the dataframe for analysis

# creat a result list for storage varibales as results list for output

result  <- list()
tmp_result <- list()
final_result <- list()
tmp <- array()
# copy the arget variables
tar_name <- arg2

# multiple files loops
for (k in 1: length(arg1) ) {
     #print(length(arg1))
     print(paste("reading file", arg1[k],"....",sep=''))
     input_nc <- open.ncdf(arg1[k])
    #str(input_nc)
for (j in 1:length(tar_name) )  { 
     for (i in 1:length(input_nc$var) ) {
     # find the interesting variable
     if ( (tar_name[j] == input_nc$var[[i]]$name) | is.na(tar_name) )  {
        print( paste("read varaible ",tar_name[j],sep='') )       
        # store each variable with respect of the var_name 
        result[[input_nc$var[[i]]$name]] <- get.var.ncdf(input_nc,input_nc$var[[i]]$name)
        # copy var name and unit to arraies 
        tmp[i] <- paste( input_nc$var[[i]]$name,
                    "  /",input_nc$var[[i]]$longname,
                    "/  [",input_nc$var[[i]]$units, "]"
                    ,sep='')
     }
     }
}

# put tmp array into list for reference
result[[ "header" ]] <- tmp
close.ncdf(input_nc)
# append the result to final_result
  
        tmp_result <- result
        
   if ( k == 1) {
        final_result<- tmp_result
        }else{
        final_result<-mapply(c, final_result, tmp_result, SIMPLIFY=FALSE)
   }

} #end k multiple files

# export the final_result list		
 final_result
} #end of fun_read_nc_var FUNCTION



#===============================================================
#
#Funtion: fun_read_forcing
#Reading nc-format forcing/observation file and output energybudget componments  
#This funtion will return 4 variables including RN, H, LE, and G  
#
#===============================================================
fun_read_fluxnet_forcing <- function(arg1) {
#load  ncdf library
library(ncdf)
#arg1: filepath fo the nc file from LSCE fluxnet forcing
infile<-arg1
print(paste("arg1: for reading file path ;", arg1)) 
# open the read in file and copy the variables to the dataframe for analysis
input_nc <- open.ncdf(infile)
flxlat  <- get.var.ncdf(input_nc,"LE_f")
flxsens <- get.var.ncdf(input_nc,"H_f")
flxsoil <- -1.0*get.var.ncdf(input_nc,"G_f") #define the direction
flxrn   <- get.var.ncdf(input_nc,"Rn_f")
close.ncdf(input_nc)
# creat a table for storage varibales in energy budget
energy_budget <- data.frame (Rn=flxrn,G=flxsoil,LE=flxlat,H=flxsens)
#export the datatable		
energy_budget
} #end func_read_fluxnet_forcing FUNCTION



#Funtion: fun_read_result_SRF
#Reading nc-format ORCHIDEE outputfile and output energybudget componments  
#This funtion will return 4 variables including RN, H, LE, and G
fun_read_result_SRF <- function(arg1=NA, arg2=FALSE) {
#load  ncdf library
library(ncdf)
#arg1: filepath fo the nc file from LSCE fluxnet forcing
infile <- arg1
ld_avg <- arg2
print(paste("SRF file path ;", arg1, "Spatil avg:", arg2,sep="")) 
#print(paste("reading SRF file")) 
# open the read in file and copy the variables to the dataframe for analysis
input_nc <- open.ncdf(infile)


#ld_avg <- 0
#do Spatial average if ld_avg set to 1 do spatial average; 0 no spatial average
	if (ld_avg == FALSE ) {
	# creat a table for storage varibales in energy budget
	for (i in 1:length(input_nc$var) ) 
	{
	   var_name <-input_nc$var[[i]]$name 
           #print (paste("var_name: ",var_name,sep=""))
	   if ( var_name == "fluxlat")   flxlat  <- get.var.ncdf(input_nc,"fluxlat")
 	   if ( var_name == "fluxsens")  flxsens <- get.var.ncdf(input_nc,"fluxsens")	
	   if ( var_name == "fluxnet")   flxnet  <- get.var.ncdf(input_nc,"fluxnet")
  	   if ( var_name == "netrad" )   flxnet  <- get.var.ncdf(input_nc,"netrad")
           if ( var_name == "fluxsoil")  flxsoil <- get.var.ncdf(input_nc,"fluxsoil")
	   if ( var_name == "soilflx")   flxsoil <- get.var.ncdf(input_nc,"soilflx")
	}
        #  
	#albedo  <- get.var.ncdf(input_nc,"Albedo")
	#ts      <- get.var.ncdf(input_nc,"temp_sol")
	#lai     <- get.var.ncdf(input_nc,"temp_sol")
	#swnet    <- get.var.ncdf(input_nc,"swnet")      
	#energy_budget <- data.frame (Rn=flxrn,G=flxsoil,LE=flxlat,H=flxsens,Albedo=albedo,Ts=ts, LAI=lai)
	energy_budget <- data.frame (Rn=flxnet,G=flxsoil,LE=flxlat,H=flxsens)
	} else {
	
	flxlat  <- get.var.ncdf(input_nc,"fluxlat")
	flxsens <- get.var.ncdf(input_nc,"fluxsens")
	flxnet   <- get.var.ncdf(input_nc,"fluxnet")
	flxsoil <- get.var.ncdf(input_nc,"fluxsoil")
	albedo  <- get.var.ncdf(input_nc,"Albedo")
	ts      <- get.var.ncdf(input_nc,"temp_sol")
	lai     <- get.var.ncdf(input_nc,"temp_sol")
	swnet    <- get.var.ncdf(input_nc,"swnet")
	#do some spatial average only for the spatial dimension
        no_value<-1e+20
	flxlat[flxlat >= no_value] <- NA
	flxlat<-apply(flxlat, 2, mean, na.rm=TRUE)
	flxsens[flxsens >= no_value] <- NA
	flxsens<-apply(flxsens, 2, mean, na.rm=TRUE)
	flxnet[flxnet >= no_value] <- NA
	flxnet<-apply(flxnet, 2, mean, na.rm=TRUE)
	flxsoil[flxsoil >= no_value] <- NA
	flxsoil<-apply(flxsoil, 2, mean, na.rm=TRUE)
	albedo[albedo >= no_value] <- NA
	albedo<-apply(albedo, 2, mean, na.rm=TRUE)
	ts[ts >= no_value] <- NA
	ts<-apply(ts, 2, mean, na.rm=TRUE)
	lai[lai >= no_value] <- NA
	lai<-apply(lai, 2, mean, na.rm=TRUE)
	swnet[swnet >= no_value] <- NA
	swnet<-apply(swnet, 2, mean, na.rm=TRUE)
	
	#col.sums <- apply(x, 2, sum)
        #row.sums <- apply(x, 1, sum)
	#flxlat  <- get.var.ncdf(input_nc,"fluxlat",start=c(1,1,1),
	#count=c(file.nc$dim$lon$len,file.nc$dim$lat$len file.nc$dim$time_counter$len)		 
	energy_budget <- data.frame (Rn=flxnet,G=flxsoil,LE=flxlat,H=flxsens,Albedo=albedo,Ts=ts, LAI=lai,SWnet=swnet)
	}
	
close.ncdf(input_nc)	
#++++++++++++++++++++
#export the datatable		
energy_budget
}



#Funtion: fun_read_result_SBG
#Reading nc-format ORCHIDEE outputfile and output energybudget componments  
#This funtion will return 4 variables including RN, H, LE, and G
fun_read_result_SBG <- function(arg1) {
#load  ncdf library
library(ncdf)
#arg1: filepath fo the nc file from LSCE fluxnet forcing
infile <- arg1
print(paste("arg1: for reading file path ;", arg1)) 
# open the read in file and copy the variables to the dataframe for analysis
input_nc <- open.ncdf(infile)
lai  <- get.var.ncdf(input_nc,"LAIPIX")
# creat a table for storage varibales in energy budget
SBG_vars <- data.frame (LAI=lai)
#export the datatable		
SBG_vars
}

#
#Function: fun_daily_mean 
#Averaging funtion return a averaged value 
#This function will calculate the time averaging accroding to the input winsize and datasize values 
fun_daily_mean <- function(winsize, datasize, rawdata) {
avgsize  <-  as.integer(datasize/winsize)     
avgdata  <-  array ( 0., dim = c(avgsize) ) 
wrkdata  <- rawdata   
  for ( i in 1:avgsize) {
    i1= (i-1)*winsize + 1
    i2=  i1 + winsize - 1
    avgdata[i] <- mean(wrkdata[i1:i2], na.rm=TRUE)
    #print(wrkdata[i1:i2])
    #print("============")
    #print(avgdata[i])
    i = i + 1
  }
#print out some text    
print( paste(avgsize,' data-stream ',"created!"))
avgdata[avgdata == NA] <- 0.
#export the datatable
avgdata
}


#
#Function: fun_despike_std
#Despike function using 3.5 std of userspecified window size
fun_despike_std <- function(winsize, rawdata, factor) {
   #initial the std/avg array for each moving windows backward looking
    datasize <- length(rawdata)
    wstd     <-  array ( 0., dim = c(datasize) )
    wavg     <-  array ( 0., dim = c(datasize) )
    wrkdata  <- rawdata
      
    #  for ( i in 2:datasize) {
         #print(wrkdata[i])
    #     if ( is.na(wrkdata[i]) ) wrkdata[i] <- wrkdata[i-1] 
    #  }
      
      
      for ( i in  1 : (datasize-1) )  {
          n2 <-  i-1 
          n1 <- (n2 - winsize )
          if ( n1 <= 1 ) n1 <- 1 
          if ( n2 <= 1 ) n2 <- 1    
          wstd[i] <- sd(wrkdata[n1:n2],na.rm = TRUE)
          wavg[i] <- mean(wrkdata[n1:n2], na.rm = TRUE)
          if (is.na(wstd[i]) | is.na(wavg[i]) ) next  # if all data are NAs do nothing
          # if (i>= 500 & i<= 510 ) {
            #  print(paste("i:",i, "n1:",n1,"n2:",n2,"std:",wstd[i],"avg:",wavg[i],"data:",wrkdata[i],sep="" ))         
          if  ( (i >= winsize) & (i < datasize ) ){ 
              if (  (abs(wrkdata[i]-wavg[i]) > factor*wstd[i]) | (is.na(wrkdata[i]))   )  {
              #print(wrkdata[i])
               wrkdata[i] <- wavg[i]
               print(paste(i,'replace value by', wavg[i] ,sep='')  )
              }   
          }
  
  #    }
      }
     
    #export the data 
   wrkdata
  # wstd
}



#======================================================
#
#Function: fun_diurnal_step
#Diurnal step funtion return a series of certain diurnal timestep 
#
#======================================================
fun_diurnal_step <- function(winsize, step, rawdata) {    
    datasize <- length (rawdata)
    stpdata  <-  array ( 0., dim =(datasize/winsize)  ) 
    wrkdata  <- rawdata
    wrkdata[wrkdata >= 999] <- NA

    ii <- 0
    for ( i in 1:datasize) {
        if ( ((i%%winsize) == 0) | ((i%%winsize) == step)   ) {
          ii <- ii+1
           #print( paste("data:",wrkdata[i],sep=""))
          stpdata[ii] =  wrkdata[i]
        }  
    }
   #output a series for cetain timestep within winsize 
   stpdata
} 



#======================================================
#
#Function: fun_diurnal_mean
#Diurnal mean funtion return a mean diurnal value 
#
#======================================================
fun_diurnal_mean <- function(winsize, rawdata) {    
    datasize <- length (rawdata)
    avgdata  <-  array ( 0., dim = c(winsize) ) 
    wrkdata  <- rawdata
    stddata  <- array(0., dim = c(winsize))
    cont <- array(0, dim=c(winsize))
    wrkdata[wrkdata >= 999] <- NA

    for ( i in 1:datasize) {
        if (is.na(wrkdata[i]) != TRUE) {
         # print( paste("data:",wrkdata[i],sep=""))
          ii <- (i%%winsize) 
          if (ii == 0) ii <- winsize 
          cont[ii] = cont[ii] + 1
          avgdata[ii] =  avgdata[ii] +  wrkdata[i]
        }  
    }
    
    
    #== do diurnal average
    for (ii in 1: winsize) {
      avgdata[ii] = avgdata[ii]/cont[ii]
      if (is.na(avgdata[ii])) avgdata[ii]<- NA
    }
    
    #== Find diurnal deviation
    for (i in 1:datasize) { 
       if (is.na(wrkdata[i]) != TRUE ) {
          ii <- (i%%winsize) 
          if (ii == 0) ii <- winsize
          stddata[ii] = stddata[ii] + (wrkdata[i]-avgdata[ii])**2.
       }
    }
    #get std
    for (ii in 1:winsize) {
       stddata[ii] = sqrt(stddata[ii]/cont[ii])
       if (is.na(stddata[ii])) stddata[ii]<- NA
    }
    #print( paste((datasize/winsize),' days diurnal',"average:",avgdata,"std:",stddata,sep=" "))
    #avgdata[is.na(avgdata)] <- 0.
    list(avg=avgdata, std=stddata)
} 


#
#Function: fun_running_apply
#Return the value with applied (math fuction, f)function the different x size running window size 
fun_running_apply <- function(x, n, f) {
  out <- rep(NA, length(x))

  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset - 1)) {
    out[i] <- f(x[(i - offset):(i + offset - 1)])
  }
  out
}


#Function:fun_creat_flxtable
#This function try to create an empty table for the flx_site observation with untis attributes

fun_creat_flxtable <- function() {
#start to gencerate the table accroding to the input var_anme_list and units for each variables 
  names <- list("Day" = numeric(0), "Hour" = numeric(0), "NEE" = numeric(0),
                "LE" = numeric(0), "H" = numeric(0), "Ustar" = numeric(0),
                "Rg" = numeric(0),  "Tair" = numeric(0), "Tsoil" = numeric(0),
                "RH" = numeric(0), "VPD" = numeric(0) )
  units <- c( "[Jday]", "[-]", "[umolm-2s-1]", "[Wm-2]", "[Wm-2]", "[ms-1]",
                            "[Wm-2]", "[degC]", "[degC]", "[%]", "[hPa]" )
  creat_table <- data.frame(names, stringsAsFactors = FALSE)
  attr(creat_table, "units") <- units
#---  
#  table_rows <- lines
#  table_cols <- length[names]
#----return table 
     return(creat_table)
}

#End functions define==========
