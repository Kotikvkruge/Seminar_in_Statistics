# Seminar Thesis SoSe 23
# Andrei Nevskii 109795

rm(list = ls())

library(tidyverse)
# install.packages(tidyverse)
library(sp)
# install.packages(sp)
library(rgdal)
# install.packages(rgdal)
library(maptools)
# install.packages(maptools)
library(rgeos)
# install.packages(rgeos)
library(tmaptools)
# install.packages(tmaptools)
library(BBmisc)
# install.packages(BBmisc)
library(reshape2)
# install.packages(reshape2)
library(sf)
#install.packages(sf)
library(spdep)
#install.packages(spdep)
library(nlme)
#install.packages(nlme)
library(gstat)
#install.packages(gstat)
library(gam)
#install.packages(gam)
library(caret)
#install.packages(caret)
library(mgcv)
#install.packages(mgcv)
library(blockCV)
#install.packages(blockCV)
library(ggmap)
#install.packages(ggmap)
library(pacman)
#install.packages(pacman)
library(osmdata)
#install.packages(osmdata)
library(pastecs)
#install.packages(pastecs)

####
#### 1) Data preparation (Rental units' characteristics) #### 
#### 

# In this part a data-frame 'dat2' is constructed. It includes all Rental units'
# characteristics, which do not depend on distance definition, such as age, price etc.

# Most of the code in this section is copied from the file '01_dataPreparation_MF.R'.

setwd("/Users/andreinevskii/Documents/Uni/4. Semester/Seminar in Stitistics/Data/10_data")
# setwd("XX")

#a) Read-in and prepare data (recode features, add features, and reclassify some observations)

dat <- read.table("datRent.txt", header = TRUE, row.names = 1)
cat <- c("city","ch","bath","ww","green","SB1")		# categorical covariates

for(i in cat){							# recode categorical variables
  dat[,i] <- as.factor(dat[,i])				#
}


#b) Re-center housing data (original data centered around St. Peter cathedral)

c.stp <- matrix(data = c(12.0985, 49.0192), ncol = 2) 		# location St Peter (starting points below)

coords.stp.t <- SpatialPoints(coords = c.stp) 				# create spatial points

proj4string(coords.stp.t) <- CRS(SRS_string = "EPSG:4326") 		# assign grid WGS 84 to coordinates (as the 

coords.stp <- spTransform(coords.stp.t, 					# transformation to German national 
                          CRS(SRS_string = "EPSG:31468"))	#  grid


x.off <- coords.stp@coords[1]							# offset to "center" coordinates 
y.off <- coords.stp@coords[2]							# 


pos.x.axis <- c(4502000, 4504000, 4506000, 4508000, 4510000, 4512000, 4514000)
pos.y.axis <- c(5426000, 5428000, 5430000, 5432000, 5434000, 5436000, 5438000)
label.x.axis		<- pos.x.axis/1000 - 4508
label.x.axis.uncentered	<- pos.x.axis/1000
label.y.axis		<- pos.y.axis/1000 - 5432
label.y.axis.uncentered	<- pos.y.axis/1000

pos.x.axis.dist.hist.in		<- seq(0,700,by=100)
label.x.axis.dist.hist.in	<- pos.x.axis.dist.hist.in/1000

pos.x.axis.dist.hist.out	<- seq(0,5000,by=1000)
label.x.axis.dist.hist.out	<- pos.x.axis.dist.hist.out/1000

pos.x.axis.dist.c		<- seq(0,6000,by=1000)
label.x.axis.dist.c	<- pos.x.axis.dist.c/1000



dat2 <- dat 									#
dat2$x <- dat2$x + coords.stp@coords[1] 					# actual X-coordinates (epsg 31468)
dat2$y <- dat2$y + coords.stp@coords[2] 					# actual Y-coordinates (epsg 31468)
mat <- matrix(data = c(dat2$x, dat2$y), ncol = 2) 			#
sp1 <- SpatialPoints(coords = mat) 						# code X- and Y-coordinates as spatial points 
#
spdf <- SpatialPointsDataFrame(sp1, data = dat2)			# add info in object dat to spatial info
proj4string(spdf) <- CRS(SRS_string = "EPSG:31468") 			# transformation to German national 


#c) Read-in shape files

reg.b <- readOGR(dsn = "stadtbezirke.shp")				# load spatial data (boroughs of Regensburg)

#center coordinates of reg.b
reg.b.original <- reg.b

reg.w <- readOGR(dsn = "gewaesser.shp") 					# load spatial data (rivers passing through   

#center coordinates of reg.w
reg.w.original <- reg.w

reg.h <- readOGR("autobahn.shp") 						# load spatial data (highways in Regensburg) 


reg.r <- readOGR("eisenbahn.shp")	 					# load spatial data (Regensburg railway system)


reg.a <- readOGR("alleenguertel.shp")	 				# load spatial data (Regensburg allenguertel)


#d) Create SpatialLinesDataFrames, extract coordinates of SpatialPolygons objects and create reg.w_lines.cut

reg.w_lines <- as(reg.w, "SpatialLinesDataFrame") 				# transform 'SpatialPolygonsDataFrame' 
reg.h_lines <- as(reg.h, "SpatialLinesDataFrame") 				#  to 'SpatialLinesDataFrame'
reg.r_lines <- as(reg.r, "SpatialLinesDataFrame") 				#
reg.a_lines <- as(reg.a, "SpatialLinesDataFrame") 				#
reg.b_lines <- as(reg.b, "SpatialLinesDataFrame")

reg.w_lines <- spTransform(reg.w_lines, CRS(SRS_string = "EPSG:31468"))		# transformation to German national grid
reg.h_lines <- spTransform(reg.h_lines, CRS(SRS_string = "EPSG:31468"))		#
reg.r_lines <- spTransform(reg.r_lines, CRS(SRS_string = "EPSG:31468"))		#
reg.a_lines <- spTransform(reg.a_lines, CRS(SRS_string = "EPSG:31468"))		#
reg.b_lines <- spTransform(reg.b_lines, CRS(SRS_string = "EPSG:31468"))		#


# do.call constructs and executes a function call from a name or a function and a list of arguments to be passed to it.
reg.w_coords <- do.call("rbind", lapply(reg.w_lines@lines, 				# combine coordinates of spatial
                                        function(x1) do.call("rbind", 			#  objects into one matrix;
                                                             lapply(x1@Lines, 					#
                                                                    function(x2) x2@coords[-nrow(x2@coords), ])))) 	# structure of accessed object: 
reg.h_coords <- do.call("rbind", lapply(reg.h_lines@lines, 				#  reg.w_lines@lines[[1]]@Lines[[1]]@coords
                                        function(x1) do.call("rbind", 			#
                                                             lapply(x1@Lines, 					# leave out last row, since first
                                                                    function(x2) x2@coords[-nrow(x2@coords), ])))) 	#  and last row are identical
reg.r_coords <- do.call("rbind", lapply(reg.r_lines@lines, 				#
                                        function(x1) do.call("rbind", 			#
                                                             lapply(x1@Lines, 					#
                                                                    function(x2) x2@coords[-nrow(x2@coords), ])))) 	#
reg.a_coords <- do.call("rbind", lapply(reg.a_lines@lines, 				#
                                        function(x1) do.call("rbind", 			#
                                                             lapply(x1@Lines, 					#
                                                                    function(x2) x2@coords[-nrow(x2@coords), ])))) 	#
reg.b_coords <- do.call("rbind", lapply(reg.b_lines@lines, 				#
                                        function(x1) do.call("rbind", 			#
                                                             lapply(x1@Lines, 					#
                                                                    function(x2) x2@coords[-nrow(x2@coords), ])))) 	#

#functions for reading and writing Well Known Text (WKT)
bounds.w <- readWKT("POLYGON((4501800 5425842, 4501800 5437736, 4512350 5437736, 4512350 5425842, 4501800 5425842))")
#code above creates boundary polygon

# Function for determining the intersection between the two given geometries
reg.w_lines.cut <- gIntersection(reg.w_lines, bounds.w)				# cut reg.w at boundary



#e) Create "historic city center boundaries" (districts 1 and 2, bounded by historic city walls in the south)

#Functions for joining intersecting geometries.
distr.1and2 <- gUnion(reg.b[1, ], reg.b[2, ])						# merge districts 1 and 2
distr.1and2_coords <- distr.1and2@polygons[[1]]@Polygons[[1]]@coords


BP.west <- matrix(reg.b_coords[(reg.b_coords[,1] < 4506060 & reg.b_coords[,1] > 4506020) & 
                                 (reg.b_coords[,2] < 5431860 & reg.b_coords[,2] > 5431840)], ncol = 2)
bp.west <- c(x = BP.west[which.max(BP.west[,1]),1], y = BP.west[which.max(BP.west[,1]),2])

BP.east <- matrix(reg.b_coords[(reg.b_coords[,1] < 4508200 & reg.b_coords[,1] > 4508100) & 
                                 (reg.b_coords[,2] < 5431400 & reg.b_coords[,2] > 5431300 )], ncol = 2)
bp.east <- c(x = BP.east[which.max(BP.east[,2]),1], y = BP.east[which.max(BP.east[,2]),2])

reg.hist_coords <- rbind(distr.1and2_coords[c(221:356), ], 				# create object which contains coordinates 
                         distr.1and2_coords[c(1:83), ] , reg.a_coords,		#  of boundaries of historic city center
                         distr.1and2_coords[221, ])
reg.hist <- SpatialPoints(coords = reg.hist_coords)
proj4string(reg.hist) <- CRS(SRS_string = "EPSG:31468")
reg.hist_lines <- as(reg.hist, "SpatialLines")

reg.hist <- Polygon(reg.hist_coords)
reg.hist <- Polygons(list(reg.hist), "b.hist")
reg.hist <- SpatialPolygons(list(reg.hist), proj4string=CRS(SRS_string = "EPSG:31468"))



###
### Insert district variable and historic center dummy
###


city.districts <- as.numeric(reg.b$SBZ) 							# vector with city district

for(j in 1:length(city.districts)){ 							# loop (over districts)
  spdf$district[gContains(reg.b, spdf, byid=TRUE)[,j]] <- city.districts[j] 	# if obs is in district: insert i into 
} 													#  district column

spdf$district <- as.factor(spdf$district)

spdf[(spdf$district == 1 | spdf$district == 2) & !(spdf$city == 1),"city"] <- 1	# adjust "city"
spdf[!(spdf$district == 1 | spdf$district == 2) & (spdf$city == 1),"city"] <- 0	# adjust "city"

spdf$SB1or2 <- spdf$city							# "SB1or2": all observations located in SB1 or SB2

spdf$city[(which(gContains(reg.hist, spdf, byid=TRUE)))]	<- 1	# recode "city" variable: all variables within historic
spdf$city[(which(!(gContains(reg.hist, spdf, byid=TRUE))))]	<- 0	#  city center obtain 1, all others 0


####
#### 2) Distances computation #### 
####

# In this section 4 metrics are applied to obtain distances from a rental unit
# to the nearest facility/(dis-)amenities. 
# Four resulting data-sets will be used in different model specifications
# in the following section 3 'Regressions'.

### 2.1) Combine objects with coordinates into one df  ####

# Hospitals 
obj_coord <- read.csv("hospitals.csv")[ ,1:3] # Read the file
obj_coord[,1] <- 'Hospital' # Assign type of the object

# City center
obj_coord <- rbind(obj_coord, c('Center', 12.0985, 49.0192)) # location St Peter

# Schools
reg.school		<- read_GPX("schools.gpx")

school_names <- reg.school[["waypoints"]][["name"]] # extract schools' names 
# to count their number

i = 1
for (s in 1:length(school_names)){ # Unpack coordinates of each school
  new_entry <- c('School', do.call(rbind, st_geometry(reg.school$waypoints$geometry[i])))
  i <- i + 1
  obj_coord <- rbind(obj_coord, new_entry) # and add them to the data-frame with other coordinates
}

# Playgrounds
reg.play		<- read_GPX("Playground.gpx")

play_names <- reg.play[["waypoints"]][["name"]]

i = 1
for (s in 1:length(play_names)){
  new_entry <- c('Playground', do.call(rbind, st_geometry(reg.play$waypoints$geometry[i])))
  i <- i + 1
  obj_coord <- rbind(obj_coord, new_entry)
}

# Waste yards
reg.waste		<- read_GPX("wasteDisposal.gpx", layer= "tracks")

waste_names <- reg.waste[["tracks"]][["name"]]

i = 1
for (s in 1:length(waste_names)){
  new_entry <- c('Waste', apply(reg.waste[["tracks"]][["geometry"]][[i]][[1]], 2, mean))
  i <- i + 1
  obj_coord <- rbind(obj_coord, new_entry)
}
#add missing waste burning location 
obj_coord <- rbind(obj_coord, c('Waste', 12.13134276302641, 49.0616639512157)) 

# Harbor
obj_coord <- rbind(obj_coord, c('Harbor', 12.121669, 49.017641))

# Parks
dat3		<- read.csv("parksAndUniversity.csv")[,1:3]
dat3[,1] <- 'Park'

obj_coord <- rbind(obj_coord, dat3)

# University
obj_coord <- rbind(obj_coord, c('Uni', dat3[21,2], dat3[21,3]))

# Industrial areas
reg.ind 		<- read_GPX("industrialAreas.gpx", layer = "tracks")

ind_names <- reg.ind[["tracks"]][["name"]]

i = 1
for (s in 1:length(ind_names)){
  new_entry <- c('Industry', apply(reg.ind[["tracks"]][["geometry"]][[i]][[1]], 2, mean))
  i <- i + 1
  obj_coord <- rbind(obj_coord, new_entry)
}


# Rental untis

c.stp <- matrix(data = c(12.0985, 49.0192), ncol = 2) # location St Peter (starting points below)

coords.stp.t <- SpatialPoints(coords = c.stp) # create spatial points
proj4string(coords.stp.t) <- CRS(SRS_string = "EPSG:4326") 	

coords.stp <- spTransform(coords.stp.t, # transformation to German national 
                          CRS(SRS_string = "EPSG:31468"))	#  grid

dat2 <- read.table("datRent.txt", header = TRUE, row.names = 1) 									#
dat2$x <- dat2$x + coords.stp@coords[1] 					# actual X-coordinates (epsg 31468)
dat2$y <- dat2$y + coords.stp@coords[2] 	

ru.reg <- dat2[,4:5] # select only properties' coordinates

ru.reg <- SpatialPoints(ru.reg) 
proj4string(ru.reg) <- CRS("+init=epsg:31468")
ru.reg <- spTransform(ru.reg, CRS("+init=epsg:4326")) # convert to lon and lat in EPSG:4326
ru.reg <- cbind('Rental unit', as.data.frame(ru.reg))

colnames(ru.reg) <- colnames(obj_coord) # adjust column names
obj_coord <- rbind(obj_coord, ru.reg)

write.csv(obj_coord, file = 'Coordinates_Regensburg.csv') # Save as .csv file to calculate
# distances between objects using the script 'FetchDistanceMatrixCode.py' in Python.


df <- dat2 
ru_min_ind <- min(which(obj_coord[,1] == 'Rental unit'))
ru_max_ind <- max(which(obj_coord[,1] == 'Rental unit'))
df$x <- as.numeric(obj_coord[ru_min_ind:ru_max_ind, 2]) # replace epsg:31468 with epsg:4326 coordinates
df$y <- as.numeric(obj_coord[ru_min_ind:ru_max_ind, 3])
df$age <- (2001 - df$BJ) # replace the BJ with age in years
df <- select(df, -c('BJ'))

obj_types <- unique(obj_coord[,1])[1:length(unique(obj_coord[,1]))-1] # Derive object types

for(t in c(1:length(obj_types))){	# create blank columns for smallest distances
  df[, obj_types[t]] <- -2			# assign some negative scalar to specify numeric type
}                               # 

num_var <- c(obj_types, "size","age") # a vector with numerical variables to be rescaled

# For illustration

# Map rental units

# get a map
reg_basemap <- get_map(location = getbb('Regensburg'), zoom = 12, source = "stamen")
# plot points on it
ggmap(reg_basemap) +
  geom_point(data = df, aes(x, y), color = "dark orange",  size = 1.5) # Figure 3.2


### 2.2) Driving distance  ####

# extract and scale the resulting distance matrix (in meters)
driving_dist <- read.csv("/Users/andreinevskii/Documents/Uni/4. Semester/Seminar in Stitistics/Data/Distances/Driving/distance_matrix.csv",
                         header = F)
# driving_dist <- read.csv("XX/distance_matrix.csv",
#                          header = F)

# add object types to the matrix
driving_dist$type <- obj_coord[,1]
len <- length(driving_dist$type) + 1 # index of type column and row
driving_dist[len, ] <- c(obj_coord[,1], '')

# add min distances to each facility type as a property characteristics
df_driving_dist <- df # Copy the data-frame

for (t in c(1:length(obj_types))) {               # Select a distance from a rental unit to
  for (i in c(1: (ru_max_ind-ru_min_ind + 1) )) { # its nearest object for each object type
    vec.temp <- driving_dist[ (ru_min_ind -1 + i), 
                              which(driving_dist[, ru_max_ind+1] == obj_types[t]) ]
    if (length(vec.temp) == 1) { # for single objects within. type, e.g. Uni or Center
      df_driving_dist[i, obj_types[t]] <- as.numeric(vec.temp[1])
    } else{
      df_driving_dist[i, obj_types[t]] <- as.numeric(apply( vec.temp, 1, FUN = min) )
    }
  }       
}

# Re-scale numeric variables
df_driving_dist_st <- df_driving_dist %>% 
  mutate_at(num_var, ~(normalize(., method = "range", range = c(0, 1)) %>% as.vector))
# and convert to spatial data frame
spdf_driving_dist_st <- SpatialPointsDataFrame(coords = df_driving_dist_st[c('x','y')] 
                                               , data = df_driving_dist_st,
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


# Summary of variables - Table 3.1
variables <- c('rpsqm', 'size', 'age', 'x', 'y', 'Center', 'Hospital', 
               'School', 'Waste', 'Park', 'Harbor', 'Uni')
sum_stat <- as.data.frame(t(stat.desc(df_driving_dist[, variables])))
write_csv(sum_stat, file = 'Summary statistics.csv')

### 2.3) Driving times  ####

# extract resulting the time matrix (in seconds)
driving_time <- read.csv("/Users/andreinevskii/Documents/Uni/4. Semester/Seminar in Stitistics/Data/Distances/Driving/durations_matrix.csv",
                         header = F)
# driving_time <- read.csv("XX/durations_matrix.csv",
#                          header = F)

# add object types to the matrix
driving_time$type <- obj_coord[,1]
len <- length(driving_time$type) + 1 # index of type column and row
driving_time[len, ] <- c(obj_coord[,1], '')

df_driving_time <- df # Copy the data-frame

for (t in c(1:length(obj_types))) {               # Select a distance from a rental unit to
  for (i in c(1: (ru_max_ind-ru_min_ind + 1) )) { # its nearest object for each object type
    vec.temp <- driving_time[(ru_min_ind -1 + i), 
                             which(driving_time[, ru_max_ind+1] == obj_types[t]) ]
    if (length(vec.temp) == 1) { # for single objects within. type, e.g. Uni or Center
      df_driving_time[i, obj_types[t]] <- as.numeric(vec.temp[1])
    } else{
      df_driving_time[i, obj_types[t]] <- as.numeric(apply( vec.temp, 1, FUN = min) )
    }
  }       
}

df_driving_time_st <- df_driving_time %>% 
  mutate_at(num_var, ~(normalize(., method = "range", range = c(0, 1)) %>% as.vector))
#  convert to spatial data frame
spdf_driving_time_st <- SpatialPointsDataFrame(coords = df_driving_time_st[c('x','y')] 
                                               , data = df_driving_time_st,
                                               proj4string = 
                                                 CRS("+proj=longlat +datum=WGS84 +
                                                     ellps=WGS84 +towgs84=0,0,0"))


### 2.4) Euclidean distance  ####

# Compute euclidean distance
eucl_dist <- as.data.frame(
  as.matrix(dist(obj_coord[,2:3], method = "euclidean", diag = T, upper = T)))

# add object types to the matrix
eucl_dist$type <- obj_coord[,1]
len <- length(eucl_dist$type) + 1 # index of type column and row
eucl_dist[len, ] <- c(obj_coord[,1], '')

df_eucl_dist <- df # Copy the data-frame

for (t in c(1:length(obj_types))) {               # Select a distance from a rental unit to
  for (i in c(1: (ru_max_ind-ru_min_ind + 1) )) { # its nearest object for each object type
    vec.temp <- eucl_dist[ (ru_min_ind -1 + i), 
                           which(eucl_dist[, ru_max_ind+1] == obj_types[t]) ]
    if (length(vec.temp) == 1) { # for single objects within. type, e.g. Uni or Center
      df_eucl_dist[i, obj_types[t]] <- as.numeric(vec.temp[1])
    } else{
      df_eucl_dist[i, obj_types[t]] <- as.numeric( apply( vec.temp, 1, FUN = min) )
    }
  }       
}

df_eucl_dist_st <- df_eucl_dist %>% 
  mutate_at(num_var, ~(normalize(., method = "range", range = c(0, 1)) %>% as.vector))
#  convert to spatial data frame
spdf_eucl_dist_st <- SpatialPointsDataFrame(coords = df_eucl_dist_st[c('x','y')] 
                                            , data = df_eucl_dist_st,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


### 2.5) Manhattan distance  ####

# Compute Manhattan distance
manh_dist <-  as.data.frame(
  as.matrix(dist(obj_coord[,2:3], method = "manhattan", diag = T, upper = T)))
manh_dist_nn <- manh_dist

# add object types to the matrix
manh_dist$type <- obj_coord[,1]
len <- length(manh_dist$type) + 1 # index of type column and row
manh_dist[len, ] <- c(obj_coord[,1], '')

df_manh_dist <- df # Copy the data-frame

for (t in c(1:length(obj_types))) {               # Select a distance from a rental unit to
  for (i in c(1: (ru_max_ind-ru_min_ind + 1) )) { # its nearest object for each object type
    vec.temp <- manh_dist[ (ru_min_ind -1 + i), 
                           which(manh_dist[, ru_max_ind+1] == obj_types[t]) ]
    if (length(vec.temp) == 1) { # for single objects within. type, e.g. Uni or Center
      df_manh_dist[i, obj_types[t]] <- as.numeric( vec.temp[1] )
    } else{
      df_manh_dist[i, obj_types[t]] <- as.numeric( apply( vec.temp, 1, FUN = min) )
    }
  }       
}

df_manh_dist_st <- df_manh_dist %>% 
  mutate_at(num_var, ~(normalize(., method = "range", range = c(0, 1)) %>% as.vector))
#  convert to spatial data frame
spdf_manh_dist_st <- SpatialPointsDataFrame(coords = df_manh_dist_st[c('x','y')] 
                                            , data = df_manh_dist_st,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


####
#### 3) Regressions #### 
####

# In this section two regression models: Ordinaty Least Squares (OLS),
# Generalized Additive Model (GAM)
# are applied to each data set.


#### 3.1) OLS Regression ####  

# OLS is estimated mostly for diagnostic purposes.

### 3.1 a) Driving distance

ols_dd <- lm(rpsqm ~ x + y + size + age + city  + bath   # Property features + coordinates + coordinates+ coordinates
             + SB1 + Center + Hospital + School + Playground + Waste + Harbor # Location features
             + Park + Uni + Industry,       #
             data = df_driving_dist_st)

summary(ols_dd)

### 3.1 b) Driving time

ols_dt <- lm(rpsqm ~ x + y + size + age + city  + bath   # Property features + coordinates
             + SB1 + Center + Hospital + School + Playground + Waste + Harbor # Location features
             + Park + Uni + Industry,       #
             data = df_driving_time_st)

summary(ols_dt)

### 3.1 c) Euclidean distance

ols_ed <- lm(rpsqm ~ x + y + size + age + city  + bath   # Property features + coordinates
             + SB1 + Center + Hospital + School + Playground + Waste + Harbor # Location features
             + Park + Uni + Industry,       #
             data = df_eucl_dist_st)

summary(ols_ed)

### 3.1 d) Manhattan distance

ols_md <- lm(rpsqm ~ x + y + size + age + city  + bath   # Property features + coordinates
             + SB1 + Center + Hospital + School + Playground + Waste + Harbor # Location features
             + Park + Uni + Industry,       #
             data = df_manh_dist_st)

summary(ols_md)



#### 3.2) GAM #### 

formula_gam <- rpsqm ~ s(size) + s(age) + te(x, y) + s(Center) + Hospital + School + Waste + Park + Harbor + Uni
# Reduce the number of explanatory variables, apply bivariate smoothing to coordinates
# and univariate to certain variables with pronounced non-linearity (based on Fritsch et al. 2019)

### 3.1 a) Driving distance

gam_dd <-  gam(formula_gam, data = df_driving_dist_st)
summary(gam_dd)

### 3.1 b) Driving time

gam_dt <-  gam(formula_gam, data = df_driving_time_st)
summary(gam_dt)

### 3.1 c) Euclidean distance

gam_ed <-  gam(formula_gam, data = df_eucl_dist_st)
summary(gam_ed)

### 3.1 d) Manhattan distance

gam_md <-  gam(formula_gam, data = df_manh_dist_st)
summary(gam_md)


####
#### 4) Cross-Validation #### 
####

RMSE_0 <- function(model){ # define loss function
  return(round(sqrt(mean(model$residuals^2)), 3))
}

MAE_0 <- function(model){ # define loss function
  return(round(mean(abs(model$residuals)), 3))
}

# create matrix for storing results
CV_res <- matrix('NA', nrow = 9, ncol = 8)
CV_res[1,] <- c('Method', 'Resubstitution', 'Hold-out', '10-Fold CV', 
                'LOO CV', 'Spatial blocking CV systematic', 'Buffered CV', 
                'Spatial blocking CV random')
CV_res[2:9,1] <- c( 'Driving distance RMSE', 
                    'Driving distance MAE', 
                    'Driving time RMSE', 
                    'Driving time MAE',
                    'Euclidean distance RMSE', 
                    'Euclidean distance MAE',
                    'Manhattan distance RMSE', 
                    'Manhattan distance MAE')
CV_res

#### 4.1) Resubstitution #### 

### 4.1 a) Driving distance

RMSE_0(ols_dd)
CV_res[2,2] <- RMSE_0(gam_dd)
CV_res[3,2] <- MAE_0(gam_dd)


### 4.1 b) Driving time

RMSE_0(ols_dd)
CV_res[4,2] <- RMSE_0(gam_dt)
CV_res[5,2] <- MAE_0(gam_dt)

### 4.1 c) Euclidean distance

RMSE_0(ols_dd)
CV_res[6,2] <- RMSE_0(gam_ed)
CV_res[7,2] <- MAE_0(gam_ed)

### 4.1 d) Manhattan distance

RMSE_0(ols_dd)
CV_res[8,2] <- RMSE_0(gam_md)
CV_res[9,2] <- MAE_0(gam_md)


#### 4.2) Hold-out #### 

Hold_out_rmse <- function(data){
  set.seed(42)
  training.samples <- data$rpsqm %>%
    createDataPartition(p = 0.8, list = FALSE)
  
  model_ho <-  gam(formula_gam, 
                   data = data[training.samples, ])
  
  RMSE_ho <- round(mean(
    sqrt((predict(model_ho, data[-training.samples, ]) 
          - data[-training.samples, 'rpsqm'])^2)), 3)
  return(RMSE_ho)
}

Hold_out_mae <- function(data){
  set.seed(42)
  training.samples <- data$rpsqm %>%
    createDataPartition(p = 0.8, list = FALSE)
  
  model_ho <-  gam(formula_gam, 
                   data = data[training.samples, ])
  
  MAE_ho <- round(mean(
    abs(predict(model_ho, data[-training.samples, ]) 
          - data[-training.samples, 'rpsqm'])), 3)
  return(MAE_ho)
}


### 4.2 a) Driving distance

CV_res[2,3] <- Hold_out_rmse(df_driving_dist_st)

CV_res[3,3] <- Hold_out_mae(df_driving_dist_st)

### 4.2 b) Driving time

CV_res[4,3] <- Hold_out_rmse(df_driving_time_st)

CV_res[5,3] <- Hold_out_mae(df_driving_time_st)

### 4.2 c) Euclidean distance

CV_res[6,3] <- Hold_out_rmse(df_eucl_dist_st)

CV_res[7,3] <- Hold_out_mae(df_eucl_dist_st)

### 4.2 d) Manhattan distance

CV_res[8,3] <- Hold_out_rmse(df_manh_dist_st)

CV_res[9,3] <- Hold_out_mae(df_manh_dist_st)

#### 4.3) 10-Fold CV #### 

CV_K.Fold_rmse <- function(dats, n.folds = 10){
  folds <- list() # flexible object for storing folds
  fold.size <- nrow(dats)/n.folds
  remain <- 1:nrow(dats) # all obs are in
  for (i in 1:n.folds){
    set.seed(42)
    select <- sample(remain, fold.size, replace = FALSE) 
    #randomly sample “fold_size” from the ‘remaining observations’
    folds[[i]] <- select # store indices
    #write a special statement for the last fold — if there are ‘leftover points’
    if (i == n.folds){
      folds[[i]] <- remain
    }
    #update remaining indices to reflect what was taken out
    remain <- setdiff(remain, select)
    remain
  }
  results <- c()
  for (i in 1:n.folds){
    # fold i
    indis <- folds[[i]] #unpack into a vector
    train <- dats[-indis, ] #split into train and test sets
    test <- dats[indis, ]
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    RMSE <- sqrt(mean((test$rpsqm - pred)^2))
    results[i]<- RMSE
  }
  return(round(mean(results), 3))
}

CV_K.Fold_mae <- function(dats, n.folds = 10){
  folds <- list() # flexible object for storing folds
  fold.size <- nrow(dats)/n.folds
  remain <- 1:nrow(dats) # all obs are in
  for (i in 1:n.folds){
    set.seed(42)
    select <- sample(remain, fold.size, replace = FALSE) 
    #randomly sample “fold_size” from the ‘remaining observations’
    folds[[i]] <- select # store indices
    #write a special statement for the last fold — if there are ‘leftover points’
    if (i == n.folds){
      folds[[i]] <- remain
    }
    #update remaining indices to reflect what was taken out
    remain <- setdiff(remain, select)
    remain
  }
  results <- c()
  for (i in 1:n.folds){
    # fold i
    indis <- folds[[i]] #unpack into a vector
    train <- dats[-indis, ] #split into train and test sets
    test <- dats[indis, ]
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    MAE <- mean(abs((test$rpsqm - pred)))
    results[i]<- MAE
  }
  return(round(mean(results), 3))
}

### 4.3 a) Driving distance

CV_res[2,4] <- CV_K.Fold_rmse(df_driving_dist_st)

CV_res[3,4] <- CV_K.Fold_mae(df_driving_dist_st)

### 4.3 b) Driving time

CV_res[4,4] <- CV_K.Fold_rmse(df_driving_time_st)

CV_res[5,4] <- CV_K.Fold_mae(df_driving_time_st)

### 4.3 c) Euclidean distance

CV_res[6,4] <- CV_K.Fold_rmse(df_eucl_dist_st)

CV_res[7,4] <- CV_K.Fold_mae(df_eucl_dist_st)

### 4.3 d) Manhattan distance

CV_res[8,4] <- CV_K.Fold_rmse(df_manh_dist_st)

CV_res[9,4] <- CV_K.Fold_mae(df_manh_dist_st)

#### 4.4) LOO CV #### 

K = dim(df)[1] # Set # of folds = # of observations

### 4.4 a) Driving distance

CV_res[2,5] <- CV_K.Fold_rmse(df_driving_dist_st, K)

CV_res[3,5] <- CV_K.Fold_mae(df_driving_dist_st, K)

### 4.4 b) Driving time

CV_res[4,5] <- CV_K.Fold_rmse(df_driving_time_st, K)

CV_res[5,5] <- CV_K.Fold_mae(df_driving_time_st, K)

### 4.4 c) Euclidean distance

CV_res[6,5] <- CV_K.Fold_rmse(df_eucl_dist_st, K)

CV_res[7,5] <- CV_K.Fold_mae(df_eucl_dist_st, K)

### 4.4 d) Manhattan distance

CV_res[8,5] <- CV_K.Fold_rmse(df_manh_dist_st, K)

CV_res[9,5] <- CV_K.Fold_mae(df_manh_dist_st, K)


#### 4.5) CV with spatial blocking #### 

set.seed(42)
CV_sb_partition_sys <- cv_spatial(spdf, k = 10, selection = "systematic") # Figure 3.5 a
CV_sb_partition_ran <- cv_spatial(spdf, k = 10, selection = "random") # Figure 3.5 b

CV_K.Fold_block_rmse <- function(dats, CV_sb_partition, n.folds = 10){
  
  results <- c()
  for (i in 1:n.folds){
    # fold i
    indis <- CV_sb_partition[["folds_ids"]]  == i
    train <- dats[-indis, ] #split into train and test sets
    test <- dats[indis, ]
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    RMSE <- sqrt(mean((test$rpsqm - pred)^2))
    results[i]<- RMSE
  }
  return(round(mean(results),3))
}

CV_K.Fold_block_mae <- function(dats, CV_sb_partition, n.folds = 10){
  
  results <- c()
  for (i in 1:n.folds){
    # fold i
    indis <- CV_sb_partition[["folds_ids"]]  == i
    train <- dats[-indis, ] #split into train and test sets
    test <- dats[indis, ]
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    MAE <- mean(abs((test$rpsqm - pred)))
    results[i]<- MAE
  }
  return(round(mean(results),3))
}

### 4.5 a) Driving distance

CV_res[2,6] <- CV_K.Fold_block_rmse(spdf_driving_dist_st, CV_sb_partition_sys)
CV_res[2,8] <- CV_K.Fold_block_rmse(spdf_driving_dist_st, CV_sb_partition_ran)

CV_res[3,6] <- CV_K.Fold_block_mae(spdf_driving_dist_st, CV_sb_partition_sys)
CV_res[3,8] <- CV_K.Fold_block_mae(spdf_driving_dist_st, CV_sb_partition_ran)

### 4.5 b) Driving time

CV_res[4,6] <- CV_K.Fold_block_rmse(spdf_driving_time_st, CV_sb_partition_sys)
CV_res[4,8] <- CV_K.Fold_block_rmse(spdf_driving_time_st, CV_sb_partition_ran)

CV_res[5,6] <- CV_K.Fold_block_mae(spdf_driving_time_st, CV_sb_partition_sys)
CV_res[5,8] <- CV_K.Fold_block_mae(spdf_driving_time_st, CV_sb_partition_ran)

### 4.5 c) Euclidean distance

CV_res[6,6] <- CV_K.Fold_block_rmse(spdf_eucl_dist_st, CV_sb_partition_sys)
CV_res[6,8] <- CV_K.Fold_block_rmse(spdf_eucl_dist_st, CV_sb_partition_ran)

CV_res[7,6] <- CV_K.Fold_block_mae(spdf_eucl_dist_st, CV_sb_partition_sys)
CV_res[7,8] <- CV_K.Fold_block_mae(spdf_eucl_dist_st, CV_sb_partition_ran)

### 4.5 d) Manhattan distance

CV_res[8,6] <- CV_K.Fold_block_rmse(spdf_manh_dist_st, CV_sb_partition_sys)
CV_res[8,8] <- CV_K.Fold_block_rmse(spdf_manh_dist_st, CV_sb_partition_ran)

CV_res[9,6] <- CV_K.Fold_block_mae(spdf_manh_dist_st, CV_sb_partition_sys)
CV_res[9,8] <- CV_K.Fold_block_mae(spdf_manh_dist_st, CV_sb_partition_ran)


#### 4.6) LOO CV with spatial buffering #### 

#Define a function returning RMSE of LOO CV with spatial buffering
LOO_CV_buffer_rmse <- function(data, distances){
  results <- c()
  for (i in c(1:dim(data)[1])) {
    # select sub-matrix of distances only among rental units
    dist_ru <- distances[ru_min_ind:ru_max_ind, ru_min_ind:ru_max_ind] 
    # omit other untis within the proximity of .1 quantile
    inds <- dist_ru[i, ] > quantile(as.numeric(as.matrix(dist_ru)), 0.1) 
    train <- data[inds, ]
    test <- data[i, ]
    # fit the model to non-neighboring observations
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    RMSE <- sqrt(mean((test$rpsqm - pred)^2))
    results[i]<- RMSE
  }
  return(round(mean(results),3))
}

LOO_CV_buffer_mae <- function(data, distances){
  results <- c()
  for (i in c(1:dim(data)[1])) {
    # select sub-matrix of distances only among rental units
    dist_ru <- distances[ru_min_ind:ru_max_ind, ru_min_ind:ru_max_ind] 
    # omit other untis within the proximity of .1 quantile
    inds <- dist_ru[i, ] > quantile(as.numeric(as.matrix(dist_ru)), 0.1) 
    train <- data[inds, ]
    test <- data[i, ]
    # fit the model to non-neighboring observations
    gam.model <- gam(formula_gam, data = train)
    pred <- predict(gam.model, newdata = test)
    MAE <- mean(abs(test$rpsqm - pred))
    results[i]<- MAE
  }
  return(round(mean(results),3))
}

### 4.6 a) Driving distance

CV_res[2,7] <- LOO_CV_buffer_rmse(df_driving_dist_st, driving_dist)
CV_res[3,7] <- LOO_CV_buffer_mae(df_driving_dist_st, driving_dist)

### 4.6 b) Driving time

CV_res[4,7] <- LOO_CV_buffer_rmse(df_driving_time_st, driving_time)
CV_res[5,7] <- LOO_CV_buffer_mae(df_driving_time_st, driving_time)

### 4.6 c) Euclidean distance

CV_res[6,7] <- LOO_CV_buffer_rmse(df_eucl_dist_st, eucl_dist)
CV_res[7,7] <- LOO_CV_buffer_mae(df_eucl_dist_st, eucl_dist)

### 4.6 d) Manhattan distance

CV_res[8,7] <- LOO_CV_buffer_rmse(df_manh_dist_st, manh_dist)
CV_res[9,7] <- LOO_CV_buffer_mae(df_manh_dist_st, manh_dist)

write.csv(CV_res, file = 'CV Results.csv') # Table 3.2


####
#### 5) Indicating Spatial Autocorrelation #### 
####


# 5.1) Moran’s I #### 

df_coord <- df[c('x', 'y')]                   # Extract coordinates of rental properties
coo <- coordinates(SpatialPoints(df_coord))   # and define them as spatial points

S.dist  <-  dnearneigh(coo, 0, quantile(dist(coo), 0.01)) # Define properties within .1 quantile distance as neighboring

lw <- nb2listw(S.dist, style="W",zero.policy=T) # define search circle

set.seed(42) # Reproducible results
# Run the MC simulation
MI  <-  moran.mc(df_manh_dist_st$rpsqm, lw, nsim = 1000, zero.policy=T) 

plot(density(MI$res), main="Moran's I distribution") # plots the results
abline(v = MI$statistic, col = 'red') # Figure 3.3 Left side

MI # p-value = 0.000999 => reject H0: zero spatial autocorrelation present in rents at \alpha=1\%

five_n <- fivenum(dist(coo))
lags <- seq(from =five_n[1], to = five_n[5], length.out = 10)

MI_stats <- c() # Empty vector for statistics at different lags

set.seed(42)
i = 1
for (l in lags) { # Compute MI at different lags
  S.dist  <-  dnearneigh(coo, 0, l)
  lw <- nb2listw(S.dist, style="W",zero.policy=T)
  MC  <-  moran.mc(df_manh_dist_st$rpsqm, lw, nsim = 1000, zero.policy=T)
  MI_stats[i] <- MC$statistic
  i = i +1
}

plot(MI_stats ~ lags, type = "b", main = "Moran's I at different lags") 
abline(h = mean(MI_stats), col = 'gray') # Figure 3.3 Right side


# 5.2) Variogramms ####

vario <- variogram(residuals(gam_dd) ~ 1, data = spdf_driving_dist_st)
plot(vario$dist, vario$gamma, type = "b")
plot.gam(gam_dd)

vario <- variogram(residuals(gam_dt) ~ 1, data = spdf_driving_dist_st)
plot(vario$dist, vario$gamma, type = "b")
plot.gam(gam_dt)

vario <- variogram(residuals(gam_ed) ~ 1, data = spdf_driving_dist_st)
plot(vario$dist, vario$gamma, type = "b")
plot.gam(gam_ed)

vario <- variogram(residuals(gam_md) ~ 1, data = spdf_driving_dist_st)
plot(vario$dist, vario$gamma, type = "b")
plot.gam(gam_md)

# 5.3) Bubble plots #### 

### 5.3 a) Driving distance

spdf_copy <- spdf_driving_dist_st
spdf_copy$Residuals <- gam_dd$residuals
bubble(spdf_copy[ ,c('Residuals', 'x', 'y')], "Residuals", maxsize = 2)

### 5.3 b) Driving time

spdf_copy$Residuals <- gam_dt$residuals
bubble(spdf_copy[ ,c('Residuals', 'x', 'y')], "Residuals", maxsize = 2) # Figure 3.4

### 5.3 c) Euclidean distance

spdf_copy$Residuals <- gam_ed$residuals
bubble(spdf_copy[ ,c('Residuals', 'x', 'y')], "Residuals", maxsize = 2)

### 5.3 d) Manhattan distance

spdf_copy$Residuals <- gam_md$residuals
bubble(spdf_copy[ ,c('Residuals', 'x', 'y')], "Residuals", maxsize = 2)



