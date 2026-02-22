library(raster)
library(MODIStsp) 

MODIStsp()

opts_file <- file.path("/netscratch/irg/grp_hancock/Shifa/latitudinal_clusters/lai/lai_2002.json")

# --> Launch the processing
MODIStsp(gui = FALSE, opts_file = opts_file, verbose = FALSE)

###################       READ AND ANALYZE 506 RASTERS FROM THE YEAR 2002 to 2012 (8-day data)   #######################

setwd("/Volumes/MY_HARD_DRI/vector1et/")

library(raster)
files<- list.files(pattern = ".tif", full.names = F)
znames = as.Date(c()) # empty dataframe to store the dates in the format for eg., '2002-03-23'
s<-stack() # empty stack to store the rasters 

s

# loop over 8-day files and attach the date to the respective raster
for (file in files) { 
  year_doy<-as.numeric(substr(file,26,32)) #get the doy from filename
  mydate<- as.Date(strptime(year_doy, "%Y%j"))
  print(mydate)
  r<- raster(file)
  znames = c(znames, mydate)
  s <- stack(s,r)
  
}

s
s<-setZ(s,znames) # attach the dates vector to raster layers

m<-stack()

for (month_name in month.name){  #here month.name is a vector of months from january to december
  print(month_name)
  tryCatch({
    # Subset layers corresponding to the current month
    month_layers <- s[[which(months(getZ(s)) == month_name)]]
    print(month_layers)
    # Calculate mean for the month (exclude NA values)
    month_mean <- mean(month_layers, na.rm = TRUE)
    print(month_mean)
    # Store the monthly mean in the list with the month name as the list element name
    m<-stack(m,month_mean)
  }, error = function(e) {
    cat("Error occurred for month", month_name, ":", conditionMessage(e), "\n")
  })
}

names(m) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
m_spat<-terra::rast(m)
writeRaster(m_spat, paste0('vector1',names(m_spat), ".tif"))


############### USING Raster- ATTACH DATES VIA LIST AND LOOP USING TERRA to calculate monthly means

library(raster)
znames = as.Date(c()) # empty dataframe to store the dates in the format for eg., '2002-03-23'
s<-list()

for (file in files) { 
  year_doy<-as.numeric(substr(file,35,41)) #get the doy from filename
  mydate<- as.Date(strptime(year_doy, "%Y%j"))
  print(mydate)
  r<- raster(file)
  znames = c(znames, mydate)
  s[[file]] <- rast(r)
  
}
s<-rast(s)
time(s)<-znames 
m_test <- tapp(s, "months", mean, cores=2)

### USING TERRA-ATTACH DATES AND TAKE MONTHLY MEANS
fnames <- list.files(pattern = ".tif")
r <- rast(fnames)
time(r) <- substr(fnames, 35, 41) |> strptime("%Y%j") |> as.Date()
monthly_means <- tapp(r, 'months',fun=mean, na.rm=T)


### for one month using terra
fnames <- list.files(pattern = ".tif")
r <- terra::rast(fnames)
time(r) <- substr(fnames, 35, 41) |> strptime("%Y%j") |> as.Date()
month_layer <- r[[which(months(time(r)) == 'November')]]
#m_test <- tapp(month_layers, 'months',fun=mean, cores=2, subset =r[[which(months(time(r)) == 'March')]])
m_test1 <- terra::app(month_layers, mean, na.rm=T)


#### for one month using raster
library(raster)
library(terra)
files<- list.files(pattern = ".tif", full.names = F)
znames = as.Date(c()) # empty dataframe to store the dates in the format for eg., '2002-03-23'
s<-stack() # empty stack to store the rasters 

s

# loop over 8-day files and attach the date to the respective raster
for (file in files) { 
  year_doy<-as.numeric(substr(file,35,41)) #get the doy from filename
  mydate<- as.Date(strptime(year_doy, "%Y%j"))
  print(mydate)
  r<- raster(file)
  znames = c(znames, mydate)
  s <- stack(s,r)
}

s

s<-setZ(s,znames)


# Subset layers corresponding to the current month
month_layers <- s[[which(months(getZ(s)) == 'September')]]
month_mean <- mean(month_layers, na.rm = TRUE)
print(month_mean)
month_mean<-rast(month_mean)
names(month_mean)<-'September'
writeRaster(month_mean, paste0('vector1','September', ".tif"))

month_layers<-terra::rast(month_layers)
m_test1 <- terra::app(month_layers, mean, na.rm=T)

## MERGING THE 3 VECTORS OF LST
library(terra)
setwd('/Volumes/MY_HARD_DRI')
v1<-rast('vector1et/vector1October.tif')
v2<-rast('vector2et/vector2October.tif')
v3<-rast('vector3et/vector3October.tif')

m1 <- merge(v1,v2,v3)
m1<-m1*0.1 ######### scale factor is 0.02 for lst, 0.0001 for ndvi, 0.1 for et
######### m1_degC<- m1 - (273.15) # turn kelvin to celsius
varnames(m1)<-'merged'
names(m1)<- 'October'

writeRaster(m1, paste0('October','_et', ".tif"))



############### MEAN, MAX, MIN OF THE RASTERS FROM JAN TO DEC ##################


setwd("/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/vars_30s/mod11a2v61_lst_night_2002_2012/")
files<- list.files(pattern = ".tif", full.names = F)
s<-terra::rast(files)
#et_mean <- terra::mean(s, na.rm = TRUE) 
# mean worked fine but using max showed error
# thats why i switched to app() for min and max 

et_max <- terra::app(s, fun=max,cores=4,na.rm = TRUE)

terra::writeRaster(et_max,filename = "unt/et_max_resampled_name.tif")


############# MODE OF VALUES

setwd("/Volumes/MY_HARD_DRI/mcd12q1_igbp_lctype1/LandCover_Type_Yearly_500m_v61/LC1")
files<- list.files(pattern = ".tif", full.names = F)
s<-terra::rast(files)

terra::modal(s, ties="first", na.rm=T, filename="MCD12Q1_LC1_2002_2012.tif")



########################## RESAMPLE AND RENAME #################

library(terra)
setwd("/Volumes/MY_HARD_DRI/mod11a2v61_lst_day_2002_2012")
s<- rast("")

resample(to_res, s, method = "near", threads=T, filename="")
names('file.tif')

#loop
fnames <- list.files(pattern = "cl_*")

for (file in fnames) {
  print(paste("Processing:", file))  # Print progress
  
  t <- rast(file)  # Load raster
  
  # Rename by removing "wc2.1_2.5m_"
  names(t) <- gsub("wc2.1_2.5m_", "", names(t)) #  gsub("wc2.1_2.5m_|c_MPI-ESM1-2-HR_ssp245_2041-2060", "", names(t))
  
  # Save with new name (optional)
  writeRaster(t, filename = paste0("renamed_", file))
}


########################## CLIPPING #################


library(terra)
r <- rast()
ext(r) <- c(-180, 180, -60, 80)
s <- ext(r) # capture the clipping extent in s object
fnames <- list.files(pattern = ".tif",full.names=F)
#t <- rast(fnames) # SpatRaster to be clipped
#crop(t,s, filename)

for (file in fnames){
  print(file)
  t<-rast(file)
  crop(t,s,filename=paste0('cl_',file))
  }

# cropping by shapfiles of countries
eth <- vect("/netscratch/irg/grp_hancock/Shifa/mapping_material/yunnan.shp")
fnames <- list.files(pattern = ".tif")

for (file in fnames){
  print(file)
  t<-rast(file)
  crop(t,eth,filename=paste0('eth_',file), mask=T)
}

### exporting single bands from multiband

r <- rast("/netscratch/irg/grp_hancock/Shifa/vars_30s/wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp245_2041-2060/wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp245_2041-2060.tif")  # Load the multi-band raster
nlyr(r)  # This will show the number of bands in the raster

r[[1]]  # check the first band
names(r[[1]])<- 'bio1' # change the name of band

for (i in 1:nlyr(r)) {
  writeRaster(r[[i]], paste0("fut_", names(r[[i]]), ".tif"))
}


##########
library(terra)
logo <- rast(system.file("ex/logo.tif", package="terra"))   
### principal components of a SpatRaster
pca <- princomp(logo)

# or use sampling if you have a large raster 
# and cannot process all cell values
sr <- spatSample(logo, 100000, "regular")
pca <- prcomp(sr)

x <- predict(logo, pca)
plot(x)


############################ GETTING SOIL DATA AND DAYLENGTH ########################

library(ggpubr)

soil_plt<-soil %>% na.omit()


geodata::soil_world(var=c(soc,bdod, cfvo, clay, nitrogen, ocd, phh2o, sand, silt), depth=15, stat=mean, path='/Volumes/MY_HARD_DRI/')

library(geosphere)
library(terra)
r <- rast(res=0.0083, ymin=0, nlyr=365)
lat <- yFromRow(r, 1:nrow(r))
d <- sapply(1:365, function(i) daylength(lat, i))
values(r) <- rep(d, each=ncol(r))



#######################

library(doParallel)
library(foreach)
# Set the number of cores to use for parallel processing
ncores <- parallel::detectCores() - 1  # Use all available cores except one
registerDoParallel(cores = ncores)

m<-stack()

foreach (month_name = month.name) %dopar% {
  print(month_name)
  # Subset layers corresponding to the current month
  month_layers <- s[[which(months(getZ(s)) == month_name)]]
  print(month_layers)
  # Calculate mean for the month (exclude NA values)
  month_mean <- mean(month_layers, na.rm = TRUE)
  print(month_mean)
  # Store the monthly mean in the list with the month name as the list element name
  m<-stack(m,month_mean)
}

# Stop the parallel backend
stopImplicitCluster()

month_avg <- stackApply(s, months(getZ(s)), fun = mean) # calculate monthly averages
month_avg 
year_avg <- calc(month_avg, fun = mean)
writeRaster(year_avg,filename = paste("mean_lst_night_", year, ".tif", sep = ""), format = "GTiff")

# backup # This also works
# for (file in files) { 
#   jdt<-as.numeric(substr(file,34,36))
#   jdt <- as.numeric(jdt-1) # R uses a 0 based index for dates only # this means it starts counting at 0 rather than 1 when working with dates
#   year<-as.numeric(substr(file,30,33))
#   mydate<- as.Date(jdt, origin = as.Date(as.character(paste(year,"-01","-01", sep=""),format="%Y%m%d")))
# }
# 
# f1<- 'MOD11A2.061_LST_Night_1km_doy2002001_aid0001.tif'
# f2<- 'MOD11A2.061_LST_Night_1km_doy2002145_aid0001.tif'
# jdt<-as.numeric(substr(f2,34,36))
# jdt <- as.numeric(jdt-1) # R uses a 0 based index for dates only # this means it starts counting at 0 rather than 1 when working with dates
# jdt
# year<-as.numeric(substr(f1,30,33))
# year
# r_date<- as.Date(jdt, origin = as.Date(as.character(paste(year,"-01","-01", sep=""),format="%Y%m%d")))
# r_date
# r1<-raster(f1)
# r2<-raster(f2)
# s1 <- setZ(r1, r_date)
# s2 <-setZ(r2,r_date)

######################################## FOR 1 YEAR ##################################################


year = 2007
ts_data <- get(load("/Users/sansari/Desktop/vectors/lai_2_1_2002_2005/LAI_8Days_500m_v61/Time_Series/RData/Terra/Lai/MCD15A2H_Lai_185_2002_361_2005_RData.RData"))
ts_data

mindate<- as.Date(as.character(paste(year,"-01","-01", sep=""),format="%Y%m%d"))
maxdate <- as.Date(as.character(paste(year, "-12","-31",sep=""), format="%Y%m%d"))

mindate
maxdate

files<- list.files(pattern = ".tif", full.names = T)
files
lai<- stack(files)
# Subset the stack for the current year
subset<- subset(lai, which(getZ(lai) >= mindate & getZ(lai) <= maxdate))

subset
# Compute monthly average
month_avg <- stackApply(subset, months(getZ(subset)), fun = mean)
month_avg
# Compute yearly average
year_avg <- calc(month_avg, fun = mean)
#plot(year_avg, axes = FALSE, horizontal = T)

writeRaster(year_avg,filename = paste("mean_lai_", year, ".tif", sep = ""), format = "GTiff")

################################################################################################



######################### FOR A SINGLE POLYGON LAYER ##################################
rm(list=ls())
ls()

library(MODIStsp,lib.loc="/home/sansari/.conda/envs/env_modistsp_2/lib/R/library")
library(raster,lib.loc="/home/sansari/.conda/envs/env_modistsp_2/lib/R/library")


# Load the time series
setwd("/Users/sansari/Desktop/vectors/lai_2_1/")
ts_data <- get(load(""))
ts_data

# plot some dates
plot(ts_data[[c(1,5)]], axes = FALSE, horizontal = T)


# Set the range of years
start_year <- 2002
end_year <- 2012

# Loop over the years
for (year in start_year:end_year) {
  mindate<- as.Date(as.character(paste(year,"-01","-01", sep=""),format="%Y%m%d"))
  maxdate <- as.Date(as.character(paste(year, "-12","-31",sep=""), format="%Y%m%d"))
  print(mindate)
  print(maxdate)
  # Subset the stack for the current year
  subset<- subset(ts_data, which(getZ(ts_data) >= mindate & getZ(ts_data) <= maxdate))
  
  # Compute monthly average
  month_avg <- stackApply(subset, months(getZ(subset)), fun = mean)
  
  # Compute yearly average
  year_avg <- calc(month_avg, fun = mean)
  print(year_avg)
  
  #plot(year_avg, axes = FALSE, horizontal = T)
  writeRaster(year_avg,filename = paste("mean_lai_polygon4_", year, ".tif", sep = ""), format = "GTiff")
}

# Mean of the raster layers from years 2002 to 2012
setwd('/Users/sansari/Desktop/vectors/mcd12q1_igbp_lctype1/LandCover_Type_Yearly_500m_v61/LC1/')
files<- list.files(pattern = ".tif", full.names = T)
files
lai<- rast(files)
#lai_mean_2002_2012<- calc (lai, fun = median)
m_test1 <- terra::app(lai, median)

#scale values
lai_mean_2002_2012_scaled<- lai_mean_2002_2012 * 0.1

#lai_mean_2002_2012_scaled
#plot(lai_mean_2012_2020_scaled, axes = FALSE, horizontal = T)

#crs(lai_mean_2002_2020_scaled)<- CRS("+init=epsg:4326")

terra::writeRaster(m_test1,filename = "landcover_igbp_lctype1_2002_2012.tif") #GTiff #ascii



########
gatk<- read.table('/Users/sansari/Downloads/To_rename_GATK_all_swapped.txt', header = T)
myfile<- openxlsx::read.xlsx('/Users/sansari/Desktop/athliana/gatk_vcf_id_match.xlsx')

final<- left_join(myfile, gatk, by = join_by(genotypes ==  vcf_id))


df1<- read.xlsx("/Users/sansari/Desktop/athliana/gatk_vcf_id_match_joined.xlsx")
df2<- read.xlsx("/Users/sansari/Desktop/athliana/athaliana_complete_file.xlsx")

#df2$Longitude <- as.numeric(df2$Longitude)  # Convert one variable to numeric
df2$Latitude <- as.character(df2$Latitude)  # Convert one variable to numeric


final1<- left_join(df2, df1, by = join_by(Genotype_id ==  check_with_gatk_renamed_file))
write.xlsx(final1,"/Users/sansari/Desktop/athliana/athaliana_complete_file_1.xlsx")

athaliana<- read.xlsx('/Users/sansari/Desktop/athliana/athaliana_complete_file.xlsx')
write.csv(athaliana,"/Users/sansari/Desktop/athliana/athaliana_complete_file.csv", quote = F, row.names = F)

