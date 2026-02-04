
#####################################       DATA CLEANING OF HERBARIUM RECORDS     ####################################

setwd("/Users/sansari/Desktop/arabis/arabis_gbif")
df<- read.csv("/Users/sansari/Desktop/arabis/arabis_gbif/occurrence.txt", sep = "\t",quote = "")
head(df)
nrow(df) #39021
unique(df$occurrenceStatus) # make sure there is only 'PRESENT' data
unique(df$species) # make sure there is only species
unique(df$year) # 1950 to until now

# remove entries with missing data for coordinates
na_removed <- subset(df,!is.na(decimalLatitude)&!is.na(decimalLongitude))
cat(nrow(df) - nrow(na_removed), "records are removed") #2299 records are removed

# check the points on map
library(geodata)
wrld <- world(path=".")
plot(wrld, xlim=c(-175,175), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(na_removed$decimalLongitude, na_removed$decimalLatitude, col='red', pch=20)

# check for longitude=0
lonzero <- subset(na_removed, decimalLongitude==0) # 10 records
lonzero$decimalLongitude
points(lonzero$decimalLongitude, lonzero$decimalLatitude, col='blue', pch=20) # seem suspicious and can be removed

library(dplyr)
na_removed_lonzero <- na_removed %>% filter(decimalLongitude != 0) 
nrow(na_removed_lonzero) #36712

# remove duplicates
dups <- duplicated (na_removed_lonzero [,c("decimalLatitude","decimalLongitude")])
df_unique <- na_removed_lonzero [!dups,] #! means opposite logic value
cat(nrow(na_removed_lonzero)-nrow(df_unique), "records are removed") #21517 records are removed
nrow(df_unique) #15195

herb_records<- df_unique[,c("decimalLatitude","decimalLongitude")]
head(herb_records)
colnames(herb_records)[1] <- "Latitude"
colnames(herb_records)[2] <- "Longitude"
nrow(herb_records) # 15195


herb_records$Species<- "Arabis alpina"

# now distribute the data based on Latitude
# with this conditioning it is essential that smaller value comes first when using between()
herb_records <- herb_records %>%
  mutate(Polygon = case_when(
    between(Latitude, -20,0) ~ "Polygon1",
    between(Latitude, 0, 20) ~ "Polygon1",
    between(Latitude, -40, -20) ~ "Polygon2",
    between(Latitude, 20, 40) ~ "Polygon2", # Lat value 40 goes to Polygon2
    between(Latitude, -60, -40) ~ "Polygon3",
    between(Latitude, 40, 60) ~ "Polygon3", # Lat value 60 goes to Polygon3
    between(Latitude, 60, 80) ~ "Polygon4",
    TRUE ~ NA_character_  # Add a default value if needed
  ))

sum(is.na(herb_records$Polygon)) 
nrow(herb_records) #15195

## add ids to remove confusion
herb_records <- tibble::rowid_to_column(herb_records, "ID")
herb_records$ID <- sub("^", "PRS_ALPINA", herb_records$ID )

nrow(herb_records) #15195

# check that there should not be any duplicate ID
sum(duplicated(herb_records$ID)) 

write.csv(herb_records,'/Users/sansari/Desktop/arabis/arabis_gbif/alp_herb_final.csv', row.names = F)
openxlsx::write.xlsx(herb_records,'/Users/sansari/Desktop/arabis/arabis_gbif/alp_herb_final.xlsx')



###############################      SAMPLING BIAS- Thinning of records         ###################################################

library(terra)
library(dplyr)
occ<- read.csv("/Users/sansari/Desktop/arabis/alp_herb_final.csv")

acv <- vect(occ, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
class(acv)

# create a SpatRaster with the extent of acgeo
r <- rast(acv)
# set the resolution of the cells to (for example) 1 degree
res(r) <- 0.0083
# extend (expand) the extent of the SpatRaster a little
r <- extend(r, ext(r)+1)
# sample:
set.seed(13)
acsel <- spatSample(acv, size=1, "random", strata=r)

# to illustrate the method and show the result
#p <- as.polygons(r)
#plot(p, border='gray')
#points(acv)
# selected points in red
#points(acsel, cex=1, col='red', pch='x')

thinned<- as.data.frame(acsel, geom='XY')
names(thinned)[names(thinned) == "x"] <- "Longitude"
names(thinned)[names(thinned) == "y"] <- "Latitude"


head(thinned)
nrow(thinned)
nrow(occ)

write.csv(thinned,"/Users/sansari/Desktop/arabis/alp_herb_final_thinned.csv", row.names = F)



########################         PSEUDOABSENCE POINTS GENERATION       ###############

#https://damariszurell.github.io/EEC-MGC/b5_pseudoabsence.html 


# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"

library(terra)
region<- rast("/Users/sansari/Desktop/elev_bg.tif")
sp<- read.csv("/Users/sansari/Desktop/arabis/alp_herb_final.csv", header = T)
set.seed(200) # 1 and 200 #use different seed to generate another set

# Make a new regional mask that contains NAs in presence locations:
sp_cells <- terra::extract(region, sp[,c("Longitude","Latitude")], cells=T)$cell
region_exclp <- region
values(region_exclp)[sp_cells] <- NA

# Randomly select background data but excluding presence locations
bg_rand_exclp <- terra::spatSample(region_exclp, 20000, "random",
                                   na.rm=T, as.points=TRUE)

# Plot the map and data
#plot(region,col='grey',legend=F)
#points(sp[,c("Longitude","Latitude")],pch='+',col='red')
#points(bg_rand_exclp,pch=19,cex=0.3)

# convert the bg points to dataframe format
bg_rand_df <- data.frame(terra::geom(bg_rand_exclp)[,c('x','y')])
names(bg_rand_df)[names(bg_rand_df) == "x"] <- "Longitude"
names(bg_rand_df)[names(bg_rand_df) == "y"] <- "Latitude"


# when data generated by QGIS
bg_rand_df<- read.csv("/Users/sansari/Desktop/arabis/ps4.csv")
head(bg_rand_df)
nrow(bg_rand_df)
names(bg_rand_df)[names(bg_rand_df) == "X"] <- "Longitude"
names(bg_rand_df)[names(bg_rand_df) == "Y"] <- "Latitude"
bg_rand_df<- bg_rand_df[,c(1:2)]


#### Save the background records with IDs 
library(dplyr)
bg_rand_df <- bg_rand_df %>%
  mutate(Polygon = case_when(
    between(Latitude, -20,0) ~ "Polygon1",
    between(Latitude, 0, 20) ~ "Polygon1",
    between(Latitude, -40, -20) ~ "Polygon2",
    between(Latitude, 20, 40) ~ "Polygon2", # Lat value 40 goes to Polygon2
    between(Latitude, -60, -40) ~ "Polygon3",
    between(Latitude, 40, 60) ~ "Polygon3", # Lat value 60 goes to Polygon3
    between(Latitude, 60, 80) ~ "Polygon4",
    TRUE ~ NA_character_  # Add a default value if needed
  ))


sum(is.na(bg_rand_df$Polygon)) # check for blanks in Polygon
bg_rand_df<- tibble::rowid_to_column(bg_rand_df, "ID")
bg_rand_df$ID <- sub("^", "PS4_ALPINA_", bg_rand_df$ID )
sum(duplicated(bg_rand_df$ID)) # check that there should not be any duplicate ID

bg_rand_df$Species<- "Arabis alpina"
head(bg_rand_df)

write.csv(bg_rand_df,'/Users/sansari/Desktop/arabis/arabis_ps4_38475_buf2degdiff_records.csv',row.names = F)



#############################       PRESENCE DATA: EXTRACTION     #######################

# read extracted data and merge 'em all
setwd('/Users/sansari/Desktop/arabis/extraction/')

library(dplyr)
df1<- read.csv("v_bio_1.csv") %>% select("ID","Longitude","Latitude","Species","Polygon","bio_1")

sum(duplicated(df1$ID)) # check that there should not be any duplicate ID
sum(is.na(df1$ID))

df2<- read.csv("v_bio_2.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df3<- read.csv("v_bio_3.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df4<- read.csv("v_bio_4.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df5<- read.csv("v_bio_5.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df6<- read.csv("v_bio_6.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df7<- read.csv("v_bio_7.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df8<- read.csv("v_bio_8.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df9<- read.csv("v_bio_9.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df10<- read.csv("v_bio_10.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df11<- read.csv("v_bio_11.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df12<- read.csv("v_bio_12.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df13<- read.csv("v_bio_13.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df14<- read.csv("v_bio_14.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df15<- read.csv("v_bio_15.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df16<- read.csv("v_bio_16.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df17<- read.csv("v_bio_17.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df18<- read.csv("v_bio_18.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df19<- read.csv("v_bio_19.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df20<- read.csv("v_soc.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df21<- read.csv("v_cfvo.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df22<- read.csv("v_clay.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df23<- read.csv("v_bdod.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df24<- read.csv("v_nitrogen.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df25<- read.csv("v_ocd.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df26<- read.csv("v_silt.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df27<- read.csv("v_sand.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df28<- read.csv("v_phh2o.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df29<- read.csv("v_treecover.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df30<- read.csv("v_gs.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df31<- read.csv("v_ai.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df32<- read.csv("v_elev.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df33<- read.csv("v_wind_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df34<- read.csv("v_srad_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df35<- read.csv("v_daylst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df36<- read.csv("v_daylst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df37<- read.csv("v_daylst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df38<- read.csv("v_nightlst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df39<- read.csv("v_nightlst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df40<- read.csv("v_nightlst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df41<- read.csv("v_ndvi_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df42<- read.csv("v_ndvi_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df43<- read.csv("v_ndvi_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df44<- read.csv("v_et_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df45<- read.csv("v_et_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df46<- read.csv("v_et_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))


comb<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,
           df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,
           df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,
           df44,df45,df46) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb,'alp_herb_final_vars.csv',row.names = F)
nrow(comb)#94226
head(comb)

## addition of new variables
#read the latest final variables file
setwd('/Users/sansari/Desktop/arabis/extraction/')
alp_vars<-read.csv('/Users/sansari/Desktop/arabis/extraction/alp_herb_final_vars.csv')

df47<- read.csv("v_slope.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))

sum(duplicated(df47$ID)) # check that there should not be any duplicate ID
sum(is.na(df47$ID))

df48<- read.csv("v_tri.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df49<- read.csv("v_tpi.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df50<- read.csv("v_aspect.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df51<- read.csv("v_lc_igbp.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))

comb2<-list(alp_vars,df47,df48,df49,df50,df51) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb2,'alp_herb_final_vars.csv',row.names = F)

# new variables
setwd('/Users/sansari/Desktop/arabis/extraction/')
alp_vars<-read.csv('/Users/sansari/Desktop/arabis/extraction/alp_herb_final_vars.csv')

df52<- read.csv("v_lc_igbp_res.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))

sum(duplicated(df52$ID)) # check that there should not be any duplicate ID
sum(is.na(df52$ID))

comb3<-list(alp_vars,df52) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb3,'alp_herb_final_vars.csv',row.names = F)


# monthly variables
setwd('/Users/sansari/Desktop/arabis/extraction/')
library(dplyr)

df1_mon<- read.csv("v_January_daylst.csv")
df2_mon<- read.csv("v_February_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df3_mon<- read.csv("v_March_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df4_mon<- read.csv("v_April_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df5_mon<- read.csv("v_May_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df6_mon<- read.csv("v_June_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df7_mon<- read.csv("v_July_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df8_mon<- read.csv("v_August_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df9_mon<- read.csv("v_September_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df10_mon<- read.csv("v_October_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df11_mon<- read.csv("v_November_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df12_mon<- read.csv("v_December_daylst.csv") %>% select(-c("ID","Species","Polygon"))
df13_mon<- read.csv("v_January_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df14_mon<- read.csv("v_February_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df15_mon<- read.csv("v_March_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df16_mon<- read.csv("v_April_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df17_mon<- read.csv("v_May_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df18_mon<- read.csv("v_June_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df19_mon<- read.csv("v_July_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df20_mon<- read.csv("v_August_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df21_mon<- read.csv("v_September_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df22_mon<- read.csv("v_October_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df23_mon<- read.csv("v_November_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df24_mon<- read.csv("v_December_nightlst.csv") %>% select(-c("ID","Species","Polygon"))
df25_mon<- read.csv("v_January_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df26_mon<- read.csv("v_February_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df27_mon<- read.csv("v_March_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df28_mon<- read.csv("v_April_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df29_mon<- read.csv("v_May_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df30_mon<- read.csv("v_June_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df31_mon<- read.csv("v_July_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df32_mon<- read.csv("v_August_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df33_mon<- read.csv("v_September_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df34_mon<- read.csv("v_October_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df35_mon<- read.csv("v_November_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df36_mon<- read.csv("v_December_ndvi.csv") %>% select(-c("ID","Species","Polygon"))
df37_mon<- read.csv("v_January_et.csv") %>% select(-c("ID","Species","Polygon"))
df38_mon<- read.csv("v_February_et.csv") %>% select(-c("ID","Species","Polygon"))
df39_mon<- read.csv("v_March_et.csv") %>% select(-c("ID","Species","Polygon"))
df40_mon<- read.csv("v_April_et.csv") %>% select(-c("ID","Species","Polygon"))
df41_mon<- read.csv("v_May_et.csv") %>% select(-c("ID","Species","Polygon"))
df42_mon<- read.csv("v_June_et.csv") %>% select(-c("ID","Species","Polygon"))
df43_mon<- read.csv("v_July_et.csv") %>% select(-c("ID","Species","Polygon"))
df44_mon<- read.csv("v_August_et.csv") %>% select(-c("ID","Species","Polygon"))
df45_mon<- read.csv("v_September_et.csv") %>% select(-c("ID","Species","Polygon"))
df46_mon<- read.csv("v_October_et.csv") %>% select(-c("ID","Species","Polygon"))
df47_mon<- read.csv("v_November_et.csv") %>% select(-c("ID","Species","Polygon"))
df48_mon<- read.csv("v_December_et.csv") %>% select(-c("ID","Species","Polygon"))


comb4<-list(df1_mon,df2_mon,df3_mon,df4_mon,df5_mon,df6_mon,df7_mon,df8_mon,df9_mon,df10_mon,df11_mon,df12_mon,df13_mon,df14_mon,df15_mon,
           df16_mon,df17_mon,df18_mon,df19_mon,df20_mon,df21_mon,df22_mon,df23_mon,df24_mon,df25_mon,df26_mon,df27_mon,df28_mon,df29_mon,
           df30_mon,df31_mon,df32_mon,df33_mon,df34_mon,df35_mon,df36_mon,df37_mon,df38_mon,df39_mon,df40_mon,df41_mon,df42_mon,df43_mon,
           df44_mon,df45_mon,df46_mon,df47_mon,df48_mon) %>% purrr::reduce(inner_join, by = c("Longitude","Latitude"))
head(comb4)
nrow(comb4) #15195
nrow(df34_mon)

write.csv(comb4,"/Users/sansari/Desktop/arabis/extraction/alp_herb_final_vars_monthly.csv", quote = F, row.names = F)



#############################        PSEUDOABSENCE DATA: EXTRACTION       ##########################

# read extracted data and merge 'em all

setwd('/Users/sansari/Desktop/arabis/extraction/ps_records/')

library(dplyr)
df1<- read.csv("ps2_bio_1.csv") %>% select("ID","Longitude","Latitude","Species","Polygon","bio_1")

sum(duplicated(df1$ID)) # check that there should not be any duplicate ID
sum(is.na(df1$ID)) # check that there should not be any missing ID

df2<- read.csv("ps2_bio_2.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df3<- read.csv("ps2_bio_3.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df4<- read.csv("ps2_bio_4.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df5<- read.csv("ps2_bio_5.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df6<- read.csv("ps2_bio_6.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df7<- read.csv("ps2_bio_7.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df8<- read.csv("ps2_bio_8.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df9<- read.csv("ps2_bio_9.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df10<- read.csv("ps2_bio_10.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df11<- read.csv("ps2_bio_11.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df12<- read.csv("ps2_bio_12.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df13<- read.csv("ps2_bio_13.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df14<- read.csv("ps2_bio_14.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df15<- read.csv("ps2_bio_15.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df16<- read.csv("ps2_bio_16.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df17<- read.csv("ps2_bio_17.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df18<- read.csv("ps2_bio_18.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df19<- read.csv("ps2_bio_19.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df20<- read.csv("ps2_soc.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df21<- read.csv("ps2_cfvo.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df22<- read.csv("ps2_clay.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df23<- read.csv("ps2_bdod.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df24<- read.csv("ps2_nitrogen.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df25<- read.csv("ps2_ocd.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df26<- read.csv("ps2_silt.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df27<- read.csv("ps2_sand.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df28<- read.csv("ps2_phh2o.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df29<- read.csv("ps2_treecover.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df30<- read.csv("ps2_gs.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df31<- read.csv("ps2_ai.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df32<- read.csv("ps2_elev.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df33<- read.csv("ps2_wind_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df34<- read.csv("ps2_srad_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df35<- read.csv("ps2_daylst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df36<- read.csv("ps2_daylst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df37<- read.csv("ps2_daylst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df38<- read.csv("ps2_nightlst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df39<- read.csv("ps2_nightlst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df40<- read.csv("ps2_nightlst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df41<- read.csv("ps2_ndvi_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df42<- read.csv("ps2_ndvi_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df43<- read.csv("ps2_ndvi_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df44<- read.csv("ps2_et_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df45<- read.csv("ps2_et_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
df46<- read.csv("ps2_et_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))


comb_ps<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,
           df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,
           df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,
           df44,df45,df46) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb_ps,'alp_ps2_final_vars.csv',row.names = F)
nrow(comb_ps) #20000
head(comb_ps)



############################   MERGING PRESENCE AND BACKGROUND  ##############################
# library(tidyverse)
# prs<- read.csv('/Users/sansari/Desktop/arabis/extraction/alp_herb_final_vars.csv')
# prs$Occurence<-1
# ncol(prs)
# 
# ps<-read.csv("/Users/sansari/Desktop/arabis/extraction/ps_records/alp_ps2_final_vars.csv")
# ps$Occurence<-0
# ncol(ps)
# 
# 
# prs_ps<-rbind(prs,ps)
# 
# unique(prs_ps$Occurence)
# unique(prs_ps$Polygon)
# unique(prs_ps$Species)
# 
# write.csv(prs_ps,'/Users/sansari/Desktop/arabis/alp_full_data_pr_ps2.csv',row.names = F)
# 
# 
# polygon1<-prs_bac%>% filter(Polygon=='Polygon1')
# nrow(polygon1)#6301
# 
# unique(polygon1$Polygon)
# unique(polygon1$Occurence)
# 
# write.csv(polygon1,'/Users/sansari/Desktop/arabis/alp_polygon1_pr_bg.csv',row.names = F)
# 
# polygon2<-prs_bac%>% filter(Polygon=='Polygon2')
# nrow(polygon2)#6723
# 
# unique(polygon2$Polygon)
# unique(polygon2$Occurence)
# 
# write.csv(polygon2,'/Users/sansari/Desktop/arabis/alp_polygon2_pr_bg.csv',row.names = F)
# 
# polygon3<-prs_bac%>% filter(Polygon=='Polygon3')
# nrow(polygon3)#14421
# 
# unique(polygon3$Polygon)
# unique(polygon3$Occurence)
# 
# write.csv(polygon3,'/Users/sansari/Desktop/arabis/alp_polygon3_pr_bg.csv',row.names = F)
# 
# polygon4<-prs_bac%>% filter(Polygon=='Polygon4')
# nrow(polygon4)#7750
# 
# unique(polygon4$Polygon)
# unique(polygon4$Occurence)
# 
# write.csv(polygon4,'/Users/sansari/Desktop/arabis/alp_polygon4_pr_bg.csv',row.names = F)

#########################################################################



