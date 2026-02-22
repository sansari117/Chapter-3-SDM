
#####################################       DATA CLEANING OF HERBARIUM AND HOME RECORDS     ####################################

setwd("/Users/sansari/Desktop/athliana/sdm/")
df<- read.csv("/Users/sansari/Desktop/athliana/sdm/occurrence.txt", sep = "\t",quote = "")
head(df)
nrow(df) #183453 
unique(df$occurrenceStatus) # make sure there is only 'PRESENT' data
unique(df$species) # make sure there is only species
unique(df$year) # 1950 to until now

# remove entries with missing data for coordinates
na_removed <- subset(df,!is.na(decimalLatitude)&!is.na(decimalLongitude))
cat(nrow(df) - nrow(na_removed), "records are removed") #3177 records are removed

# check the points on map
library(geodata)
wrld <- world(path=".")
plot(wrld, xlim=c(-175,175), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(na_removed$decimalLongitude, na_removed$decimalLatitude, col='red', pch=20)

# check for longitude=0
lonzero <- subset(na_removed, decimalLongitude==0) # 29 records
lonzero$decimalLongitude
points(lonzero$decimalLongitude, lonzero$decimalLatitude, col='blue', pch=20) # seem suspicious and can be removed

library(dplyr)
na_removed_lonzero <- na_removed %>% filter(decimalLongitude != 0) 
nrow(na_removed_lonzero) #180247

# remove duplicates
dups <- duplicated (na_removed_lonzero [,c("decimalLatitude","decimalLongitude")])
df_unique <- na_removed_lonzero [!dups,] #! means opposite logic value
cat(nrow(na_removed_lonzero)-nrow(df_unique), "records are removed") #87960 records are removed
nrow(df_unique) #92287

herb_records<- df_unique[,c("decimalLatitude","decimalLongitude")]
head(herb_records)
colnames(herb_records)[1] <- "Latitude"
colnames(herb_records)[2] <- "Longitude"
nrow(herb_records) # 92287
points(herb_records$Longitude, herb_records$Latitude, col='blue', pch=20) # seem suspicious and can be removed

# herb_records can now be merged with home records and published sources which couldn't make it to the GBIF


# read the home records file
library(openxlsx)
df1<- read.xlsx("/Users/sansari/Desktop/athliana/sdm/athaliana_complete_file.xlsx")
nrow(df1) #5962

# remove entries with missing data for coordinates
na_removed_df1 <- subset(df1,(!is.na(Latitude))&(!is.na(Longitude)))
cat(nrow(df1) - nrow(na_removed_df1), "records are removed") #4

# remove duplicates
dups_df1 <- duplicated (na_removed_df1 [,c("Latitude","Longitude")])
df1_unique <- na_removed_df1 [!dups_df1,] #! means opposite logic value
cat(nrow(na_removed_df1)-nrow(df1_unique), "records are removed") # 3961

home_records<- df1_unique[,c("Latitude","Longitude")]
nrow(home_records) #1997

# merge records and again remove duplicates
records<- rbind(herb_records,home_records)
nrow(records) #94284
dups_records<- duplicated (records [,c("Latitude","Longitude")])
unique_records <- records [!dups_records,] #! means opposite logic value
cat(nrow(records)-nrow(unique_records), "records are removed") # 56
nrow(unique_records) #94228

unique_records$Species<- "Arabidopsis thaliana"

# now distribute the data based on Latitude
# with this conditioning it is essential that smaller value comes first when using between()
unique_records <- unique_records %>%
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

sum(is.na(unique_records$Polygon)) 
#2 missing which are latitude values:82.233333 and -62.23, which can be removed 
unique_records <- unique_records %>% filter(Latitude!=82.233333 & Latitude != -62.23)
nrow(unique_records) #94226

## add ids to remove confusion
unique_records <- tibble::rowid_to_column(unique_records, "ID")
unique_records$ID <- sub("^", "PRS_", unique_records$ID )

nrow(unique_records) #94226
sum(duplicated(unique_records$ID)) # check that there should not be any duplicate ID

write.csv(unique_records,'tha_herb_home_final.csv', row.names = F)
write.xlsx(unique_records,'tha_herb_home_final.xlsx')


#### add comments
comments<-read.csv('/Users/sansari/Desktop/athliana/sdm/tha_herb_home_final_comments.csv')
nrow(comments) #94226

######################## PR DATA: EXTRACTION     #################
# read extracted data and merge 'em all
#bioclims
library(dplyr)
df1<- read.csv("v_bio_1.csv") %>% select("ID","Longitude","Latitude","Species","Polygon","Comment","bio_1")

sum(duplicated(df1$ID)) # check that there should not be any duplicate ID
sum(is.na(df1$ID))

df2<- read.csv("v_bio_2.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df3<- read.csv("v_bio_3.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df4<- read.csv("v_bio_4.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df5<- read.csv("v_bio_5.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df6<- read.csv("v_bio_6.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df7<- read.csv("v_bio_7.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df8<- read.csv("v_bio_8.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df9<- read.csv("v_bio_9.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df10<- read.csv("v_bio_10.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df11<- read.csv("v_bio_11.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df12<- read.csv("v_bio_12.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df13<- read.csv("v_bio_13.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df14<- read.csv("v_bio_14.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df15<- read.csv("v_bio_15.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df16<- read.csv("v_bio_16.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df17<- read.csv("v_bio_17.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df18<- read.csv("v_bio_18.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df19<- read.csv("v_bio_19.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df20<- read.csv("v_soc.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df21<- read.csv("v_cfvo.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df22<- read.csv("v_clay.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df23<- read.csv("v_bdod.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df24<- read.csv("v_nitrogen.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df25<- read.csv("v_ocd.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df26<- read.csv("v_silt.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df27<- read.csv("v_sand.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df28<- read.csv("v_phh2o.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df29<- read.csv("v_treecover.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df30<- read.csv("v_gs.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df31<- read.csv("v_ai.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df32<- read.csv("v_elev.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df33<- read.csv("v_wind_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df34<- read.csv("v_srad_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df35<- read.csv("v_daylst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df36<- read.csv("v_daylst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df37<- read.csv("v_daylst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df38<- read.csv("v_nightlst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df39<- read.csv("v_nightlst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df40<- read.csv("v_nightlst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df41<- read.csv("v_ndvi_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df42<- read.csv("v_ndvi_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df43<- read.csv("v_ndvi_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df44<- read.csv("v_et_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df45<- read.csv("v_et_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df46<- read.csv("v_et_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))

                                          
comb<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,
           df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,
           df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,
           df44,df45,df46) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb,'tha_herb_home_final_vars.csv',row.names = F)
nrow(comb)#94226
head(comb)

## addition of new variables
#read the latest final variables file
setwd('/Users/sansari/Desktop/athliana/sdm_files/extraction/')
tha_vars<- read.csv('/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv')
df47<- read.csv("v_slope.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))

sum(duplicated(df47$ID)) # check that there should not be any duplicate ID
sum(is.na(df47$ID))

df48<- read.csv("v_tri.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df49<- read.csv("v_tpi.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df50<- read.csv("v_aspect.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df51<- read.csv("v_lc_igbp.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))

comb2<-list(tha_vars,df47,df48,df49,df50,df51) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb2,"/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv",row.names = F)

#read the latest final variables file
setwd('/Users/sansari/Desktop/athliana/sdm_files/extraction/')

tha_vars<- read.csv('/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv')
df52<- read.csv("v_lc_igbp_res.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))

sum(duplicated(df52$ID)) # check that there should not be any duplicate ID
sum(is.na(df52$ID))


comb3<-list(tha_vars,df52) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb3,"/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv",row.names = F)

# monthly variables
setwd('/Users/sansari/Desktop/athliana/sdm_files/extraction/')
library(dplyr)

df1_mon<- read.csv("v_January_daylst.csv")
head(df1_mon)
df2_mon<- read.csv("v_February_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df3_mon<- read.csv("v_March_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df4_mon<- read.csv("v_April_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df5_mon<- read.csv("v_May_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df6_mon<- read.csv("v_June_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df7_mon<- read.csv("v_July_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df8_mon<- read.csv("v_August_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df9_mon<- read.csv("v_September_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df10_mon<- read.csv("v_October_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df11_mon<- read.csv("v_November_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df12_mon<- read.csv("v_December_daylst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df13_mon<- read.csv("v_January_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df14_mon<- read.csv("v_February_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df15_mon<- read.csv("v_March_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df16_mon<- read.csv("v_April_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df17_mon<- read.csv("v_May_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df18_mon<- read.csv("v_June_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df19_mon<- read.csv("v_July_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df20_mon<- read.csv("v_August_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df21_mon<- read.csv("v_September_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df22_mon<- read.csv("v_October_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df23_mon<- read.csv("v_November_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df24_mon<- read.csv("v_December_nightlst.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df25_mon<- read.csv("v_January_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df26_mon<- read.csv("v_February_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df27_mon<- read.csv("v_March_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df28_mon<- read.csv("v_April_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df29_mon<- read.csv("v_May_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df30_mon<- read.csv("v_June_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df31_mon<- read.csv("v_July_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df32_mon<- read.csv("v_August_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df33_mon<- read.csv("v_September_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df34_mon<- read.csv("v_October_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df35_mon<- read.csv("v_November_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df36_mon<- read.csv("v_December_ndvi.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df37_mon<- read.csv("v_January_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df38_mon<- read.csv("v_February_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df39_mon<- read.csv("v_March_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df40_mon<- read.csv("v_April_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df41_mon<- read.csv("v_May_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df42_mon<- read.csv("v_June_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df43_mon<- read.csv("v_July_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df44_mon<- read.csv("v_August_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df45_mon<- read.csv("v_September_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df46_mon<- read.csv("v_October_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df47_mon<- read.csv("v_November_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))
df48_mon<- read.csv("v_December_et.csv") %>% select(-c("ID","Species","Polygon","Comment"))


comb4<-list(df1_mon,df2_mon,df3_mon,df4_mon,df5_mon,df6_mon,df7_mon,df8_mon,df9_mon,df10_mon,df11_mon,df12_mon,df13_mon,df14_mon,df15_mon,
            df16_mon,df17_mon,df18_mon,df19_mon,df20_mon,df21_mon,df22_mon,df23_mon,df24_mon,df25_mon,df26_mon,df27_mon,df28_mon,df29_mon,
            df30_mon,df31_mon,df32_mon,df33_mon,df34_mon,df35_mon,df36_mon,df37_mon,df38_mon,df39_mon,df40_mon,df41_mon,df42_mon,df43_mon,
            df44_mon,df45_mon,df46_mon,df47_mon,df48_mon) %>% purrr::reduce(inner_join, by = c("Longitude","Latitude"))
head(comb4)
nrow(comb4)#94226
nrow(df34_mon)

write.csv(comb4,"/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars_monthly.csv", quote = F, row.names = F)




############################        BACKGROUND         #######################################

# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
studyArea<- raster("/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/latitudinal_clusters/polygon1/variables_fr_sdm/bio_1.asc")
set.seed(1)
bg <- sampleRandom(x=studyArea,
                   size=5000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 
bg1<-as.data.frame(bg)
write.csv(bg1,"/Users/sansari/Desktop/athliana/sdm/bg_polygon1.csv", row.names = F)

plot(studyArea[[1]])
# add the background points to the plotted raster
plot(bg,add=T) 

# add the occurrence data to the plotted raster
plot(occ_final,add=T,col="red")

#### Save the backgrund records with IDs 

bg_polygon1<- read.csv('/Users/sansari/Desktop/athliana/sdm/bg_polygon1.csv')%>% select("Longitude","Latitude")
bg_polygon2<- read.csv('/Users/sansari/Desktop/athliana/sdm/bg_polygon2.csv')%>% select("Longitude","Latitude")
bg_polygon3<- read.csv('/Users/sansari/Desktop/athliana/sdm/bg_polygon3.csv')%>% select("Longitude","Latitude")
bg_polygon4<- read.csv('/Users/sansari/Desktop/athliana/sdm/bg_polygon4.csv')%>% select("Longitude","Latitude")

bg_records<- rbind(bg_polygon1,bg_polygon2,bg_polygon3,bg_polygon4)
bg_records<- tibble::rowid_to_column(bg_records, "ID")
bg_records$ID <- sub("^", "BAC_", bg_records$ID )
sum(duplicated(bg_records$ID)) # check that there should not be any duplicate ID


write.csv(bg_records,'bg_records.csv',row.names = F)


########################         PSEUDOABSENCE       ###############
#https://damariszurell.github.io/EEC-MGC/b5_pseudoabsence.html#3_BackgroundPseudo-absence_data_selection

# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
library(terra)
region<- rast("/Users/sansari/Desktop/elev_bg.tif")
sp<- read.csv("/Users/sansari/Desktop/athliana/sdm_files/tha_herb_home_final.csv", header = T)
set.seed(100) # use different seed to generate another set

# Make a new regional mask that contains NAs in presence locations:
sp_cells <- terra::extract(region, sp[,c("Longitude","Latitude")], cells=T)$cell
region_exclp <- region
values(region_exclp)[sp_cells] <- NA

# Randomly select background data but excluding presence locations
bg_rand_exclp <- terra::spatSample(region_exclp, 100000, "random",
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
bg_rand_df<- read.csv("/Users/sansari/Desktop/athliana/sdm_files/ps8.csv")
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
bg_rand_df$ID <- sub("^", "PS8_ARABIDOPSIS_", bg_rand_df$ID )
sum(duplicated(bg_rand_df$ID)) # check that there should not be any duplicate ID

bg_rand_df$Species<- "Arabidopsis thaliana"
bg_rand_df$Comment<- NA

write.csv(bg_rand_df,'/Users/sansari/Desktop/athliana/sdm_files/tha_ps8_247374_buf2degdiff_records.csv',row.names = F)


###############         PS DATA: EXTRACTION ############# 
setwd('/Users/sansari/Desktop/athliana/sdm_files/extraction/ps_records/')
library(dplyr)
df1<- read.csv("ps1_bio_1.csv") %>% select("ID","Longitude","Latitude","Species","Polygon","Comment","bio_1")

sum(duplicated(df1$ID)) # check that there should not be any duplicate ID
sum(is.na(df1$ID)) # check that there should not be any missing ID

df2<- read.csv("ps1_bio_2.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df3<- read.csv("ps1_bio_3.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df4<- read.csv("ps1_bio_4.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df5<- read.csv("ps1_bio_5.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df6<- read.csv("ps1_bio_6.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df7<- read.csv("ps1_bio_7.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df8<- read.csv("ps1_bio_8.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df9<- read.csv("ps1_bio_9.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df10<- read.csv("ps1_bio_10.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df11<- read.csv("ps1_bio_11.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df12<- read.csv("ps1_bio_12.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df13<- read.csv("ps1_bio_13.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df14<- read.csv("ps1_bio_14.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df15<- read.csv("ps1_bio_15.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df16<- read.csv("ps1_bio_16.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df17<- read.csv("ps1_bio_17.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df18<- read.csv("ps1_bio_18.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df19<- read.csv("ps1_bio_19.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df20<- read.csv("ps1_soc.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df21<- read.csv("ps1_cfvo.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df22<- read.csv("ps1_clay.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df23<- read.csv("ps1_bdod.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df24<- read.csv("ps1_nitrogen.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df25<- read.csv("ps1_ocd.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df26<- read.csv("ps1_silt.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df27<- read.csv("ps1_sand.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df28<- read.csv("ps1_phh2o.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df29<- read.csv("ps1_treecover.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df30<- read.csv("ps1_gs.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df31<- read.csv("ps1_ai.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df32<- read.csv("ps1_elev.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df33<- read.csv("ps1_wind_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df34<- read.csv("ps1_srad_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df35<- read.csv("ps1_daylst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df36<- read.csv("ps1_daylst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df37<- read.csv("ps1_daylst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df38<- read.csv("ps1_nightlst_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df39<- read.csv("ps1_nightlst_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df40<- read.csv("ps1_nightlst_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df41<- read.csv("ps1_ndvi_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df42<- read.csv("ps1_ndvi_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df43<- read.csv("ps1_ndvi_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df44<- read.csv("ps1_et_mean.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df45<- read.csv("ps1_et_min.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))
df46<- read.csv("ps1_et_max.csv") %>% select(-c("Longitude","Latitude","Species","Polygon","Comment"))


comb_ps<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,
              df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,
              df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,
              df44,df45,df46) %>% purrr::reduce(inner_join, by = "ID")

write.csv(comb_ps,'tha_ps1_final_vars.csv',row.names = F)
nrow(comb_ps)#50000
head(comb_ps)



############################   MERGING PRESENCE AND BACKGROUND/PSEUDOABSENCE  ##############################
library(tidyverse)
prs<- read.csv('/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv')
prs$Occurence<-1
ncol(prs)

ps<-read.csv("/Users/sansari/Desktop/athliana/sdm_files/extraction/ps_records/tha_ps1_final_vars.csv")
ps$Occurence<-0
ncol(ps)


prs_ps<-rbind(prs,ps)

unique(prs_ps$Occurence)
unique(prs_ps$Polygon)
unique(prs_ps$Species)

write.csv(prs_ps,'/Users/sansari/Desktop/athliana/sdm_files/tha_herb_home_full_data_pr_ps1.csv',row.names = F)

## polygons
polygon1<-prs_bac%>% filter(Polygon=='Polygon1') %>% filter(if_any(Comment, is.na))
nrow(polygon1)#5196

unique(polygon1$Polygon)
unique(polygon1$Occurence)
unique(polygon1$Comment)
write.csv(polygon1,'/Users/sansari/Desktop/athliana/sdm/polygon1_pr_ps.csv',row.names = F)

polygon2<-prs_bac%>% filter(Polygon=='Polygon2') %>% filter(if_any(Comment, is.na))
nrow(polygon2)#4853

unique(polygon2$Polygon)
unique(polygon2$Occurence)
unique(polygon2$Comment)
write.csv(polygon2,'/Users/sansari/Desktop/athliana/sdm/polygon2_pr_ps.csv',row.names = F)

polygon3<-prs_bac%>% filter(Polygon=='Polygon3') %>% filter(if_any(Comment, is.na))
nrow(polygon3)#88762

unique(polygon3$Polygon)
unique(polygon3$Occurence)
unique(polygon3$Comment)
write.csv(polygon3,'/Users/sansari/Desktop/athliana/sdm/polygon3_pr_ps.csv',row.names = F)

polygon4<-prs_bac%>% filter(Polygon=='Polygon4') %>% filter(if_any(Comment, is.na))
nrow(polygon4)#9934

unique(polygon4$Polygon)
unique(polygon4$Occurence)
unique(polygon4$Comment)
write.csv(polygon4,'/Users/sansari/Desktop/athliana/sdm/polygon4_pr_ps.csv',row.names = F)



library(geodata)
wrld <- world(path="/Users/sansari/Desktop/athliana/sdm/gadm/")
plot(wrld, xlim=c(-175,175), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(polygon2$Longitude, polygon2$lLatitude, col='red', pch=20)

##################################     SAMPLING BIAS          ###################################################
library(terra)
library(dplyr)
occ<- read.csv("/Users/sansari/Desktop/athliana/sdm_files/tha_herb_home_final.csv")


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

write.csv(thinned,"/Users/sansari/Desktop/athliana/sdm_files/tha_herb_home_final_comments_thinned.csv",row.names = F)


########### 

prs_bac<- read.csv("/Users/sansari/Desktop/athliana/sdm/full_data_pr_ps_thin.csv")
#prs_bac$Comment <- na_if(prs_bac$Comment, '')

polygon1<-prs_bac%>% filter(Polygon=='Polygon1') %>% filter(if_any(Comment, is.na))
nrow(polygon1)#6295

unique(polygon1$Polygon)
unique(polygon1$Occurence)
unique(polygon1$Comment)
write.csv(polygon1,'/Users/sansari/Desktop/athliana/sdm/polygon1_pr_ps_thin.csv',row.names = F)

polygon2<-prs_bac%>% filter(Polygon=='Polygon2') %>% filter(if_any(Comment, is.na))
nrow(polygon2)#8362

unique(polygon2$Polygon)
unique(polygon2$Occurence)
unique(polygon2$Comment)
write.csv(polygon2,'/Users/sansari/Desktop/athliana/sdm/polygon2_pr_ps_thin.csv',row.names = F)

polygon3<-prs_bac%>% filter(Polygon=='Polygon3') %>% filter(if_any(Comment, is.na))
nrow(polygon3)#80868

unique(polygon3$Polygon)
unique(polygon3$Occurence)
unique(polygon3$Comment)
write.csv(polygon3,'/Users/sansari/Desktop/athliana/sdm/polygon3_pr_ps_thin.csv',row.names = F)

polygon4<-prs_bac%>% filter(Polygon=='Polygon4') %>% filter(if_any(Comment, is.na))
nrow(polygon4)#6808

unique(polygon4$Polygon)
unique(polygon4$Occurence)
unique(polygon4$Comment)
write.csv(polygon4,'/Users/sansari/Desktop/athliana/sdm/polygon4_pr_ps_thin.csv',row.names = F)

######################          PLOTTING      #####################################
prs_toplot<-read.csv("/Users/sansari/Desktop/athliana/sdm/extraction/tha_herb_home_final_vars.csv")
prs_toplot$Comment <- dplyr::na_if(prs_toplot$Comment, '')
library(tidyr)
prs_toplot$Comment<-prs_toplot$Comment %>% replace_na('Mainland')

## Remove 'NA=Mainland from dataframe'
prs_toplot<- prs_toplot %>% dplyr::filter(Comment=='Mainland')
nrow(prs_toplot) #94043

# plotting JAN to DEC using long form of data
library(reshape)
tavg_long<-melt(tavg,id=c("ID",'Polygon','Longitude','Latitude','Species'),na.rm = T)
soil_long<-melt(soil,id=c("ID",'Polygon','Longitude','Latitude','Species'),na.rm = T)

lst_nt<-prs_toplot[,c(5,25:36)]
lst_nt_long<-melt(lst_nt,id=c('Polygon'),na.rm = T)


lst_dy<-prs_toplot[,c(5,55:66)]
lst_dy_long<-melt(lst_dy,id=c('Polygon'),na.rm = T)



library(ggplot2)

ggplot(data=lst_nt_long) +
  geom_point(mapping = aes(x = variable, y = value)) +
  facet_wrap(facets = vars(Polygon), ncol = 2)+
  theme_classic()+
  scale_y_continuous(name="Temperature°C", limits=c(-30, 50))+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, color = "black")
  )

  

prs_diurnal<-mutate(prs_toplot,
                    jan_diu_temp=jan_lst_dy - jan_lst_nt,
                    feb_diu_temp=feb_lst_dy - feb_lst_nt,
                    mar_diu_temp=mar_lst_dy - mar_lst_nt,
                    apr_diu_temp=apr_lst_dy - apr_lst_nt,
                    may_diu_temp=may_lst_dy - may_lst_nt,
                    jun_diu_temp=jun_lst_dy - jun_lst_nt,
                    jul_diu_temp=jul_lst_dy - jul_lst_nt,
                    aug_diu_temp=aug_lst_dy - aug_lst_nt,
                    sep_diu_temp=sep_lst_dy - sep_lst_nt,
                    oct_diu_temp=oct_lst_dy - oct_lst_nt,
                    nov_diu_temp=nov_lst_dy - nov_lst_nt,
                    dec_diu_temp=dec_lst_dy - dec_lst_nt)
prs_diurnal<-prs_diurnal[,c(5,69:80)]
prs_diurnal_long<-melt(prs_diurnal,id=c('Polygon'),na.rm = T)

ggplot(data = prs_diurnal_long) +
  geom_point(mapping = aes(x = variable, y = value)) +
  geom_line(mapping = aes(x = variable, y = value)) +
  facet_wrap(facets = vars(Polygon), ncol = 2)+
  theme_classic()+
  scale_y_continuous(name="Temperature°C", limits=c(-30, 50))+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black")
  )

#tavg data
tavg1<- read.csv("tavg_01.csv") %>% select("ID","Longitude","Latitude","Species","Polygon","jan_tavg")
tavg2<- read.csv("tavg_02.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg3<- read.csv("tavg_03.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg4<- read.csv("tavg_04.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg5<- read.csv("tavg_05.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg6<- read.csv("tavg_06.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg7<- read.csv("tavg_07.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg8<- read.csv("tavg_08.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg9<- read.csv("tavg_09.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg10<- read.csv("tavg_10.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg11<- read.csv("tavg_11.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))
tavg12<- read.csv("tavg_12.csv") %>% select(-c("Longitude","Latitude","Species","Polygon"))

tavg<-list(tavg1,tavg2,tavg3,tavg4,tavg5,tavg6,tavg7,tavg8,tavg9,tavg10,tavg11,tavg12) %>% purrr::reduce(inner_join, by = "ID")
head(tavg)
nrow(tavg)#94226
sum(is.na(tavg$jan_tavg)) #3289

tavg<-tavg[,c(5,6:17)]
tavg_long<-melt(tavg,id=c('Polygon'),na.rm = T)

ggplot(data = tavg_long) +
  geom_point(mapping = aes(x = variable, y = value)) +
  facet_wrap(facets = vars(Polygon), ncol = 2)+
  theme_classic()+
  scale_y_continuous(name="Temperature°C", limits=c(-30, 50))+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black")
  )

#boxplots
col_scale<-c("#999999", "#E69F00", "#56B4E9","#FF7043","#CC0000","#00CC00","#0066FF","#FF33CC","#2E7D32","#AB47BC")

ggplot(data=subset(prs_toplot, !is.na(bio15)), aes(x=Polygon, y=bio15,fill=Comment)) + 
  geom_boxplot(outlier.colour="blue", outlier.shape=1,outlier.size=0.6) +
  scale_fill_manual(values=col_scale)+
  scale_y_continuous(limits = c(0,200))+
  theme_classic()

#climate data

prs_toplot$Polygon <- factor(prs_toplot$Polygon,levels = c('Polygon1','Polygon2','Polygon3','Polygon4'))

library(ggpubr)
ggplot(data=subset(prs_toplot, !is.na(bio19)), aes(x=Polygon, y=bio19)) +
  geom_boxplot()+
  stat_compare_means(method = "anova", label.y = 900) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")+
  theme_classic()# Pairwise comparison against reference(.all. refers to base mean)

# plotting prs and bg data to check the distribution of variables
fb<-read.csv('/Users/sansari/Desktop/athliana/sdm/full_data_pr_ps_thin.csv')
pb1<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon1_pr_bg.csv") 
pb2<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon2_pr_bg.csv") 
pb3<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon3_pr_bg.csv")
pb4<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon4_pr_bg.csv") 

ggplot(data=subset(fb, !is.na(bio8)), aes(x=factor(Occurence), y=bio8))+
  geom_boxplot()+
  theme( legend.position = "none" )

############## CORRELATION #############
library(dplyr)
p1<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon1_pr_ps.csv") %>%
  dplyr::filter(Occurence == 1, Polygon == "Polygon1")

p2<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon2_pr_ps.csv") %>%
  dplyr::filter(Occurence == 1, Polygon == "Polygon2")

p3<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon3_pr_ps.csv") %>%
  dplyr::filter(Occurence == 1, Polygon == "Polygon3")

p4<- read.csv("/Users/sansari/Desktop/athliana/sdm/polygon4_pr_ps.csv") %>%
  dplyr::filter(Occurence == 1, Polygon == "Polygon4")

f1<- read.csv("/Users/sansari/Desktop/athaliana_urban_nonurban/sdm/nonurb.csv") %>%
  filter(Occurence == 1)


f1df<- select(f1,c('bio2',
                   'Latitude',
                   'bio8',
                   'bio9',
                   'bio15',
                   'bio18',
                   'bio19',
                   'bdod_15',
                   'cfvo_15',
                   'clay_15',
                   'phh2o_15',
                   'silt_15',
                   'gs',
                   'elev',
                   'ai',
                   'treecover'))

f1df<- select(f1,-c('Occurence','bio1','bio3','bio5','bio6','bio7','bio12','bio13','bio14',
                    'ID','Longitude','Latitude','Species','Polygon','wind_mean','lc_type',
                    'jan_lst_nt','feb_lst_nt','mar_lst_nt','apr_lst_nt','may_lst_nt','jun_lst_nt',
                    'jul_lst_nt','aug_lst_nt','sep_lst_nt','oct_lst_nt','nov_lst_nt','dec_lst_nt',
                    'jan_lst_dy','feb_lst_dy','mar_lst_dy','apr_lst_dy','may_lst_dy','jun_lst_dy',
                    'jul_lst_dy','aug_lst_dy','sep_lst_dy','oct_lst_dy','nov_lst_dy','dec_lst_dy',"mode_lc_igbp","urban_nonurban","crop_noncrop"))

corrplot::corrplot(cor(na.omit(f1df),method = 'spearman'),method = 'number', type='lower',
                   diag=F, tl.col='black',number.cex = 0.7, title = "thaliana_corr")

# selecting variables only on the basis of presences, sp.cols argument can be used to have the
# response variable indicating presences and absences
set.seed(1)
colnames(f1df)
f1df<- na.omit(f1df)
p2df<- na.omit(p2df)
p3df<- na.omit(p3df)
p4df<- na.omit(p4df)

vars_selection<-fuzzySim::corSelect(f1df,var.cols = 1:27, cor.thresh=0.75,use="pairwise.complete.obs", method='spearman')



