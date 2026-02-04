setwd('/u/sansari/sdm_alpina')
library(biomod2)
library(terra)
terraOptions(verbose=T, memfrac=0.2)

# Load species occurrences
d<- read.csv("alp_herb_final_thinned.csv")
d$Occurence<-1
print(nrow(d))

#########################    TO REMOVE POINTS WHICH ARE CLOSE TO ARABIDOPSIS THALIANA  ###############

# Read the alpina points to remove
#e<-read.csv("alp_points_near0.02degoftha.csv")

#e$within2km<- "yes"

#e<- e[c(4,5,6)] #1929

#d <- merge(df, e, by = c("Latitude","Longitude"), all.x = T)

#d<-dplyr:: filter(d,if_any(within2km, is.na))

#d<- dplyr::select(d,-c("within2km"))

#print(head(d))

######################################################################################################

# Read the absence records
p<- read.csv("arabis_ps4_38475_buf2degdiff_records.csv")

p$Occurence<-0

dp<-rbind(d,p)

# Select the name of the studied species
myRespName <- 'arabis'

# Get corresponding presence/absence data
myResp <- as.numeric(dp[,"Occurence"])

# Get corresponding XY coordinates
myRespXY <- dp[, c('Longitude', 'Latitude')]

var_combinations <- list(
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_gs.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_4.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_4.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_ndvi_mean.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_4.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_ndvi_mean.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_4.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_17.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_11.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_11.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_17.tif", "/u/sansari/sdm_vars_sc/clipped/cl_nightlst_min.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_nightlst_min.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_11.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_16.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_nightlst_min.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_16.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_ndvi_mean.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"),
  c("/u/sansari/sdm_vars_sc/clipped/cl_daylst_max.tif", "/u/sansari/sdm_vars_sc/clipped/cl_bio_19.tif", "/u/sansari/sdm_vars_sc/clipped/cl_ndvi_mean.tif","/u/sansari/sdm_vars_sc/clipped/cl_bio_15.tif"))


# Loop through each combination
for (i in seq_along(var_combinations)) {
  # Load the environmental variables for the current combination
  files <- var_combinations[[i]]
  myExpl <- rast(files)

  # Format data with pseudo-absences
  myBiomodData.r <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = myExpl,
    resp.xy = myRespXY,
    resp.name = myRespName
  )

  # Save the output with a unique file name
  save(
    myBiomodData.r,
    file = paste0("myBiomodData_alp_eco", i, "_38475_buf2degdiff.RData")
  )

  # Optional: Print progress
  cat("Completed combination", i, "out of", length(var_combinations), "\n")
}

cat("All combinations processed successfully!\n")
