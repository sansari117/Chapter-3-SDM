setwd('/u/sansari/sdm_thaliana')
library(biomod2)
library(terra)
terraOptions(steps=10,verbose=T)
# Load species occurrences
d<- read.csv("tha_herb_home_final_comments_thinned.csv")

d$Comment <- dplyr::na_if(d$Comment, '')
d<-dplyr:: filter(d,if_any(Comment, is.na))

write.csv(d,"tha_herb_home_final_comments_thinned_nooutliers.csv", row.names = F, quote = F)
d$Occurence<-1
e<-read.csv("tha_points_near0.02degofalp.csv")
e$within2km<- "yes"
e<- e[c(5,6,7)] #1980

s <- merge(d, e, by = c("Latitude","Longitude"), all.x = T)
p<-dplyr:: filter(s,if_any(within2km, is.na))
# Select the name of the studied species
myRespName <- 'arabidopsis'

# Get corresponding presence/absence data
myResp <- as.numeric(d[,"Occurence"])

# Get corresponding XY coordinates
myRespXY <- d[, c('Longitude', 'Latitude')]

# Load environmental variables
files<- c("/u/sansari/sdm_vars_sc/clipped/cl_bio_8.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_bio_9.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_bio_13.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_bio_14.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_bio_18.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_bdod.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_phh2o.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_gs.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_ai.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_nightlst_max.tif",
          "/u/sansari/sdm_vars_sc/clipped/cl_ndvi_mean.tif")

myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData.r,
  modeling.id = 'Arabidopsis_Models',
  models = c("ANN"),
  CV.strategy = 'user.defined',
  CV.user.table = cv.s,
  OPT.data.type = "binary",
  OPT.strategy = 'user.defined',
  OPT.user.val = list(size=6),
  var.import = 10,
  metric.eval = c('TSS', 'ROC', 'KAPPA', 'BOYCE'),
  seed.val = 100,
  nb.cpu = 20
)

user.val <- list(
  'GBM.binary.gbm.gbm' = list(
    '_allData_RUN1' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN2' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN3' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN4' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN5' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN6' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN7' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN8' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN9' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN10' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN11' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN12' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN13' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN14' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN15' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN16' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN17' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN18' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN19' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5),
    '_allData_RUN20' = list(n.trees = 1000, shrinkage = 0.01, cv.folds = 10, n.cores =5)
  )
)

user.val <- list(
  'RF.binary.randomForest.randomForest' = list(
    '_allData_RUN1' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN2' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN3' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN4' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN5' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN6' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN7' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN8' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN9' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN10' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN11' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN12' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN13' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN14' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN15' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN16' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN17' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN18' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN19' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN20' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN21' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN22' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN23' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN24' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN25' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN26' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN27' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN28' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN29' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5),
    '_allData_RUN30' = list(ntree = 1000, mtry = NULL, strata = factor(c(0, 1)), sampsize = NULL, nodesize = 5)))
    
    
###################     BIOMOD RESULT ANALYSIS   TOPO AND SOIL  ##############################

# to analyze the results, we have to set the same directory as of .RData output
setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_247374_buf2degdiff_toponew/toponew/')

library(biomod2)
library(dplyr)

myBiomodModelOut<-get(load('myBiomodModelOut_'))
get_built_models(myBiomodModelOut, full.name = NULL, PA = NULL, run = NULL, algo = NULL)
myBiomodModelOut
get_options(myBiomodModelOut)

###################         TOPO AND SOIL  ##############################

# to analyze the results, we have to set the same directory as of .RData output
setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm_arabis/sdm_38475_buf2degdiff_toponew/toponew/')

library(biomod2)
library(dplyr)

myBiomodModelOut<-get(load('myBiomodModelOut_alp_toponew_38475_buf2degdiff.RData'))
get_built_models(myBiomodModelOut, full.name = NULL, PA = NULL, run = NULL, algo = NULL)
myBiomodModelOut
get_options(myBiomodModelOut)


# VARIABLE IMPORTANCE
imp_vars<-get_variables_importance( myBiomodModelOut) 
# "XGBOOST","FDA","ANN","GBM","GLM","GAM"

imp_vars$var.imp<-imp_vars$var.imp *100

library(ggplot2)

p<-ggplot(imp_vars, aes(x = expl.var, y = var.imp)) +
  geom_boxplot(width = 0.55) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian(ylim = c(0, 100)) +   # zoom without dropping
  
  # for topo
  scale_x_discrete(labels = c("elev" = "elevation", "TPI" = "position index","TRI" = "ruggedness index")) +
  
  # for soil
  #scale_x_discrete(labels = c("cfvo15" = "coarse fragments", "clay15" = "clay","nitrogen15" = "nitrogen","phh2o15" = "ph","silt15" = "silt")) +
  
  labs(x = " ", y = "Importance (%)") +
  theme_classic(base_size = 9) +
  theme( 
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.7),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4),size = 8),
    axis.text.y  = element_text(size = 8),
    
  )


ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_topo_thaliana.pdf",plot = p, width = 120, height = 100, units = "mm", device = cairo_pdf)
ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_soil_thaliana.pdf",plot = p, width = 100, height = 100, units = "mm", device = cairo_pdf)



metrics<-get_evaluations(myBiomodModelOut)


tha_soil_metrics<- get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  mutate(species = "thaliana")

tha_topo_metrics<- get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  mutate(species = "thaliana")



######################################      BIOMOD RESULT ANALYSIS     ####################################################    
    
    
# to analyze the results, we have to set the same directory as of .RData output
library(biomod2)
setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_247374_buf2degdiff_topo//')
myBiomodModelOut<-get(load('myBiomodModelOut_topo_247374_buf2degdiff.RData'))
get_built_models(myBiomodModelOut, full.name = NULL, PA = NULL, run = NULL, algo = NULL)
myBiomodModelOut
get_options(myBiomodModelOut)

# variable importance
imp_vars<-get_variables_importance( myBiomodModelOut) 
# "XGBOOST","RF","FDA","ANN","GBM","GLM","GAM"

imp_vars$var.imp<- imp_vars$var.imp *100


imp_vars<- imp_vars %>% filter(algo != 'ANN')

ggplot(imp_vars, aes(x = expl.var, y = var.imp)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20))+
  theme() +
  labs(title = 'all_algos_runs',
       x= '',
       y='')+
  theme(plot.title = element_text(size = 10, hjust = 0),
        axis.text.x = element_text(angle = 35, hjust =1,
                                   color="black", 
                                   size=9),
        axis.text.y = element_text(color="black", 
                                   size=9))

p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(ncol = 3)


metrics<-get_evaluations(myBiomodModelOut) %>% filter(metric.eval=='TSS')

tha_soil_metrics<- get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  mutate(species = "thaliana")

tha_topo_metrics<- get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval %in% c("TSS", "ROC")) %>%
  mutate(species = "thaliana")


library(dplyr)
metrics %>% filter(metric.eval=='TSS'& algo %in% c("XGBOOST","RF","FDA","ANN","GBM","GLM","GAM")) %>%
  ggplot(aes(x = algo, y = validation)) +
  geom_boxplot() +
  theme()+
  scale_y_continuous(limits = c(0, 1),breaks=seq(0,1,0.1)) +
  labs(title = 'TSS',
       x= '',
       y='Validation')+
  theme(plot.title = element_text(size = 10, hjust = 0),
        axis.text.x = element_text(angle = 35, hjust =1,
                                   color="black", 
                                   size=9),
        axis.text.y = element_text(color="black", 
                                   size=9))

tha_metrics<-get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval=='TSS') %>%
  select(full.name,algo,validation)


# get the metrics from each algo and then merge them by row
setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_247374_buf2degdiff_extreme/glm/')
myBiomodModelOut<-get(load('myBiomodModelOut247374_extreme_buf2degdiff.RData'))

metrics1<-get_evaluations(myBiomodModelOut)
metrics2<-get_evaluations(myBiomodModelOut)
metrics3<-get_evaluations(myBiomodModelOut)
metrics4<-get_evaluations(myBiomodModelOut)
metrics5<-get_evaluations(myBiomodModelOut)
metrics6<-get_evaluations(myBiomodModelOut)
metrics7<-get_evaluations(myBiomodModelOut)
                          
metrics<-rbind(metrics1,metrics2,metrics3,metrics4,metrics5)
library(dplyr)
library(ggplot2)

metrics %>% filter(metric.eval=='ROC') %>%
  ggplot(aes(x = algo, y = validation)) +
  geom_boxplot() +
  theme_classic()+
  scale_y_continuous(limits = c(0, 1),breaks=seq(0,1,0.1)) +
  labs(title = 'ROC',
       x= '',
       y='Validation')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10))

################################ response curves

object <- get(load(paste0('myBiomod')))

response <- bm_PlotResponseCurves(
  object,
  models.chosen = 'all', # all algo , all runs , all replicates
  fixed.var = "median")

response_tab <- response$tab
write.csv(response_tab, file = paste0('/u/sansari/sdm_thaliana/vif_response.csv'), row.names = FALSE, quote = F)



####################################      ENSEMBLES         ##################################################

# to analyze the results, we have to set the same directory as of .RData output
library(biomod2)

path<- '/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_247374_buf2degdiff_econew/eco'

for (i in 1:15){
  object<- paste0(path,i,'/myBiomodEM_eco',i,'_247374_buf2degdiff.RData')
  assign(paste0('eco', i), get(load(object)))
}

#########  VARIABLE IMPORTANCE

combined_imp_vars <- data.frame()

for (i in 1:15) {
  # Dynamically refer to eco1, eco2, ..., eco15
  eco_variable <- get(paste0("eco", i))
  
  # Extract variable importance
  imp_vars <- get_variables_importance(eco_variable, algo = "EMmean")
  
  # Add an identifier column for the eco dataset
  imp_vars$eco_id <- paste0("eco", i)
  
  # Combine results using rbind
  combined_imp_vars <- rbind(combined_imp_vars, imp_vars)
}

# View the combined results
head(combined_imp_vars[,c(7,9,10)])
unique(combined_imp_vars$rand)

combined_imp_vars$var.imp<-combined_imp_vars$var.imp *100

library(dplyr)
result<- combined_imp_vars %>% 
  group_by(expl.var, eco_id) %>% 
  summarise(mean.var.imp = mean(var.imp, na.rm = TRUE), .groups = 'drop')

ggplot(result, aes(x = expl.var, y = mean.var.imp)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, colour='blue') +
  theme()+
  scale_y_continuous(limits = c(0, 100),breaks=seq(0,100,20))+
  labs(
    title = "",
    x = "",
    y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold", color="black", 
                                   size = 10),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 10))


#library(patchwork)
#p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol = 3)


############
############    ENSEMBLE METRICS

# Initialize an empty data frame to store the combined results
combined_metrics <- data.frame()

for (i in 1:15) {
  # Dynamically refer to eco1, eco2, ..., eco14
  eco_variable <- get(paste0("eco", i))
  
  # Extract variable importance
  metrics <- get_evaluations(eco_variable)
  
  # Add an identifier column for the eco dataset
  metrics$eco_id <- paste0("eco", i)
  
  # Combine results using rbind
  combined_metrics <- rbind(combined_metrics, metrics)
}

# specify the order
#combined_metrics$eco_id <- factor(
#  combined_metrics$eco_id,
#  levels = c("eco1","eco2","eco3","eco4","eco5",
#             "eco6","eco7","eco8","eco9","eco10","eco11","eco12","eco13","eco14","eco15"))

# check the data
head(combined_metrics)

# to plot the boxplot of all models -> alp_tha.R
tha_metrics<-combined_metrics%>%
  filter(metric.eval %in% c("TSS","ROC"),
         algo == "EMmean") %>%
  mutate(species = "thaliana")


# (Mean of probabilities over the selected models)
# ggplot(combined_metrics %>% filter(metric.eval == "ROC" & algo == "EMmean"),
#        aes(x = eco_id, y = calibration)) +
#   geom_point(size=4, color="darkblue") +
#   theme()+
#   scale_y_continuous(limits = c(0, 1),breaks=seq(0,1,0.1)) +
#   labs(title = "TSS Scores based on EMmean: A. thaliana", 
#        x = "eco ID",
#        y = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,
#                                    face = "bold", color="black", 
#                                    size = 10),
#         axis.text.y = element_text(face = "bold", color="black", 
#                                    size = 10))

#############   RESPONSE CURVES

library(biomod2)

for (i in (1:9)){
  eco_name<- paste0('eco',i)
  setwd(paste0('/u/sansari/sdm_thaliana/eco/',eco_name))
  eco_object <- get(load(paste0('myBiomodEM_',eco_name,'_247374_buf2degdiff.RData')))
  response <- bm_PlotResponseCurves(
    eco_object,
    models.chosen = "arabidopsis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo",
    fixed.var = "median")
  response_tab <- response$tab
  write.csv(response_tab, file = paste0('/u/sansari/sdm_thaliana/eco/response_result_eco/',eco_name,'_response.csv'), row.names = FALSE, quote = F)
  }


############# Now visualization of response curve data


# Define the path to the folder containing the CSV files
input_dir <- '/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_247374_buf2degdiff_econew/response_result/'

# Initialize an empty data frame to store all data
combined_response <- data.frame()

# Loop through each file (eco1 to eco15)
for (i in 1:9) {
  # Construct the file path
  file_path <- file.path(input_dir, paste0("eco", i, "_response.csv"))
  
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Add a column to identify the eco object
  df$eco <- paste0("eco", i)
  
  # Combine into the main data frame
  combined_response <- rbind(combined_response, df)
}

str(combined_response)
# Filter data for the variable and Create the plot

library(ggplot2)
library(dplyr)

ggplot(combined_response %>% filter(expl.name =='nightlst_min'),
       aes(x = expl.val, y = pred.val, color = eco)) +
  geom_line(linewidth=1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))+
  guides(color = guide_legend(title = "Ensemble\nmodel")) +
  theme_minimal()+
  labs(title = "A. thaliana", 
       x = "ndvi_mean",
       y = "") +
  theme(axis.text.x = element_text(hjust = 1,
                                   face = "bold", color="black", 
                                   size = 9),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 9))




#######################.  ensembles 

setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm/sdm_4vars_2_5/')
myBiomodEM<-get(load('myBiomodEM_57379_ps_4vars_2_5res.RData'))

myBiomodEMProjFut<-get(load('myBiomodEMProjFut_57379_ps_4vars_2_5res.RData'))
myBiomodEMProjLGM<-get(load('myBiomodEMProjLGM_57379_ps_4vars_2_5res.RData'))

bm_PlotEvalBoxplot(bm.out = myBiomodEMProjFut, group.by = c('full.name', 'full.name'))



