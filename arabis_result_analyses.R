
###################         RESULT ANALYSES  ##############################

# to analyze the results, we have to set the same directory as of .RData output
library(biomod2)
setwd('/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm_arabis/sdm_38475_buf2degdiff_soil//')
myBiomodModelOut<-get(load('myBiomodModelOut_alp_soil_38475_buf2degdiff.RData'))
get_built_models(myBiomodModelOut, full.name = NULL, PA = NULL, run = NULL, algo = NULL)
myBiomodModelOut
get_options(myBiomodModelOut)

imp_vars<-get_variables_importance( myBiomodModelOut) 
# "XGBOOST","RF","FDA","ANN","GBM","GLM","GAM"

imp_vars$var.imp<-imp_vars$var.imp *100

imp_vars<- imp_vars %>% filter(algo != 'ANN')
# VARIABLE IMPORTANCE
#library(ggplot2)
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


metrics<-get_evaluations(myBiomodModelOut)

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

alp_metrics<- get_evaluations(myBiomodModelOut)%>%
  filter(metric.eval=='TSS') %>%
  select(full.name,algo,validation)

library(dplyr)
metrics %>% filter(algo =='XGBOOST'& metric.eval %in% c("ACCURACY","BOYCE","KAPPA","ROC","TSS") ) %>%
  ggplot(aes(x = metric.eval, y = validation)) +
  geom_boxplot() +
  theme_classic()+
  ylim(0,1)+
  scale_y_continuous(limits = c(0, 0.1),breaks=seq(0,0.25,0.50,0.75,0.1))
  labs(title = 'XGBOOST',
       x= '',
       y='')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10))

# response curves
# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                        models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                        fixed.var = 'median')
                      
eco_object <- get(load(paste0('myBiomodEM_',eco_name,'_38475_buf2degdiff.RData')))
response <- bm_PlotResponseCurves(
  eco_object,
  models.chosen = "arabis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo",
  fixed.var = "median")

response_tab <- response$tab
write.csv(response_tab, file = paste0('/u/sansari/sdm_/eco/response_result_eco/',eco_name,'_response.csv'), row.names = FALSE, quote = F)

ggplot(data=response$tab,aes(x = expl.val, y = pred.val,color = pred.name)) +
  geom_line() +
  theme_classic()+
  #ylim(0.0,0.1)+
  scale_y_continuous(
    limits = c(0,1),        
    breaks = seq(0, 1, 0.25))+
  #labs(title = 'XGBOOST',
  #     x= '',
   #    y='')+
  theme(legend.position = "none")
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10))

  
####################################      ENSEMBLES         ##################################################
  
# to analyze the results, we have to set the same directory as of .RData output
library(biomod2)
  
#########  GET ALL THE CLIMATE OBJECTS
  
path<- '/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm_arabis/sdm_38475_buf2degdiff_econew/eco'

for (i in 1:15){
  object<- paste0(path,i,'/myBiomodEM_eco',i,'_38475_buf2degdiff.RData')
  assign(paste0('eco', i), get(load(object)))
}


#####################################   VARIABLE IMPORTANCE        ############################################

combined_imp_vars <- data.frame()

for (i in 1:15) {
  # Dynamically refer to eco1, eco2, ..., eco14
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
unique(combined_imp_vars$eco_id)

combined_imp_vars$var.imp<-combined_imp_vars$var.imp *100

# Group by eco_id and expl.var, then summarize
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

library(patchwork)
# Specify layout with plot_layout
#p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol = 3)



######################          ENSEMBLE METRICS         ################### 

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
combined_metrics$eco_id <- factor(
  combined_metrics$eco_id,
  levels = c("eco1","eco2","eco3","eco4","eco5",
             "eco6","eco7","eco8","eco9","eco10","eco11","eco12","eco13","eco14","eco15"))
# check the data
head(combined_metrics)


# to plot the boxplot of all models -> alp_tha.R
alp_metrics<-combined_metrics %>% filter(metric.eval == "TSS" & algo == "EMmean")


# EMmean: Mean of probabilities over the selected models
ggplot(combined_metrics %>% filter(metric.eval == "TSS" & algo == "EMmean"),
       aes(x = eco_id, y = calibration)) +
  geom_point(size=4, color="darkblue") +
  theme()+
  scale_y_continuous(limits = c(0, 1),breaks=seq(0,1,0.1)) +
  labs(title = "TSS Scores based on EMmean: A. alpina", 
    x = "eco ID",
    y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold", color="black", 
                                   size = 10),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 10))





#########################       RESPONSE CURVES           #########################

setwd('/u/sansari/sdm_thaliana/')

for (i in (1:9)){
  eco_name<- paste0('eco',i)
  setwd(paste0('/u/sansari/sdm_alpina/eco/',eco_name))
  eco_object <- get(load(paste0('myBiomodEM_',eco_name,'_38475_buf2degdiff.RData')))
  response <- bm_PlotResponseCurves(
    eco_object,
    models.chosen = "arabis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo",
    fixed.var = "median")
  response_tab <- response$tab
  write.csv(response_tab, file = paste0('/u/sansari/sdm_alpina/eco/response_result_eco/',eco_name,'_response.csv'), row.names = FALSE, quote = F)
}

### visualization

# Define the path to the folder containing the CSV files
input_dir <- '/Users/sansari/MPIPZ/netscratch-1/irg/grp_hancock/Shifa/sdm_arabis/sdm_38475_buf2degdiff_eco/response_result_eco'

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
ggplot(combined_response %>% filter(expl.name =='daylst_max'),
       aes(x = expl.val, y = pred.val, color = eco)) +
  geom_line(linewidth=1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))+
  guides(color = guide_legend(title = "Ensemble\nmodel")) +
  theme_minimal()+
  labs(title = "A. alpina", 
       x = "bio_14 (mm)",
       y = "") +
  theme(axis.text.x = element_text(hjust = 1,
                                   face = "bold", color="black", 
                                   size = 9),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 9))















