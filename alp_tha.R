library(dplyr)
alp<-read.csv('/Users/sansari/Desktop/arabis/extraction/alp_herb_final_vars.csv')
tha<-read.csv('/Users/sansari/Desktop/athliana/sdm_files/extraction/tha_herb_home_final_vars.csv') %>% dplyr::select(-Comment)

analysisData<-rbind(alp,tha)
colnames(analysisData)
# check if there are differences in values after resampling the rasters
# differing_indices <- alp$lc_igbp != alp$lc_igbp_res
# sum(differing_indices) #2870 in alpina
# differing_indices <- tha$lc_igbp != tha$lc_igbp_res
# sum(differing_indices) #20679 in thaliana

##################################        PCA           ####################

toplot_pca<-analysisData[,-c(1,2,3,4,5,56,57)]

colnames(toplot_pca)

#rownames(toplot_pca) <- toplot_pca[,1]
#toplot_pca[,1] <- NULL
#toplot_pca_urb<-toplot_pca %>% filter(urban_nonurban==1)
#toplot_pca_nonurb<-toplot_pca %>% filter(urban_nonurban==0)

#toplot_pca<-toplot_pca[!grepl('relict', toplot_pca$Admixture_Group),]
library(dplyr)
toplot_pca<-toplot_pca%>% na.omit() #72908
res.pca <- prcomp(toplot_pca, scale = TRUE)

library(factoextra)
fviz_pca_var(res.pca, col.var = "black",repel = T)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#groups <- as.factor(toplot_pca[1])
fviz_pca_ind(res.pca,
             geom=c("arrow", "text"),
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 1:6)


############################           PLOTTING #################################
library(ggplot2)

tha_landcover_prop<- tha %>% 
  tidyr::drop_na('lc_igbp_res') %>% 
  count(lc_igbp_res)  %>% 
  mutate(prop = n / sum(n) *100) %>% 
  mutate(Species = 'Arabidopsis thaliana')

  
alp_landcover_prop<- alp  %>% tidyr::drop_na('lc_igbp_res') %>% 
  count(lc_igbp_res)  %>% 
  mutate(prop = n / sum(n) *100) %>%
  mutate(Species = 'Arabis alpina')

landcover <- rbind(tha_landcover_prop, alp_landcover_prop) %>% filter(!(lc_igbp_res %in% c(2, 3, 6)))

ggplot(landcover,aes(x = factor(lc_igbp_res), y = prop, fill = Species))+
  geom_bar(stat = "identity", position = 'stack') +
  scale_x_discrete(labels=land_cover_names) +
  scale_y_continuous(limits = c(0, 70),breaks=seq(0,70,20))+
  scale_fill_manual(values= c('skyblue','green'))+
  theme_minimal()+
  labs(x='',y='Proportion')+
  #theme(legend.position = 'none')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold", color="black", 
                                   size = 9, margin = margin(t = -9)),
        axis.text.y = element_text(color="black", 
                                   size = 10),
        axis.title.y = element_text(face = "bold", color="black", 
                                   size = 10))
  
land_cover_names<- c(
  "1" = "Everg. needleleaf",
  "2" = "Evergreen Broadleaf Forests", #removed
  "3" = "Deciduous Needleleaf Forests", # removed
  "4" = "Decid. broadleaf",
  "5" = "Mixed forests",
  "6" = "Closed shrublands", #removed
  "7" = "Open shrublands",
  "8" = "Woody savannas ",
  "9" = "Savannas",
  "10" = "Grasslands",
  "11" = "Permanent wetlands",
  "12" = "Croplands",
  "13" = "Urban and built-up lands",
  "14" = "Cropland/Natural Vegetation", #Mosaics
  "15" = "Permanent snow and ice",
  "16" = "Barren",
  "17" = "Water bodies",
  "255" = "Unclassified"
)

# with elevation
grassland <- analysisData %>% filter((lc_igbp_res %in% c(10)))

# Plot elevation for Grassland landcover
ggplot(analysisData, aes(x = Species, y = elev, fill = Species)) +
  geom_boxplot() +
  scale_x_discrete(labels=land_cover_names) +
  scale_y_continuous(limits = c(-15, 5000),breaks=seq(0,5000,2000))+
  scale_fill_manual(values= c('skyblue','pink'))+
  theme_minimal()+
  labs(x='',y='')+
  #theme(legend.position = 'none')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold", color="black", 
                                   size = 9, margin = margin(t = -9)),
        axis.text.y = element_text(color="black", 
                                   size = 10),
        axis.title.y = element_text(face = "bold", color="black", 
                                    size = 10))

######################    WHEN one variable and grouped by 2 species ####################

var_names<-analysisData %>% select(!c(ID:Polygon)) %>% colnames()

# Helper function
plot_func <- function(df, y) {
  ggplot(df %>% tidyr::drop_na(!! sym(y)),
         aes(x =factor(Species), y= !! sym(y),fill = factor(Species))) +       #group=factor(Species) # for density
    #geom_bar(stat = 'count') +                                         # for boxplot
    #geom_density(adjust=1.5,alpha=0.4) +
    #geom_violin()+
    geom_boxplot() + geom_jitter(alpha=0.25, colour='skyblue') +
    #scale_x_discrete(labels=land_cover_names) +
    theme_minimal()+
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +        # set the angle of x axis labels
    theme(legend.position = "none") +                                 # drop the legend
    #ylim(-28,6000)+                                                   # set limit of y axis
    labs(x = " ",
      #y = " ",
      fill = "Species",
      title = " ")
}

# Main loop through the columns and dataset
for(varR in var_names){
  name <- paste0(varR, "_species")
  png(paste0(name, ".png"),res = 100)
  print(plot_func(df=analysisData, y=varR))
  dev.off()
}



#######################################      KS TEST #########################################

var_names<-tha %>% select(!c(ID:Polygon)) %>% colnames()
for (col_name in var_names) {
  alp_1<-alp%>% select(col_name) %>% tidyr::drop_na()
  tha_1<-tha%>% select(col_name) %>% tidyr::drop_na()
  png(paste0(col_name, ".png"),res = 100)
  plot(ecdf(alp_1[[col_name]]), # add line for alpina
     xlim = range(c(alp_1[[col_name]], tha_1[[col_name]])), 
     col = "blue",
     lwd = 2,
     main = paste("CDF Plot for", col_name),
     ylab = 'Cumulative Probability',
     xlab = col_name)
  plot(ecdf(tha_1[[col_name]]), # add line for thaliana
     add = TRUE,
     col = "red",
     lwd = 1)
  # Add legend to the plot
  legend("bottomright", 
         legend = c("alp", "tha"), 
         col = c("blue", "red"), 
         lty = c(1, 1))
  ks_result <- ks.test(alp_1[[col_name]], tha_1[[col_name]])
  # Add KS test result to the plot
  ks_text <- paste("KS Test: p-value =", format.pval(ks_result$p.value), 
                   "\nD-statistic =", format(ks_result$statistic, digits = 3))
  text(x = mean(range(c(alp_1[[col_name]], tha_1[[col_name]]))), 
       y = 0.5, 
       labels = ks_text, 
       cex = 0.8, 
       col = "black", 
       adj = 0)
  dev.off()
}

#################            PLOTTING SOIL DATA         #######################
colnames(analysisData)

# Example: Histograms for the 2 species
ggplot(analysisData, aes(x = elev, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +  # Position "identity" keeps both histograms
  labs(title = "",
       x = "Elevation", # cm³/dm³
       y = "") +
  scale_fill_manual(values = c("skyblue", "red")) +  # Custom colors for the species
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 13, face = "bold"),  # Increase x-axis label size and make it bold
    #axis.title.y = element_text(size = 14, face = "bold"),  # Increase y-axis label size and make it bold
    axis.text.x = element_text(size = 11, color = "black"), # Increase x-axis ticks size and set color
    axis.text.y = element_text(size = 11, color = "black")  # Increase y-axis ticks size and set color
  )

# Example: 
ggplot(analysisData %>% filter(Species == "Arabis alpina"), aes(x = bio_9, y= bio_17,color =Species)) +
  geom_point(alpha=0.3, size =3) + 
  scale_color_manual(values = c("blue", "orange")) +
  labs(title = "",
       x = "", # cm³/dm³
       y = "") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 13, face = "bold"),  # Increase x-axis label size and make it bold
    #axis.title.y = element_text(size = 14, face = "bold"),  # Increase y-axis label size and make it bold
    axis.text.x = element_text(size = 11, color = "black"), # Increase x-axis ticks size and set color
    axis.text.y = element_text(size = 11, color = "black")  # Increase y-axis ticks size and set color
  )

##############################        SELECTING COMMON FILES IN 2 FOLDERS     ##################################


# Define the paths to the two folders
folder1 <- "/Users/sansari/Desktop/sdm_thaliana/correlation_matrices_eco/"
folder2 <- "/Users/sansari/Desktop/sdm_alpina/correlation_matrices_eco/"

# List all files in both folders
files_folder1 <- list.files(folder1)
files_folder2 <- list.files(folder2)

# Find the common files
common_files <- intersect(files_folder1, files_folder2)

# Print the common files
cat("Number of common files:", length(common_files), "\n")
print(common_files)

#####################################################################

## common vars between alpina and thalaana after VIF

#bio_2
#bio_8
#bio_9
#bio_15
#bio_18
#bio_19
#treecover
#wind_mean
#srad_mean
#daylst_max
#ndvi_min
#ndvi_max

###########################################        BIOMOD RESULTS       #############################################

# emsemble model plots for thaliana and alpina
head(alp_metrics)
head(tha_metrics)

metrics_result<-rbind(alp_metrics, tha_metrics)

# Check normality for A. thaliana
shapiro.test(metrics_result$calibration[metrics_result$full.name == "arabidopsis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo"])

# Check normality for A. alpina
shapiro.test(metrics_result$calibration[metrics_result$full.name == "arabis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo"])

#If the p-value from the Shapiro-Wilk test is < 0.05, the data is not normally distributed.
# If the data is normal we can proceed with t-test
t_test<-t.test(calibration ~ full.name, data = metrics_result, paired=T)
p_value <- t_test$p.value

# If the data is NOT normal we can proceed with wilcox test
wilcox_test<-wilcox.test(calibration ~ full.name, data = metrics_result, paired=T)
p_value <- wilcox_test$p.value

# now plot with p value from t test

ggplot(metrics_result, aes(x = full.name, y = calibration)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, colour='blue') +
  theme_minimal()+
  scale_x_discrete(labels = c("arabis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo" = "A. alpina", "arabidopsis_EMmeanByTSS_mergedData_mergedRun_mergedAlgo" = "A. thaliana")) +
  scale_y_continuous(limits = c(0.5, 1),breaks=seq(0.5,1,0.25)) +
  labs(
    title = "",
    x = "",
    y = "") +
  theme(axis.text.x = element_text(hjust = 1,
                                   face = "bold", color="black", 
                                   size = 10),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 10))+
  annotate("text", x = 1.5, y = 0.7, label = paste0("Paired wilcox-test,\np = ", round(p_value, 4)), size = 4)


# topo and soil
head(alp_metrics)
head(tha_metrics)

alp_metrics$Species<-"A. alpina"
tha_metrics$Species<-"A. thaliana"

metrics_result<-rbind(tha_metrics,alp_metrics)
metrics_result$Species <- factor(metrics_result$Species, levels = c("A. thaliana", "A. alpina"))

# Check normality for A. thaliana
shapiro.test(metrics_result$validation[metrics_result$Species == "A. alpina"])

#

# If the data is NOT normal we can proceed with wilcox test
wilcox_test<-wilcox.test(validation ~ Species, data = metrics_result, paired=T)
p_value <- wilcox_test$p.value

mean_vals <- metrics_result %>%
  group_by(Species, algo) %>%
  summarise(mean_validation = mean(validation), .groups = "drop")




ggplot(mean_vals, aes(x = Species, y = mean_validation)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, colour='blue') +
  theme_minimal()+
  scale_y_continuous(limits = c(0.5, 1),breaks=seq(0.5,1,0.25)) +
  labs(
    title = "",
    x = "",
    y = "") +
  theme(axis.text.x = element_text(hjust = 1,
                                   face = "bold", color="black", 
                                   size = 10),
        axis.text.y = element_text(face = "bold", color="black", 
                                   size = 10))

##########################################################################################################################
##########################################################################################################################




