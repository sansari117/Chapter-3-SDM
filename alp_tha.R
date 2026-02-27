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

###########################################        CLIMATE METRICS       #############################################

# emsemble model plots for thaliana and alpina
head(alp_eco_metrics)
head(tha_eco_metrics)

metrics_eco_result<-rbind(alp_eco_metrics, tha_eco_metrics)

head(alp_extreme_metrics)
head(tha_extreme_metrics)

metrics_extreme_result<-rbind(alp_extreme_metrics, tha_extreme_metrics)


library(dplyr)

regular_tss <- metrics_eco_result %>%
  filter(metric.eval == "TSS") %>%
  group_by(species, id) %>%
  summarise(TSS = mean(calibration), .groups = "drop")

extreme_tss <- metrics_extreme_result %>%
  filter(metric.eval == "TSS") %>%
  group_by(species, id) %>%
  summarise(TSS = mean(calibration), .groups = "drop")


library(tidyr)

# paired test Regular (n = 15)
regular_wide <- regular_tss %>%
  pivot_wider(names_from = species, values_from = TSS)

wilcox.test(regular_wide$alpina,
            regular_wide$thaliana,
            paired = TRUE)

# paired test Extreme (n = 9)
extreme_wide <- extreme_tss %>%
  pivot_wider(names_from = species, values_from = TSS)

wilcox.test(extreme_wide$alpina,
            extreme_wide$thaliana,
            paired = TRUE)

#Plot
library(dplyr)
library(ggplot2)

# Rename datasets
regular_tss2 <- regular_tss %>% mutate(dataset = "Seasonal-climate")
extreme_tss2 <- extreme_tss %>% mutate(dataset = "Extreme-climate")

combined <- bind_rows(regular_tss2, extreme_tss2)

# Order
combined$species <- factor(combined$species, levels = c("alpina", "thaliana"))
combined$dataset <- factor(combined$dataset,
                           levels = c("Seasonal-climate", "Extreme-climate"))

p<-ggplot(combined, aes(x = species, y = TSS, group = id)) +
  
  # paired lines
  geom_line(aes(color = species), alpha = 0.35, linewidth = 0.6) +
  
  # points
  geom_point(aes(fill = species, color = species),
             shape = 21, size = 2, stroke = 0.4) +
  
  facet_wrap(~ dataset, nrow = 1) +
  coord_cartesian(ylim = c(0.5, 1)) +
  
  # Italic species names
  scale_x_discrete(labels = c(
    "alpina"   = expression(italic("A.")~italic("alpina")),
    "thaliana" = expression(italic("A.")~italic("thaliana"))
  )) +
  
  # Consistent colours
  scale_fill_manual(values = c(
    "alpina"   = "#2C7BB6",
    "thaliana" = "#D95F02"
  )) +
  scale_color_manual(values = c(
    "alpina"   = "#2166AC",
    "thaliana" = "#D95F02"
  )) +
  
  labs(x = " ", y = "TSS") +
  
  theme_classic(base_size = 9) +
  theme(
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4), size = 8),
    axis.text.y  = element_text(size = 8),
    
    legend.position = "none"
  )

ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/tss_seasonal_extreme.pdf",plot = p, width = 80, height = 80, units = "mm", device = cairo_pdf)


# SUMMARY
combined %>%
  group_by(dataset, species) %>%
  summarise(
    mean_TSS = mean(TSS),
    sd_TSS   = sd(TSS),
    n        = n(),
    .groups = "drop"
  )

summary_stats


##################################      CLIMATE VARIMP- SEASONAL         ######################################################################
head(tha_varimp)
head(alp_varimp)

library(dplyr)
library(ggplot2)


seasonal_imp <- bind_rows(alp_varimp, tha_varimp) %>%
  mutate(dataset = "Seasonal Climate")

# Optional: order variables by overall mean importance
var_order <- seasonal_imp %>%
  group_by(expl.var) %>%
  summarise(m = mean(mean.var.imp, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(m)) %>%
  pull(expl.var)

seasonal_imp$expl.var <- factor(seasonal_imp$expl.var, levels = var_order)

# --- Plot ---
p<-ggplot(seasonal_imp,
            aes(x = expl.var, y = mean.var.imp, fill = species)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.6,
    alpha = 0.6,
    linewidth = 0.3,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = species),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.9,
    size = 0.8
  ) +
  scale_fill_manual(
    values = c("alpina" = "#2C7BB6", "thaliana" = "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
    ),
    name = "Species"
  ) +
  scale_color_manual(
    values = c("alpina" = "#2166AC", "thaliana" = "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
    ),
    name = "Species"
  ) +
  labs(x = " ", y = "Mean variable importance (%)") +
  theme_minimal(base_size = 8) +
  theme(
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6),
    axis.ticks.length = unit(1.5, "mm"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(angle = 45, hjust = 1,
                                margin = margin(t = 4), size = 7),
    axis.text.y  = element_text(size = 7),
    legend.position = "top"
  )

p


ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_seasonal.pdf",plot = p, width = 100, height = 90, units = "mm", device = cairo_pdf)


##################################     CLIMATE VARIMP- EXTREME          ######################################################################

head(tha_varimp)
head(alp_varimp)

library(dplyr)
library(ggplot2)


extreme_imp <- bind_rows(alp_varimp, tha_varimp) %>%
  mutate(dataset = "Extreme Climate")


# Optional: order variables by overall mean importance
var_order <- extreme_imp  %>%
  group_by(expl.var) %>%
  summarise(m = mean(mean.var.imp, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(m)) %>%
  pull(expl.var)

extreme_imp$expl.var <- factor(extreme_imp$expl.var, levels = var_order)


# --- Plot ---
p<-ggplot(extreme_imp,
          aes(x = expl.var, y = mean.var.imp, fill = species)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.6,
    alpha = 0.6,
    linewidth = 0.3,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = species),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    alpha = 0.9,
    size = 0.8
  ) +
  scale_fill_manual(
    values = c("alpina" = "#2C7BB6", "thaliana" = "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
    ),
    name = "Species"
  ) +
  scale_color_manual(
    values = c("alpina" = "#2166AC", "thaliana" = "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
    ),
    name = "Species"
  ) +
  labs(x = " ", y = "Mean variable importance (%)") +
  theme_minimal(base_size = 8) +
  theme(
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6),
    axis.ticks.length = unit(1.5, "mm"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(angle = 45, hjust = 1,
                                margin = margin(t = 4), size = 7),
    axis.text.y  = element_text(size = 7),
    legend.position = "top"
  )

p


ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_extreme.pdf",plot = p, width = 100, height = 90, units = "mm", device = cairo_pdf)

##########################################################################################################################

#####################   SOIL and TOPO METRICS  #########################################

# TOPO
head(alp_topo_metrics)
head(tha_topo_metrics) 


# Combine
df_all_topo <- bind_rows(alp_topo_metrics, tha_topo_metrics) %>% filter(metric.eval=="TSS")


# (Optional) nicer ordering
df_all_topo$species <- factor(df_all_topo$species, levels = c("alpina","thaliana"))


# 3) Plot: two boxes per metric, faceted by metric
p<-ggplot(df_all_topo, aes(x = species, y = validation,fill = species)) +
  geom_boxplot(width = 0.55,alpha = 0.6) +
  geom_jitter(aes(color = species), width = 0.12, alpha = 1, size = 0.9)+
  coord_cartesian(ylim = c(0.25, 1)) +
  labs(x = " ", y = "TSS") +
  scale_x_discrete(
    labels = c(
      "thaliana" = expression(italic("A.")~italic("thaliana")),
      "alpina"   = expression(italic("A.")~italic("alpina"))
    )
  ) + 
  # Consistent species colours
  scale_fill_manual(values = c(
    "alpina"   =  "#2C7BB6",
    "thaliana" =  "#D95F02"
  )) +
  scale_color_manual(values = c(
    "alpina"   = "#2166AC",
    "thaliana" = "#D95F02"
  )) +
  theme_classic(base_size = 9) +
  theme( 
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4),size = 8),
    axis.text.y  = element_text(size = 8),
    
    legend.position = "none"
  )


ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/tss_topo.pdf",plot = p, width = 80, height = 80, units = "mm", device = cairo_pdf)




# STATISTICAL SIGNIFICANCE
library(dplyr)
df_test <- df_all_topo %>%
  filter(metric.eval == "TSS") %>%
  filter(!is.na(validation))

# Wilcoxon rank-sum (Mann–Whitney U)
wilc <- wilcox.test(validation ~ species, data = df_test, exact = FALSE)

# Useful descriptive stats
summ <- df_test %>%
  group_by(species) %>%
  summarise(
    n = n(),
    median = median(validation),
    mean = mean(validation),
    sd = sd(validation),
    .groups = "drop"
  )

wilc
summ


# SOIL
head(alp_soil_metrics)
head(tha_soil_metrics) 


# Combine
df_all_soil <- bind_rows(alp_soil_metrics, tha_soil_metrics) %>% filter(metric.eval=="TSS")


# (Optional) nicer ordering
df_all_soil$species <- factor(df_all_soil$species, levels = c("alpina","thaliana"))


# 3) Plot: two boxes per metric, faceted by metric
p<-ggplot(df_all_soil, aes(x = species, y = validation,fill = species)) +
  geom_boxplot(width = 0.55,alpha = 0.6) +
  geom_jitter(aes(color = species), width = 0.12, alpha = 1, size = 0.9)+
  coord_cartesian(ylim = c(0.25, 1)) +
  labs(x = " ", y = "TSS") +
  scale_x_discrete(
    labels = c(
      "thaliana" = expression(italic("A.")~italic("thaliana")),
      "alpina"   = expression(italic("A.")~italic("alpina"))
    )
  ) + 
  # Consistent species colours
  scale_fill_manual(values = c(
    "alpina"   =  "#2C7BB6",
    "thaliana" =  "#D95F02"
  )) +
  scale_color_manual(values = c(
    "alpina"   = "#2166AC",
    "thaliana" = "#D95F02"
  )) +
  theme_classic(base_size = 9) +
  theme( 
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth =0.6 ),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4),size = 8),
    axis.text.y  = element_text(size = 8),
    
    legend.position = "none"
  )


ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/tss_soil.pdf",plot = p, width = 80, height = 80, units = "mm", device = cairo_pdf)



# STATISTICAL SIGNIFICANCE
library(dplyr)
df_test <- df_all_soil %>%
  filter(metric.eval == "TSS") %>%
  filter(!is.na(validation))

# Wilcoxon rank-sum (Mann–Whitney U)
wilc <- wilcox.test(validation ~ species, data = df_test, exact = FALSE)

# Useful descriptive stats
summ <- df_test %>%
  group_by(species) %>%
  summarise(
    n = n(),
    median = median(validation),
    mean = mean(validation),
    sd = sd(validation),
    .groups = "drop"
  )

wilc
summ



######################   SOIL and TOPO VARIABLE IMPORTANCE  #########################################

#SOIL
head(alp_soil_varimp)
head(tha_soil_varimp)

soil_varimp <- bind_rows(alp_soil_varimp, tha_soil_varimp) %>% rename()
  

p<-ggplot(soil_varimp, aes(x = expl.var, y = var.imp,  fill = species)) +
  geom_boxplot(width = 0.55,linewidth = 0.2) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian(ylim = c(0, 100)) +   # zoom without dropping
  
  # for soil
  scale_x_discrete(labels = c("cfvo15" = "coarse fragments", "clay15" = "clay","nitrogen15" = "nitrogen","phh2o15" = "ph","silt15" = "silt")) +
  
  labs(x = " ", y = "Importance (%)") +
  scale_fill_manual(
    name = "Species",   # legend title
    values = c(
      "alpina"   =  "#2C7BB6",
      "thaliana" =  "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
      )) +
  theme_classic(base_size = 9) +
  theme( 
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4),size = 8),
    axis.text.y  = element_text(size = 8),
    
    legend.position="none"
    
    
  )

ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_soil.pdf",plot = p, width = 130, height = 100, units = "mm", device = cairo_pdf)

# FIND THE MEAN VALUES
soil_varimp %>%
  group_by(species, expl.var) %>%
  summarise(
    mean_importance = mean(var.imp, na.rm = TRUE),
    sd_importance   = sd(var.imp, na.rm = TRUE),
    n               = n(),
    .groups = "drop"
  )



# TOPO
head(alp_topo_varimp)
head(tha_topo_varimp)

topo_varimp <- bind_rows(alp_topo_varimp, tha_topo_varimp) %>% rename()


p<-ggplot(topo_varimp, aes(x = expl.var, y = var.imp,  fill = species)) +
  geom_boxplot(width = 0.55,linewidth = 0.2) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian(ylim = c(0, 100)) +   # zoom without dropping
  
  # for topo
  scale_x_discrete(labels = c("elev" = "elevation", "TPI" = "position index","TRI" = "ruggedness index")) +
  
  labs(x = " ", y = "Importance (%)") +
  scale_fill_manual(
    name = "Species",   # legend title
    values = c(
      "alpina"   =  "#2C7BB6",
      "thaliana" =  "#D95F02"),
    labels = c(
      "alpina"   = expression(italic("A.")~italic("alpina")),
      "thaliana" = expression(italic("A.")~italic("thaliana"))
    )) +
  theme_classic(base_size = 9) +
  theme( 
    text = element_text(family = "Arial"),
    axis.line  = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(1.5, "mm"),
    
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(margin = margin(t = 4),size = 8),
    axis.text.y  = element_text(size = 8),
    
    legend.key.size = unit(4, "mm")
    
  )

ggsave("/Users/sansari/Desktop/chapter3-sdm/figures/varimp_topo.pdf",plot = p, width = 130, height = 100, units = "mm", device = cairo_pdf)


# FIND THE MEAN VALUES
topo_varimp %>%
  group_by(species, expl.var) %>%
  summarise(
    mean_importance = mean(var.imp, na.rm = TRUE),
    sd_importance   = sd(var.imp, na.rm = TRUE),
    n               = n(),
    .groups = "drop"
  )


