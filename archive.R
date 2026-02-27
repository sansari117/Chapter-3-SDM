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
