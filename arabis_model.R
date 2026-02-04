library(biomod2)
library(caret)
library(gam)

args <- commandArgs(trailingOnly = TRUE)
eco <- args[1]  # First argument is the eco value

# Dynamically construct file paths and parameters
eco_name <- paste0("eco", eco)
input_file <- paste0('/viper/u/sansari/sdm_alpina/myBiomodData_alp_', eco_name, '_38475_buf2degdiff.RData')
output_dir <- paste0("/viper/u/sansari/sdm_alpina/eco/", eco_name)
model_file <- paste0("myBiomodModelOut_", eco_name, "_38475_buf2degdiff.RData")
em_file <- paste0("myBiomodEM_", eco_name, "_38475_buf2degdiff.RData")

# Load the corresponding Biomod data
message("Processing: ", eco_name)
myBiomodData.r <- get(load(input_file))

setwd(output_dir)

# # stratified selection (geographic)
cv.s <- bm_CrossValidation(bm.format = myBiomodData.r,
                           strategy = "kfold",
                           k = 5,
                           nb.rep=10)

print(head(cv.s))

# Model
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.r,
                                    modeling.id = 'Arabis_Models',
                                    models = c("XGBOOST","RF","FDA","ANN","GBM","GLM","GAM"),
                                    CV.strategy = 'user.defined',
                                    CV.user.table = cv.s,
                                    #CV.nb.rep = 10,
                                    #CV.perc = 0.70,
                                    OPT.strategy = 'bigboss',
                                    var.import = 10,
                                    metric.eval = c('TSS','ROC'),
                                    seed.val = 100,
                                    nb.cpu=20)

save(myBiomodModelOut, file = model_file)

# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMmean', 'EMcv', 'EMmedian', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.8),
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 10,
                                      #EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional', nb.cpu = 20)

# Save the ensemble model output
save(myBiomodEM, file = em_file)

