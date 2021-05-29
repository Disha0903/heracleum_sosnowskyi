setwd('C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2')

getwd()
par(mar = rep(2, 4))

## load the required packages
library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)

## read data ----
HerSos_occ <- read.csv('C:/Users/lk311/Downloads/Biomod/HerSos_occ.csv')

summary(HerSos_occ)

HerSos_occ

HerSos_clean <- HerSos_occ[!duplicated(HerSos_occ),]
HerSos_occ_ready <- HerSos_clean[!is.na(HerSos_clean$lon),]

HerSos_occ_ready

bioclim_all_new <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_1_a.asc',
      bio_4  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_4_a.asc',
      bio_5  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_5_a.asc',
      bio_9  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_9_a.asc',
      bio_12  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_12_a.asc',
      bio_19  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/data/wc2.1_30s_bio/_bio_19_a.asc'
    )
  )

bioclim_all_new

## format the data ----
HerSos_data <- 
  BIOMOD_FormatingData(
    resp.var = HerSos_occ_ready['Heracleum.sosnowskyi'],
    resp.xy = HerSos_occ_ready[, c('long', 'lat')],
    expl.var = bioclim_all_new,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 2,
    PA.nb.absences = 9000,
    PA.strategy = 'SRE'
  )


## formatted object summary
HerSos_data

## plot of selected pseudo-absences
plot(HerSos_data)


HerSos_opt <- 
  BIOMOD_ModelingOptions(
    GLM = list(type = 'quadratic', interaction.level = 1),
    GBM = list(n.trees = 1000),
    GAM = list(algo = 'GAM_mgcv')
 )

## run the individual models ----
HerSos_models <- 
  BIOMOD_Modeling(
    data = HerSos_data,
    models = c("GBM", "GLM"),
    models.options = HerSos_opt,
    NbRunEval = 3,
    DataSplit = 80,
    VarImport = 3,
    models.eval.meth = c('TSS','ROC'),
    do.full.models=FALSE,
    modeling.id="demo1"
  )

    
HerSos_models

## get models evaluation scores
HerSos_models_scores <- get_evaluations(HerSos_models)
HerSos_models_scores

models_scores_graph(
  HerSos_models, 
  by = "models",
  metrics = c("ROC", 'TSS'),
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)

gg1 <- models_scores_graph( HerSos_models,
                            by = 'models',
                            metrics = c('ROC','TSS') )

gg1_custom <- 
  gg1 + 
  ggtitle("Evaluation scores of models") + ## add title
  scale_colour_manual(values=c("green", "blue", 'red','black')) ## change colors

gg1_custom


models_scores_graph(
  HerSos_models, 
  by = "cv_run" , 
  metrics = c("ROC","TSS"), 
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)

models_scores_graph(
  HerSos_models, 
  by = "data_set", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)


library(biomod2)
library(raster)
library(reshape2)
library(ggplot2)

col_vec = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#e31a1c'
            ,'#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
col_fun = colorRampPalette(col_vec)

## rearrange data
ModEval = melt(HerSos_models_scores)
ModEval = ModEval[which(ModEval$Var2 == "Testing.data"), ]
head(ModEval)

ggplot(ModEval, aes(x = Var1, y = value, color = interaction(Var5,Var3))) +
  scale_color_manual("", values = rep(col_fun(5), each = 2)) +
  geom_boxplot(size = 0.6) +
  labs(x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )




MyBiomodModelVarImp <- get_variables_importance(HerSos_models)
dimnames(MyBiomodModelVarImp)


MyBiomodModelVarImp[,,"RUN2", 'PA2']

str(MyBiomodModelVarImp)

rf.imp <- MyBiomodModelVarImp[1:6,1:4,1:2,1:2]
##Have a look at the average value....
rfimp.m <- melt(rf.imp) #condenses data to more readable 
rfimp.av <- aggregate(value~ X1, data =rfimp.m, FUN = mean) #take average value of all runs for each covariate, mean importance 



dimnames(HerSos_models_scores)
HerSos_models_scores["ROC","Testing.data",,,]

(HerSos_models_var_import <- get_variables_importance(HerSos_models))
apply(HerSos_models_var_import, c(1,2), mean)

RF_bio18 <- c(0.117,0.117,0.115,0.117)
sd(RF_bio18)




env.present <- bioclim_ZA_sub
plot(env.present)
ggsave(filename="biovariables.png", plot=env.present, device="png",
       dpi=500)
dev.off()


HerSos_mars <- BIOMOD_LoadModels(HerSos_models, models='MARS')
HerSos_gbm <- BIOMOD_LoadModels(HerSos_models, models='GBM')
HerSos_rf <- BIOMOD_LoadModels(HerSos_models, models='RF')
HerSos_cta <- BIOMOD_LoadModels(HerSos_models, models='CTA')

mars_eval_strip <- 
  biomod2::response.plot2(
    models  = HerSos_mars,
    Data = get_formal_data(HerSos_models,'expl.var'), 
    show.variables= get_formal_data(HerSos_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(HerSos_models,'resp.var')
  )


gbm_eval_strip <- 
  biomod2::response.plot2(
    models  = HerSos_gbm,
    Data = get_formal_data(HerSos_models,'expl.var'), 
    show.variables= get_formal_data(HerSos_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(HerSos_models,'resp.var')
  )

rf_eval_strip <- 
  biomod2::response.plot2(
    models  = HerSos_rf,
    Data = get_formal_data(HerSos_models,'expl.var'), 
    show.variables= get_formal_data(HerSos_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(HerSos_models,'resp.var')
  )



cta_eval_strip <- 
  biomod2::response.plot2(
    models  = HerSos_cta,
    Data = get_formal_data(HerSos_models,'expl.var'), 
    show.variables= get_formal_data(HerSos_models,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = 'median',
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(HerSos_models,'resp.var')
  )

## run the ensemble models ----
HerSos_ensemble_models <- 
  BIOMOD_EnsembleModeling(
    modeling.output = HerSos_models,
    em.by = 'all',
    eval.metric = 'TSS',
    eval.metric.quality.threshold = 0.8,
    models.eval.meth = c('TSS','ROC'),
    prob.mean = FALSE,
    prob.cv = TRUE, 
    committee.averaging = TRUE,
    prob.mean.weight = TRUE,
    VarImport = 0
  )

## asses ensemble models quality ----
(HerSos_ensemble_models_scores <- get_evaluations(HerSos_ensemble_models))


## do models projections ----

## current projections
bioclim_ZA_sub <- bioclim_all_new
HerSos_models_proj_current <- 
  BIOMOD_Projection(
    modeling.output = HerSos_models,
    new.env = bioclim_ZA_sub,
    proj.name = "current",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
)

HerSos_ensemble_models_proj_current <- 
  BIOMOD_EnsembleForecasting(
    EM.output = HerSos_ensemble_models,
    projection.output = HerSos_models_proj_current,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

plot_proj <- plot(HerSos_ensemble_models_proj_current)

## future projections

## load 2050 bioclim variables
bioclim_ZA_2050_BC45 <-
  stack(
    c(
      bio_1 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio1_50.asc",
      bio_4 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio4_50.asc",
      bio_5 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio5_50.asc",
      bio_9 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio9_50.asc",
      bio_16 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio16_50.asc",
      bio_18 = "C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/cmip5/10m/_bio18_50.asc"
    )
  )


bioclim_ZA_2050_BC45

HerSos_models_proj_2050_BC45 <- 
  BIOMOD_Projection(
    modeling.output = HerSos_models,
    new.env = bioclim_ZA_2050_BC45,
    proj.name = "2050_BC45",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

HerSos_ensemble_models_proj_2050_BC45 <- 
  BIOMOD_EnsembleForecasting(
    EM.output = HerSos_ensemble_models,
    projection.output = HerSos_models_proj_2050_BC45,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

plot_proj <- plot(HerSos_ensemble_models_proj_2050_BC45)

files.names = list.files(path = "Heracleum.sosnowskyi/proj_2050_BC45/individual_projections/"
                         , pattern = "_PA1_"
                         , full.names = TRUE)
files.names = files.names[grep(".img$", files.names)]
files.names = files.names[-grep("TSSbin.img$", files.names)]
stk.outputs = stack(files.names)
names(stk.outputs) = sub("proj_2050_BC45_Heracleum.sosnowskyi_", "", names(stk.outputs))

stk.pts = rasterToPoints(stk.outputs)
stk.pts = as.data.frame(stk.pts)
stk.pts = melt(stk.pts, id.vars = c("x", "y"))

ggplot(stk.pts, aes(x = x, y = y, fill = value/1000)) +
  geom_tile() +
  scale_fill_viridis_c("Probability") +
  facet_wrap(~ variable) +
  labs(x = "", y = "") +
  theme(
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )


## PART 3
###################################################################################

## get some outputs maps
files.names = list.files(path = "Heracleum.sosnowskyi/proj_current/individual_projections/"
                         , pattern = "_PA1_"
                         , full.names = TRUE)
files.names = files.names[grep(".img$", files.names)]
files.names = files.names[-grep("TSSbin.img$", files.names)]
stk.outputs = stack(files.names)
names(stk.outputs) = sub("proj_current_Heracleum.sosnowskyi_", "", names(stk.outputs))

stk.pts = rasterToPoints(stk.outputs)
stk.pts = as.data.frame(stk.pts)
stk.pts = melt(stk.pts, id.vars = c("x", "y"))

ggplot(stk.pts, aes(x = x, y = y, fill = value/1000)) +
  geom_tile() +
  scale_fill_viridis_c("Probability") +
  facet_wrap(~ variable) +
  labs(x = "", y = "") +
  theme(
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )

## get coefficient of variation
ras.cv = raster("Heracleum.sosnowskyi/proj_current/individual_projections/Heracleum.sosnowskyi_EMcvByTSS_mergedAlgo_mergedRun_mergedData.img")

ras.cv.pts = rasterToPoints(ras.cv)
ras.cv.pts = as.data.frame(ras.cv.pts)
colnames(ras.cv.pts) = c("x", "y", "CV")

ggplot(ras.cv.pts, aes(x = x, y = y, fill = CV)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "", y = "") +
  theme(
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )

##Future
## get some outputs maps
files.names = list.files(path = "Heracleum.sosnowskyi/proj_2050_BC45/individual_projections/"
                         , pattern = "_PA2_"
                         , full.names = TRUE)
files.names = files.names[grep(".img$", files.names)]
files.names = files.names[-grep("TSSbin.img$", files.names)]
stk.outputs = stack(files.names)
names(stk.outputs) = sub("proj_2050_BC45_Heracleum.sosnowskyi_", "", names(stk.outputs))

stk.pts = rasterToPoints(stk.outputs)
stk.pts = as.data.frame(stk.pts)
stk.pts = melt(stk.pts, id.vars = c("x", "y"))

ggplot(stk.pts, aes(x = x, y = y, fill = value/1000)) +
  geom_tile() +
  scale_fill_viridis_c("Probability") +
  facet_wrap(~ variable) +
  labs(x = "", y = "") +
  theme(
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )

## get coefficient of variation
ras.cv = raster("Heracleum.sosnowskyi/proj_current/individual_projections/Heracleum.sosnowskyi_EMcvByTSS_mergedAlgo_mergedRun_mergedData.img")

ras.cv.pts = rasterToPoints(ras.cv)
ras.cv.pts = as.data.frame(ras.cv.pts)
colnames(ras.cv.pts) = c("x", "y", "CV")

ggplot(ras.cv.pts, aes(x = x, y = y, fill = CV)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "", y = "") +
  theme(
    panel.border = element_rect(size = 1, fill = NA),
    panel.grid.major = element_line(linetype = 2, color = "grey", size = 0.8)
  )













ggsave(filename="gg-higher-respl.png", plot=plot_proj, device="png",
       dpi=500)
dev.off()



#future data