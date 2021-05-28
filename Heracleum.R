setwd('C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2')
par(mar = rep(2, 4))
## load the required packages
library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)
library(maptools)



## read data ----
HS_occ <- read.csv('C:/Users/lk311/Downloads/Biomod/HerSos_occ_3k.csv')


summary(HS_occ)

library(SDMtune)

HS_clean <- HS_occ[!duplicated(HS_occ),]
HS_occ_ready <- HS_clean[!is.na(HS_clean$lon),]
HS_occ_ready
summary(HS_occ_ready)


plot(HS_occ_ready$lon, HS_occ_ready$lat)

HS_occ_ready <- HS_occ_ready[HS_occ_ready$lon < 70,]


long <- HS_occ_ready$long
lat <- HS_occ_ready$lat
coords <- data.frame(long, lat)

library(leaflet)
library(maps)
library(spThin)


# visualizing the ten coordinates we generated:
library(dplyr)
blue = makeIcon("/Users/jazzurro/Documents/Stack Overflow/blue.png", iconWidth = 24, iconHeight =32)

ID <- 1:10

pal <- colorNumeric(palette = "Blues", domain = quakes$mag)

leaflet(data=coords) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long, ~lat,
                   #label=as.character(ID), 
                   radius = 6,
                   color = "navy",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0.3, weight = 1)




bioclim <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_1_i.asc',
      bio_2  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_2_i.asc',
      bio_5  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_5_i.asc',
      bio_18  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_18_i.asc'
    )
  )


soil <- 
  raster::stack(
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_soc_5-15.asc',
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_silt_5-15.asc'
  )

plot(soil)
soil <- soil/10

a <- resample(soil,bioclim)
res(a)
plot(a)


env <- stack(a,bioclim)

plot(env)

library(rgdal)
denpasar  <- readOGR('C:/Users/lk311/Downloads/RUS_adm0.shp')
#denpasar <- spTransform(x = denpasar, CRSobj = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
#plot(denpasar)
masked <- mask(x = env, mask = denpasar)
plot(masked)
cropped_env <- crop(x = masked, y = extent(denpasar))
plot(cropped_env)

cropped_env <- stack(cropped_env)
names(cropped_env)


HS_data <- 
  BIOMOD_FormatingData(
    resp.var = HS_occ_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_occ_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 1700,
    PA.dist.max = 25000,
    PA.strategy = 'disk'
  )

plot(HS_data)


## SDMtune
library(SDMtune)
library(zeallot)

# Prepare data

library(dplyr)
## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA
  )
}

## function to get background mask
get_mask <- function(bfd){
  bfd@data.mask
}

(pa.all.xy <- get_PAtab(HS_data) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

pa.all.xy

(pres.xy <- get_PAtab(HS_data) %>% 
    filter(status == 1) %>%
    select(x, y)) %>%
  distinct()

pres.xy
plot(HS_data)

##Second PA tundra
HS_data2 <- 
  BIOMOD_FormatingData(
    resp.var = HS_occ_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_occ_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 5000,
    PA.strategy = 'random'
  )

plot(HS_data2)


(pa.all_2.xy <- get_PAtab(HS_data2) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

pa.all_2.xy

pa2 <- pa.all_2.xy[pa.all_2.xy$x >40 & pa.all_2.xy$y > 64,]
pa2
pa2 <- pa2[1:700,]
#450 smaples for pa
pa_data_all = rbind(pa2,pa.all.xy)
pa2
pa.all.xy

#install.packages('tmap')
library(tmap)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = pres.xy, aes(x = x, y = y), color = "blue",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = pres.xy, aes(x = x, y = y), color = "blue",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = pa_data_all, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = pa_data_all, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

library(blockCV)


# loading raster library
library(raster)
library(sf)

pa_data_all$Species <- 0
#pa.all.xy$Species <- 0
pres.xy$Species <- 1

pa<- rbind(pa_data_all, pres.xy,by=c('x', 'y'))
#pa <- rbind(pa.all.xy, pres.xy,by=c('x', 'y'))
pa<- na.omit(pa)



# import raster data
awt <- cropped_env
awt
crs(awt) <- '+proj=longlat +datum=WGS84'

#pa <- pa[-c(5896),]
pa <- head(pa,-3)
tail(pa)

pa <- pa[, c( 3,1,2)]
# make a SpatialPointsDataFrame object from data.frame
pa_data <- st_as_sf(pa, coords = c("x", "y"), crs = crs(awt))
# see the first few rows
pa_data

plot(awt$bio_1)

par(mfrow = c(2,3))
plot(awt[[3]], labs(x = "longitude", y = "latitude"))# plot raster data
plot(pa_data[which(pa_data$Species==1), ], pch = 1, col="red", add=TRUE) # add presence points
plot(pa_data[which(pa_data$Species==0), ], pch = 1, col="blue", add=TRUE) # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(2,2), bty="n")



set.seed(1)
sb <- spatialBlock(speciesData = pa_data,
                   species = "Species",
                   rasterLayer = awt,
                   theRange = 100000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

# spatial blocking by rows and columns with checkerboard assignment
sb2 <- spatialBlock(speciesData = pb_data, # presence-background data
                    species = "Heracleum.sosnowskyi",
                    rasterLayer = awt,
                    rows = 5,
                    cols = 6,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)

# spatial blocking by rows with systematic assignment
sb3 <- spatialBlock(speciesData = pa_data,
                    species = "Heracleum.sosnowskyi",
                    rasterLayer = awt,
                    rows = 6,
                    selection = "checkerboard",
                    biomod2Format = TRUE)


# adding points on saptialBlock plot
library(ggplot2)

sb$plots + geom_sf(data = pa_data, alpha = 0.3)

foldExplorer(blocks = sb, 
             rasterLayer = awt, 
             speciesData = pa_data)


# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
mydata <- raster::extract(awt, pa_data, df = TRUE)
# adding species column to the dataframe
mydata$Species <- as.factor(pa_data$Species)
# remove extra column (ID)
mydata <- mydata[,-1]

# extract the foldIDs in SpatialBlock object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb$folds

# create a data.frame to store the prediction of each fold (record)
testTable <- pa_data
testTable$pred <- NA

set.seed(1)
cross <- for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(Species~., mydata[trainSet, ], ntree = 900, importance=TRUE) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
}



# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)


autoplot(precrec_obj)
autoplot(sspoints)
#saveRDS(rf, "./final_final_model.rds")
par(mfrow = c(2,4))

varImpPlot(rf, scale=TRUE)
importance(rf)

saveRDS(rf, "model_rf.rds")
my_model <- readRDS("model_rf.rds")


##HOLdout____________________________________________________________
## read data ----
HS_hd <- read.csv('C:/Users/lk311/Downloads/Biomod/holdout/holdout.csv')


summary(HS_hd)

library(SDMtune)

HS_hd_clean <- HS_hd[!duplicated(HS_hd),]
HS_hd_ready <- HS_hd_clean[!is.na(HS_hd_clean$lon),]

HS_hd_ready

summary(HS_occ_ready)


plot(HS_hd_ready$lon, HS_hd_ready$lat)

HS_hd_ready <- HS_hd_ready[HS_hd_ready$lon < 70,]


long_1 <- HS_hd_ready$long
lat_1 <- HS_hd_ready$lat
coords_1 <- data.frame(long_1, lat_1)


ID <- 1:10

pal <- colorNumeric(palette = "Blues", domain = quakes$mag)


leaflet(data=coords_1) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long_1, ~lat_1,
                   #label=as.character(ID), 
                   radius = 6,
                   color = "navy",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0.3, weight = 1)


HS_hd <- 
  BIOMOD_FormatingData(
    resp.var = HS_hd_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_hd_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 100,
    PA.strategy = 'random'
  )

plot(HS_hd)

(pa.hd.xy <- get_PAtab(HS_hd) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

pa.hd.xy
pa.hd.xy['Species']=0

(pres.hd.xy <- get_PAtab(HS_hd) %>% 
    filter(status == 1) %>%
    select(x, y)) %>%
  distinct()

pres.hd.xy
pres.hd.xy['Species'] = 1

HS_hd1 <- 
  BIOMOD_FormatingData(
    resp.var = HS_hd_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_hd_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 800,
    PA.strategy = 'random'
  )
(pa.hd.xy1 <- get_PAtab(HS_hd1) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()
pa22 <- pa.all_2.xy[pa.hd.xy1 $x >40 & pa.hd.xy1 $y > 64,]
pa22
pa22 <- pa2[1:100,]
pa22$Species <- 0

hd.pa22 <- rbind(pa.hd.xy, pa22) 

ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = hd.pa22, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)

#hd.xy <- rbind(pres.hd.xy, pa.hd.xy)
hd.xy <- rbind(pres.hd.xy, hd.pa22)
hd_data <- st_as_sf(hd.xy, coords = c("x", "y"), crs = crs(awt))
# see the first few rows
hd_data

hddata <- raster::extract(awt, hd_data, df = TRUE)
# adding species column to the dataframe
hddata$Species <- as.factor(hd_data$Species)
# remove extra column (ID)
hddata <- hddata[,-1]


holdotTable <- hd_data
holdotTable$pred <- NA
holdotTable$pred <- predict(rf, hddata, type = "prob")[,2]


precrec_hd <- evalmod(scores = holdotTable$pred, labels = holdotTable$Species)

autoplot(precrec_hd)

##HOLDOUT Without negative points
hd_data_only_pres <- st_as_sf(pres.hd.xy, coords = c("x", "y"), crs = crs(awt))
hddata_only_pres <- raster::extract(awt, hd_data_only_pres, df = TRUE)
hddata_only_pres$Species <- as.factor(hd_data_only_pres$Species)
hddata_only_pres <- hddata_only_pres[,-1]

holdotTable <- hd_data_only_pres 
holdotTable$pred <- NA
holdotTable$pred <- predict(rf, hddata_only_pres, type = "prob")[,2]

precrec_hd <- evalmod(scores = holdotTable$pred, labels = holdotTable$Species)



##HOLDOUT map prediction
##Make map prediction
library(mecofun)
bio_hd <- data.frame(rasterToPoints(awt))
bio_hd$pred_rf_hd <- mecofun::predictSDM(rf, bio_hd)

df22 = data.frame(st_coordinates(holdotTable[,2]))
df44 <- as.data.frame(holdotTable$pred)
df22['pred'] <- df44
df22


r_pred_hd<- as.data.frame(df22, xy = TRUE)


bio_curr_df <- data.frame(rasterToPoints(cropped_env))
#bio_fut_df$eo_mask <- mecofun::eo_mask(pa[,pred], bio_fut_df[,pred])
bio_curr_df$pred <- mecofun::predictSDM(rf, bio_curr_df)



r_pred_hd <- rasterFromXYZ(df22)
r_pred_hd$thr1 <- ifelse(r_pred_hd$pred>=0.18, 1, 0)
#r_pred_hd <- rasterFromXYZ(r_pred_hd[,-(3)]) 
r_pred_hd_1 <- r_pred_hd[,-(3)]
hd_thr1 <- st_as_sf(r_pred_hd_1, coords = c("X", "Y"), crs = crs(awt))

par(mar = rep(2, 4))
plot(awt[[1]]) # plot raster data
plot(hd_thr1[which(hd_thr1$thr1==1), ], pch = 2, col="red", add=TRUE) # add presence points
plot(hd_thr1[which(hd_thr1$thr1==0), ], pch = 2, col="blue", add=TRUE) # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(2,2), bty="n")

par(mar = rep(2, 4))
plot(awt[[1]]) # plot raster data
hs_hd_initial <- st_as_sf(HS_hd_ready, coords = c("long", "lat"), crs = crs(awt))
plot(hs_hd_initial[which(hs_hd_initial$Heracleum.sosnowskyi==1), ], pch = 2, col="red", add=TRUE) # add presence points


plot(awt[[1]])
hs_hd_absence <- st_as_sf(hd.xy, coords = c("x", "y"), crs = crs(awt))
plot(hs_hd_absence[which(hs_hd_absence$Species==0), ], pch = 2, col="blue", add=TRUE) # add absence points

require('caret')
require('e1071')
df <- data.frame(predicted = r_pred_hd$thr1, actual = hd.xy$Species)
#assuming pred column contains the predicted species
confusionMatrix(data = df$predicted, reference = df$actual)
class(df$actual)
length(which(df$predicted == "0" & df$actual=='1'))


df<-df%>%mutate(New_Column = case_when(
  is.na(Age) & Pclass==1 ~ 40,
  is.na(Age) & Pclass==2 ~ 30,
  is.na(Age) & Pclass==3 ~ 25,
  TRUE~Age
))

rf.confusionMatrix <- confusionMatrix(rf.class , data.test$Class, positive = "T")


example <- confusionMatrix(data=df$predicted, reference = df$actual)



library(dplyr)
url = 'https://gist.githubusercontent.com/michhar/2dfd2de0d4f8727f873422c5d959fff5/raw/ff414a1bcfcba32481e4d4e8db578e55872a2ca1/titanic.csv'
df_ex = read.csv(url, sep="\t")








##Make map prediction
library(mecofun)
bio_curr_df <- data.frame(rasterToPoints(awt))
bio_curr_df$pred_rf <- mecofun::predictSDM(rf, bio_curr_df)

#bio_curr_df$thr1 <- 

bio_curr_df$thr1 <- ifelse(bio_curr_df$pred_rf>=0.45, 1, 0)
r_pred_thr <- rasterFromXYZ(bio_curr_df[,-c(3:9)])
plot(r_pred_thr)
par(mar = rep(2, 3))

r_pred_curr <- rasterFromXYZ(bio_curr_df[,-c(3:8)])
plot(r_pred_curr)

r_
map_thr_cur <- gplot(r_pred_thr) +
  geom_tile(aes(fill = value)) +
  labs(title = "The projection of habitat suitability
       for H.S. in 1985-2007",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
jpeg(
  filename="map_bin_cur",
  width=8,
  height=5,
  units="in",
  res=500)
map_thr_cur
dev.off()
cellStats(r_pred_thr, 'sum')

library(ggplot2)

map_c <- gplot(r_pred_curr) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "The projection of habitat suitability 
       for H.S. in 1895???2007",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())






jpeg(
  filename="map_current_25disk_last",
  width=8,
  height=5,
  units="in",
  res=500)
map_c
dev.off()

summary(map_c)

##3d curves
partial_response(rf,predictors = bio_curr_df)

library(RColorBrewer)
library(lattice)
# We prepare the response surface by making a dummy data set where two predictor variables range from their minimum to maximum value, and the remaining predictor is kept constant at its mean:
xyz <- data.frame(expand.grid(seq(min(avi_df[,pred[1]]),max(avi_df[,pred[1]]),length=50), seq(min(avi_df[,pred[2]]),max(avi_df[,pred[2]]),length=50)), mean(avi_df[,pred[3]]))
names(xyz) <- pred

# Make predictions
xyz$z <- predict(m_glm, xyz, type='response')
summary(xyz)
pa




library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(png)
attr(map_c, "info")


# Make binary predictions:
bio_curr_df$bin_rf <- ifelse(bio_curr_df$pred_rf > eval_rf$thresh, 1, 0)
(eval_rf <- mecofun::evalSDM(observation = avi_df[!is.na(avi_df$blockCV_tile),1], predictions = crosspred_rf))




## future
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio1.asc',
      bio_2  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio2.asc',
      bio_5  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio5.asc',
      bio_18  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio18.asc'
    )
  )
bioclim_future <- resample(bioclim_future,bioclim)

env_future <- stack(bioclim_future, a)
plot(env_future)

masked_f <- mask(x = env_future, mask = denpasar)
plot(masked_f)
cropped_env_f <- crop(x = masked_f, y = extent(denpasar))
plot(cropped_env_f)


library(rgdal)
denpasar  <- readOGR('C:/Users/lk311/Downloads/RUS_adm0.shp')
#denpasar <- spTransform(x = denpasar, CRSobj = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
#plot(denpasar)
masked <- mask(x = env_future , mask = denpasar)
plot(masked)
cropped_env_f <- crop(x = masked, y = extent(denpasar))
plot(cropped_env_f)
plot(awt)

awt.df <- as.data.frame(awt)

#all_data <- rbing(pa, )

plot(cropped_env_f$bio_1)
cropped_env_f$bio_1 <- cropped_env_f$bio_1/10
cropped_env_f$bio_2 <- cropped_env_f$bio_2/10
cropped_env_f$bio_5 <- cropped_env_f$bio_5/10


plot(cropped_env_f$bio_1)



bio_fut_df <- data.frame(rasterToPoints(cropped_env_f))
#bio_fut_df$eo_mask <- mecofun::eo_mask(pa[,pred], bio_fut_df[,pred])
bio_fut_df$pred_rf <- mecofun::predictSDM(rf, bio_fut_df)


r_pred_fut <- rasterFromXYZ(bio_fut_df[,-c(3:8)])
plot(r_pred_fut)

bio_fut_df$thr1 <- ifelse(bio_fut_df$pred_rf>=0.45, 1, 0)
r_pred_fut_thr1 <- rasterFromXYZ(bio_fut_df[,-c(3:9)]) 

cellStats(r_pred_fut_thr1, 'sum')
plot(r_pred_fut_thr1)

write.csv(dataframe_ensemble,'ensemble.csv')



#map_f <-
  gplot(r_pred_fut) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "Future projection of habitat suitability
       for H.S. in 2040-2060",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


map_thr_fut <- 
  gplot(r_pred_fut_thr1) +
  geom_tile(aes(fill = as.factor(value))) +
  labs(title = "The projection of habitat suitability 
       for H.S. in 1895???2007",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() + 
  labs(fill='Occurence')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_manual(values = c("#2c7bb6", 
                               "#d7191c",
                                 "lightgoldenrod1"))


map_c <- gplot(r_pred_curr) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "Current projection for habitat suitability 
       of H.S. on the territory of Russia",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())



jpeg(
  filename="map_future_25disk1_last.jpeg",
  width=8,
  height=5,
  units="in",
  res=500)
map_f
dev.off()

awt
plot(awt$bio_1)
plot(cropped_$bio_1)

knitr::opts_chunk$set(fig.width=12, fig.height=8) 


summary(bio_fut_df)


myBiomodModelOut <- c(my_model, rf)
myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = awt,
                                        proj.name = 'current',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)
pa


## For ensemble
rf1 <- rf
prediction1 <- bio_fut_df[,c(1,2,9)]

rf2 <- rf
prediction2 <- bio_fut_df[,c(1,2,9)]

rf3 <- rf
prediction3 <- bio_fut_df[,c(1,2,9)]

rf4 <- rf
prediction4 <- bio_fut_df[,c(1,2,9)]

rf5 <- rf
prediction5 <- bio_fut_df[,c(1,2,9)]

rf6 <- rf
prediction6 <- bio_fut_df[,c(1,2,9)]

rf7 <- rf
prediction7 <- bio_fut_df[,c(1,2,9)]

rf8 <- rf
prediction8 <- bio_fut_df[,c(1,2,9)]

rf9 <- rf
prediction9 <- bio_fut_df[,c(1,2,9)]

rf10 <- rf
prediction10 <- bio_fut_df[,c(1,2,9)]

prediction10$pred_rf10 <- prediction10$pred_rf
prediction10 <- prediction10[, c(1,2,4)]

dataframe_ensemble <- merge(dataframe_ensemble,prediction10)

dataframe_ensemble$pred_avg<-(dataframe_ensemble$pred_rf+dataframe_ensemble$pred_rf2+dataframe_ensemble$pred_rf3+dataframe_ensemble$pred_rf4+dataframe_ensemble$pred_rf5+dataframe_ensemble$pred_rf6+dataframe_ensemble$pred_rf7+dataframe_ensemble$pred_rf8+dataframe_ensemble$pred_rf9+dataframe_ensemble$pred_rf10)/10

ensemble_pred_fut <- rasterFromXYZ(dataframe_ensemble[,-c(3:12)])
plot(ensemble_pred_fut)

map_ensemble <- gplot(ensemble_pred_fut) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "Future projection of habitat suitability
       for H.S. in 2040-2060",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

leaf_map_fut <- bio_fut_df[,c(1,2,10)]
leaf_map_fut1 <- leaf_map_fut[leaf_map_fut$thr1 != 0, ] 
leaf_map_fut1 <- as.data.frame(leaf_map_fut1)
bio_fut_df

leaflet(data=leaf_map_fut) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~x, ~y,
                   #label=as.character(ID), 
                   radius = 6,
                   color = "navy",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0.3, weight = 1)
