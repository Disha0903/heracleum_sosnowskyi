setwd('C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2')

getwd()
par(mar = rep(2, 4))

## load the required packages
library(biomod2)
library(ggplot2)

library(gridExtra)
library(raster)

library(rasterVis)
library(leaflet)

## read data ----
HS_occ <- read.csv('C:/Users/lk311/Downloads/Biomod/borsh_soil_elev_clim.csv')
HS_occ
pca <- read.csv('C:/Users/lk311/Downloads/Biomod/pca.csv')

HS_occ <- HS_occ[ -c(5:6, 26:28, 39:46) ]

LatLong <- HS_occ[,2:3] # coordinates of points
Expl.Var <- HS_occ[,5:33] # bioclimatic variables
Resp.Var <- HS_occ[,4] # species occurences

##INTERPOLATION
clay <- HS_occ[ ,c(2,3,27)] 
library(data.table)
dt <- data.table(as.data.frame(clay, xy = TRUE))
dt

pal <- colorFactor(
  palette = 'Dark2',
  domain = dt$type
)

leaflet(df) %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1, 
             radius = ~size, popup = ~popup, color = ~pal(type))

ID <- 1:10
leaflet(data=dt) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long, ~lat, label=as.character(ID), color = ~pal(Clay.....))%>%
  addLegend(pal=pal, values = ~Clay.....)

leaflet(dt)

x <- raster(xmn=19, xmx=72, ymn=40, ymx=68, res=0.5, crs="+proj=longlat +datum=WGS84")
clay <- rasterize(clay[, c('long', 'lat')], x, clay[, 'Clay.....'], fun=mean)
plot(clay)

#install.packages('sf')
#install.packages('e1071', dependencies=TRUE)
library(sf)
clay_soil= st_as_sf(dt, coords = c("long", "lat"), crs = 32636)
clay_soil
elev
##install.packages('stars')
library(stars)
#borders = st_read("C:/Users/lk311/Downloads/RUS_adm0.shp")
plot(borders)
library(rgdal)
denpasar  <- readOGR('C:/Users/lk311/Downloads/RUS_adm0.shp')
#denpasar <- spTransform(x = denpasar, CRSobj = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
plot(denpasar)
built_up  <- elev
plot(built_up)
masked <- mask(x = built_up, mask = denpasar)
plot(masked)
cropped <- crop(x = masked, y = extent(denpasar))
plot(cropped)

plot(eval, breaks = "equal", col = terrain.colors(11), reset = FALSE)
plot(st_geometry(clay_soil), add = TRUE)
grid = st_as_sfc(st_bbox(borders))
grid = st_as_stars(grid, dx = 1000, dy = 1000)
dem = st_warp(src = eval, grid, method = "average", use_gdal = TRUE)

library(stars)
dem1 = read_stars("C:/Users/lk311/Downloads/Biomod/out3.tif")
dem2 = read_stars("C:/Users/lk311/Downloads/Biomod/out4.tif")
dem3 = read_stars("C:/Users/lk311/Downloads/44-46.tif")
dem4 = read_stars("C:/Users/lk311/Downloads/46-48.tif")
dem5 = read_stars("C:/Users/lk311/Downloads/48-50.tif")
dem6 = read_stars("C:/Users/lk311/Downloads/50-52.tif")
dem7 = read_stars("C:/Users/lk311/Downloads/52-54.tif")
dem8 = read_stars("C:/Users/lk311/Downloads/54-56.tif")
dem9 = read_stars("C:/Users/lk311/Downloads/56-58.tif")
dem10 = read_stars("C:/Users/lk311/Downloads/58-60.tif")
dem11 = read_stars("C:/Users/lk311/Downloads/60-62.tif")
dem12 = read_stars("C:/Users/lk311/Downloads/62-64.tif")
dem13 = read_stars("C:/Users/lk311/Downloads/64-66.tif")
dem14 = read_stars("C:/Users/lk311/Downloads/66-68.tif")
dem15 = read_stars("C:/Users/lk311/Downloads/68-70.tif")

l1 = raster("C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/workdir2/test_1.tif")
plot(l1)

plot(dem1, breaks = "equal", col = terrain.colors(10), axes = TRUE)
plot(dem2, breaks = "equal", col = terrain.colors(10), axes = TRUE)

dim(dem1)
dim(dem2)
st_bbox(dem1)
st_bbox(dem2)
dem = st_mosaic(dem1, dem2,dem3, dem4,dem5, dem6, dem7,dem8, dem9,dem10, dem11, dem12, dem13)
names(dem) = "soil"
plot(dem, axes = TRUE, breaks = "equal", col = terrain.colors(10))

dem <- raster(dem)
library(ggplot2)
writeRaster(dem, filename ="C:/Users/lk311/Downloads/long20-22.tif", overwrite = TRUE)
save(dem, file = "dem.RData")
save.image()

write_stars(st_as_stars(dem), dsn = "gg.tif")







r1 <- raster("C:/Users/lk311/Downloads/Biomod/out3.tif")
r2 <- raster("C:/Users/lk311/Downloads/Biomod/out4.tif")
r3 = raster("C:/Users/lk311/Downloads/44-46.tif")
r4 = raster("C:/Users/lk311/Downloads/46-48.tif")
r5 = raster("C:/Users/lk311/Downloads/48-50.tif")
r6 = raster("C:/Users/lk311/Downloads/50-52.tif")
r7 = raster("C:/Users/lk311/Downloads/52-54.tif")
r8 = raster("C:/Users/lk311/Downloads/54-56.tif")
r9 = raster("C:/Users/lk311/Downloads/56-58.tif")
r10 = raster("C:/Users/lk311/Downloads/58-60.tif")
r11 = raster("C:/Users/lk311/Downloads/60-62.tif")
r12 = raster("C:/Users/lk311/Downloads/62-64.tif")
r13 = raster("C:/Users/lk311/Downloads/64-66.tif")
r14 = raster("C:/Users/lk311/Downloads/66-68.tif")
r15 = raster("C:/Users/lk311/Downloads/68-70.tif")


p1 <- raster("C:/Users/lk311/Downloads/long22_40_42.tif")
p2 <- raster("C:/Users/lk311/Downloads/long22_42_44.tif")
p3 = raster("C:/Users/lk311/Downloads/long22_44_46.tif")
p4 = raster("C:/Users/lk311/Downloads/long22_46_48.tif")
p5 = raster("C:/Users/lk311/Downloads/long22_48_50.tif")
p6 = raster("C:/Users/lk311/Downloads/long22_50_52.tif")
p7 = raster("C:/Users/lk311/Downloads/long22_52_54.tif")
p8 = raster("C:/Users/lk311/Downloads/long22_54_56.tif")
p9 = raster("C:/Users/lk311/Downloads/long22_56_58.tif")
p10 = raster("C:/Users/lk311/Downloads/long22_58_60.tif")
p11 = raster("C:/Users/lk311/Downloads/long22_60_62.tif")
p12 = raster("C:/Users/lk311/Downloads/long22_62_64.tif")
p13 = raster("C:/Users/lk311/Downloads/long22_64_66.tif")
p14 = raster("C:/Users/lk311/Downloads/long22_66_68.tif")
p15 = raster("C:/Users/lk311/Downloads/long22_68_70.tif")

o1 <- raster("C:/Users/lk311/Downloads/long24_40_42.tif")
o2 <- raster("C:/Users/lk311/Downloads/long24_42_44.tif")
o3 = raster("C:/Users/lk311/Downloads/long24_44_46.tif")
o4 = raster("C:/Users/lk311/Downloads/long24_46_48.tif")
o5 = raster("C:/Users/lk311/Downloads/long24_48_50.tif")
o6 = raster("C:/Users/lk311/Downloads/long24_50_52.tif")
o7 = raster("C:/Users/lk311/Downloads/long24_52_54.tif")
o8 = raster("C:/Users/lk311/Downloads/long24_54_56.tif")
o9 = raster("C:/Users/lk311/Downloads/long24_56_58.tif")
o10 = raster("C:/Users/lk311/Downloads/long24_58_60.tif")
o11 = raster("C:/Users/lk311/Downloads/long24_60_62.tif")
o12 = raster("C:/Users/lk311/Downloads/long24_62_64.tif")
o13 = raster("C:/Users/lk311/Downloads/long24_64_66.tif")
o14 = raster("C:/Users/lk311/Downloads/long24_66_68.tif")
o15 = raster("C:/Users/lk311/Downloads/long24_68_70.tif")

a1 <- raster("C:/Users/lk311/Downloads/long26_40_42.tif")
a2 <- raster("C:/Users/lk311/Downloads/long26_42_44.tif")
a3 = raster("C:/Users/lk311/Downloads/long26_44_46.tif")
a4 = raster("C:/Users/lk311/Downloads/long26_46_48.tif")
a5 = raster("C:/Users/lk311/Downloads/long26_48_50.tif")
a6 = raster("C:/Users/lk311/Downloads/long26_50_52.tif")
a7 = raster("C:/Users/lk311/Downloads/long26_52_54.tif")
a8 = raster("C:/Users/lk311/Downloads/long26_54_56.tif")
a9 = raster("C:/Users/lk311/Downloads/long26_56_58.tif")
a10 = raster("C:/Users/lk311/Downloads/long26_58_60.tif")
a11 = raster("C:/Users/lk311/Downloads/long26_60_62.tif")
a12 = raster("C:/Users/lk311/Downloads/long26_62_64.tif")
a13 = raster("C:/Users/lk311/Downloads/long26_64_66.tif")
a14 = raster("C:/Users/lk311/Downloads/long26_66_68.tif")
a15 = raster("C:/Users/lk311/Downloads/long26_68_70.tif")


u1 <- raster("C:/Users/lk311/Downloads/long28_40_42.tif")
u2 <- raster("C:/Users/lk311/Downloads/long28_42_44.tif")
u3 = raster("C:/Users/lk311/Downloads/long28_44_46.tif")
u4 = raster("C:/Users/lk311/Downloads/long28_46_48.tif")
u5 = raster("C:/Users/lk311/Downloads/long28_48_50.tif")
u6 = raster("C:/Users/lk311/Downloads/long28_50_52.tif")
u7 = raster("C:/Users/lk311/Downloads/long28_52_54.tif")
u8 = raster("C:/Users/lk311/Downloads/long28_54_56.tif")
u9 = raster("C:/Users/lk311/Downloads/long28_56_58.tif")
u10 = raster("C:/Users/lk311/Downloads/long28_58_60.tif")
u11 = raster("C:/Users/lk311/Downloads/long28_60_62.tif")
u12 = raster("C:/Users/lk311/Downloads/long28_62_64.tif")
u13 = raster("C:/Users/lk311/Downloads/long28_64_66.tif")
u14 = raster("C:/Users/lk311/Downloads/long28_66_68.tif")
u15 = raster("C:/Users/lk311/Downloads/long28_68_70.tif")

s1 <- raster("C:/Users/lk311/Downloads/long30_40_42.tif")
s2 <- raster("C:/Users/lk311/Downloads/long30_42_44.tif")
s3 = raster("C:/Users/lk311/Downloads/long30_44_46.tif")
s4 = raster("C:/Users/lk311/Downloads/long30_46_48.tif")
s5 = raster("C:/Users/lk311/Downloads/long30_48_50.tif")
s6 = raster("C:/Users/lk311/Downloads/long30_50_52.tif")
s7 = raster("C:/Users/lk311/Downloads/long30_52_54.tif")
s8 = raster("C:/Users/lk311/Downloads/long30_54_56.tif")
s9 = raster("C:/Users/lk311/Downloads/long30_56_58.tif")
s10 = raster("C:/Users/lk311/Downloads/long30_58_60.tif")
s11 = raster("C:/Users/lk311/Downloads/long30_60_62.tif")
s12 = raster("C:/Users/lk311/Downloads/long30_62_64.tif")
s13 = raster("C:/Users/lk311/Downloads/long30_64_66.tif")
s14 = raster("C:/Users/lk311/Downloads/long30_66_68.tif")
s15 = raster("C:/Users/lk311/Downloads/long30_68_70.tif")

d1 <- raster("C:/Users/lk311/Downloads/long32_40_42.tif")
d2 <- raster("C:/Users/lk311/Downloads/long32_42_44.tif")
d3 = raster("C:/Users/lk311/Downloads/long32_44_46.tif")
d4 = raster("C:/Users/lk311/Downloads/long32_46_48.tif")
d5 = raster("C:/Users/lk311/Downloads/long32_48_50.tif")
d6 = raster("C:/Users/lk311/Downloads/long32_50_52.tif")
d7 = raster("C:/Users/lk311/Downloads/long32_52_54.tif")
d8 = raster("C:/Users/lk311/Downloads/long32_54_56.tif")
d9 = raster("C:/Users/lk311/Downloads/long32_56_58.tif")
d10 = raster("C:/Users/lk311/Downloads/long32_58_60.tif")
d11 = raster("C:/Users/lk311/Downloads/long32_60_62.tif")
d12 = raster("C:/Users/lk311/Downloads/long32_62_64.tif")
d13 = raster("C:/Users/lk311/Downloads/long32_64_66.tif")
d14 = raster("C:/Users/lk311/Downloads/long32_66_68.tif")
d15 = raster("C:/Users/lk311/Downloads/long32_68_70.tif")
plot(d7)

q1 <- raster("C:/Users/lk311/Downloads/long34_40_42.tif")
q2 <- raster("C:/Users/lk311/Downloads/long34_42_44.tif")
q3 = raster("C:/Users/lk311/Downloads/long34_44_46.tif")
q4 = raster("C:/Users/lk311/Downloads/long34_46_48.tif")
q5 = raster("C:/Users/lk311/Downloads/long34_48_50.tif")
q6 = raster("C:/Users/lk311/Downloads/long34_50_52.tif")
q7 = raster("C:/Users/lk311/Downloads/long34_52_54.tif")
q8 = raster("C:/Users/lk311/Downloads/long34_54_56.tif")
q9 = raster("C:/Users/lk311/Downloads/long34_56_58.tif")
q10 = raster("C:/Users/lk311/Downloads/long34_58_60.tif")
q11 = raster("C:/Users/lk311/Downloads/long34_60_62.tif")
q12 = raster("C:/Users/lk311/Downloads/long34_62_64.tif")
q13 = raster("C:/Users/lk311/Downloads/long34_64_66.tif")
q14 = raster("C:/Users/lk311/Downloads/long34_66_68.tif")
q15 = raster("C:/Users/lk311/Downloads/long34_68_70.tif")

g1 <- raster("C:/Users/lk311/Downloads/long36_40_42.tif")
g2 <- raster("C:/Users/lk311/Downloads/long36_42_44.tif")
g3 = raster("C:/Users/lk311/Downloads/long36_44_46.tif")
g4 = raster("C:/Users/lk311/Downloads/long36_46_48.tif")
g5 = raster("C:/Users/lk311/Downloads/long36_48_50.tif")
g6 = raster("C:/Users/lk311/Downloads/long36_50_52.tif")
g7 = raster("C:/Users/lk311/Downloads/long36_52_54.tif")
g8 = raster("C:/Users/lk311/Downloads/long36_54_56.tif")
g9 = raster("C:/Users/lk311/Downloads/long36_56_58.tif")
g10 = raster("C:/Users/lk311/Downloads/long36_58_60.tif")
g11 = raster("C:/Users/lk311/Downloads/long36_60_62.tif")
g12 = raster("C:/Users/lk311/Downloads/long36_62_64.tif")
g13 = raster("C:/Users/lk311/Downloads/long36_64_66.tif")
g14 = raster("C:/Users/lk311/Downloads/long36_66_68.tif")
g15 = raster("C:/Users/lk311/Downloads/long36_68_70.tif")

h1 <- raster("C:/Users/lk311/Downloads/long38_40_42.tif")
h2 <- raster("C:/Users/lk311/Downloads/long38_42_44.tif")
h3 = raster("C:/Users/lk311/Downloads/long38_44_46.tif")
h4 = raster("C:/Users/lk311/Downloads/long38_46_48.tif")
h5 = raster("C:/Users/lk311/Downloads/long38_48_50.tif")
h6 = raster("C:/Users/lk311/Downloads/long38_50_52.tif")
h7 = raster("C:/Users/lk311/Downloads/long38_52_54.tif")
h8 = raster("C:/Users/lk311/Downloads/long38_54_56.tif")
h9 = raster("C:/Users/lk311/Downloads/long38_56_58.tif")
h10 = raster("C:/Users/lk311/Downloads/long38_58_60.tif")
h11 = raster("C:/Users/lk311/Downloads/long38_60_62.tif")
h12 = raster("C:/Users/lk311/Downloads/long38_62_64.tif")
h13 = raster("C:/Users/lk311/Downloads/long38_64_66.tif")
h14 = raster("C:/Users/lk311/Downloads/long38_66_68.tif")
h15 = raster("C:/Users/lk311/Downloads/long38_68_70.tif")

l1 <- raster("C:/Users/lk311/Downloads/long40_40_42.tif")
l2 <- raster("C:/Users/lk311/Downloads/long40_42_44.tif")
l3 = raster("C:/Users/lk311/Downloads/long40_44_46.tif")
l4 = raster("C:/Users/lk311/Downloads/long40_46_48.tif")
l5 = raster("C:/Users/lk311/Downloads/long40_48_50.tif")
l6 = raster("C:/Users/lk311/Downloads/long40_50_52.tif")
l7 = raster("C:/Users/lk311/Downloads/long40_52_54.tif")
l8 = raster("C:/Users/lk311/Downloads/long40_54_56.tif")
l9 = raster("C:/Users/lk311/Downloads/long40_56_58.tif")
l10 = raster("C:/Users/lk311/Downloads/long40_58_60.tif")
l11 = raster("C:/Users/lk311/Downloads/long40_60_62.tif")
l12 = raster("C:/Users/lk311/Downloads/long40_62_64.tif")
l13 = raster("C:/Users/lk311/Downloads/long40_64_66.tif")
l14 = raster("C:/Users/lk311/Downloads/long40_66_68.tif")
l15 = raster("C:/Users/lk311/Downloads/long40_68_70.tif")

t1 <- raster("C:/Users/lk311/Downloads/long42_40_42.tif")
t2 <- raster("C:/Users/lk311/Downloads/long42_42_44.tif")
t3 = raster("C:/Users/lk311/Downloads/long42_44_46.tif")
t4 = raster("C:/Users/lk311/Downloads/long42_46_48.tif")
t5 = raster("C:/Users/lk311/Downloads/long42_48_50.tif")
t6 = raster("C:/Users/lk311/Downloads/long42_50_52.tif")
t7 = raster("C:/Users/lk311/Downloads/long42_52_54.tif")
t8 = raster("C:/Users/lk311/Downloads/long42_54_56.tif")
t9 = raster("C:/Users/lk311/Downloads/long42_56_58.tif")
t10 = raster("C:/Users/lk311/Downloads/long42_58_60.tif")
t11 = raster("C:/Users/lk311/Downloads/long42_60_62.tif")
t12 = raster("C:/Users/lk311/Downloads/long42_62_64.tif")
t13 = raster("C:/Users/lk311/Downloads/long42_64_66.tif")
t14 = raster("C:/Users/lk311/Downloads/long42_66_68.tif")
t15 = raster("C:/Users/lk311/Downloads/long42_68_70.tif")

f1 <- raster("C:/Users/lk311/Downloads/long44_40_42.tif")
f2 <- raster("C:/Users/lk311/Downloads/long44_42_44.tif")
f3 = raster("C:/Users/lk311/Downloads/long44_44_46.tif")
f4 = raster("C:/Users/lk311/Downloads/long44_46_48.tif")
f5 = raster("C:/Users/lk311/Downloads/long44_48_50.tif")
f6 = raster("C:/Users/lk311/Downloads/long44_50_52.tif")
f7 = raster("C:/Users/lk311/Downloads/long44_52_54.tif")
f8 = raster("C:/Users/lk311/Downloads/long44_54_56.tif")
f9 = raster("C:/Users/lk311/Downloads/long44_56_58.tif")
f10 = raster("C:/Users/lk311/Downloads/long44_58_60.tif")
f11 = raster("C:/Users/lk311/Downloads/long44_60_62.tif")
f12 = raster("C:/Users/lk311/Downloads/long44_62_64.tif")
f13 = raster("C:/Users/lk311/Downloads/long44_64_66.tif")
f14 = raster("C:/Users/lk311/Downloads/long44_66_68.tif")
f15 = raster("C:/Users/lk311/Downloads/long44_68_70.tif")

missed1 <- raster("C:/Users/lk311/Downloads/long28_64_66(missed).tif")
missed2 <- raster("C:/Users/lk311/Downloads/long32_50_52(missed).tif")



res(r2) <- c(xres(r1), yres(r1))
values(r2) <- 1:ncell(r2)
rm <- merge(r1, r2)
plot(r2)
origin(r1)
origin(r2)
rm <- mosaic(r1, r2)
plot(rm)

x <- list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15, tolerance=0.5)
# add arguments such as filename
# x$filename <- 'test.tif'
m <- do.call(merge, x)
# }
plot(m)
writeRaster(m,'m3.tif',options=c('TFW=YES'))

y <- list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,missed1, missed2, tolerance=0.5)
# add arguments such as filename
# x$filename <- 'test.tif'
n <- do.call(merge, y)
# }
plot(n)

t_testj <- merge(m,n, tolerance=0.5)
plot(t_testj)

b1 <- raster("C:/Users/lk311/Downloads/long46_40_42.tif")
b2 <- raster("C:/Users/lk311/Downloads/long46_42_44.tif")
b3 = raster("C:/Users/lk311/Downloads/long46_44_46.tif")
b4 = raster("C:/Users/lk311/Downloads/long46_46_48.tif")
b5 = raster("C:/Users/lk311/Downloads/long46_48_50.tif")
b6 = raster("C:/Users/lk311/Downloads/long46_50_52.tif")
b7 = raster("C:/Users/lk311/Downloads/long46_52_54.tif")
b8 = raster("C:/Users/lk311/Downloads/long46_54_56.tif")
b9 = raster("C:/Users/lk311/Downloads/long46_56_58.tif")
b10 = raster("C:/Users/lk311/Downloads/long46_58_60.tif")
b11 = raster("C:/Users/lk311/Downloads/long46_60_62.tif")
b12 = raster("C:/Users/lk311/Downloads/long46_62_64.tif")
b13 = raster("C:/Users/lk311/Downloads/long46_64_66.tif")
b14 = raster("C:/Users/lk311/Downloads/long46_66_68.tif")
b15 = raster("C:/Users/lk311/Downloads/long46_68_70.tif")

i1 <- raster("C:/Users/lk311/Downloads/long48_40_42.tif")
i2 <- raster("C:/Users/lk311/Downloads/long48_42_44.tif")
i3 = raster("C:/Users/lk311/Downloads/long48_44_46.tif")
i4 = raster("C:/Users/lk311/Downloads/long48_46_48.tif")
i5 = raster("C:/Users/lk311/Downloads/long48_48_50.tif")
i6 = raster("C:/Users/lk311/Downloads/long48_50_52.tif")
i7 = raster("C:/Users/lk311/Downloads/long48_52_54.tif")
i8 = raster("C:/Users/lk311/Downloads/long48_54_56.tif")
i9 = raster("C:/Users/lk311/Downloads/long48_56_58.tif")
i10 = raster("C:/Users/lk311/Downloads/long48_58_60.tif")
i11 = raster("C:/Users/lk311/Downloads/long48_60_62.tif")
i12 = raster("C:/Users/lk311/Downloads/long48_62_64.tif")
i13 = raster("C:/Users/lk311/Downloads/long48_64_66.tif")
i14 = raster("C:/Users/lk311/Downloads/long48_66_68.tif")
i15 = raster("C:/Users/lk311/Downloads/long48_68_70.tif")

x1 <- raster("C:/Users/lk311/Downloads/long50_40_42.tif")
x2 <- raster("C:/Users/lk311/Downloads/long50_42_44.tif")
x3 = raster("C:/Users/lk311/Downloads/long50_44_46.tif")
x4 = raster("C:/Users/lk311/Downloads/long50_46_48.tif")
x5 = raster("C:/Users/lk311/Downloads/long50_48_50.tif")
x6 = raster("C:/Users/lk311/Downloads/long50_50_52.tif")
x7 = raster("C:/Users/lk311/Downloads/long50_52_54.tif")
x8 = raster("C:/Users/lk311/Downloads/long50_54_56.tif")
x9 = raster("C:/Users/lk311/Downloads/long50_56_58.tif")
x10 = raster("C:/Users/lk311/Downloads/long50_58_60.tif")
x11 = raster("C:/Users/lk311/Downloads/long50_60_62.tif")
x12 = raster("C:/Users/lk311/Downloads/long50_62_64.tif")
x13 = raster("C:/Users/lk311/Downloads/long50_64_66.tif")
x14 = raster("C:/Users/lk311/Downloads/long50_66_68.tif")
x15 = raster("C:/Users/lk311/Downloads/long50_68_70.tif")

w1 <- raster("C:/Users/lk311/Downloads/long52_40_42.tif")
w2 <- raster("C:/Users/lk311/Downloads/long52_42_44.tif")
w3 = raster("C:/Users/lk311/Downloads/long52_44_46.tif")
w4 = raster("C:/Users/lk311/Downloads/long52_46_48.tif")
w5 = raster("C:/Users/lk311/Downloads/long52_48_50.tif")
w6 = raster("C:/Users/lk311/Downloads/long52_50_52.tif")
w7 = raster("C:/Users/lk311/Downloads/long52_52_54.tif")
w8 = raster("C:/Users/lk311/Downloads/long52_54_56.tif")
w9 = raster("C:/Users/lk311/Downloads/long52_56_58.tif")
w10 = raster("C:/Users/lk311/Downloads/long52_58_60.tif")
w11 = raster("C:/Users/lk311/Downloads/long52_60_62.tif")
w12 = raster("C:/Users/lk311/Downloads/long52_62_64.tif")
w13 = raster("C:/Users/lk311/Downloads/long52_64_66.tif")
w14 = raster("C:/Users/lk311/Downloads/long52_66_68.tif")
w15 = raster("C:/Users/lk311/Downloads/long52_68_70.tif")

e1 <- raster("C:/Users/lk311/Downloads/long54_40_42 (2).tif")
e2 <- raster("C:/Users/lk311/Downloads/long54_42_44 (2).tif")
e3 = raster("C:/Users/lk311/Downloads/long54_44_46 (2).tif")
e4 = raster("C:/Users/lk311/Downloads/long54_46_48 (2).tif")
e5 = raster("C:/Users/lk311/Downloads/long54_48_50 (2).tif")
e6 = raster("C:/Users/lk311/Downloads/long54_50_52 (2).tif")
e7 = raster("C:/Users/lk311/Downloads/long54_52_54 (2).tif")
e8 = raster("C:/Users/lk311/Downloads/long54_54_56 (2).tif")
e9 = raster("C:/Users/lk311/Downloads/long54_56_58 (2).tif")
e10 = raster("C:/Users/lk311/Downloads/long54_58_60 (2).tif")
e11 = raster("C:/Users/lk311/Downloads/long54_60_62 (2).tif")
e12 = raster("C:/Users/lk311/Downloads/long54_62_64 (2).tif")
e13 = raster("C:/Users/lk311/Downloads/long54_64_66 (2).tif")
e14 = raster("C:/Users/lk311/Downloads/long54_66_68 (2).tif")
e15 = raster("C:/Users/lk311/Downloads/long54_68_70 (2).tif")

ee1 <- raster("C:/Users/lk311/Downloads/long56_40_42.tif")
ee2 <- raster("C:/Users/lk311/Downloads/long56_42_44.tif")
ee3 = raster("C:/Users/lk311/Downloads/long56_44_46.tif")
ee4 = raster("C:/Users/lk311/Downloads/long56_46_48.tif")
ee5 = raster("C:/Users/lk311/Downloads/long56_48_50.tif")
ee6 = raster("C:/Users/lk311/Downloads/long56_50_52.tif")
ee7 = raster("C:/Users/lk311/Downloads/long56_52_54.tif")
ee8 = raster("C:/Users/lk311/Downloads/long56_54_56.tif")
ee9 = raster("C:/Users/lk311/Downloads/long56_56_58.tif")
ee10 = raster("C:/Users/lk311/Downloads/long56_58_60.tif")
ee11 = raster("C:/Users/lk311/Downloads/long56_60_62.tif")
ee12 = raster("C:/Users/lk311/Downloads/long56_62_64.tif")
ee13 = raster("C:/Users/lk311/Downloads/long56_64_66.tif")
ee14 = raster("C:/Users/lk311/Downloads/long56_66_68.tif")
ee15 = raster("C:/Users/lk311/Downloads/long56_68_70.tif")


kl1 <- raster("C:/Users/lk311/Downloads/long58_40_42.tif")
kl2 <- raster("C:/Users/lk311/Downloads/long58_42_44.tif")
kl3 = raster("C:/Users/lk311/Downloads/long58_44_46.tif")
kl4 = raster("C:/Users/lk311/Downloads/long58_46_48.tif")
kl5 = raster("C:/Users/lk311/Downloads/long58_48_50.tif")
kl6 = raster("C:/Users/lk311/Downloads/long58_50_52.tif")
kl7 = raster("C:/Users/lk311/Downloads/long58_52_54.tif")
kl8 = raster("C:/Users/lk311/Downloads/long58_54_56.tif")
kl9 = raster("C:/Users/lk311/Downloads/long58_56_58.tif")
kl10 = raster("C:/Users/lk311/Downloads/long58_58_60.tif")
kl11 = raster("C:/Users/lk311/Downloads/long58_60_62.tif")
kl12 = raster("C:/Users/lk311/Downloads/long58_62_64.tif")
kl13 = raster("C:/Users/lk311/Downloads/long58_64_66.tif")
kl14 = raster("C:/Users/lk311/Downloads/long58_66_68.tif")
kl15 = raster("C:/Users/lk311/Downloads/long58_68_70.tif")

ww1 <- raster("C:/Users/lk311/Downloads/long60_40_42.tif")
ww2 <- raster("C:/Users/lk311/Downloads/long60_42_44.tif")
ww3 = raster("C:/Users/lk311/Downloads/long60_44_46.tif")
ww4 = raster("C:/Users/lk311/Downloads/long60_46_48.tif")
ww5 = raster("C:/Users/lk311/Downloads/long60_48_50.tif")
ww6 = raster("C:/Users/lk311/Downloads/long60_50_52.tif")
ww7 = raster("C:/Users/lk311/Downloads/long60_52_54.tif")
ww8 = raster("C:/Users/lk311/Downloads/long60_54_56.tif")
ww9 = raster("C:/Users/lk311/Downloads/long60_56_58.tif")
ww10 = raster("C:/Users/lk311/Downloads/long60_58_60.tif")
ww11 = raster("C:/Users/lk311/Downloads/long60_60_62.tif")
ww12 = raster("C:/Users/lk311/Downloads/long60_62_64.tif")
ww13 = raster("C:/Users/lk311/Downloads/long60_64_66.tif")
ww14 = raster("C:/Users/lk311/Downloads/long60_66_68.tif")
ww15 = raster("C:/Users/lk311/Downloads/long60_68_70.tif")

sw1 <- raster("C:/Users/lk311/Downloads/long62_40_42.tif")
sw2 <- raster("C:/Users/lk311/Downloads/long62_42_44.tif")
sw3 = raster("C:/Users/lk311/Downloads/long62_44_46.tif")
sw4 = raster("C:/Users/lk311/Downloads/long62_46_48.tif")
sw5 = raster("C:/Users/lk311/Downloads/long62_48_50.tif")
sw6 = raster("C:/Users/lk311/Downloads/long62_50_52.tif")
sw7 = raster("C:/Users/lk311/Downloads/long62_52_54.tif")
sw8 = raster("C:/Users/lk311/Downloads/long62_54_56.tif")
sw9 = raster("C:/Users/lk311/Downloads/long62_56_58.tif")
sw10 = raster("C:/Users/lk311/Downloads/long62_58_60.tif")
sw11 = raster("C:/Users/lk311/Downloads/long62_60_62.tif")
sw12 = raster("C:/Users/lk311/Downloads/long62_62_64.tif")
sw13 = raster("C:/Users/lk311/Downloads/long62_64_66.tif")
sw14 = raster("C:/Users/lk311/Downloads/long62_66_68.tif")
sw15 = raster("C:/Users/lk311/Downloads/long62_68_70.tif")

aw1 <- raster("C:/Users/lk311/Downloads/long62_40_42.tif")
aw2 <- raster("C:/Users/lk311/Downloads/long62_42_44.tif")
aw3 = raster("C:/Users/lk311/Downloads/long62_44_46.tif")
aw4 = raster("C:/Users/lk311/Downloads/long62_46_48.tif")
aw5 = raster("C:/Users/lk311/Downloads/long62_48_50.tif")
aw6 = raster("C:/Users/lk311/Downloads/long62_50_52.tif")
aw7 = raster("C:/Users/lk311/Downloads/long62_52_54.tif")
aw8 = raster("C:/Users/lk311/Downloads/long62_54_56.tif")
aw9 = raster("C:/Users/lk311/Downloads/long62_56_58.tif")
aw10 = raster("C:/Users/lk311/Downloads/long62_58_60.tif")
aw11 = raster("C:/Users/lk311/Downloads/long62_60_62.tif")
aw12 = raster("C:/Users/lk311/Downloads/long62_62_64.tif")
aw13 = raster("C:/Users/lk311/Downloads/long62_64_66.tif")
aw14 = raster("C:/Users/lk311/Downloads/long62_66_68.tif")
aw15 = raster("C:/Users/lk311/Downloads/long62_68_70.tif")

dw1 <- raster("C:/Users/lk311/Downloads/long64_40_42.tif")
dw2 <- raster("C:/Users/lk311/Downloads/long64_42_44.tif")
dw3 = raster("C:/Users/lk311/Downloads/long64_44_46.tif")
dw4 = raster("C:/Users/lk311/Downloads/long64_46_48.tif")
dw5 = raster("C:/Users/lk311/Downloads/long64_48_50.tif")
dw6 = raster("C:/Users/lk311/Downloads/long64_50_52.tif")
dw7 = raster("C:/Users/lk311/Downloads/long64_52_54.tif")
dw8 = raster("C:/Users/lk311/Downloads/long64_54_56.tif")
dw9 = raster("C:/Users/lk311/Downloads/long64_56_58.tif")
dw10 = raster("C:/Users/lk311/Downloads/long64_58_60.tif")
dw11 = raster("C:/Users/lk311/Downloads/long64_60_62.tif")
dw12 = raster("C:/Users/lk311/Downloads/long64_62_64.tif")
dw13 = raster("C:/Users/lk311/Downloads/long64_64_66.tif")
dw14 = raster("C:/Users/lk311/Downloads/long64_66_68.tif")
dw15 = raster("C:/Users/lk311/Downloads/long64_68_70.tif")

z1 <- raster("C:/Users/lk311/Downloads/long66_40_42.tif")
z2 <- raster("C:/Users/lk311/Downloads/long66_42_44.tif")
z3 = raster("C:/Users/lk311/Downloads/long66_44_46.tif")
z4 = raster("C:/Users/lk311/Downloads/long66_46_48.tif")
z5 = raster("C:/Users/lk311/Downloads/long66_48_50.tif")
z6 = raster("C:/Users/lk311/Downloads/long66_50_52.tif")
z7 = raster("C:/Users/lk311/Downloads/long66_52_54.tif")
z8 = raster("C:/Users/lk311/Downloads/long66_54_56.tif")
z9 = raster("C:/Users/lk311/Downloads/long66_56_58.tif")
z10 = raster("C:/Users/lk311/Downloads/long66_58_60.tif")
z11 = raster("C:/Users/lk311/Downloads/long66_60_62.tif")
z12 = raster("C:/Users/lk311/Downloads/long66_62_64.tif")
z13 = raster("C:/Users/lk311/Downloads/long66_64_66.tif")
z14 = raster("C:/Users/lk311/Downloads/long66_66_68.tif")
z15 = raster("C:/Users/lk311/Downloads/long66_68_70.tif")

jw1 <- raster("C:/Users/lk311/Downloads/long68_40_42.tif")
jw2 <- raster("C:/Users/lk311/Downloads/long68_42_44.tif")
jw3 = raster("C:/Users/lk311/Downloads/long68_44_46.tif")
jw4 = raster("C:/Users/lk311/Downloads/long68_46_48.tif")
jw5 = raster("C:/Users/lk311/Downloads/long68_48_50.tif")
jw6 = raster("C:/Users/lk311/Downloads/long68_50_52.tif")
jw7 = raster("C:/Users/lk311/Downloads/long68_52_54.tif")
jw8 = raster("C:/Users/lk311/Downloads/long68_54_56.tif")
jw9 = raster("C:/Users/lk311/Downloads/long68_56_58.tif")
jw10 = raster("C:/Users/lk311/Downloads/long68_58_60.tif")
jw11 = raster("C:/Users/lk311/Downloads/long68_60_62.tif")
jw12 = raster("C:/Users/lk311/Downloads/long68_62_64.tif")
jw13 = raster("C:/Users/lk311/Downloads/long68_64_66.tif")
jw14 = raster("C:/Users/lk311/Downloads/long68_66_68.tif")
jw15 = raster("C:/Users/lk311/Downloads/long68_68_70.tif")


list2 <- list(jw1,jw2,jw3,jw4,jw5,jw6,jw6,jw7,jw8,jw9,jw10,jw11,jw12,jw13,jw14,jw15,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,dw1,dw2,dw3,dw4,dw5,dw6,dw7,dw8,dw9,dw10,dw11,dw12,dw13,dw14,dw15,aw1,aw2,aw3,aw4,aw5,aw6,aw7,aw8,aw9,aw10,aw11,aw12,aw13,aw14,aw15,sw1,sw2,sw3,sw4,sw5,sw6,sw7,sw8,sw9,sw10,sw11,sw12,sw13,sw14,sw15,ww1,ww2,ww3,ww4,ww5,ww6,ww7,ww8,ww9,ww10,ww11,ww12,ww13,ww14,ww15,kl1,kl2,kl3,kl4,kl5,kl6,kl7,kl8,kl9,kl10,kl11,kl12,kl13,kl14,kl15,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,ee1,ee2,ee3,ee4,ee5,ee6,ee7,ee8,ee9,ee10,ee11,ee12,ee13,ee14,ee15,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15, tolerance=0.5)
# add arguments such as filename
# x$filename <- 'test.tif'
part2 <- do.call(merge, list2)
# }
plot(part2)
writeRaster(part2,'m3.tif',options=c('TFW=YES'))
plot(e1)

jok <- merge(t_testj, part2, tolerance=0.5)
plot(jok)
writeRaster(jok,'sand_full.tif',options=c('TFW=YES'))

soil <- 
  raster::stack(
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_soc_5-15.asc',
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_silt_5-15.asc',
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_sand_5-15.asc',
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_cec_5-15.asc',
    'C:/Users/lk311/Downloads/Biomod/soilgrids/soil_russia_2500m/_cfvo_5-15.asc'
  )


plot(soil)
sand <- 
  raster::stack(
    'C:/Users/lk311/Downloads/sand_full.tif'
  )

plot(sand)
res(sand)

##agregate
sand.aggregate <- aggregate(sand, fact=10)
res(sand.aggregate)


writeRaster(sand.aggregate, filename="sand_allint.asc", format = "ascii", datatype='INT4S', overwrite=TRUE)



bioclim <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_1_i.asc',
      bio_2  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_2_i.asc',
      bio_3  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_3_i.asc',
      bio_4  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_4_i.asc',
      bio_5  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_5_i.asc',
      bio_6  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_6_i.asc',
      bio_7  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_7_i.asc',
      bio_8  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_8_i.asc',
      bio_9  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_9_i.asc',
      bio_10  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_10_i.asc',
      bio_11  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_11_i.asc',
      bio_12  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_12_i.asc',
      bio_13  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_13_i.asc',
      bio_14  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_14_i.asc',
      bio_15  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_15_i.asc',
      bio_16  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_16_i.asc',
      bio_17  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_17_i.asc',
      bio_18  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_18_i.asc',
      bio_19  = 'C:/Users/lk311/Downloads/wc2.1_10m_bio/_bio_19_i.asc'
    )
  )

plot(bioclim$bio_1)

elev <- 
  raster::stack(
    'C:/Users/lk311/Downloads/elev_current.asc'
  )

r2 <- crop(bioclim,extent(elev))
r22 <- raster(vals=values(r2),ext=extent(elev), nrows=dim(elev)[1],ncols=dim(elev)[2])
r22
stack(r22,r1)

library(SDMtune)

p_coords <- HS_occ[HS_occ$Heracleum.sosnowskyi %in% c(1), ]
p_coords <- p_coords[,2:3]
p_coords


a_coords <- HS_occ[HS_occ$Heracleum.sosnowskyi %in% c(0), ]
a_coords <- a_coords[,2:3]
a_coords

library(ggplot2)

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = p_coords, aes(x = long, y = lat), color = "red",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = as.data.frame(a_coords), aes(x = long, y = lat),
              color = "blue", alpha = 0.4, size = 0.5) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

data <- prepareSWD(species = "Heracleum sosnowskyi", p = p_coords, a = a_coords,
                   env = r2)
head(data@data)
head(data@coords)
data@species

model <- train(method = "Maxent", data = data)
model
slotNames(model)
slotNames(model@model)
model <- train(method = "Maxent", data = data, fc = "lqph", reg = 1, iter = 500)
model <- train(method = "Maxent", data = data, fc = "lh", reg = 0.5, iter = 700)


##make predictions
pred <- predict(model, data = data, type = "cloglog")
head(pred)
p <- data@data[data@pa == 1, ]
pred <- predict(model, data = p, type = "cloglog")
tail(pred)

##Create distr map
map <- predict(model, data = bioclim, type = "cloglog")
map
map <- predict(model, data = bioclim, type = "cloglog", file = "my_map",
               format = "GTiff")
e = raster::extent(c(19, 71, 40, 68))
# Now use the extent to make the prediction
map_e <- predict(model, data = bioclim, type = "cloglog", extent = e)

##Plot map

plotPred(map)
plotPred(map, lt = "Habitat\nsuitability",
         colorramp = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))

ths <- thresholds(model, type = "cloglog")
ths

plotPA(map, th = ths[3, 2])
plotPA(map, th = ths[3, 2], filename = "my_pa_map", format = "GTiff")

##evaluation
auc(model)
#install.packages('plotROC')
library(plotROC)
plotROC(model)
tss(model)
aicc(model, env = predictors)


#Testing and training
library(zeallot)  # For unpacking assignment
c(train, test) %<-% trainValTest(data, test = 0.2, only_presence = TRUE,
                                 seed = 25)
model <- train("Maxent", data = train)
auc(model)
auc(model, test = test)
plotROC(model, test = test)



h <- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
gs <- gridSearch(model, hypers = h, metric = "auc", test = test)
head(gs@results[order(-gs@results$test_AUC), ])

##Cross val
folds <- randomFolds(data, k = 4, only_presence = TRUE, seed = 25)
cv_model <- train("Maxent", data = data, folds = folds)
cv_model
auc(cv_model)
auc(cv_model, test = TRUE)

