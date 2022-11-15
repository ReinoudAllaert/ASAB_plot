## Reproduce asab plots with dummy data
## Written by: Reinoud Allaert, reinoud.alaert@ugent.be

warning("THIS SCRIPT SOURCES A .tif file that needs manual downloading from:
https://land.copernicus.eu/pan-european/corine-land-cover")

# load and install all required packages
if (!require("raster")) install.packages("raster")  
if (!require("dplyr")) install.packages("dplyr")
if (!require("suncalc")) install.packages("suncalc")  
if (!require("dbscan")) install.packages("dbscan") 
if (!require("geosphere")) install.packages("geosphere")  
if (!require("rnaturalearth")) install.packages("rnaturalearth") 
if (!require("raster")) install.packages("raster")  
if (!require("sp")) install.packages("sp") 
if (!require("ggplot2")) install.packages("ggplot2") 
if (!require("rgdal")) install.packages("rgdal") 
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata") 
require(raster)
require(dplyr)
require(suncalc)
require(dbscan)
require(geosphere)
require(rnaturalearth)
require(rnaturalearthdata)
require(rgdal)
require(sp)
require(ggplot2)

## setwd
setwd("~/Documents/Github/ASAB_plot")
## dummy data
GPS_data <- read.csv("~/Documents/Github/ASAB_plot/dummy_gps_data.csv")

## corine land cover and legend

#### DOWNLOAD CORINE FILE FROM:
#### https://land.copernicus.eu/pan-european/corine-land-cover
corine <- raster("corine_land_cover.tif")
clc_legend <- read.csv("~/Documents/Github/ASAB_plot/clc_legend.csv")

## add date
GPS_data$Collecting_Date <- date(GPS_data$timestamp)

## merge land cover with GPS-position, add legend
GPS_data$Corine_grid_code   <- raster::extract(corine,SpatialPoints(GPS_data[,c("location.long","location.lat")],
                                                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))  )
GPS_data$Corine_type  <- clc_legend[match(GPS_data$Corine_grid_code,clc_legend$GRID_CODE),"CLC_CODE"]
GPS_data$Corine_label  <- clc_legend[match(GPS_data$Corine_grid_code,clc_legend$GRID_CODE),"LABEL2"]

## add timedif
GPS_data <- GPS_data %>%
  arrange(individual.local.identifier, timestamp) %>%
  group_by(individual.local.identifier) %>%
  mutate(diff_sec = strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1]))

## convert to minutes
GPS_data$delta <- as.numeric(GPS_data$diff_sec)/60

## add column with sun height in radians

data <- data.frame(date = GPS_data$timestamp,
                   lat = GPS_data$location.lat,
                   lon = GPS_data$location.long)

sun_calc <- getSunlightPosition(data = data, 
                                keep = c("altitude"))

GPS_data$Sun_height <- sun_calc$altitude

## 0.17 rad = 10 deg
## assuming that birds only start foraging as soon as sun is over 10 degrees under horizon

GPS_data$day <- "day"
GPS_data$day[GPS_data$Sun_height <= -0.17] <- "night"


## crude way of behaviour labeling
## start unlabeled
GPS_data$beh <- "no label"

## if speed lower than 0.5m/s and no sun <- roost
GPS_data$beh[GPS_data$Sun_height <= -0.26 & GPS_data$ground.speed <= 0.2] <- "roosting"

## if speed lover than 0.5m/s and sun <- roost-day
GPS_data$beh[GPS_data$Sun_height >= -0.26 & GPS_data$ground.speed <= 0.2] <- "resting"

## if speed higher than 4m/s <- flight
GPS_data$beh[GPS_data$ground.speed >= 4] <- "flight"

## inbetween speeds during day = foraging
GPS_data$beh[GPS_data$ground.speed >= 0.2 & GPS_data$ground.speed <= 4 & GPS_data$day == 'day'] <- "foraging" 



## classify Corine values
## Corine values <200 (i.e. ‘artificial surfaces’) were thereby considered as ‘urban habitat
## values between 200 and 300 (i.e. ‘agricultural areas’) were  considered  as  ‘agricultural  habitat’
## values  above  520 (i.e. ‘marine waters’) were considered as ‘marine habitat’
GPS_data$habitat <- "Not classified"
GPS_data$habitat[GPS_data$Corine_type < 200] <- "Urban" 
GPS_data$habitat[GPS_data$Corine_type >= 200 & GPS_data$Corine_type < 300] <- "Agriculture"
GPS_data$habitat[GPS_data$Corine_type >= 420 & GPS_data$Corine_type <= 522] <- "Intertidal"
GPS_data$habitat[GPS_data$Corine_type == 523] <- "Marine"

## manually add colour
GPS_data$colour <- "white"
GPS_data$colour[GPS_data$habitat == "Urban"] <- "brown" 
GPS_data$colour[GPS_data$habitat == "Agriculture"] <- "green" 
GPS_data$colour[GPS_data$habitat == "Intertidal"] <- "cornsilk" 
GPS_data$colour[GPS_data$habitat == "Marine"] <- "blue" 


GPS_data_day <- subset(GPS_data, (day == 'day'))
GPS_data_night <- subset(GPS_data, (day == 'night'))

###### level 1: Habitat use

GPS_forage <- subset(GPS_data_day, (beh == 'foraging'))
foraging_table <- GPS_forage %>% 
  group_by(colour_ring, Collecting_Date, Corine_label) %>% 
  summarise(Frequency = sum(delta))

foraging_table$Frequency <- as.character(foraging_table$Frequency)

## quick overview of foraging proportions
table(GPS_forage$habitat)
prop.table(table(GPS_forage$habitat))*100

temp <- GPS_forage %>% 
  group_by(habitat) %>% 
  summarise(Frequency = sum(delta)/(60*24))

temp$Frequency <- 100*temp$Frequency/(sum(temp$Frequency))

## plot frequencies
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )



ggplot(temp, aes(x = "", y = Frequency, fill = habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(Frequency, 1)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer() + 
  guides(fill = guide_legend(title = "Habitat")) +
  blank_theme


## get random bird from dataset
random_bird <- sample(unique(GPS_forage$colour_ring), 1)
random_bird <- subset(GPS_forage, (colour_ring == random_bird))

## plot track on map
worldmap <- ne_countries(
  scale = "medium", type = "map_units",
  returnclass = "sf"
)
xlim <- range(random_bird$location.long) + 0.1 * c(-1, 1)
ylim <- range(random_bird$location.lat) + 0.1 * c(-1, 1)

## compensate for crs offset
random_bird$location.lat <- random_bird$location.lat + 0.02

## make colour pal
pal <- random_bird |>
  dplyr::distinct(habitat, colour) 
pal <- subset(pal, select=-c(individual.local.identifier))|>
  tibble::deframe()

## plot
base <- ggplot() +
  geom_sf(data = worldmap) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", title = unique(random_bird$colour_ring)) +
  theme(plot.title = element_text(hjust = 0.5), legend.key = element_rect(fill = "grey80"))+
  geom_path(data = random_bird, aes(x = location.long, y = location.lat, colour = colour)) +
  scale_color_identity(guide = "legend")+
  geom_path(data = random_bird, aes(x = location.long, y = location.lat, colour = habitat)) +
  scale_color_manual(values = pal)+
  theme(title=element_text(size=14, hjust=0.5),
        axis.title=element_text(size=12),
        axis.text = element_text(size=8))


base


## days since start
GPS_forage$date_diff <- as.Date(as.character(GPS_forage$timestamp))-
  as.Date(as.character(GPS_forage$deploy_start))

## freq per bird per habitat-type
Freq_df <- NULL
for (bird in unique(GPS_forage$colour_ring)) {
  random_bird <- GPS_forage %>% 
    filter(colour_ring == bird)
  random_bird <- random_bird %>% 
    filter(delta < 31)%>% 
    group_by(habitat) %>% 
    summarise(Frequency = sum(delta))%>% 
    mutate(ID = bird)
  random_bird$Frequency <- 100*random_bird$Frequency/(sum(random_bird$Frequency))
  
  Freq_df <- rbind(Freq_df,random_bird)
  
}

## plot 
ggplot(Freq_df, aes(x = ID, y = Frequency, fill = habitat)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Frequency), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Time spent foraging (%)") +
  xlab("Colour ring") + theme(axis.text.x=element_text(angle=90,hjust=1)) + ggtitle("Time expenditure") + theme(plot.title = element_text(hjust = 0.5))


## per 3 days, at least 9 days of data 
inter_spec <- GPS_forage %>% 
  filter(date_diff >= 9)
inter_spec <- unique(inter_spec$colour_ring)
GPS_data_9days <-  GPS_forage %>% 
  filter(colour_ring %in% inter_spec)%>% 
  filter(date_diff > 0)

## per 3 days
GPS_data_9days$date_diff <- ceiling(as.numeric(GPS_data_9days$date_diff)/3)
random_bird <- GPS_data_9days
random_bird <- random_bird %>% 
  filter(delta < 31)%>% 
  group_by(habitat, date_diff) %>% 
  summarise(Frequency = sum(delta))%>% 
  mutate(ID = unique(random_bird$colour_ring))

freqs <- NULL
for (date in unique(random_bird$date_diff)) { 
  temp <- random_bird %>% 
    filter(date_diff == date)
  temp$Frequency <- 100*temp$Frequency/(sum(temp$Frequency))
  freqs <- rbind(freqs, temp)
  
}

## plot over time
ggplot(freqs, aes(x = date_diff, y = Frequency, fill = habitat)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Frequency), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Time spent foraging (%)") +
  xlab("Days since release (per 3)") + theme(axis.text.x=element_text(angle=90,hjust=1)) + ggtitle(freqs$ID) + theme(plot.title = element_text(hjust = 0.5))


#### level 2
#### look at mean amount of plots visited per day 
## remove first and last incomplete day

GPS_forage %>% group_by(colour_ring) %>% 
  filter(Collecting_Date != max(Collecting_Date))%>% 
  filter(Collecting_Date != min(Collecting_Date))  -> filteredData


## only foraging data

GPS_forage <- filteredData
GPS_forage <- GPS_forage %>% 
  filter(day != "night")
GPS_forage$bird_day <- paste0(GPS_forage$colour_ring, "_",GPS_forage$Collecting_Date)

t <- split(GPS_forage, GPS_forage$bird_day)  

## for each bird_day check amount of foraging patches visited
FUN <- function(day){
  day$cluster <- NA
  y=day$location.lat
  x=day$location.long
  points = data.frame(x, y)
  t <- dbscan(points, eps = 0.010, minPts = 14)
  t <- (t[["cluster"]])
  day$cluster <-  t
  return(day)
  
}
t <- lapply(t, FUN)
t <- bind_rows(t, .id = "column_label")

t$cluster_ID <- paste0(t$bird_day, "_", t$cluster)

## graph of cumulative amount of patches over time
## keep only one row per cluster_ID
clusters <- t[!duplicated(t$cluster_ID),]

clusters <- clusters %>% 
  filter(cluster != 0)



## total amount of patches per bird:

FUN <- function(day){
  xy <- SpatialPointsDataFrame(
    matrix(c(day$location.long, day$location.lat), ncol=2), data.frame(ID=seq(1:length(day$location.lat))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  mdist <- distm(xy)
  d=500
  hc <- hclust(as.dist(mdist), method="complete")
  day$foraging_patch_ID <- cutree(hc, h=d)
  return(day)
}

t <- split(clusters, clusters$colour_ring)  

t <- lapply(t, FUN)
t <- bind_rows(t, .id = "column_label")




FUN <- function(day){
  day = day[order(day$date_diff), ]
  day$patch_id = as.integer(factor(day$foraging_patch_ID, levels = unique(day$foraging_patch_ID)))
  day$cum_patch = cummax(day$patch_id)
  return(day)
}

t <- split(t, t$colour_ring)  

t <- lapply(t, FUN)
t <- bind_rows(t, .id = "column_label")


# draw ggplot2 time series plot
ggplot(t,                            
       aes(x = date_diff, y = cum_patch ,group = colour_ring)) + geom_line()+ ylab("Cumulative amount of patches") +
  xlab("Days since release") + theme(text = element_text(size=10))+ ggtitle("Amount of patches visited since release") + theme(plot.title = element_text(hjust = 0.5))


