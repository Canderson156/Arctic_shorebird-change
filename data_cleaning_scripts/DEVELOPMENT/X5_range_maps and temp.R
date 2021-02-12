####VERIFY THAT SPECIES ORDERS MATCH UP TO DATA WHEH ADDED SEPERATELY



tax <- read.csv(datapath("Range maps/taxonomy.csv")) %>%
  select(code, binomial)



#PRISM shorebird breeding shapefiles

#ranges <- readOGR(datapath("Range maps/PRISM_shorebird_ranges_breeding"), "PRISM_shorebird_ranges_breeding")


ranges <- st_read(datapath("Range maps/PRISM_shorebird_ranges_breeding"), "PRISM_shorebird_ranges_breeding")
#ranges <- st_read(datapath("Range maps/PRISM_shorebird_ranges_breeding"))



#save relevent columns

ranges <- ranges %>%
  select("binomial", "seasonal", "Shape_Area", "geometry")


#can't view for unkown reason, this object allows me to look at the data -geometry
test <- st_drop_geometry(ranges)




#merge together into 1 multipolygon per species

ranges2 <- ranges %>%
  group_by(binomial) %>%
  select(-seasonal, -Shape_Area) %>%
  summarize()
  

test2 <- st_drop_geometry(ranges2)



#Species Temperature Index


#slow. try with terra package

mean_temp <- raster(datapath("Climate/World Clim 30yr Monthly Climate/wc2.1_30s_tmin_06.tif"))
STI <- raster::extract(mean_temp, ranges2, fun = mean, na.rm = TRUE)

#add species names

STI <- data.frame(binomial = ranges2$binomial, STI = STI)
STI <- merge(STI, tax)

saveRDS(STI, "Robjects/STI.RDS")
STI <- readRDS("Robjects/STI.RDS")



#change in temperature over time

filelist_temp <- list.files(datapath("Climate/World Clim 1 yr Monthly Weather/LCC June and July/June"), full.names = TRUE)
temp_rasters <- rast(filelist_temp)
names(temp_rasters) <- c(1994:2018)


#convert ranges to spatial vector
ranges3 <- as(ranges2, "Spatial")
ranges3 <- vect(ranges3)
ranges3 <- terra::project(ranges3, crs(temp_rasters))


#error message can be ignored

tic()
temp_change <- terra::extract(temp_rasters, ranges3, fun = mean, na.rm = TRUE)
toc()


saveRDS(temp_change, "Robjects/temp_change.RDS")
temp_change <- readRDS("Robjects/temp_change.RDS")

temp_change$species <- STI$code

temp_change <- temp_change %>%
  select(-ID) %>%
  pivot_longer(   cols = -species,
                  names_to = "year",
                  values_to = "min_june_temp",
                  values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year))


temp_change2 <-  temp_change %>%
  filter(species %in% sb_rep2$Species)




lm_range_temp <- lmer(min_june_temp ~ year + (1|species), data = temp_change, REML = F)
summary(lm_range_temp)

qqnorm(residuals(lm_range_temp))

lme_range_temp <- lme(min_june_temp ~ year, random = ~1|species, data = temp_change, method = "ML")
summary(lme_range_temp)

lm_range_temp_null <- lmer(min_june_temp ~ (1|species), data = temp_change, REML = F)

lrtest(lm_range_temp_null, lm_range_temp)

#plotting temp change


ggplot(temp_change2, aes(x = year, y = min_june_temp, colour = species)) +
  geom_line(size = 1) +
  scale_x_continuous(n.breaks = 13)




#model by species

temp_change_sp <- temp_change2 %>%
  ungroup() %>%
  group_split(species)

names(temp_change_sp) <- unique(temp_change2$species)

lms_range_sp <- lapply(temp_change_sp, function(x) lm(min_june_temp ~ year, data = x))

lapply(lms_range_sp, summary)


#do these results make sense? Is worldclim adequate?



#changepoints


temp_ts_amgp <- ts(data = temp_change_sp[[12]]$min_june_temp,
                  start = 1994,
                  end = 2018)


changepoints_temp_amgp = temp_ts_amgp %>% 
  changepoint::cpt.meanvar() 

changepoints_temp_amgp %>% summary()



#detrending

temp_ts_amgp3 <- SMA(temp_ts_amgp, 3)

temp_ts_amgp4 <- SMA(temp_ts_amgp, 4)

temp_ts_amgp8 <- SMA(temp_ts_amgp, 8)

autoplot(temp_ts_amgp3) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(temp_ts_amgp4) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(temp_ts_amgp8) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))
