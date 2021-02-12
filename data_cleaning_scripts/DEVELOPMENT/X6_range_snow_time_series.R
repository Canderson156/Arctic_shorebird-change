
#load snowmelt data


filelist_snow <- list.files(datapath("Date of Snow Melt/Snowmelt_Timing_maps/data/Snowmelt_years"), full.names = TRUE)
snow_rasters <- rast(filelist_snow)
names(snow_rasters) <- c(2001:2015)


#convert ranges to spatial vector
ranges4 <- as(ranges2, "Spatial")
ranges4 <- vect(ranges4)
ranges4 <- terra::project(ranges4, crs(snow_rasters))


#extract snowcover melt data
#error message can be ignored

tic()
snow_change <- terra::extract(snow_rasters, ranges4, fun = mean, na.rm = TRUE)
toc()

#took 33 mins

saveRDS(snow_change, "Robjects/snow_change.RDS")
snow_change <- readRDS("Robjects/snow_change.RDS")


snow_change$species <- STI$code

snow_change <- snow_change %>%
  select(-ID) %>%
  pivot_longer(   cols = -species,
                  names_to = "year",
                  values_to = "snowmelt_DOY",
                  values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year))



snow_change2 <-  snow_change %>%
  filter(species %in% sb_rep2$Species)


lm_range_snow <- lmer(snowmelt_DOY ~ year + (1|species), data = snow_change, REML = F)
summary(lm_range_snow)


qqnorm(residuals(lm_range_snow))

lme_range_snow <- lme(snowmelt_DOY ~ year, random = ~1|species, data = snow_change, method = "ML")
summary(lme_range_snow)

lm_range_snow_null <- lmer(snowmelt_DOY ~ (1|species), data = snow_change, REML = F)

lrtest(lm_range_snow_null, lm_range_snow)







#plotting snow change


ggplot(snow_change2, aes(x = year, y = snowmelt_DOY, colour = species)) +
  geom_line(size = 1) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_reverse()




#model by species

snow_change_sp <- snow_change %>%
  ungroup() %>%
  group_split(species)

names(snow_change_sp) <- unique(snow_change$species)

lms_range_snow_sp <- lapply(snow_change_sp, function(x) lm(snowmelt_DOY ~ year, data = x))

lapply(lms_range_snow_sp, summary)

# some species yes, other species no









#changepoints


snow_ts_amgp <- ts(data = snow_change_sp[[12]]$snowmelt_DOY,
                   start = 2001,
                   end = 2015)


changepoints_snow_amgp = snow_ts_amgp %>% 
  changepoint::cpt.meanvar() 

changepoints_snow_amgp %>% summary()



#detrending

snow_ts_amgp3 <- SMA(snow_ts_amgp, 3)

snow_ts_amgp4 <- SMA(snow_ts_amgp, 4)

snow_ts_amgp8 <- SMA(snow_ts_amgp, 8)

autoplot(snow_ts_amgp3) +
  labs(x = "year", y = "Snowmelt DOY") +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(snow_ts_amgp4) +
  labs(x = "year", y = "Snowmelt DOY")  +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(snow_ts_amgp8) +
  labs(x = "year", y = "Snowmelt DOY")  +
  scale_x_continuous(breaks = seq(1994,2018,2))






# species snowmelt index



mean_snow <- rast(datapath("Date of Snow Melt/Snowmelt_Timing_maps/data/Snowmelt_summary/Snowmelt_Timing_North_America_Mean_2001_2015.tif"))
SSI <- terra::extract(mean_snow, ranges4, fun = mean, na.rm = TRUE)

#add species names

SSI <- data.frame(binomial = ranges2$binomial, SSI = SSI)
SSI <- merge(SSI, tax)

saveRDS(SSI, "Robjects/STI.RDS")
SSI <- readRDS("Robjects/STI.RDS")






