# import the list of annual mean june temp data

mcp_regions <- readRDS("Robjects/mcp_regions.RDS")
polygons_ras_pci <- readRDS("Robjects/all_polygons.RDS")
filelist_temp <- list.files(datapath("Climate/World Clim 1 yr Monthly Weather/LCC June and July/June"), full.names = TRUE)
temp_rasters <- rast(filelist_temp)
names(temp_rasters) <- c(1994:2018)

#### LOOK INTO THIS FUNCTION
#temp <- raster::getData("worldclim", var = "tmean", res = 10)


#convert mcps to spatvector

mcp_regions2 <- as(mcp_regions, "Spatial")
mcp_regions2 <- vect(mcp_regions2)
mcp_regions2 <- terra::project(mcp_regions2, crs(temp_rasters))


#extract the temps for each year for each plot


mean_temp <- terra::extract(temp_rasters, mcp_regions2, fun = mean, na.rm = TRUE)

mean_temp$region <- mcp_regions$region


mean_temp <- mean_temp %>%
  select(-ID) %>%
  pivot_longer(   cols = -region,
                  names_to = "year",
                  values_to = "meantemp",
                  values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year))


#make a time series of how temp has changed over time


ggplot(mean_temp, aes(x = year, y = meantemp, colour = region)) +
  geom_line()

#modeling has there been a change over time


mt_pci <- mean_temp %>%
  filter(region == "PCI")

mt_ras <- mean_temp %>%
  filter(region == "Rasmussen")

temp_ts_pci <- ts(data = mt_pci$meantemp,
              start = 1994,
              end = 2018)

temp_ts_ras <- ts(data = mt_ras$meantemp,
                  start = 1994,
                  end = 2018)

autoplot(temp_ts_pci) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(temp_ts_ras) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))



#test if there are changepoints

changepoints_pci = temp_ts_pci %>% 
  changepoint::cpt.meanvar() 

changepoints_pci %>% summary()


changepoints_pci %>% 
  param.est() %>% 
  data.frame() %>% 
  mutate(segment = c("First", "Second")) %>% select(segment, mean, variance) %>%
  kableExtra::kable(format = "html", digits = 2, 
                    caption = "Mean and variance of change point for daily sst",
                    col.names = c("Segment", "Mean", "Variance")) %>%
  kableExtra::column_spec(column = 1:3, width = "3cm") %>%
  kableExtra::add_header_above(c("", "Parameters" = 2))

#non-parametric
changepoints_pci_np = temp_ts_pci %>% changepoint.np::cpt.np() 

changepoints_pci_np %>% summary()

changepoints_pci_np%>% changepoint::plot()

#none

arima_pci <- arima(temp_ts_pci)



#######

changepoints_ras = temp_ts_ras %>% 
  changepoint::cpt.meanvar() 

changepoints_ras %>% summary()


changepoints_ras %>% 
  param.est() %>% 
  data.frame() %>% 
  mutate(segment = c("First", "Second")) %>% select(segment, mean, variance) %>%
  kableExtra::kable(format = "html", digits = 2, 
                    caption = "Mean and variance of change point for daily sst",
                    col.names = c("Segment", "Mean", "Variance")) %>%
  kableExtra::column_spec(column = 1:3, width = "3cm") %>%
  kableExtra::add_header_above(c("", "Parameters" = 2))

#non-parametric
changepoints_ras_np = temp_ts_ras %>% changepoint.np::cpt.np() 

changepoints_ras_np %>% summary()

changepoints_ras_np%>% changepoint::plot()

#none

#having the changepoint in the last section doesn't make sense to me
#both have only one point in the second section, so there is no variance




#detrending

temp_ts_pci3 <- SMA(temp_ts_pci, 3)
temp_ts_ras3 <- SMA(temp_ts_ras, 3)

temp_ts_pci5 <- SMA(temp_ts_pci, 5)
temp_ts_ras5 <- SMA(temp_ts_ras, 5)

temp_ts_pci8 <- SMA(temp_ts_pci, 8)
temp_ts_ras8 <- SMA(temp_ts_ras, 8)

autoplot(temp_ts_pci8) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))

autoplot(temp_ts_ras8) +
  labs(x = "year", y = expression(Temperature~(degree*C))) +
  scale_x_continuous(breaks = seq(1994,2018,2))



#regressions

lm_pci <- lm(meantemp ~ year, data = mt_pci)
summary(lm_pci)

ggplot(mt_pci, aes(x = year, y = meantemp)) +
  geom_line() +
  stat_smooth(method = "lm", col = "red")


lm_ras <- lm(meantemp ~ year, data = mt_ras)
summary(lm_ras)

ggplot(mt_ras, aes(x = year, y = meantemp)) +
  geom_line() +
  stat_smooth(method = "lm", col = "red")




ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_line() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(meantemp ~ year, data = mt_ras))
ggplotRegression(lm(meantemp ~ year, data = mt_pci))





##################### SNOW


#convert mcps to spatvector

mcp_regions3 <- as(mcp_regions, "Spatial")
mcp_regions3 <- vect(mcp_regions3)
mcp_regions3 <- terra::project(mcp_regions2, crs(snow_rasters))

snow_rasters2 <- terra::project(snow_rasters, crs(temp_rasters))


#extract the snow for each year for each region


mean_snow <- terra::extract(snow_rasters, mcp_regions2, fun = mean, na.rm = TRUE)

mean_snow$region <- mcp_regions$region


mean_snow <- mean_snow %>%
  select(-ID) %>%
  pivot_longer(   cols = -region,
                  names_to = "year",
                  values_to = "meansnow",
                  values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year))


#make a time series of how snow has changed over time


ggplot(mean_snow, aes(x = year, y = meansnow, colour = region)) +
  geom_line()



