
#load data
sb_year <- readRDS("Robjects/sb_year.RDS")
all_polygons <- readRDS("Robjects/all_polygons.RDS")



#filter to keep only the years where there were repeated observations (only occured in two regions)
sb_rep <- sb_year %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year == 2019]) %>%
  filter(Species %notin% c("LESA", "SAND", "SEPL", "WHIM", "WISN")) %>%
  filter(Region_name %notin% c("North Archipelago"))
#why is this so slow?



#rename variables
#group the early surveys since they didn't all happen in the same year
#group the rasmussen plots from different region codes together
#keep the max number of birds observed in that plot at one time
#re-add the presence variable
#change plot area from km2 to ha



sb_rep2 <- sb_rep %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97" ),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region, Species, Plot_area) %>%
  summarize(n_birds = ceiling(mean(max_birds))) %>%
  ungroup() %>%
  mutate(presence = ifelse(n_birds == 0, FALSE, TRUE)) %>%
  mutate(Plot_area = Plot_area*100)




#filter only the plots I'm using in this analysis

keep <- sb_rep2$Plot

polygons_ras_pci <- all_polygons %>%
  filter(Plot %in% keep)

st_geometry(polygons_ras_pci) 
plot(polygons_ras_pci)

#merge with sb_rep data


keep <- sb_rep2 %>%
  select(Plot, time_period, region, Plot_area) %>%
  distinct()

polygons_ras_pci <- merge(polygons_ras_pci, keep)



#save
saveRDS(polygons_ras_pci, "Robjects/polygons_ras_pci.RDS")
dir.create("exported/polygons_ras_pci")
st_write(polygons_ras_pci, "exported/polygons_ras_pci/polygons_ras_pci.shp", append=FALSE)



#### making a minimum convex polygon


#seperate pci and ras points

poly_list <- polygons_ras_pci %>%
  ungroup() %>%
  group_split(region)

names(poly_list) <- unique(polygons_ras_pci$region)


##convert CRS to LCC so that st_centroid within mcp function will work properly
poly_list <- lapply(poly_list, function(x) st_transform(x, LCC))

#make a polygon for each

mcp_regions <- lapply(poly_list, function(x) st_mcp(x, percent=100))


#unlist

mcp_regions <- rbind(st_as_sf(mcp_regions[[1]]), st_as_sf(mcp_regions[[2]]))
mcp_regions$region <- c("PCI", "Rasmussen")

#convert back to basic CRS

mcp_regions <- st_transform(mcp_regions, NAD83)

#save

saveRDS(mcp_regions, "Robjects/mcp_regions.RDS")
dir.create("exported/mcp_regions")
st_write(mcp_regions, "exported/mcp_regions/mcp_regions.shp", append=FALSE)


