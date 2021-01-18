
#load data set of number of shorebirds observed per plot per year
sb_year <- readRDS("Robjects/sb_year.RDS")



#filter to keep only the years where there were repeated observations (only occured in two regions)
sb_rep <- sb_year %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year == 2019]) %>%
  filter(Species %notin% c("LESA", "SAND", "SEPL", "WHIM", "WISN"))




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


#read in polygons shapefile
all_polygons <- readRDS("Robjects/all_polygons.RDS")


#filter only the plots I'm using in this analysis

polygons_ras_pci <- all_polygons %>%
  filter(Plot %in% sb_rep2$Plot)

st_geometry(polygons_ras_pci) <- "geometry"

saveRDS(polygons_ras_pci, "Robjects/all_polygons.RDS")
st_write(polygons_ras_pci, "exported/polygons_ras_pci.shp", append=FALSE)



#testing why filter isnt working

nc <- st_read(system.file("shape/nc.shp", package="sf"))

filter_counties <- c("Yancey", "Washington", "Anson", "Burke", "Avery")

nc_filter <- nc %>%
  filter(NAME %in% filter_counties)

#works fine here
#both filtering instructions are character vectors

#https://stackoverflow.com/questions/65778678/how-can-i-make-a-reproducible-version-of-an-sf-dataset-to-provide-for-stack-over
#https://stackoverflow.com/questions/65684471/r-how-can-i-use-data-table-package-with-sf-geometry-column



x <- structure(list(shape = c("polygon 1", "polygon 2"), geometry = structure(list(
  structure(list(structure(c(-4e-04, -4e-04, -3e-05, -3e-05, 
                             -4e-04, 51.199, 51.1975, 51.1975, 51.199, 51.199), .Dim = c(5L,                                                                                        2L))), class = c("XY", "POLYGON", "sfg"), precision = 0, bbox = structure(c(xmin = -4e-04,                                                                                                                                                                      ymin = 51.1975, xmax = -3e-05, ymax = 51.199), class = "bbox"), crs = structure(list(                                                                                                                                                                       input = NA_character_, wkt = NA_character_), class = "crs"), n_empty = 0L), 
  structure(list(structure(c(5e-05, 5e-05, 0.003, 0.003, 5e-05, 
                             51.1972, 51.1967, 51.1967, 51.1972, 51.1972), .Dim = c(5L,                                                                              2L))), class = c("XY", "POLYGON", "sfg"), precision = 0, bbox = structure(c(xmin = 5e-05,                                                                                                                                                       ymin = 51.1967, xmax = 0.003, ymax = 51.1972), class = "bbox"), crs = structure(list(                                                                                                                                                              input = NA_character_, wkt = NA_character_), class = "crs"), n_empty = 0L)), class = 
    c("sfc_POLYGON", 
      "sfc"), precision = 0, bbox = structure(c(xmin = -4e-04, ymin = 51.1967, 
                                                xmax = 0.003, ymax = 51.199), class = "bbox"), crs = structure(list(
                                                  input = NA_character_, wkt = NA_character_), class = "crs"), n_empty = 0L)), row.names = c(NA, 
                                                                                                                                             -2L), sf_column = "geometry", agr = structure(c(shape = NA_integer_), .Label = c("constant", 
                                                                                                                                                                                                                              "aggregate", "identity"), class = "factor"), class = c("sf", 
                                                                                                                                                                                                                                                                                     "tbl_df", "tbl", "data.frame"))
x2 <- x %>%
  filter(shape %in% c("polygon 1"))
#this one works fine too

#wait for stack over flow question, finish troubleshooting tomorrow
