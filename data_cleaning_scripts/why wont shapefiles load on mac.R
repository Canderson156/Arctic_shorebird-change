

#### dont need this, but using it for testing why reading shapefiles won't work

#filepath of the birdlife shorebird rangemaps

range_gdb <-  "BOTW.gdb"

file.exists(range_gdb)
list.files(range_gdb)

#create a list of all of the objects inside those geodatabases
subset(ogrDrivers(), grepl("GDB", name)) 

range_lyrs <- ogrListLayers(range_gdb)


#create a list of all plot shapefiles
shapefile_list <- readOGR_multi(range_gdb, range_lyrs[[1]]) # slow as hell
add <- readOGR_multi(range_gdb, range_lyrs[[2]])
shapefile_list <- c(shapefile_list, add)
rm(add)




#this works fine. This suggests to me that the problem might be with the new versions of proj and gdal

nc <- st_read(system.file("shape/nc.shp", package="sf"))

nc2 <- st_read(datapath("shape/nc.shp"))

