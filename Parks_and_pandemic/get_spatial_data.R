get_spatial_data=function(){
  UK<- readOGR(here("/Parks_and_pandemic/data/Shapefiles/england_ct_2011.shp"), layer = "england_ct_2011",GDAL1_integer64_policy = TRUE)
  
  UK_latlon <- spTransform(UK, CRS("+proj=longlat +datum=WGS84"))
  
  UK_latlon<-rmapshaper::ms_simplify(UK_latlon, keep=0.05, keep_shapes=TRUE)
  
  return(UK_latlon)
}

#writeOGR(UK_latlon, ".", "UK_latlon.shp", driver="ESRI Shapefile") #also you were missing the driver argument
# make_plot_map=function(){ggplot()+ 
#   geom_polygon(data=UK_latlon, aes(long, lat, group = group), 
#                        colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)+
#   theme_minimal()
#   
#   }
