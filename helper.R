library(plotly)
library(DT)
library(rgdal)
library(sp)
library(data.table)
library(raster)
library(reshape2)
library(ggmap)
library(ggplot2)
library (mapproj)
library(jpeg)

ebd <- readRDS ("panchayat_data.RDS")
setorder(ebd, LATITUDE, LONGITUDE)
map <- readRDS("panchayat.RDS")

processMap <- function(region)
{
    if(region == "All")
    {
      return (ebd)
    }
    else
    {
      return (ebd [ebd$REGION == region,] )
    }
}
 
ToLink <- function(txt,link) {
  paste0("<a href=",link,">",txt,"</a>")
}

generateLocalities <- function(locality)  
{
  req(locality)
  names(locality)
  setorder(locality, REGION)
  locality$LOCALITY.ID <- ToLink(paste0("L",locality$LOCALITY.ID), paste0("https://ebird.org/barchart?byr=1900&eyr=2019&bmo=1&emo=12&r=","L",locality$LOCALITY.ID))
  dt <- data.table (locality$REGION, locality$LOCALITY.ID, locality$LATITUDE, locality$LONGITUDE, locality$SAMPLES)
  names(dt)
  print(ncol(dt))
  colnames(dt) <- c("Region", "Locations", "Latitude", "Longitude", "Lists")
  #  dt <- data.table( c("L12345", "L12345", "L12345"),
  #                    c(1, 2, 3))
  return (dt)
}


urlmaker <- function (locations)
{
  # Concatenates the default bar chart URL of eBird with location number concatenated with L and comma  
  url <- sub (".$", "", paste ("https://ebird.org/barchart?byr=1900&eyr=2019&bmo=1&emo=12&r=", 
                               paste0 ("L", locations,",") %>% unlist() %>% paste (collapse = ''), sep='')) 
  return (ToLink("Click",url))
}

generateBarChartLinks <- function (locality)  
{
  req(locality)
  print(names(locality))
  setorder(locality, REGION, -SAMPLES) # We want the localities with more samples to be earlier in the URL
  dt <- acast (locality, REGION~., value.var = 'LOCALITY.ID', fun.aggregate = urlmaker)
#  dt <- data.table( c("L12345", "L23455", "L23455"),
#                    c(ToLink ("Link1","http://ebird.org/india"), ToLink ("Link2","http://ebird.org/india"), ToLink ("Link3","http://ebird.org/india")))
  return (dt)
}

plotDisplayMap <- function (map, points)
{
    #Pick bounding square and convert to form usable in google
    if( region != "All" )
    {
      fields <- c("PANCHAYAT", "MUNICIPAL", "CORPORATE")
      regionindex <- NULL
      
      index <- 1
      while ( (index < length(fields) + 1) && ( length (regionindex) < 1) )
      {
        fieldindex <- grep(paste0("\\",fields [index],"\\>"),names(map@data))
        regionindex <- grep(region, map@data[fieldindex][,1])
        index <- index + 1; 
      }

      print(regionindex)
      
      if(length (regionindex) > 0)
      {
        map <- map[grep(region, map@data[fieldindex][,1]), ]
        map <- map[min(which(map@data[fieldindex][,1] == as.character(region))), ]
      
        bounds <- bbox(map)
        sbbox <- make_bbox(lon = c(bounds[1], bounds[3]), lat = c(bounds[2], bounds[4]), f = 1)
        #TODO: Get a good map as background
        gmap <- get_map (location=sbbox, maptype = "hybrid", source = "osm", col="color")
        
        points$Region <- NULL
        points$Locations <- NULL
        colnames(points) <- c("lat", "lon", "lists")
        print(names(points))
        print(head(points, 20))
        
        ggmap(gmap) +
            geom_polygon(data=fortify(map), aes(x=long, y=lat, group=group), color="red", fill = "purple", alpha=0.05) +
            geom_point(data=fortify(points), aes(x=lon, y=lat), color="red") +
            coord_map()  +
            theme(legend.position = "none")
      }
  }
}

############################################################################
########### Processing World Locality Data to obtain Panchayat Data########

#ebd <- readRDS("locality.RDS")
#setorder(ebd, LATITUDE, LONGITUDE)
#map <- readRDS("panchayat.RDS")

processEbdData <- function(map, field, region)
{
  print(field)
  print(region)
  
  fieldindex <- grep(paste0("\\",field,"\\>"),names(map@data))
  print (fieldindex)
  
  if(!is.null(fieldindex))
  {
    box <- bbox(map)
    ebd$LATITUDE <- ebd$LATITUDE / 100000
    ebd$LONGITUDE <- ebd$LONGITUDE / 100000
    
    # Reduce the scope to the locations in the boundingbox
    ebd <- subset(ebd, LATITUDE > box[2,1] & LATITUDE < box[2,2] & LONGITUDE > box[1,1] & LONGITUDE < box[1,2])
    
    # Obtain a unique list of regions
    regions <- map@data[fieldindex]
    print(nrow(regions))
    
    if(nrow(regions) > 0)
    {
      if(region == "All")
      {
        regions  <-  unique (regions)
      }
      else
      {
        regions <-   as.data.frame(region)
      }
      
      ebd_with_region <- NULL
      
      print(nrow(regions))
      for (regionindex in 1:nrow(regions))
      {
        # Filter lists according to set filter polygons 
        #      print(regions[regionindex,1])
        #      print(paste0("\\",as.character(regions[regionindex,1]), "\\>"))
        #      print(grep(paste0("\\",as.character(regions[regionindex,1]), "\\>"), map@data[fieldindex][,1]))
        #     region_selected  <- map[grep(paste0("\\",as.character(regions[regionindex,1]), "\\>"), map@data[fieldindex][,1]), ]
        region_selected  <- map[min(which(map@data[fieldindex][,1] == as.character(regions[regionindex,1]))), ]
        #     region_selected  <- map[grep(as.character(regions[regionindex,1]), map@data[fieldindex][,1]), ]
        rgl_ebd   <- ebd
        
        box <- bbox(region_selected)
        # Reduce the scope to the locations in the boundingbox
        rgl_ebd <- subset(rgl_ebd, LATITUDE > box[2,1] & LATITUDE < box[2,2] & LONGITUDE > box[1,1] & LONGITUDE < box[1,2])
        
        #        print(nrow(rgl_ebd))
        if(nrow(rgl_ebd) > 0)
        {
          sp::coordinates(rgl_ebd) <- ~LONGITUDE+LATITUDE
          # Map the CRS
          crs(rgl_ebd) <- crs(map)
          rgl_ebd <- rgl_ebd[region_selected, ]
          
          rgl_ebd <- as.data.frame(rgl_ebd)
          
          #        print(nrow(rgl_ebd))
          if(nrow(rgl_ebd) > 0)
          {
            # For all filtered locations, assign the filter_index
            rgl_ebd$REGION     <- as.character (regions[regionindex, 1]) 
            
            if(!is.null(ebd_with_region))
            {
              ebd_with_region <- rbind (ebd_with_region, rgl_ebd)
            }
            else
            {
              ebd_with_region <- rgl_ebd
            }
          }
        }
      }
    }
    else 
    {
      print ("No polygons in the file")
    }
    print(names(ebd_with_region))
    print(nrow(ebd_with_region))
  }
  #  write.csv2(unique(ebd_with_region$REGION), paste0(field,"_panchayat_with_data.csv"))
  #  write.csv2(unique(map@data$PANCHAYAT),paste0("all_panchayats.csv"))
  #  write.csv2(unique(map@data$MUNCIPAL),paste0("all_municpalities.csv"))
  #  write.csv2(unique(map@data$CORPORATE),paste0("all_corporations.csv"))
  return (ebd_with_region)
}

# ebd_p <- processEbdData (map, "PANCHAYAT", "All")
# ebd_m <- processEbdData (map, "MUNICIPAL", "All")
# ebd_c <- processEbdData (map, "CORPORATE", "All")
# ebd_all <- rbind (ebd_p, ebd_m, ebd_c)
# saveRDS (ebd_all, "panchayat_data.RDS")