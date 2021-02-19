#---- load packages ------------------------------------------------------------

library(ggtext)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(tidyverse)

#---- set options --------------------------------------------------------------

font_add_google('raleway')
showtext_auto()

#---- Functions ----------------------------------------------------------------

coor_dist <- function(lat_1, lon_1, lat_2, lon_2){
  
  # measures the distance in meters between to coordinate points, 
  # (lat_1, lon_1) and (lat_2, lon_2), based on distance formula found at
  # https://www.movable-type.co.uk/scripts/latlong.html
  
  # convert lat and lon into radians
  phi_1 <- 
    lat_1 * pi / 180
  phi_2 <- 
    lat_2 * pi / 180
  lambda_1 <- 
    lon_1 * pi / 180
  lambda_2 <- 
    lon_2 * pi / 180
  
  delta_lambda <- 
    lambda_2 - lambda_1
  delta_phi <- 
    phi_2 - phi_1
  
  a <- 
    sin(delta_phi/2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda/2)^2
  cc <- 
    2 * atan2(sqrt(a), sqrt(1 - a))
  d <- 
    6371e3 * cc # earth radius in meters is 6371e3
  
  return(d)
  
}

coor_new <- function(lat_0, lon_0, dist, side){
  
  # When side = 'h', the function finds coordinates lat_b and lat_t so that 
  # (lat_b, lon_0) and (lat_t, lon_0) are located at dist meters from each 
  # other. When side = 'w', the function finds coordinates lon_r and lon_l so 
  # that (lat_0, lon_r) and (lat_0, lon_l) are located at dist meters from
  # each other.
  
  if(side == 'h'){
    
    cc <- 
      dist / 6371e3 # earth radius in meters 
    
    a <- 
      sin(cc/2)^2
    
    delta_phi <- 
      asin(sqrt(a)) * 2
    
    hh_add <- 
      180 * delta_phi / (2 * pi)
    
    lat_b_new <- 
      lat_0 - hh_add
    lat_t_new <- 
      lat_0 + hh_add
    
    lat_new <-
      c(lat_b_new, lat_t_new)
    
    names(lat_new) <-
      c('lat_b', 'lat_t')
    
    return(lat_new)
    
  } else if(side == 'w'){
    
    cc <- 
      dist/6371e3 # earth radius in meters
    
    a <- 
      sin(cc/2)^2
    
    delta_lambda <-
      2 * asin(sqrt(a) / cos(lat_0 * pi / 180)) # use lat_0 in radians
    
    ww_add <- 
      180 * delta_lambda / (2 * pi)
    
    lon_l_new <- 
      lon_0 - ww_add
    lon_r_new <- 
      lon_0 + ww_add
    
    lon_new <-
      c(lon_l_new, lon_r_new)
    
    names(lon_new) <-
      c('lon_l', 'lon_r')
    
    return(lon_new)
    
  }
  
}

coor_expand <- function(lat_b, lon_l, lat_t, lon_r, distance, side){ 
  
  # Given bounding coordiantes lat_b, lon_l, lat_t, and lon_r, the function
  # calculates the center coordiantes (lat_c, lon_c). 
  #
  # When side = 'h', the function returns a  
  # matrix with bounding coordinates lat_b_new, lon_l, lat_t_new and lon_r where
  # coordinate points (lat_b_new, lon_c) and (lat_t_new, lon_c) are located
  # at distance meters from each other.
  #
  # When side = 'w', the it returns a matrix with bounding coordiantes lat_b,
  # lon_l_new, lat_t, lon_r_new where coordinate points (lat_b, lon_l_new) and
  # (lat_t, lon_r_new) are located at a distance meters from each other.
  
  lat_c <-
    (lat_t - lat_b)/2 + lat_b
  lon_c <-
    (lon_r - lon_l)/2 + lon_l
  
  if(side == 'w'){
    
    lon_ideal <-
      coor_new(lat_c, lon_c, distance, side)
    
    lat_b_new <-
      lat_b
    lat_t_new <-
      lat_t
    
    lon_l_new <-
      lon_ideal['lon_l']
    lon_r_new <-
      lon_ideal['lon_r']
    
  } else if(side == 'h'){
    
    lat_ideal <-
      coor_new(lat_c, lon_c, distance, 'h')
    
    lat_b_new <- 
      lat_ideal['lat_b']
    lat_t_new <-
      lat_ideal['lat_t']
    
    lon_l_new <-
      lon_l
    lon_r_new <-
      lon_r
    
  }
  
  coor_matrix <-
    matrix(c(lon_l_new, 
             lat_b_new, 
             lon_r_new, 
             lat_t_new), 
           nrow = 2,
           dimnames = list(c('x', 'y'), c('min', 'max')))
  
  return(coor_matrix)
  
}  

draw_map <- function(location,
                     lat,
                     lon,
                     area = 5,
                     page_size = 'A4',
                     page_l,
                     page_w,
                     orientation = 'portrait',
                     custom_label){
  
  # draw_map() uses ggplot to create the map of a location and save it as a
  # pdf file with selected length and width.
  
  # current timestamp is used to label the final pdf file.
  current_timestamp <- 
    Sys.time()
  
  # if pdf dimensions are explicitly given, then override info in page_size 
  if(!missing(page_l) && !missing(page_w)){
    
    length_mm <-
      page_l * 10 # in millimeters
    
    width_mm <-
      page_w * 10 # in millimiters
    
    orientation_selected <- 
      orientation
    
  }else{
    
    size_selected <- 
      page_size
    
    orientation_selected <- 
      orientation
    
    length_mm <-
      if(size_selected == 'l'){
        
        910
        
      }else if(size_selected == 'm'){
        
        610
        
      }else if(size_selected == 's'){
        
        430
        
      }else if(size_selected == 'A4'){
        
        297
        
      }
    
    
    width_mm <-
      if(size_selected == 'l'){
        
        610
        
      }else if(size_selected == 'm'){
        
        460
        
      }else if(size_selected == 's'){
        
        280
        
      }else if(size_selected == 'A4'){
        
        210
        
      }
    
    
  }
  
  
  
  if(orientation_selected == 'landscape'){
    
    width_pdf <-
      length_mm
    
    height_pdf <-
      width_mm
    
    width_used <-
      length_mm
    
    length_used <-
      width_mm
    
  } else if(orientation_selected == 'portrait'){
    
    width_pdf <-
      width_mm
    
    height_pdf <-
      length_mm
    
    width_used <-
      width_mm
    
    length_used <-
      length_mm
    
  }
  

    
  # custom size should be correlated to overall map surface.
  font_size <- 
    ceiling(5 * sqrt(length_mm * width_mm / 62370 * 4)/2) # font size 20 when a4
  
  
  # margin_size <-
  #   ceiling(15 * length_mm * width_mm / 555100) # margin size 15 when large
  
  # if a location is explicitly given, then use getbb() to find bounding 
  # coordinates. Else check for custom lat lon coordinates given.
  if(!missing(location)){
    
    city <-
      location
    
    coor_city <- 
      getbb(city)
    
    lat_t <- 
      coor_city[2, 2]
    lat_b <- 
      coor_city[2, 1]
    lon_l <- 
      coor_city[1, 1]
    lon_r <- 
      coor_city[1, 2]
    
    hh <- 
      coor_dist(lat_b, lon_l, lat_t, lon_l)
    ww <-
      coor_dist(lat_b, lon_l, lat_b, lon_r)   
    
    hh_ideal <- 
      length_used/width_used * ww
    
    ww_ideal <- 
      width_used/length_used * hh
    
    
    if(orientation_selected == 'landscape'){
      
      
      dist_var <-
        if(ww <= hh || hh >= hh_ideal){
          
          ww_ideal
          
        }else{
          
          hh_ideal
          
        }
      
      side_var <-
        if(ww <= hh || hh >= hh_ideal){
          
          'w'
          
        }else{
          
          'h'
        }  
      
    } else if(orientation_selected == 'portrait'){
      
      dist_var <-
        if(hh <= ww || ww >= ww_ideal){
          
          hh_ideal
          
        }else{
          
          ww_ideal
        }
      
      side_var <-
        if(hh <= ww || ww >= ww_ideal){
          
          'h'
          
        }else{
          
          'w'
        }
      
    }
    
    coor_city_new <-
      coor_expand(lat_b, lon_l, lat_t, lon_r, dist_var, side_var)
    
    # if location is given, use string as label
    map_label <-
      location
    
  }else if(!missing(lat) && !missing(lon) && missing(location)){
    
    lat0 <-
      lat
    
    lon0 <- 
      lon
    
    area_value <- 
      area
    
    if(orientation_selected == 'landscape'){
      
      hh_ideal <- 
        sqrt(area_value/(width_used/length_used)) 
      
      ww_ideal <- 
        width_used/length_used * hh_ideal
      
    }else if(orientation_selected == 'portrait'){
      
      hh_ideal <- 
        sqrt(area_value/(width_used/length_used)) 
      
      ww_ideal <- 
        width_used/length_used * hh_ideal
      
    }
    
    latitudes <- 
      coor_new(lat0, lon0, hh_ideal * 1000, 'h')
    
    longitudes <- 
      coor_new(lat0, lon0, ww_ideal * 1000, 'w')
    
    coor_city_new <- 
      matrix(rbind(longitudes, latitudes), 
             nrow = 2, 
             dimnames = list(c('x', 'y'), c('min', 'max')))
    
    # create label based on lat and lon coordinates
    lat0_degree <- 
      floor(lat0)

    lat0_minute <- 
      floor((round(lat0, 2) - lat0_degree) * 60)
    
    lat0_second <- 
      (lat0 - lat0_degree - lat0_minute/60) * 3600
    
    lon0_degree <- 
      floor(lon0)
    
    lon0_minute <- 
      floor((round(lon0, 2) - lon0_degree) * 60)
    
    lon0_second <- 
      (lon0 - lon0_degree - lon0_minute/60) * 3600
    
    map_label <-
      paste0('N ',
             lat0_degree, '° ',
             lat0_minute, '\' ',
             round(lat0_second, 2), '" ',
             'E ',
             lon0_degree, '° ',
             lon0_minute, '\' ',
             round(lon0_second, 2), '"')   
    
  }
  
  # Find the sf data object for bounding coordinates coor_city_new. These lines
  # are a slight modification from the code found
  # http://joshuamccrain.com/tutorials/maps/streets_tutorial.html.
  
  big_streets <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c ("motorway", 
                               "primary", 
                               "motorway_link", 
                               "primary_link")) %>%
    osmdata_sf()
  
  med_streets <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("secondary", 
                              "tertiary", 
                              "secondary_link", 
                              "tertiary_link")) %>%
    osmdata_sf()
  
  small_streets <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("residential", 
                              "living_street",
                              "unclassified",
                              "service", 
                              "footway"
                    )) %>%
    osmdata_sf()
  
  trunk_streets <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("trunk", 
                              "trunk_link",
                              "trunk_loop"
                    )) %>%
    osmdata_sf()
  
  railway <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "railway", value="rail") %>%
    osmdata_sf()
  
  river <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "waterway", value = c("river")) %>%
    osmdata_sf()
  
  canal <- coor_city_new%>%
    opq()%>%
    add_osm_feature(key = "waterway", value = c('canal')) %>%
    osmdata_sf()
  
  # if custom label is given, then override plot label
  if(missing(custom_label)){
    
    map_label <- 
      toupper(map_label)  
    
  }else if(!missing(custom_label)){
    
    
    map_label <-
      custom_label
    
  }
  
  # Plot map given sf objects retrieved above. The following lines are based on
  # the tutorial http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
  
  pp <-
    ggplot() +
    geom_sf(data = river$osm_lines,
            inherit.aes = FALSE,
            color = "steelblue",
            size = .9,
            alpha = .3) +
    geom_sf(data = canal$osm_lines,
            inherit.aes = FALSE,
            color = "steelblue",
            size = .5,
            alpha = .3) +
    geom_sf(data = railway$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            linetype="dotdash",
            alpha = .5) +
    geom_sf(data = med_streets$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .3,
            alpha = .5) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#666666",
            size = .2,
            alpha = .3) +
    geom_sf(data = big_streets$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .5,
            alpha = .6) +
    geom_sf(data = trunk_streets$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .5,
            alpha = .6) +
    coord_sf(xlim = coor_city_new[1, ],
             ylim = coor_city_new[2, ],
             expand = FALSE) +
    theme_void() + # get rid of background color, grid lines, etc.
    # theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(plot.margin=margin(t = 0, b = 0, l = 10, r = 10, 'mm')) +
    annotate(geom="richtext",
             x = (coor_city_new[1, 2] - coor_city_new[1, 1])/2 + coor_city_new[1, 1],
             y = (coor_city_new[2, 2] - coor_city_new[2, 1]) * .10 + coor_city_new[2, 1],
             label = map_label,
             label.size = NA,
             family = 'raleway',
             fill = 'white',
             size = font_size)  
  
  # create pdf file label, according to current_timestamp
  date_string <-
    gsub('-', '', lubridate::date(current_timestamp))
  
  time_string <-
    paste0(
      ifelse(
        nchar(lubridate::hour(current_timestamp)) < 2, 
        paste0('0', lubridate::hour(current_timestamp)), 
        lubridate::hour(current_timestamp)),
      ifelse(
        nchar(lubridate::minute(current_timestamp)) < 2, 
        paste0('0', lubridate::minute(current_timestamp)), 
        lubridate::minute(current_timestamp)),
      ifelse(
        nchar(round(lubridate::second(current_timestamp), 0)) < 2, 
        paste0('0', round(lubridate::second(current_timestamp), 0)), 
        round(lubridate::second(current_timestamp), 0)))

  file_string <-
    paste0(paste(date_string, time_string, sep = '_'), '.pdf')
  
  # save file under getwd()
  ggsave(file_string, 
         plot = pp, 
         width = width_pdf, 
         height = height_pdf, 
         units = 'mm') 
  
  return(paste('Your map was saved under the name', 
               file_string))    
  
}