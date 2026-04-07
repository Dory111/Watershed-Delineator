# ------------------------------------------------------------------------------------------------
# testing values
# uniform flow to center sink, two outlets
# rast <- raster(ncol = 10, nrow = 10,
#                xmn = 1000, xmx = 2000,
#                ymn = 1000, ymx = 2000,
#                crs = 3310)
# start_value <- 100
# subtract <- 20
# start_row <- rep(start_value, ncol(rast))
# half_rows <- list(start_row)
# for(i in 2:(nrow(rast)/2)){
#   row <- half_rows[[i-1]]
#   inds <- c(i,ncol(rast)-i+1)
#   row[inds[1]:inds[2]] <- row[inds[1]:inds[2]] - subtract
#   half_rows[[i]] <- row
# }
# half_rows2 <- do.call(rbind, half_rows[5:1])
# half_rows <- do.call(rbind, half_rows)
# 
# 
# all_rows <- rbind(half_rows,
#                   half_rows2)
# values(rast) <- all_rows
# 
# 
# 
# raster <- rast
# out_dir <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.06/00 ISW/Output/Raster'
# flow_dir_rast_name <- 'Flow_Dir_Test'
# flow_to_outlet_rast_name <- 'Outlet_Test'
# min_slope <- 1
# diff_x <- NULL
# diff_y <- NULL
# zunit <- 'm'
# suppress_loading_bar <- FALSE
# suppress_console_messages <- FALSE
# spinning_bar_update_cycle <- 1
# sink_code <- -4444
# flat_code <- -9999
# outlet_location_CRS <- NULL
# outlet_location_is_sf <- TRUE
# outlet_location <- st_sf(st_sfc(st_point(x = c(1350,1350)),
#                                 crs = 3310))
# st_geometry(outlet_location) <- 'geometry'
# outlet_location2 <- st_sf(st_sfc(st_point(x = c(1350,1650)),
#                                  crs = 3310))
# st_geometry(outlet_location2) <- 'geometry'
# outlet_location <- rbind(outlet_location,
#                          outlet_location2)
# st_geometry(outlet_location) <- 'geometry'
# -----------------------------------------------------------------------------------------------



raster <- rast(system.file('ex/elev.tif',package="terra"))
outlet_location = matrix(c(6.2, 49.87), ncol = 2, byrow = TRUE)
out_dir <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.06/00 ISW/Output/Raster'
flow_dir_rast_name <- 'Flow_Dir_Test'
flow_to_outlet_rast_name <- 'Outlet_Test'
min_slope <- 0
diff_x <- NULL
diff_y <- NULL
zunit <- 'm'
suppress_loading_bar <- FALSE
suppress_console_messages <- FALSE
flow_accumulation_rast_name <- NULL
spinning_bar_update_cycle <- 1
sink_code <- -4444
flat_code <- -9999
outlet_location_CRS <- crs(raster)
outlet_location_is_sf <- FALSE
outlet_location_is_line <- FALSE
outlet_location_line_density <- 100
Watershed_Delineator(raster,
                     out_dir,
                     outlet_location_is_sf = FALSE,
                     outlet_location = outlet_location, flow_dir_rast_name = 'Flow_Dir_Test')
r1 <- rast(file.path(out_dir,paste0('flow_accumulation_rast','.tif')))
deg <- r1$degrees
v <- values(deg)
v[v == -4444] <- NA
values(deg) <- v
plot(deg)
r <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.06/00 ISW/Data/Raster/DEM/testflow_dir.tif')
plot(r)


# ------------------------------------------------------------------------------------------------
# testing values
# sink at DEM corner
# rast <- raster(ncol = 10, nrow = 10,
#                xmn = 1000, xmx = 2000,
#                ymn = 1000, ymx = 2000,
#                crs = 3310)
# 
# 
# values <- matrix(data = NA, nrow = 10, ncol = 10)
# values[1, ] <- seq(from = 50, to = 0, length.out = 10) 
# values[ ,1] <- seq(from = 50, to = 100, length.out = 10)
# values[ ,10] <- seq(from = 0, to = 50, length.out = 10)
# values[10, ] <- seq(from = 100, to = 50, length.out = 10)
# for(i in 1:9){
#   values[i,2:9] <- seq(from = values[i,1], to = values[i,10], length.out = 10)[2:9] + runif(n = 8, min = -2, max = 2)
# }
# values[values < 0] <- 0
# values[1, ] <- 100
# values[ ,1] <- 100
# values[ ,10] <- 100
# values[10, ] <- 100
# 
# values(rast) <- as.vector(values)
# 
# 
# 
# 
# raster <- rast
# out_dir <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.06/00 ISW/Output/Raster'
# flow_dir_rast_name <- 'Flow_Dir_Test'
# flow_to_outlet_rast_name <- 'Outlet_Test'
# min_slope <- 1
# diff_x <- NULL
# diff_y <- NULL
# zunit <- 'm'
# suppress_loading_bar <- FALSE
# suppress_console_messages <- FALSE
# spinning_bar_update_cycle <- 1
# sink_code <- -4444
# flat_code <- -9999
# outlet_location_CRS <- NULL
# outlet_location_is_sf <- TRUE
# outlet_location <- st_sf(st_sfc(st_point(x = c(1150,1150)),
#                           crs = 3310))
# # st_geometry(outlet_location) <- 'geometry'
# # outlet_location2 <- st_sf(st_sfc(st_point(x = c(1150,1250)),
# #                                  crs = 3310))
# # st_geometry(outlet_location2) <- 'geometry'
# # outlet_location <- rbind(outlet_location,
# #                          outlet_location2)
# st_geometry(outlet_location) <- 'geometry'
# outlet_location_is_line <- FALSE
# outlet_location_line_density <- 100
# ------------------------------------------------------------------------------------------------


# ==================================================================================================
# Delineates watersheds by first determining the flow direction of a DEM, and then recursively finding
# all the cells that flow towards a chosen outlet location
# ==================================================================================================
Watershed_Delineator <- function(raster,
                                 out_dir,
                                 outlet_location = NULL,
                                 outlet_location_CRS = NULL,
                                 outlet_location_is_sf = TRUE,
                                 outlet_location_is_line = FALSE,
                                 outlet_location_line_density = 100,
                                 flow_to_outlet_rast_name = NULL,
                                 flow_dir_rast_name = NULL,
                                 flow_accumulation_rast_name = NULL,
                                 min_slope = 0,
                                 flat_code = -9999,
                                 resolve_flats = TRUE,
                                 sink_code = -4444,
                                 diff_x = NULL,
                                 diff_y = NULL,
                                 zunit = 'm',
                                 suppress_loading_bar = FALSE,
                                 suppress_console_messages = FALSE,
                                 spinning_bar_update_cycle = 1)
{
  ############################################################################################################
  ################################## HELPER FUNCTIONS ########################################################
  ############################################################################################################
  # ==================================================================================================
  # Function to give user information to compare their computer
  # to the testing computer
  # ==================================================================================================
  wdl_machine_specs <- function()
  {
    cat(paste0('Watershed delineation functions were tested on a\n',
               'Lenovo thinkpad with:\n\n',
               '16 GB of RAM\n',
               'and an Intel i7-1365U, 1800Mhz 10 core processor\n\n'))
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Simple function to concatenate a loading bar
  # ==================================================================================================
  loading_bar <- function(iter, total, width = 50, optional_text = '') 
  {
    # ------------------------------------------------------------------------------------------------
    # percent completion
    pct <- iter / total
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # how much to fill the bar
    filled <- round(width * pct)
    bar <- paste0(rep("=", filled), collapse = "")
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # how much is left
    space <- paste0(rep(" ", width - filled), collapse = "")
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    cat(sprintf("\r[%s%s] %3d%% %s",
                bar,
                space,
                round(100*pct),
                optional_text))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    if (iter == total){
      cat("\n")
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Simple function to concatenate a loading bar
  spinning_bar <- function(optional_text, character, iter) 
  {
    # ------------------------------------------------------------------------------------------------
    if(iter == 1){
      cat('\n')
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    cat(sprintf("\r[ %s ] %s",
                character,
                optional_text))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ==================================================================================================
  # calculate distance based on latlon
  # ==================================================================================================
  Haversine_Formula <- function(LatA,LonA,
                                LatB,LonB,
                                Re = 6371)
  {
    LatA <- LatA * (3.14159/180)
    LonA <- LonA * (3.14159/180)
    LatB <- LatB * (3.14159/180)
    LonB <- LonB * (3.14159/180)
    
    dlat <- LatB - LatA
    dlon <- LonB - LonA
    
    a <- sin(dlat / 2)^2 + cos(LatA) * cos(LatB) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a),sqrt(1-a))
    
    d <- Re * c
    return(d)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  #===========================================================================================
  # install required packages if not present
  #===========================================================================================
  require_package <- function(pkg)
  {
    if(require(pkg, character.only = TRUE) == FALSE){
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  #===========================================================================================
  # if outlet location is passed as an XY point then move it to an sf object
  #===========================================================================================
  coerce_outlet_location <- function(outlet_location)
  {
    out_list <- list()
    
    # ------------------------------------------------------------------------------------------------
    # if user modifies to indicate that outlet location is not as an sf dataframe but an XY point
    if(outlet_location_is_sf == FALSE){
      if(is.null(outlet_location_CRS) == TRUE){
        for(i in 1:nrow(outlet_location)){
          out_list[[i]] <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                                 crs = st_crs(raster)))
        }
      } else {
        for(i in 1:nrow(outlet_location)){
          out <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                       crs = outlet_location_CRS))
          out <- st_transform(out,
                              st_crs(raster))
          out_list[[i]] <- out
        }
      }
    } 
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # if user indicates that default case is true (location is an sf dataframe) then check if that
    # is actually the case
    # If it isnt the case and default left on by accident then coerce to sf if possible
    if(outlet_location_is_sf == TRUE){
      # ------------------------------------------------------------------------------------------------
      # move vector to sf dataframe
      if(class(outlet_location)[1] != 'sf'){
        if(is.null(outlet_location_CRS) == TRUE){
          for(i in 1:nrow(outlet_location)){
            out_list[[i]] <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                                          crs = st_crs(raster)))
          }
        } else {
          for(i in 1:nrow(outlet_location)){
            out <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                         crs = outlet_location_CRS))
            out <- st_transform(outlet_location,
                                st_crs(raster))
            out_list[[i]] <- out
          }
        }
      } else {}
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    if(length(out_list) > 0){
      names(out_list) <- NULL
      out_list <- do.call(rbind, out_list)
      st_geometry(out_list) <- 'geometry'
      outlet_location <- out_list
    }
    # ------------------------------------------------------------------------------------------------

    return(outlet_location)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Uses passed bounds and the flow angles of cell neighbors to determine what cells flow
  # to current cell
  # ==================================================================================================
  check_outlet_neighbors <- function(bounds,
                                     outlet_neighbors)
  {
    # ------------------------------------------------------------------------------------------------
    # checking whether outlet flows to current cell
    TF_points <- rep(TRUE, length(outlet_neighbors))
    checked <- rep(TRUE, length(outlet_neighbors))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 1
    if(is.na(outlet_neighbors[1]) == TRUE){
      TF_points[1] <- FALSE
    } else {
      if(outlet_neighbors[1] > min(bounds[1, ]) &
         outlet_neighbors[1] < max(bounds[1, ])){
        TF_points[1] <- TRUE
      } else {
        TF_points[1] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 2
    if(is.na(outlet_neighbors[2]) == TRUE){
      TF_points[2] <- FALSE
    } else {
      if(outlet_neighbors[2] > min(bounds[2, ]) &
         outlet_neighbors[2] < max(bounds[2, ])){
        TF_points[2] <- TRUE
      } else {
        TF_points[2] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 3
    if(is.na(outlet_neighbors[3]) == TRUE){
      TF_points[3] <- FALSE
    } else {
      if(outlet_neighbors[3] > min(bounds[3, ]) &
         outlet_neighbors[3] < max(bounds[3, ])){
        TF_points[3] <- TRUE
      } else {
        TF_points[3] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 4
    if(is.na(outlet_neighbors[4]) == TRUE){
      TF_points[4] <- FALSE
    } else {
      if(outlet_neighbors[4] > min(bounds[4, ]) &
         outlet_neighbors[4] < max(bounds[4, ])){
        TF_points[4] <- TRUE
      } else {
        TF_points[4] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 5
    if(is.na(outlet_neighbors[5]) == TRUE){
      TF_points[5] <- FALSE
    } else {
      if(outlet_neighbors[5] > min(bounds[5, ]) &
         outlet_neighbors[5] < max(bounds[5, ])){
        TF_points[5] <- TRUE
      } else {
        TF_points[5] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 6
    if(is.na(outlet_neighbors[6]) == TRUE){
      TF_points[6] <- FALSE
    } else {
      if(outlet_neighbors[6] > min(bounds[6, ]) &
         outlet_neighbors[6] < max(bounds[6, ])){
        TF_points[6] <- TRUE
      } else {
        TF_points[6] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 7
    if(is.na(outlet_neighbors[7]) == TRUE){
      TF_points[7] <- FALSE
    } else {
      if(outlet_neighbors[7] == 0){
        outlet_neighbors[7] <- 360
      }
      
      if(outlet_neighbors[7] >= 0 &
         outlet_neighbors[7] < min(bounds[7, ])){
        TF_points[7] <- TRUE
      } else if (outlet_neighbors[7] > max(bounds[7, ]) &
                 outlet_neighbors[7] <= 360){
        TF_points[7] <- TRUE
      } else {
        TF_points[7] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # check cell 8
    if(is.na(outlet_neighbors[8]) == TRUE){
      TF_points[8] <- FALSE
    } else {
      if(outlet_neighbors[8] == 0){
        outlet_neighbors[8] <- 360
      }
      
      if(outlet_neighbors[8] > max(bounds[8, ]) &
         outlet_neighbors[8] < 360){
        TF_points[8] <- TRUE
      } else {
        TF_points[8] <- FALSE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    return(list(TF_points,
                checked))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ==================================================================================================
  # Uses passed bounds and the flow angles of current cell into cell neighbors to return where
  # to allocate flow
  # ==================================================================================================
  check_outlet_neighbors2 <- function(cardinals, 
                                      bounds,
                                      current_cell_value)
  {
    # ------------------------------------------------------------------------------------------------
    # checking whether outlet flows to current cell
    TF_points <- rep(FALSE, 8)
    mult <- 1
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # if it directly points at a cell
    if(current_cell_value %in% cardinals){
      cardinal_index <- which(cardinals == current_cell_value)
      TF_points[cardinal_index] <- TRUE
      mult <- 1
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    if(!current_cell_value %in% cardinals){
      for(i in 1:nrow(bounds)){
        if(current_cell_value > min(bounds[i, ]) &
           current_cell_value < max(bounds[i, ])){
          inds <- c(i, i+1)
          inds[inds == 9] <- 1
          TF_points[inds] <- TRUE
          mult <- rev(abs(current_cell_value - bounds[i, ])/abs(bounds[i,1] - bounds[i,2]))
        }
      }
    }
    # ------------------------------------------------------------------------------------------------

    # ------------------------------------------------------------------------------------------------
    return(list(TF_points, mult))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  # ==================================================================================================
  # Interpolates between points on a line
  # ==================================================================================================
  custom_st_line_sample <- function(sfobj, n)
  {
    # ------------------------------------------------------------------------------------------------
    # getting coordinates
    coords <- st_coordinates(sfobj)
    new_line_points <- list()
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    for(i in 1:(nrow(coords)-1)){
      # ------------------------------------------------------------------------------------------------
      # interpolating between points
      interp_x <- seq(from = coords[i,1],
                      to = coords[i+1,1], length.out = outlet_location_line_density)
      interp_y <- seq(from = coords[i,2],
                      to = coords[i+1,2], length.out = outlet_location_line_density)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      bound <- cbind(interp_x,
                     interp_y)
      bound <- unique(bound)
      
      intermediate <- list()
      for(j in 1:nrow(bound)){
        if(is.na(outlet_location_CRS) == FALSE){
          intermediate[[j]] <- st_sf(st_sfc(st_point(bound[j, ])),
                                     crs = outlet_location_CRS)
        } else {
          intermediate[[j]] <- st_sf(st_sfc(st_point(bound[j, ])),
                                     crs = st_crs(raster))
        }
      }
      names(intermediate) <- NULL
      intermediate <- do.call(rbind, intermediate)
      st_geometry(intermediate) <- 'geometry'
      # ------------------------------------------------------------------------------------------------
      
      new_line_points[[i]] <- intermediate
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    names(new_line_points) <- NULL
    new_line_points <- do.call(rbind, new_line_points)
    return(new_line_points)
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## MAIN FUNCTIONS ##########################################################
  ############################################################################################################
  
  
  # ==================================================================================================
  # Generates the likely direction of flow from a DEM (0-360 degrees, 90 being north)
  # based on D (inf) method of Tarboton 1997 (https://doi.org/10.1029/96WR03137)
  # ==================================================================================================
  flow_dir_of_DEM <- function(raster){
    # ==================================================================================================
    # Takes current cell and uses 8 neighbors to find the flow across that cell
    # ==================================================================================================
    flow_dir_of_neighbors <- function(raster,
                                      values = NULL,
                                      row = 2,
                                      column = 2,
                                      diff_x = NULL,
                                      diff_y = NULL,
                                      min_slope = min_slope)
    {
      
      # ==================================================================================================
      # Calculates the cross product of two vectors in matrix form
      # ==================================================================================================
      Cross_Product_Matrix <- function(V,
                                       U)
      {
        # ------------------------------------------------------------------------------------------------
        imat <- cbind(V[c(1:8) ,c(2:3)],
                      U[c(1:8) ,c(2:3)])
        jmat <- cbind(V[c(1:8) ,c(1,3)],
                      U[c(1:8) ,c(1,3)])
        kmat <- cbind(V[c(1:8) ,c(1:2)],
                      U[c(1:8) ,c(1:2)])
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # ad-bc
        ifinal <- (imat[,c(1)]*imat[,c(4)]) - (imat[,c(2)]*imat[,c(3)])
        jfinal <- (jmat[,c(2)]*jmat[,c(3)]) - (jmat[,c(1)]*jmat[,c(4)]) # bc - ad
        kfinal <- (kmat[,c(1)]*kmat[,c(4)]) - (kmat[,c(2)]*kmat[,c(3)])
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        a <- -(ifinal/kfinal)
        b <- -(jfinal/kfinal)
        dir_of_travel <- atan2(-b,-a)
        inner <- ifinal**2 + jfinal**2 + kfinal**2
        inclination_of_plane <- acos(abs(kfinal/ sqrt(inner)))
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        return(list(c(ifinal,jfinal,kfinal),
                    a,
                    b,
                    dir_of_travel,
                    inclination_of_plane))
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      
      # ==================================================================================================
      # Calculates the cross product of two vectors in vector form
      # ==================================================================================================
      Cross_Product <- function(V,
                                U)
      {
        # ------------------------------------------------------------------------------------------------
        imat <- matrix(nrow = 2,
                       ncol = 2,
                       data = c(V[2:3],
                                U[2:3]),
                       byrow = TRUE)
        jmat <- matrix(nrow = 2,
                       ncol = 2,
                       data = c(V[c(1,3)],
                                U[c(1,3)]),
                       byrow = TRUE)
        kmat <- matrix(nrow = 2,
                       ncol = 2,
                       data = c(V[1:2],
                                U[1:2]),
                       byrow = TRUE)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # ad-bc
        ifinal <- (imat[1,1]*imat[2,2]) - (imat[1,2]*imat[2,1])
        jfinal <- (jmat[1,2]*jmat[2,1]) - (jmat[1,1]*jmat[2,2])
        kfinal <- (kmat[1,1]*kmat[2,2]) - (kmat[1,2]*kmat[2,1])
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        a <- -(ifinal/kfinal)
        b <- -(jfinal/kfinal)
        dir_of_travel <- atan2(-b,-a)
        inner <- ifinal**2 + jfinal**2 + kfinal**2
        inclination_of_plane <- acos(abs(kfinal/ sqrt(inner)))
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        return(list(c(ifinal,jfinal,kfinal),
                    a,
                    b,
                    dir_of_travel,
                    inclination_of_plane))
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      
      ################################## ERRORS ########################################################
      # ------------------------------------------------------------------------------------------------
      # dimensions and values of raster
      if(is.matrix(values) == FALSE){
        if(all(is.na(values[1,1:ncol(values)])) == FALSE |
           all(is.na(values[1:nrow(values),1])) == FALSE |
           all(is.na(values[1:ncol(values),nrow(values)])) == FALSE |
           all(is.na(values[1:nrow(values),ncol(values)])) == FALSE){
          stop(paste0('flow_dir_of_DEM:\n\n',
                      'Matrix must have border of NA values\n'))
        }
        stop(paste0('flow_dir_of_DEM:\n\n',
                    'values must be in form of matrix\n'))
      } else {
        ncol <- ncol(values)
        nrow <- nrow(values)
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # calculate position of cell
      position <- (ncol * (row-1)) +
        column
      
      if(is.null(diff_x) == TRUE |
         is.null(diff_y) == TRUE){
        diff_x <- res(raster)[1]
        diff_y <- res(raster)[2]
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # adjust position accoutning for NA pad
      row <- row + 1
      column <- column + 1
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # matrix of neighbor values
      current_cell_value <- values[row,column]
      neighbors <- c(values[row-1,column-1], # NW
                     values[row-1,column], # N
                     values[row-1,column+1], # NE
                     values[row,column-1], # W
                     values[row,column], # C
                     values[row,column+1], # E
                     values[row+1,column-1], # SW
                     values[row+1,column], # S
                     values[row+1,column+1]) # SE
      neighbors <- matrix(nrow = 3,
                          ncol = 3,
                          byrow = TRUE,
                          data = neighbors)
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # if an eight direction method wont work
      # will only 4 cardinal directions?
      if(any(is.na(neighbors) == TRUE)){
        
        # ------------------------------------------------------------------------------------------------
        # get cardinal values
        ctr_x <- 2
        ctr_y <- 2
        east_value <- neighbors[ctr_x + 0,
                                ctr_y + 1]
        south_value <- neighbors[ctr_x + 1,
                                 ctr_y + 0]
        west_value <- neighbors[ctr_x + 0,
                                ctr_y + -1]
        north_value <- neighbors[ctr_x + -1,
                                 ctr_y + 0]
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # are any of the four neighbors NA
        if(any(is.na(c(east_value,
                       south_value,
                       west_value,
                       north_value)) == TRUE)){
          
          final_dir <- NA
          final_dir_deg <- NA
          max_slope <- NA
          
        } else {
          
          dzdx <- (east_value - west_value)/ (2*diff_x)
          dzdy <- (north_value - south_value)/(2*diff_y)
          final_dir <- atan2(-dzdy,-dzdx)
          final_dir_deg <- final_dir * (180/3.14159)
          final_dir_deg <- (final_dir_deg + 360) %% 360
          max_slope <- -(dzdx*cos(final_dir) + dzdy*sin(final_dir))
          max_slope <- atan2(max_slope,1) * (180/3.14195)
          
        }
        # ------------------------------------------------------------------------------------------------
      } else {}
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # data is fully present
      dir_wedges <- c()
      slopes_wedges <- c()
      if(all(is.na(neighbors) == FALSE)){
        # ------------------------------------------------------------------------------------------------
        # neighbors from east, clockwise repeating east
        nbr_ids <- 1:9
        diff_dx <- c( 1,  1,  0, -1, -1, -1,  0,  1)
        diff_dy <- c( 0, -1, -1, -1,  0,  1,  1,  1)
        
        diff_dx_shifted <- c(1,  0, -1, -1, -1,  0,  1,  1)
        diff_dy_shifted <-c(-1, -1, -1,  0,  1,  1,  1,  0)
        
        nbr_dx  <- c( 1,  1,  0, -1, -1,  -1,  0,  1)
        nbr_dy  <- c( 0,  1,  1,  1,  0,  -1, -1, -1)
        
        nbr_dx_shifted  <- c(1,  0, -1, -1,  -1,  0,  1,  1)
        nbr_dy_shifted  <- c(1,  1,  1,  0,  -1, -1, -1,  0)
        
        nbr_angles <- c(  0, 45, 90, 135, 180, 225, 270, 315, 0)
        ctr_x <- 2
        ctr_y <- 2
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # ijk coordinates
        p0z <- neighbors[ctr_x,
                         ctr_y]
        p1z <- neighbors[cbind(ctr_y + nbr_dy,
                               ctr_x + nbr_dx)]
        p2z <- neighbors[cbind(ctr_y + nbr_dy_shifted,
                               ctr_x + nbr_dx_shifted)]
        
        p0x <- 0
        p1x <- diff_x*diff_dx
        p2x <- diff_x*diff_dx_shifted
        
        p0y <- 0
        p1y <- diff_y*diff_dy
        p2y <- diff_y*diff_dy_shifted
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # edge vectors of the plane
        V <- c(p1x - p0x,
               p1y - p0y,
               p1z - p0z)
        U <- c(p2x - p0x,
               p2y - p0y,
               p2z - p0z)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # get bounds of wedges
        # bounds <- matrix(nrow = 8, ncol = 2,
        #                  data = c(0,       -pi/4,
        #                           -pi/4,   -pi/2,
        #                           -pi/2,   -3*pi/4,
        #                           -3*pi/4, -pi,
        #                           pi,      3*pi/4,
        #                           3*pi/4,  pi/2,
        #                           pi/2,    pi/4,
        #                           pi/4,    0),
        #                  byrow= T)
        bounds <- cbind(atan2(diff_y*diff_dy,
                              diff_x*diff_dx),
                        atan2(diff_y*diff_dy_shifted,
                              diff_x*diff_dx_shifted))
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # bind ijk
        V <- cbind(V[1:8],
                   V[9:16],
                   V[17:24])
        U <- cbind(U[1:8],
                   U[9:16],
                   U[17:24])
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # output cross product
        output <- Cross_Product_Matrix(V,U) 
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # is there a valid angle
        if(any(is.na(output[[4]])) == FALSE){
          # ------------------------------------------------------------------------------------------------
          # convert from signed to unsigned (0-2*pi) angle
          bounds_comp <- cbind(bounds, output[[4]])
          bounds_comp <- (bounds_comp + (2*pi)) %% (2*pi)
          bounds_comp <- bounds_comp * (180/pi)
          bounds_comp[1,1] <- 360
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # do any directions exceed the wedge bounds
          inds <- which(bounds_comp[,3] < apply(bounds_comp[,1:2],1,min)|
                        bounds_comp[,3] > apply(bounds_comp[,1:2],1,max))
          if(length(inds) > 0){
            bounds_comp_min_inds <- apply(matrix(bounds_comp[inds,3] - bounds_comp[inds,1:2],
                                                 ncol = 2),1,which.min)
            bounds_comp[inds,3] <- bounds_comp[cbind(inds,bounds_comp_min_inds)]
          }
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # convert to unsigned angle
          bounds_comp <- bounds_comp * (pi/180)
          # ------------------------------------------------------------------------------------------------
          
          
          # ------------------------------------------------------------------------------------------------
          # convert back to signed angle
          final_out <- ifelse(bounds_comp[,3] > pi,
                              bounds_comp[,3] - 2*pi,
                              bounds_comp[,3])
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # slopes of vectors
          a <- output[[2]]
          b <- output[[3]]
          
          slopes_wedges <- -((a*cos(final_out)) + (b*sin(final_out)))
          slopes_wedges <- atan2(slopes_wedges,1) * (180/3.14159)
          dir_wedges <- final_out
          # ------------------------------------------------------------------------------------------------
          
        } else {
          slopes_wedges <- NA
          dir_wedges <- NA
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        slopes_wedges <- slopes_wedges[is.na(slopes_wedges) == FALSE]
        dir_wedges <- dir_wedges[is.na(dir_wedges) == FALSE]
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # if there are no slopes then all surrounding cells must be NA
        # if there are slopes which is the maximum increase
        if(length(slopes_wedges) > 0){
          # ------------------------------------------------------------------------------------------------
          # is cell a sink ('s') or flat ('f')
          # if all less than 0 (all pointing to cell) its a sink
          # if not zero but all less than min slope its a flat
          # if neither of these criteria is fulfilled
          if(all(slopes_wedges < 0)){
            final_dir <- sink_code
            final_dir_deg <- sink_code
            max_slope <- 0
          } else if (all(slopes_wedges < min_slope)){
            final_dir <- flat_code
            final_dir_deg <- flat_code
            max_slope <- 0
          } else {
            
            ind <- which(slopes_wedges == max(slopes_wedges))
            # ------------------------------------------------------------------------------------------------
            if(length(ind) > 1){
              final_dir <- mean(dir_wedges[ind], na.rm = T)
              final_dir_deg <- final_dir * (180/3.14159)
              final_dir_deg <- (final_dir_deg + 360) %% 360
              max_slope <- mean(slopes_wedges[ind], na.rm = T)
            } else {
              final_dir <- dir_wedges[ind]
              final_dir_deg <- final_dir * (180/3.14159)
              final_dir_deg <- (final_dir_deg + 360) %% 360
              max_slope <- slopes_wedges[ind]
            }
            # ------------------------------------------------------------------------------------------------
            
            # ------------------------------------------------------------------------------------------------
            final_dir <- round(final_dir, 0)
            final_dir_deg <- round(final_dir_deg, 0)
            if(final_dir == 2*pi){
              final_dir <- 0
            }
            if(final_dir_deg == 360){
              final_dir_deg <- 0
            }
            # ------------------------------------------------------------------------------------------------
          }
          # ------------------------------------------------------------------------------------------------
        } else{
          final_dir <- NA
          final_dir_deg <- NA
          max_slope <- NA
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      return(list(final_dir,
                  final_dir_deg,
                  max_slope))
    }
    # ------------------------------------------------------------------------------------------------
    

    
    ######################################### FIND FLOW DIR ACROSS ALL NEIGHBORS ####################
    
    # ------------------------------------------------------------------------------------------------
    # getting flow directions
    flow_dir_deg_output <- list()
    flow_dir_rad_output <- list()
    flow_dir_slope_output <- list()
    counter <- 0
    for(i in 1:nrow(raster)){
      # ------------------------------------------------------------------------------------------------
      # update loading bar
      if(suppress_loading_bar == FALSE){
        loading_bar(i,
                    nrow(raster),
                    width = 50)
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # run function
      for(j in 1:ncol(raster)){
        counter <- counter + 1
        
        output <- flow_dir_of_neighbors(raster = raster,
                                        values = values,
                                        row = i,
                                        column = j,
                                        diff_x = diff_x,
                                        diff_y = diff_y,
                                        min_slope = min_slope)
        flow_dir_deg_output[[counter]] <- output[[2]]
        flow_dir_rad_output[[counter]] <- output[[1]]
        flow_dir_slope_output[[counter]] <- output[[3]]
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    return(list(flow_dir_deg_output,
                flow_dir_rad_output,
                flow_dir_slope_output))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  # ==================================================================================================
  # finds continuous flat areas and gives them an outlet flow direction
  # ==================================================================================================
  resolve_flats_function <- function(input_flow_degree,
                                     input_flow_rad,
                                     raster,
                                     flat_code,
                                     sink_code)
  {
    # ------------------------------------------------------------------------------------------------
    # get matrices of which cells are the flat code, what the dem values are, and which cells have been checked
    flow_degree_values <- matrix(values(input_flow_degree),
                                 ncol = ncol(raster),
                                 nrow = nrow(raster), byrow = TRUE)
    flow_rad_values <- matrix(values(input_flow_rad),
                              ncol = ncol(raster),
                              nrow = nrow(raster), byrow = TRUE)
    raster_values <- matrix(values(raster),
                            ncol = ncol(raster),
                            nrow = nrow(raster), byrow = TRUE)
    check_cells <- matrix(FALSE,
                          ncol = ncol(raster),
                          nrow = nrow(raster), byrow = TRUE)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # neighbor direction
    outlet_neighbors_dx <- c( 0,  1, 1, 1, 0, -1, -1, -1)
    outlet_neighbors_dy <- c(-1, -1, 0, 1, 1,  1,  0, -1)
    diff_dx <- c(0, 1, 1,  1,  0, -1, -1, -1)
    diff_dy <- c(1, 1, 0, -1, -1, -1,  0,  1)
    
    diff_dx_shifted <- c(1, 1,  1, 0, -1, -1, -1, 0) 
    diff_dy_shifted <- c(1, 0, -1, -1, -1, 0,  1, 1)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    all_flats <- list()
    outer_counter <- 0
    loading_counter <- 0
    for(i in 1:nrow(raster)){
      # ------------------------------------------------------------------------------------------------
      for(j in 1:ncol(raster)){
        # ------------------------------------------------------------------------------------------------
        # getting whether the current cell is NA or flat
        row <- i
        column <- j
        current_cell_value <- flow_degree_values[row,column]
        current_cell_check <- check_cells[row,column]
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        if(suppress_loading_bar == FALSE){
          loading_counter <- loading_counter + 1
          loading_bar(loading_counter,
                      ncell(raster),
                      width = 50,
                      optional_text = '')
        }
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # if NA its edge piece (forbidden)
        if(is.na(current_cell_value) != TRUE){
          # ------------------------------------------------------------------------------------------------
          # check if flat code (wanted)
          if(current_cell_value == flat_code & current_cell_check == FALSE){
            # ------------------------------------------------------------------------------------------------
            # find neighbors of current cell, visualize for diagnostic purposes
            outer_counter <- outer_counter + 1
            bound <- cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)
            current_neighbors <- flow_degree_values[cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)]
            current_check <- check_cells[cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)]
            current_cell_value <- flow_degree_values[row,column]
            visual_neighbors <- c(current_neighbors[8], current_neighbors[1:2],
                                  current_neighbors[7], current_cell_value, current_neighbors[3],
                                  current_neighbors[6:4])
            visual_neighbors <- matrix(visual_neighbors,
                                       ncol = 3, nrow = 3, byrow = TRUE)
            # ------------------------------------------------------------------------------------------------
            
            # ------------------------------------------------------------------------------------------------
            done <- FALSE
            current_flats <- list()
            counter <- 0
            remove_first_row <- TRUE
            go_back_and_check <- matrix(c(1,1), nrow = 1)
            while(done == FALSE){
              # ------------------------------------------------------------------------------------------------
              # are any of the current cell neighbors flat?
              flat_neighbors <- which(current_neighbors == flat_code)
              rm <- which(current_check == TRUE)
              if(length(rm) > 0){
                flat_neighbors <- flat_neighbors[-c(which(flat_neighbors %in% rm))]
              }
              # ------------------------------------------------------------------------------------------------
              
              # ------------------------------------------------------------------------------------------------
              # are there any flat neighbors to check next
              if(length(flat_neighbors) != 0){
                counter <- counter + 1
                # ------------------------------------------------------------------------------------------------
                # append which neighbors are flat
                # designate next cell to search from based on which cells are flat
                # whatever is not designated as next cell and is flat, set aside to search from later
                current_flats[[counter]] <- rbind(bound[flat_neighbors,],
                                                  c(row, column))
                check_cells[rbind(bound,
                                  c(row, column))] <- TRUE
                next_chosen_cell <- round(runif(n = 1,
                                                min = 1,
                                                max = nrow(matrix(bound[flat_neighbors,], ncol = 2))))
                
                go_back_and_check <- rbind(go_back_and_check,
                                           bound[flat_neighbors[-c(next_chosen_cell)],])
                next_chosen_cell <- bound[flat_neighbors[next_chosen_cell],]
                row <- next_chosen_cell[1]
                column <- next_chosen_cell[2]
                
                # ------------------------------------------------------------------------------------------------
                
                # ------------------------------------------------------------------------------------------------
                # remove first dummy row?
                if(remove_first_row == TRUE){
                  go_back_and_check <- matrix(go_back_and_check[-c(1), ], ncol = 2)
                  remove_first_row <- FALSE
                }
                # ------------------------------------------------------------------------------------------------
              }
              # ------------------------------------------------------------------------------------------------
              
              # ------------------------------------------------------------------------------------------------
              # if nothing left to check loop must be complete
              # if there are no nearby flats but there are some set aside then check them
              if(length(flat_neighbors) == 0 & nrow(go_back_and_check) == 0){
                current_flats[[counter]] <- matrix(c(row,column), nrow = 1)
                done <- TRUE
                
              } else if(length(flat_neighbors) == 0 & nrow(go_back_and_check) != 0){
                # ------------------------------------------------------------------------------------------------
                # finding next search starting point from what was set aside
                done2 <- FALSE
                while(done2 == FALSE){
                  next_chosen_cell <- round(runif(n = 1,
                                                  min = 1,
                                                  max = nrow(go_back_and_check)))
                  row <- go_back_and_check[next_chosen_cell, 1]
                  column <- go_back_and_check[next_chosen_cell, 2]
                  go_back_and_check <- matrix(go_back_and_check[-c(next_chosen_cell), ], ncol = 2)
                  counter <- counter + 1
                  check_cells[row,column] <- TRUE
                  current_flats[[counter]] <- matrix(c(row,column), nrow = 1)
                  
                  if(nrow(go_back_and_check) == 0){
                    done2 <- TRUE
                    
                  } else if(any(check_cells[cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)]  == FALSE)){
                    done2 <- TRUE
                  } else{}
                }
                # ------------------------------------------------------------------------------------------------
                
                # ------------------------------------------------------------------------------------------------
                # remove first dummy row?
                if(remove_first_row == TRUE){
                  go_back_and_check <- matrix(go_back_and_check[-c(1), ], ncol = 2)
                  remove_first_row <- FALSE
                }
                # ------------------------------------------------------------------------------------------------
              }
              # ------------------------------------------------------------------------------------------------
              
              
              # ------------------------------------------------------------------------------------------------
              # find neighbors of current cell, visualize for diagnostic purposes
              bound <- cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)
              current_neighbors <- flow_degree_values[cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)]
              current_check <- check_cells[cbind(row + outlet_neighbors_dy, column + outlet_neighbors_dx)]
              current_cell_value <- flow_degree_values[row,column]
              visual_neighbors <- c(current_neighbors[8], current_neighbors[1:2],
                                    current_neighbors[7], current_cell_value, current_neighbors[3],
                                    current_neighbors[6:4])
              visual_neighbors <- matrix(visual_neighbors,
                                         ncol = 3, nrow = 3, byrow = TRUE)
              # ------------------------------------------------------------------------------------------------
              
            }
            # ------------------------------------------------------------------------------------------------
            
            # ------------------------------------------------------------------------------------------------
            all_flats[[outer_counter]] <- do.call(rbind, current_flats)
            # ------------------------------------------------------------------------------------------------
          } else {}
          # ------------------------------------------------------------------------------------------------
        } else {}
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    


    
    
    
    # ------------------------------------------------------------------------------------------------
    if(length(all_flats) > 0){

      # ------------------------------------------------------------------------------------------------
      for(i in 1:length(all_flats)){
        # ------------------------------------------------------------------------------------------------
        # set up list
        flat_neighbors_outlet <- list()
        counter <- 0
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        for(j in 1:nrow(all_flats[[i]])){
          # ------------------------------------------------------------------------------------------------
          # get all neighbors and directions of the current flat region
          counter <- counter + 1
          flat_neighbors_outlet[[counter]] <- cbind(all_flats[[i]][j,1] + outlet_neighbors_dy,
                                                    all_flats[[i]][j,2] + outlet_neighbors_dx)
          current_dem_value <- raster_values[cbind(all_flats[[i]][j,1],
                                                   all_flats[[i]][j,2])]
          neighbors_direction <- flow_degree_values[cbind(all_flats[[i]][j,1] + outlet_neighbors_dy,
                                                          all_flats[[i]][j,2] + outlet_neighbors_dx)]
          neighbor_dem_delta <- current_dem_value - raster_values[cbind(all_flats[[i]][j,1] + outlet_neighbors_dy,
                                                                        all_flats[[i]][j,2] + outlet_neighbors_dx)]
          flat_neighbors_outlet[[counter]] <- cbind(neighbors_direction,
                                                    neighbor_dem_delta,
                                                    all_flats[[i]][j,1] + outlet_neighbors_dy,
                                                    all_flats[[i]][j,2] + outlet_neighbors_dx)
          # ------------------------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # bind
        # only find where there is a direction and the cell is at or lower than the current cell
        # we only want where the flat will flow down, not where the flow is coming from
        # if the flat region has no cells lower with a valid direction it must be a sink
        flat_neighbors_outlet <- do.call(rbind, flat_neighbors_outlet)
        flat_neighbors_outlet <- unique(flat_neighbors_outlet)
        flat_neighbors_outlet <- flat_neighbors_outlet[is.na(flat_neighbors_outlet[ ,1]) == FALSE &
                                                         is.na(flat_neighbors_outlet[ ,2]) == FALSE, ]
        if(nrow(flat_neighbors_outlet) != 0){
          flat_neighbors_outlet <- flat_neighbors_outlet[flat_neighbors_outlet[ ,1] != flat_code &
                                                           flat_neighbors_outlet[ ,2] >= 0, ]
          if(nrow(flat_neighbors_outlet) != 0){
            
            all_flats[[i]] <- unique(all_flats[[i]])
            deg <- list()
            rad <- list()
            for(k in 1:nrow(all_flats[[i]])){
              row_deltas <- all_flats[[i]][k,1] - flat_neighbors_outlet[,3]
              column_deltas <- all_flats[[i]][k,2] - flat_neighbors_outlet[,4]
              rad_s1 <- atan2(sum(row_deltas*diff_y),sum(column_deltas*-1*diff_x))
              rad[[k]] <- rad_s1
              deg_s1 <- rad_s1 * (180/pi)
              deg[[k]] <- (deg_s1 + 360) %% 360
            }
            
            
            # y_component <- sin((flat_neighbors_outlet[,1])*(pi/180))
            # x_component <- cos((flat_neighbors_outlet[,1])*(pi/180))
            # rad <- atan(sum(y_component)/sum(x_component))
            # deg <- (rad * 180)/3.14159
            # deg <- round(deg)
            flow_degree_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- as.vector(unlist(deg))
            flow_rad_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- as.vector(unlist(rad))
            
          } else {
            flow_degree_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- sink_code
            flow_rad_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- sink_code
          }
        } else {
          flow_degree_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- sink_code
          flow_rad_values[cbind(all_flats[[i]][,1],all_flats[[i]][,2])] <- sink_code
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
    } else {}
    # ------------------------------------------------------------------------------------------------
    return(list(flow_degree_values,
                flow_rad_values))
  }
  # ------------------------------------------------------------------------------------------------
  
  

  
  
  # ==================================================================================================
  # Accumulates flow going to all cells
  # ==================================================================================================
  neighbors_pointing_to_current_cell <- function(raster,
                                                diff_x,
                                                diff_y,
                                                flow_dir_deg_output,
                                                outlet_cell)
  {
    # ------------------------------------------------------------------------------------------------
    edge1 <- 1:ncol(raster) # top edge
    edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
    edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
    edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
    edges <- c(edge1,edge2,edge3,edge4)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # directions pointing directly at neighboring cells
    cardinalsx <- c(0, 1, 1,  1,  0, -1, -1, -1)
    cardinalsy <- c(1, 1, 0, -1, -1, -1,  0,  1)
    cardinals <- atan2(diff_y*cardinalsy,
                       diff_x*cardinalsx)
    cardinals <- (cardinals + (2*pi)) %% (2*pi)
    cardinals <- cardinals * (180/pi)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    outlet_neighbors_dx <- c( 0,  1, 1, 1, 0, -1, -1, -1)
    outlet_neighbors_dy <- c(-1, -1, 0, 1, 1,  1,  0, -1)
    diff_dx <- c(0, 1, 1,  1,  0, -1, -1, -1)
    diff_dy <- c(1, 1, 0, -1, -1, -1,  0,  1)
    
    diff_dx_shifted <- c(1, 1,  1, 0, -1, -1, -1, 0) 
    diff_dy_shifted <- c(1, 0, -1, -1, -1, 0,  1, 1)
    
    bounds <- cbind(atan2(diff_y*diff_dy,
                          diff_x*diff_dx),
                    atan2(diff_y*diff_dy_shifted,
                          diff_x*diff_dx_shifted))
    bounds <- (bounds + (2*pi)) %% (2*pi)
    bounds <- bounds * (180/pi)
    bounds[3,1] <- 360
    # ------------------------------------------------------------------------------------------------

    # ------------------------------------------------------------------------------------------------
    # get degree values
    flow_dir_deg_mat <- matrix(flow_dir_deg_output,
                               ncol = ncol(raster),
                               nrow = nrow(raster),
                               byrow = TRUE)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # angles that neighbors can point to
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      diff_x <- res(raster)[1]
      diff_y <- res(raster)[2]
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # which cells have been checked, and which flow to the outlet cell
    flow_accumulation <- matrix(0, ncol = ncol(raster), nrow = nrow(raster))
    # ------------------------------------------------------------------------------------------------
    
    
    for(i in 1:length(outlet_cell)){
      # ------------------------------------------------------------------------------------------------
      # progenitor outlet column, not changed, outlet column and row are updated in while loops while this
      # one only updated in the for loop
      outlet_column <- outlet_cell[i] %% ncol(raster)
      if(outlet_column == 0){
        outlet_column <- ncol(raster)
        outlet_row <- floor(outlet_cell[i]/ncol(raster))
      } else {
        outlet_row <- floor(outlet_cell[i]/ncol(raster))
        outlet_row <- outlet_row + 1
      }
      current_cell_value <- flow_dir_deg_mat[cbind(outlet_row, outlet_column)]
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        loading_bar(i,
                    length(outlet_cell),
                    width = 50,
                    optional_text = paste0('Cell: ',i))
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      if(is.na(current_cell_value) == FALSE){

        # ------------------------------------------------------------------------------------------------
        output <- check_outlet_neighbors2(cardinals,
                                          bounds,
                                          current_cell_value)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # if the current cell points to any neighbors, increment their values by 1
        inds_to_update <- which(output[[1]] == TRUE)
        if(length(inds_to_update) > 0){
          current_accumulation <- flow_accumulation[cbind(outlet_row + outlet_neighbors_dy[inds_to_update],
                                                          outlet_column + outlet_neighbors_dx[inds_to_update])]
          new_accumulation <- current_accumulation + 1
          flow_accumulation[cbind(outlet_row + outlet_neighbors_dy[inds_to_update],
                                  outlet_column + outlet_neighbors_dx[inds_to_update])] <- new_accumulation * output[[2]]
          
        } else {}
        # ------------------------------------------------------------------------------------------------
      } else {}
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    return(flow_accumulation)
  }
  # ------------------------------------------------------------------------------------------------  
    

  
  
  # ==================================================================================================
  # Accumulates flow going to all cells
  # ==================================================================================================
  accumulate_flow_across_all_cells <- function(raster,
                                               diff_x,
                                               diff_y,
                                               flow_dir_deg_output)
  {
    # ------------------------------------------------------------------------------------------------
    edge1 <- 1:ncol(raster) # top edge
    edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
    edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
    edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
    edges <- c(edge1,edge2,edge3,edge4)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # directions pointing directly at neighboring cells
    cardinalsx <- c(0, 1, 1,  1,  0, -1, -1, -1)
    cardinalsy <- c(1, 1, 0, -1, -1, -1,  0,  1)
    cardinals <- atan2(diff_y*cardinalsy,
                       diff_x*cardinalsx)
    cardinals <- (cardinals + (2*pi)) %% (2*pi)
    cardinals <- cardinals * (180/pi)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    outlet_neighbors_dx <- c( 0,  1, 1, 1, 0, -1, -1, -1)
    outlet_neighbors_dy <- c(-1, -1, 0, 1, 1,  1,  0, -1)
    diff_dx <- c(0, 1, 1,  1,  0, -1, -1, -1)
    diff_dy <- c(1, 1, 0, -1, -1, -1,  0,  1)
    
    diff_dx_shifted <- c(1, 1,  1, 0, -1, -1, -1, 0) 
    diff_dy_shifted <- c(1, 0, -1, -1, -1, 0,  1, 1)
    
    bounds <- cbind(atan2(diff_y*diff_dy,
                          diff_x*diff_dx),
                    atan2(diff_y*diff_dy_shifted,
                          diff_x*diff_dx_shifted))
    bounds <- (bounds + (2*pi)) %% (2*pi)
    bounds <- bounds * (180/pi)
    bounds[3,1] <- 360
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # get degree values
    flow_dir_deg_mat <- matrix(flow_dir_deg_output,
                               ncol = ncol(raster),
                               nrow = nrow(raster),
                               byrow = TRUE)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # angles that neighbors can point to
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      diff_x <- res(raster)[1]
      diff_y <- res(raster)[2]
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # which cells have been checked, and which flow to the outlet cell
    flow_accumulation <- matrix(1, ncol = ncol(raster), nrow = nrow(raster))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    vals <- matrix(values(raster), ncol = ncol(raster), nrow = nrow(raster), byrow = T)
    ord  <- order(vals, decreasing = TRUE)
    ordered_indices <- arrayInd(ord, dim(vals))
    counter <- 0
    characters <- c('|', '/', '-','\\')

    for(i in 1:nrow(ordered_indices)){
      counter <- counter + 1
      # ------------------------------------------------------------------------------------------------
      # progenitor outlet column, not changed, outlet column and row are updated in while loops while this
      # one only updated in the for loop
      outlet_row <- ordered_indices[i,1]
      outlet_column <- ordered_indices[i,2]
      current_cell_value <- flow_dir_deg_mat[cbind(outlet_row, outlet_column)]
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # update loading bar
      if(suppress_loading_bar == FALSE){
        loading_bar(i,
                    nrow(ordered_indices),
                    width = 50,
                    optional_text = paste0('Cell: ',i))
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      if(is.na(current_cell_value) == TRUE){
        
      } else if (current_cell_value %in% c(flat_code, sink_code)){
        
      } else {
        # ------------------------------------------------------------------------------------------------
        output <- check_outlet_neighbors2(cardinals,
                                          bounds,
                                          current_cell_value)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # if the current cell points to any neighbors, increment their values by 1
        inds_to_update <- which(output[[1]] == TRUE)
        if(length(inds_to_update) > 0){
          
          # ------------------------------------------------------------------------------------------------
          # update the accumulation by adding one
          current_accumulation_of_start_cell <- flow_accumulation[cbind(outlet_row, outlet_column)]
          downstream_accumulation <- flow_accumulation[cbind(outlet_row + outlet_neighbors_dy[inds_to_update],
                                                             outlet_column + outlet_neighbors_dx[inds_to_update])]
          
          flow_accumulation[cbind(outlet_row + outlet_neighbors_dy[inds_to_update],
                                  outlet_column + outlet_neighbors_dx[inds_to_update])] <- downstream_accumulation + 
            (current_accumulation_of_start_cell * output[[2]])
          # ------------------------------------------------------------------------------------------------
          
        } else {}
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------

    cat('\n')
    return(flow_accumulation)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Recursively checks all cells that flow to the outlet
  # ==================================================================================================
  check_all_cells_flowing_to_outlet <- function(raster,
                                                diff_x,
                                                diff_y,
                                                flow_dir_deg_output,
                                                outlet_cell)
  {
    # ------------------------------------------------------------------------------------------------
    outlet_neighbors_dx <- c( 0,  1, 1, 1, 0, -1, -1, -1)
    outlet_neighbors_dy <- c(-1, -1, 0, 1, 1,  1,  0, -1)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # angles that neighbors can point to
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      diff_x <- res(raster)[1]
      diff_y <- res(raster)[2]
    }
    
    diff_dx <- c(-1, -1, -1, 0, 1, 1,  1,  0)
    diff_dy <- c(-1,  0,  1, 1, 1, 0, -1, -1)
    
    diff_dx_shifted <- c( 1,  0, -1, -1, -1, 0, 1, 1) 
    diff_dy_shifted <- c(-1, -1, -1,  0,  1, 1, 1, 0)
    
    bounds <- cbind(atan2(diff_y*diff_dy,
                          diff_x*diff_dx),
                    atan2(diff_y*diff_dy_shifted,
                          diff_x*diff_dx_shifted))
    bounds <- (bounds + (2*pi)) %% (2*pi)
    bounds <- bounds * (180/pi)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # get degree values
    flow_dir_deg_mat <- matrix(flow_dir_deg_output,
                               ncol = ncol(raster),
                               nrow = nrow(raster),
                               byrow = TRUE)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    final_all_check <- list()
    final_outlet <- list()
    for(i in 1:length(outlet_cell)){
      # ------------------------------------------------------------------------------------------------
      # which cells have been checked, and which flow to the outlet cell
      all_check <- matrix(FALSE,
                          ncol = ncol(raster),
                          nrow = nrow(raster))
      cells_flowing_to_outlet <- all_check
      # ------------------------------------------------------------------------------------------------
      

      
      # ------------------------------------------------------------------------------------------------
      # which column and row is the outlet in
      # if its a multiple of the columns it must be the last column and then the row stays the same
      # else a value of something like 64 in a 10 by 10 matrix will actually be located in the 7th row at position 4
      # so add one to the outlet row
      outlet_column <- outlet_cell[i] %% ncol(raster)
      if(outlet_column == 0){
        outlet_column <- ncol(raster)
        outlet_row <- floor(outlet_cell[i]/ncol(raster))
      } else {
        outlet_row <- floor(outlet_cell[i]/ncol(raster))
        outlet_row <- outlet_row + 1
      }
      all_check[outlet_row, outlet_column] <- TRUE
      cells_flowing_to_outlet[outlet_row, outlet_column] <- TRUE
      
      all_row_columns <- matrix(data = c(outlet_row, outlet_column), nrow = 1, ncol = 2)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # positions of all eight cell neighbors N, NE, E, SE, S, SW, W, NW

      outlet_neighbors <- flow_dir_deg_mat[cbind(outlet_row + outlet_neighbors_dy,
                                                 outlet_column + outlet_neighbors_dx)]
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      finished <- FALSE
      counter <- 0
      ncheck <- 0
      nstep <- 0
      niter <- 0
      go_back_and_check <- matrix(c(1,1), ncol = 2)
      characters <- c('|', '/', '-','\\')
      while(finished == FALSE){
        niter <- niter + 1
        
        # ------------------------------------------------------------------------------------------------
        # check what cells flow to current cell
        output <- check_outlet_neighbors(bounds,
                                         outlet_neighbors)
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # update matrices
        row_columns <- cbind(outlet_row + outlet_neighbors_dy,
                             outlet_column + outlet_neighbors_dx)
        update_needed <- which(cells_flowing_to_outlet[row_columns] == FALSE)
        ncheck <- ncheck + length(which(all_check[row_columns] == FALSE))
        if(length(update_needed) != 0){
          cells_flowing_to_outlet[matrix(row_columns[update_needed, ], ncol = 2)] <- output[[1]][update_needed]
          all_check[matrix(row_columns[update_needed, ], ncol = 2)] <- output[[2]][update_needed]
        } else {}
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # find which rows and columns to check next
        next_row_column <- matrix(row_columns[which(output[[1]] == TRUE), ],
                                  ncol = 2)
        key <- paste0(all_row_columns[, 1], all_row_columns[ ,2])
        key_next <- paste0(next_row_column[ ,1], next_row_column[ ,2])
        rm <- which((key_next %in% key) == TRUE)
        if(length(rm) > 0){
          next_row_column <- matrix(next_row_column[-c(rm), ], ncol = 2)
        } else {}
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # update loading bar
        if(suppress_loading_bar == FALSE){
          if(niter %% spinning_bar_update_cycle == 0){
            nstep <- nstep + 1
            pos <- nstep%%length(characters)
            if(pos == 0){
              pos <- 1
            }
            spinning_bar(optional_text = paste0('Outlet Cell: ',i,
                                                ' | Niter: ', niter,
                                                ' | Unique Cell Checked: ', ncheck),
                         character = characters[pos],
                         iter = i + niter-1)
          } else {
            if(niter == 1){
              pos <- 1
            } else {}
            spinning_bar(optional_text = paste0('Outlet Cell: ',i,
                                                ' | Niter: ', niter,
                                                ' | Unique Cell Checked: ', ncheck),
                         character = characters[pos],
                         iter = i + niter-1)
          }
        }
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # if the current cell has neighbors still to check then check them
        # if more than one cell to check choose a random one first and set other aside
        # if current cell has none to check look back to the ones that have been set aside
        # and choose a random one from that set
        # if no neighbors need to be checked on current cell, and none are left set aside set break loop
        if(nrow(next_row_column) >= 1){
          # ------------------------------------------------------------------------------------------------
          # get the next row and column
          to_check_currently <- round(runif(n = 1, min = 1, max = nrow(next_row_column)))
          set_aside <- c(1:nrow(next_row_column))[-c(to_check_currently)]
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # if there are items to set aside put it in the matrix
          if(length(set_aside) > 0){
            # ------------------------------------------------------------------------------------------------
            # if its the first time remove dummy values, if not just bind it
            if(counter == 0){
              counter <- counter + 1
              go_back_and_check <- rbind(go_back_and_check,
                                         next_row_column[set_aside, ])
              go_back_and_check <- matrix(go_back_and_check[-c(1), ],
                                          ncol = 2)
            } else {
              go_back_and_check <- rbind(go_back_and_check,
                                         next_row_column[set_aside, ])
            }
            # ------------------------------------------------------------------------------------------------
          } else{}
          next_row_column <- matrix(next_row_column[to_check_currently, ], ncol = 2)
          # ------------------------------------------------------------------------------------------------
        } else {
          
          # ------------------------------------------------------------------------------------------------
          # if theres nothing left set aside, then the loop must be over
          if(nrow(go_back_and_check) != 0 &
             counter != 0){
            # ------------------------------------------------------------------------------------------------
            # get the next row and column
            go_back_and_check <- unique(go_back_and_check)
            
            # ------------------------------------------------------------------------------------------------
            # while the randomly selected row and column are already in the set of checked cells continue
            # continually remove cells from go_back_and_check if they are already checked
            # if it is not in the set of checked cells set that to the next one to check
            # break loop
            row_column_new <- FALSE
            while(row_column_new == FALSE){
              # ------------------------------------------------------------------------------------------------
              to_check_currently <- round(runif(n = 1, min = 1, max = nrow(go_back_and_check)))
              set_aside <- c(1:nrow(go_back_and_check))[-c(to_check_currently)]
              # ------------------------------------------------------------------------------------------------
              
              # ------------------------------------------------------------------------------------------------
              # if its already in the set of things to check, remove it and redo
              step1 <- matrix(all_row_columns[all_row_columns[ ,1] == go_back_and_check[to_check_currently, 1], ],
                              ncol = 2)
              step2 <- matrix(step1[step1[ ,2] == go_back_and_check[to_check_currently, 2], ],
                              ncol = 2)
              if(nrow(step2) > 0){
                # ------------------------------------------------------------------------------------------------
                # if there are no cells to go back and check, at this point in the program the list
                # of primary cells has also been exhausted. Therefore there are no primary cells to check
                # and no cells to go back and check. Everything is exhausted. Break loop.
                go_back_and_check <- matrix(go_back_and_check[-c(to_check_currently), ],
                                            ncol = 2)
                if(nrow(go_back_and_check) == 0){
                  row_column_new <- TRUE
                  finished <- TRUE
                  next_row_column <- c(2,2)
                }
                # ------------------------------------------------------------------------------------------------
              }
              # ------------------------------------------------------------------------------------------------
              
              # ------------------------------------------------------------------------------------------------
              if(nrow(step2) == 0){
                next_row_column <- matrix(go_back_and_check[to_check_currently, ], ncol = 2)
                go_back_and_check <- matrix(go_back_and_check[set_aside, ],
                                            ncol = 2)
                row_column_new <- TRUE
              }
              # ------------------------------------------------------------------------------------------------
            }
            # ------------------------------------------------------------------------------------------------
          } else {
            finished <- TRUE
            next_row_column <- c(2,2)
          }
          # ------------------------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # increment loop to the next step
        outlet_row <- next_row_column[1]
        outlet_column <- next_row_column[2]
        outlet_neighbors <- flow_dir_deg_mat[cbind(outlet_row + outlet_neighbors_dy,
                                                   outlet_column + outlet_neighbors_dx)]
        all_row_columns <- rbind(all_row_columns,
                                 next_row_column)
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      final_all_check[[i]] <- all_check
      final_outlet[[i]] <- cells_flowing_to_outlet
    }
    cat('\n')
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # cbinding to see where any cell is true
    names(final_all_check) <- NULL
    final_all_check <- lapply(final_all_check, function(x){as.vector(x)})
    final_all_check <- do.call(cbind, final_all_check)
    
    names(final_outlet) <- NULL
    final_outlet <- lapply(final_outlet, function(x){as.vector(x)})
    final_outlet <- do.call(cbind, final_outlet)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # if any cell is true set final output in that position to true
    all_check <- rep(FALSE, nrow(final_all_check))
    cells_flowing_to_outlet <- rep(FALSE, nrow(final_outlet))
    for(i in 1:nrow(final_all_check)){
      if(any(final_all_check[i, ] == TRUE)){
        all_check[i] <- TRUE
      }
      if(any(final_outlet[i, ] == TRUE)){
        cells_flowing_to_outlet[i] <- TRUE
      }
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # making matrices
    all_check <- matrix(all_check,
                        ncol = ncol(raster),
                        nrow = nrow(raster))
    cells_flowing_to_outlet <- matrix(cells_flowing_to_outlet,
                                      ncol = ncol(raster),
                                      nrow = nrow(raster))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # return to higher level
    return(list(all_check,
                cells_flowing_to_outlet))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  ############################################################################################################
  ################################## ERRORS ##################################################################
  ############################################################################################################
  
  # ------------------------------------------------------------------------------------------------
  # not a raster error
  if(class(raster)[1] != 'SpatRaster'){
    if(class(raster)[1] != 'RasterLayer'){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Inputs not in the form of a raster\n'))
    } else {
      raster <- rast(raster)
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # raster too small error
  if(ncell(raster) < 16){
    stop(paste0('\nWatershed_Delineator:\n\n',
                'Raster too small (<16 cells) to be meaningful\n')) 
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # forgot to pass outlet location error
  if(is.null(outlet_location) == TRUE){
    stop(paste0('\nWatershed_Delineator:\n\n',
                'Outlet location passed is NULL\n'))
  } else {
    outlet_location <- coerce_outlet_location(outlet_location)
    outlet_cell <- terra::cellFromXY(raster, st_coordinates(outlet_location))
    
    # ------------------------------------------------------------------------------------------------
    # is outlet location outside raster extent
    if(any(is.na(outlet_cell)) == TRUE){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Outlet location passed is not within raster extent\n',
                  'Is passed CRS of outlet location correct?\n'))
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # testing to see if outlet location is border cell (forbidden)
    # border is replaced with NA in first step of calculating flow direction, as direction cant be determined
    # when neighbors of cell are not defined
    edge1 <- 1:ncol(raster) # top edge
    edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
    edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
    edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
    if(any(outlet_cell %in% c(edge1, edge2, edge3, edge4))){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Outlet location passed is on border cell of raster (forbidden)\n',
                  'Either pass more interior location or extend DEM past current location'))
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------

  
  
  
  # ------------------------------------------------------------------------------------------------
  # make outlet location into a line if necessary
  if(is.null(outlet_location) == FALSE){
    if(outlet_location_is_line == TRUE){
      if(nrow(outlet_location) < 2){
        stop(paste0('\nWatershed_Delineator:\n\n',
                    'Outlet location is marked as line, yet only single point passed.\n',
                    'Please either designate outlet as a point, or pass multiple points\n',
                    'between which to draw a line.\n'))
      } else {
        # ------------------------------------------------------------------------------------------------
        # making linestring
        out_line <- st_coordinates(outlet_location)
        out_line <- st_linestring(out_line)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # moving to sf object
        if(is.na(outlet_location_CRS) == FALSE){
          out_line <- st_sf(st_sfc(out_line),
                            crs = outlet_location_CRS)
        } else {
          out_line <- st_sf(st_sfc(out_line),
                            crs = st_crs(raster))
        }
        st_geometry(out_line) <- 'geometry'
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        new_line_points <- custom_st_line_sample(sfobj = out_line,
                                                 n = outlet_location_line_density)
        outlet_location <- new_line_points
        outlet_cell <- terra::cellFromXY(raster, st_coordinates(new_line_points))
        outlet_cell <- unique(outlet_cell)
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # testing to see if outlet location is border cell (forbidden)
        # border is replaced with NA in first step of calculating flow direction, as direction cant be determined
        # when neighbors of cell are not defined
        edge1 <- 1:ncol(raster) # top edge
        edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
        edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
        edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
        if(any(outlet_cell %in% c(edge1, edge2, edge3, edge4))){
          stop(paste0('\nWatershed_Delineator:\n\n',
                      'Outlet location passed is on border cell of raster (forbidden)\n',
                      'Either pass more interior location or extend DEM past current location'))
        }
        # ------------------------------------------------------------------------------------------------
      }
    } else {}
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## CORRECTIONS #############################################################
  ############################################################################################################
  
  
  # ------------------------------------------------------------------------------------------------
  # accounting for no entered names
  if(is.null(flow_dir_rast_name) == TRUE){
    flow_dir_rast_name <- 'flow_dir_rast'
  }
  
  if(is.null(flow_to_outlet_rast_name) == TRUE){
    flow_to_outlet_rast_name <- 'flow_to_outlet_rast'
  }
  
  if(is.null(flow_accumulation_rast_name) == TRUE){
    flow_accumulation_rast_name <- 'flow_accumulation_rast'
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # ensure correct packages are loaded
  required_packages <- c('sf','sp','raster','terra')
  for(i in 1:length(required_packages)){
    require_package(required_packages[i])
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # getting km res from arc second raster
  axis <- strsplit(crs(raster),'\n')[[1]]
  axis <- axis[grep('AXIS',axis)] %>%
    trimws() %>% strsplit("\"")
  if(length(grep('Lat',axis)) > 0){ # can latitude be found in the axis def, if so its latlon
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      y <- (ymin(raster) + ymax(raster))/2
      x <- (xmin(raster) + xmax(raster))/2
      diff_x <- Haversine_Formula(y, x,
                                  y, x + res(raster)[1]) * 1000
      diff_y <- Haversine_Formula(y, x,
                                  y + res(raster)[2],x) * 1000
      
      if(zunit == 'ft'){
        diff_x <- diff_x * 3.28
        diff_y <- diff_y * 3.28
      }
    }
  } else {
    diff_x <- res(raster)[1]
    diff_y <- res(raster)[2]
    
    if(zunit == 'ft'){
      diff_x <- diff_x * 3.28
      diff_y <- diff_y * 3.28
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # padding values
  values <- values(raster)
  nrow <- nrow(raster)
  ncol <- ncol(raster)
  values <- matrix(values,
                   nrow = nrow,
                   ncol = ncol,
                   byrow = TRUE)
  values <- rbind(values,
                  rep(NA,ncol))
  values <- rbind(rep(NA,ncol),
                  values)
  values <- cbind(values,
                  rep(NA,nrow+2))
  values <- cbind(rep(NA,nrow+2),
                  values)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## RUN FUNCTIONS ###########################################################
  ############################################################################################################

  ################################################## FLOW DIR FROM DEM #######################################
  
  # ------------------------------------------------------------------------------------------------
  # notify user
  if(suppress_console_messages == FALSE){
    mils <- ncell(raster)/1e6
    seconds <- mils*159
    minutes <- round(seconds/60,2)
    
    cat(paste0('\n####################################################################################\n'))
    cat(paste0('WATERSHED DELINEATOR\n'))
    cat(paste0('####################################################################################\n\n'))
    cat(paste0('\nFor details on the machine these functions were tested on please call wdl_machine_specs()\n\n'))
    cat(paste0('Calculating flow direction raster.\n',
               'Based on historical performance and the size of your raster this will take:\n',
               minutes,' minutes\n\n'))
    cat('Step (1/3)\n')
  }
  # ------------------------------------------------------------------------------------------------
  

  # ------------------------------------------------------------------------------------------------
  # building raster and writing out
  cat('Flow Direction and Slope \n')
  output <- flow_dir_of_DEM(raster = raster)
  
  flow_dir_deg_output <- as.numeric(as.vector(unlist(output[[1]])))
  flow_dir_rad_output <- as.numeric(as.vector(unlist(output[[2]])))
  flow_dir_slope_output <- as.numeric(as.vector(unlist(output[[3]])))
  flow_dir_rast <- rast(ncol = ncol(raster),
                        nrow = nrow(raster),
                        crs = crs(raster),
                        xmin = xmin(raster),
                        xmax = xmax(raster),
                        ymin = ymin(raster),
                        ymax = ymax(raster))
  flow_dir_deg_rast <- flow_dir_rast
  flow_dir_rad_rast <- flow_dir_rast
  flow_dir_slope_rast <- flow_dir_rast
  
  values(flow_dir_deg_rast) <- flow_dir_deg_output
  values(flow_dir_rad_rast) <- flow_dir_rad_output
  values(flow_dir_slope_rast) <- flow_dir_slope_output

  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  if(resolve_flats == TRUE){
    cat(paste0('Resolving Flats\n'))
    output <- resolve_flats_function(input_flow_degree = flow_dir_deg_rast,
                                     input_flow_rad = flow_dir_rad_rast,
                                     raster = raster,
                                     flat_code = flat_code,
                                     sink_code = sink_code)
    flow_dir_deg_output <- list()
    for(i in 1:nrow(output[[1]])){
      flow_dir_deg_output[[i]] <- output[[1]][i,]
    }
    flow_dir_deg_output <- as.numeric(as.vector(unlist(flow_dir_deg_output)))
    values(flow_dir_deg_rast) <- output[[1]]
    values(flow_dir_rad_rast) <- output[[2]]
    cat('\n\n')
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  stack <- c(flow_dir_deg_rast,
             flow_dir_rad_rast,
             flow_dir_slope_rast)
  names(stack) <- c('degrees','radians','slope')
  terra::writeRaster(stack,
              file.path(out_dir,paste0(flow_dir_rast_name,'.tif')),
              overwrite = TRUE)
  # ------------------------------------------------------------------------------------------------

  
  
  
  
  
  
  
  ################################################## FLOW ACCUMULATION #######################################
  
  # ------------------------------------------------------------------------------------------------
  # notify user
  if(suppress_console_messages == FALSE){
    
    cat(paste0('Calculating watershed of specified outlet point.\n'))
    cat('Step (2/3)\n')
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  cat('Indegree Raster\n')
  indegree <- neighbors_pointing_to_current_cell(raster = raster,
                                                 diff_x = diff_x,
                                                 diff_y = diff_y,
                                                 flow_dir_deg_output = flow_dir_deg_output,
                                                 outlet_cell = 1:ncell(raster))
  cat('Flow Accumulation Raster\n')
  flow_accum <- accumulate_flow_across_all_cells(raster = raster,
                                                 diff_x = diff_x,
                                                 diff_y = diff_y,
                                                 flow_dir_deg_output = flow_dir_deg_output)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  flow_dir_rast <- rast(ncol = ncol(raster),
                        nrow = nrow(raster),
                        crs = crs(raster),
                        xmin = xmin(raster),
                        xmax = xmax(raster),
                        ymin = ymin(raster),
                        ymax = ymax(raster))
  flow_rast <- flow_dir_rast
  indegree_rast <- flow_dir_rast
  
  values(flow_rast) <- flow_accum
  values(indegree_rast) <- indegree
  
  stack <- c(flow_rast,
             indegree_rast)
  names(stack) <- c('Flow Accumulation','In Degree')
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # writeout
  terra::writeRaster(stack,
              file.path(out_dir,paste0(flow_accumulation_rast_name,'.tif')),
              overwrite = TRUE)
  cat('\n')
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  ################################################## UPSTREAM RECURSION #######################################
  
  # ------------------------------------------------------------------------------------------------
  # notify user
  if(suppress_console_messages == FALSE){

    cat(paste0('Calculating watershed of specified outlet point.\n'))
    cat('Step (3/3)\n')
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  output <- check_all_cells_flowing_to_outlet(raster = raster,
                                              diff_x = diff_x,
                                              diff_y = diff_y,
                                              flow_dir_deg_output = flow_dir_deg_output,
                                              outlet_cell = outlet_cell)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  flow_dir_rast <- rast(ncol = ncol(raster),
                        nrow = nrow(raster),
                        crs = crs(raster),
                        xmin = xmin(raster),
                        xmax = xmax(raster),
                        ymin = ymin(raster),
                        ymax = ymax(raster))
  flow_rast <- flow_dir_rast
  check_rast <- flow_dir_rast
  
  values(flow_rast) <- output[[2]]
  values(check_rast) <- output[[1]]

  stack <- c(flow_rast,
             check_rast)
  names(stack) <- c('Flow to Outlet','Checked Cells')
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # writeout
  terra::writeRaster(stack,
              file.path(out_dir,paste0(flow_to_outlet_rast_name,'.tif')),
              overwrite = TRUE)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------