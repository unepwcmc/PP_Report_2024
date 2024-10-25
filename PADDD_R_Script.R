###########################################################################################################
## Protected Planet Report 2024 - Protected Area Downgrade, Downsize, and Degazettement (PADDD) Analysis ##
###########################################################################################################

# load packages
library(dplyr)
library(tibble)
library(tidyr)
library(sf)
library(tidyverse)
library(ggplot2)
library(classInt)
library(tmap)
library(lwgeom)

# input files
PADDDEvents <- read.csv("path_to_PADDD_file_directory/PADDDEvents.csv")
# this file was produced by exporting the PADDDevents with polygon data to ArcGIS
# and reprojecting it there to WGS84 before calculatign the geodesic area for each event
all_polygons_geodesic_area <- read.csv("path_to_PADDD_file_directory/all_geo_polygons_desig_area.csv")
# the following datasets contain the geodesic areas in WGS84 for PAs and countries
geodesic_country_areas <- read.csv("path_to_PADDD_file_directory/National_Land_Marine_Area_WGS1984_Geodesic.csv")
geodesic_PA_areas <- read.csv("path_to_PADDD_file_directory/National_PA_OECM_Area_WGS1984_Geodesic.csv")

sf_use_s2(TRUE)

# PADDD events broken down by D and enacted/proposed
PADDD_segregated <- PADDDEvents %>%
  group_by(EventType, EnactedPro) %>%
  summarise(
    number_PADDD_ID = n_distinct(PADDD_ID),
    number_primary_name = n_distinct(primarynam)
  )

# how many PADDD events were reversed or have unknown reversal status, by PADDD_ID and primarynam
PADDD_summary_reversals <- PADDDEvents %>%
  group_by(Reversal, EnactedPro) %>%
  summarise(number_PADDD_ID = n_distinct(PADDD_ID),
            number_primary_name = n_distinct(primarynam))

# PADDD events segregated by reversal, D and enacted/proposed
PADDD_segregated_D_Enact_pro_rev <- PADDDEvents %>%
  group_by(EventType, EnactedPro, Reversal) %>%
  summarise(
    number_PADDD_ID = n_distinct(PADDD_ID),
    number_primary_name = n_distinct(primarynam)
  )

# read in shapefiles
padd_poly <-  st_read("path_to_PADDD_file_directory/PADDD_poly_WGS84_repaired/")
# one PADDD_ID was incorrectly assigned in the padd_poly dataset and corrected here
padd_poly <- padd_poly %>%
  mutate(PADDD_ID = ifelse(PADDD_ID == "USA534644A4" & EnactedPro == "Enacted", "USA147E721E", PADDD_ID)) 

# read in the points shapefile
padd_pts <- st_read("path_to_PADDD_file_directory/PADDD_pts_WGS84_repaired/")

# read in reversal shapefiles
padd_reversal_poly <-  st_read("path_to_PADDD_file_directory/PADDD_poly_rev_WGS84_repaired/")
padd_reversal_pts <- st_read("path_to_PADDD_file_directory/PADDD_pts_rev_WGS84_repaired/")

## add all geometry sets together
# Identify the missing columns in padd_pts
missing_columns_geom <- setdiff(names(padd_poly), names(padd_pts))

# Add the missing columns to the second dataframe, filling them with NA
padd_pts[missing_columns_geom] <- NA

# Combine the dataframes
padd_poly_pts <- bind_rows(padd_poly, padd_pts)

#combine events from reversals
missing_paddd_ids <- padd_reversal_poly %>%
  filter(!(PADDD_ID %in% padd_poly$PADDD_ID))

# Identify the missing columns in missing_paddd_ids
missing_columns_rev <- setdiff(names(padd_reversal_poly), names(padd_reversal_pts))

# Add the missing columns to the second dataframe, filling them with NA
padd_reversal_pts[missing_columns_rev] <- NA

# Combine the dataframes
padd_poly_pts_rev <- bind_rows(padd_reversal_poly, padd_reversal_pts)

##############  COMBINE all geom datasets
missing_col_geo_2 <- c("Rev_Type", "Rev_Area", "PADDDRevID")
padd_poly_pts[missing_col_geo_2] <- NA
padd_poly_pts_rev["Reversal"] <- NA

padd_geo_combined <- bind_rows(padd_poly_pts %>% select(PADDD_ID, Reversal, Rev_Type, Rev_Area, PADDDRevID, geometry),
                               padd_poly_pts_rev %>% select(PADDD_ID, Reversal, Rev_Type, Rev_Area, PADDDRevID, geometry))
padd_geo_combined <- padd_geo_combined %>%
  mutate(geometry = st_make_valid(geometry))
# trim white space to ensure PADDD_IDs are comparable between datasets
padd_geo_combined$PADDD_ID <- trimws(padd_geo_combined$PADDD_ID)

# drop geometry to be able to check for duplicated PADDD_IDs
padd_geo_combined_no_geo <- st_drop_geometry(padd_geo_combined)
# check duplicated PADDD_IDs
padd_duplicates <- padd_geo_combined_no_geo %>%
  group_by(PADDD_ID) %>%
  summarise(no_ID = length(PADDD_ID)) %>%
  filter(no_ID > 1)
### add duplicates together

### these are all the PADDD_IDs that occurred twice
#   keeping the important details plus merging the geometries
padd_geo_combined_revs <- padd_geo_combined %>%
  filter(PADDD_ID %in% padd_duplicates$PADDD_ID)%>%
  filter(!is.na(Rev_Type))%>%
  group_by(PADDD_ID) %>%
  summarise(Reversal = Reversal,
            Rev_Type = Rev_Type, 
            Rev_Area = Rev_Area, 
            PADDDRevID = PADDDRevID,
            geometry = st_union(geometry))

# pull out all events without duplicates
padd_geo_combined_non_revs <- padd_geo_combined %>%
  filter(!(PADDD_ID %in% padd_duplicates$PADDD_ID))

# add back together to have final geo dataset without duplicates
all_geo_ready <- bind_rows(padd_geo_combined_revs, padd_geo_combined_non_revs) # 4773

## trim white space from PADDD_IDs to make them compatible across datasets
all_geo_ready$PADDD_ID <- as.character(all_geo_ready$PADDD_ID)
PADDDEvents$PADDD_ID <- as.character(PADDDEvents$PADDD_ID)

all_geo_ready$PADDD_ID <- trimws(all_geo_ready$PADDD_ID)
PADDDEvents$PADDD_ID <- trimws(PADDDEvents$PADDD_ID)

## check if any events in the geo dataset are missing in the main dataset
events_missing_in_main_df <- all_geo_ready %>%
  filter(!(PADDD_ID %in% PADDDEvents$PADDD_ID))
## two events are missing but both are lacking polygon and Areaaffected so can't be used anyway

# check how many entries have unknown Areaaffect and also check if there are negative Areaaffect
unknown_Areaaffect <- PADDDEvents %>%
  filter(Areaaffect == "unk") %>%
  group_by(EventType, EnactedPro) %>%
  summarise(
    number = length(PADDD_ID))
sum(unknown_Areaaffect$number)# 817 events with unknown Areaaffect

known_Areaaffect <- PADDDEvents %>%
  filter(Areaaffect != "unk") %>%
  filter(Areaaffect >0) #4145 events with known Areaaffect and over 0

# how many PADDD_IDs are not present in all datasets
known_Areaaffects_events_without_geo_data <- list(unique(known_Areaaffect$PADDD_ID[!known_Areaaffect$PADDD_ID %in%
                                                                                     c(padd_poly$PADDD_ID,
                                                                                       padd_reversal_poly$PADDD_ID)]))
# IDs present in excel sheet but not in either polygon dataset = 746

### filter out PADDDevents that have unknown area and no polygon data associated = 781
PADDD_events_no_area <- PADDDEvents %>%
  left_join(select(all_geo_ready, PADDD_ID, geometry), by = "PADDD_ID") %>%
  filter(Areaaffect == "unk" & st_is_empty(geometry)| GeoDataTyp == "Point" & Areaaffect == "unk")

# adjust padd_poly_added to Mollweide
st_crs(all_geo_ready)
#all_geo_ready <- st_transform(all_geo_ready, crs = "+proj=moll +datum=WGS84 +lon_0=0") #WGS84 is 4326, Mollweide is 54009
#all_geo_ready <- st_transform(all_geo_ready, crs = 4326)
# fix geometries with issues
all_geo_ready <- all_geo_ready %>% #4773
  mutate(geometry = st_make_valid(geometry)) %>%
  mutate(geometry = if_else(st_is_valid(geometry), 
                geometry, 
                st_sfc(st_geometrycollection(), crs = st_crs(geometry))) )

### check if there are any events with invalid geometries
invalid_geometries <- all_geo_ready %>%
  filter(!st_is_valid(geometry)) #%>%
  mutate(geometry = st_buffer(geometry, 0))
#  mutate(geometry = st_simplify(geometry, dTolerance = 0.001))
still_invalid <- invalid_geometries %>%
  mutate(geometry = st_is_empty(geometry))

###exports only polygon data of all_geo_ready

# Filter for POLYGON and MULTIPOLYGON geometries
all_geo_ready_polygons <- all_geo_ready %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
st_write(all_geo_ready_polygons, "path_to_PADDD_file_directory/all_geo_ready.shp",
         driver = "ESRI Shapefile")
##  this file was then imported into ArcGIS and reprojected to WGS84 before
## calculating geodesic areas for all events 
## it was subsequently read in above as all_polygons_geodesic_area

###########################
### join geom data to main dataset
###########################

PADDD_ready <- PADDDEvents %>%
  left_join(all_polygons_geodesic_area, by = "PADDD_ID") %>%
  select(-IUCN_pre, -IUCN_post, -Offset, -Systemic, -Supporting, -Sources, -Map_Details, 
         -Map_Sources, -Notes, -AddedBy, -Peer_Reviewed, -Off_Type, -Off_Area, -Off_Details, -Off_Source) %>%
  mutate(Areaaffect = as.numeric(ifelse(Areaaffect == "unk", NA, Areaaffect)),
         combined_area = ifelse(!is.na(desic_area), desic_area, Areaaffect),
         ratio_desic_Areaaffect = ifelse(is.na(desic_area), NA, desic_area/Areaaffect))# %>%

###############################################################
###             FUNCTION FOR MULTIPLE EVENTS WITHOUT SPATIAL DATA   
###############################################################
# this function is for all PAs that have events affecting them without polygon data
# for PAs only affected by events with polygon data provided, I will simply calculate
# the area without counting overlaps several times

# Helper function to handle overlaps and different event types within a specific primarynam
handle_overlaps <- function(events) {
  # Initialize variables to track the total affected area
  total_area_affected <- 0
  
  # Filter events by type
  degazettements <- events %>% filter(EventType == "Degazette")
  downgrades <- events %>% filter(EventType == "Downgrade")
  downsizes <- events %>% filter(EventType == "Downsize")
  
  # Part 1: Handle degazettements and downgrades
  if (nrow(degazettements) > 0 | nrow(downgrades) > 0) {
    # Get the earliest year for both degazettements and downgrades
    earliest_degazette_downgrade_year <- min(c(degazettements$YearPADDD, downgrades$YearPADDD), na.rm = TRUE)
    
    # Count downsizes only if they happened before the first degazettement/downgrade
    early_downsizes <- downsizes %>% filter(YearPADDD < earliest_degazette_downgrade_year)
    
    # Calculate the total affected area for degazettements/downgrades plus any early downsizes
    total_area_affected <- max(sum(degazettements$combined_area, na.rm = TRUE), 
      sum(downgrades$combined_area, na.rm = TRUE)) +
      sum(early_downsizes$combined_area, na.rm = TRUE)
  }
  
  # Part 2: Handle downsizes only if no degazettements/downgrades occurred
  if (total_area_affected == 0 & nrow(downsizes) > 0) {
    # Sum all downsizes if they happened without degazettements/downgrades
    total_area_affected <- sum(downsizes$combined_area, na.rm = TRUE)
  }
  
  return(total_area_affected)
}
###############################################################
###############################################################
###############################################################

PADDD_events_plus_rev <- PADDD_ready %>%
  filter(!(PADDD_ID %in% PADDD_events_no_area$PADDD_ID)) 

###############################################################################################################
#############     PLUS EVENTS THAT WERE REVERSED LATER  
###############################################################################################################

## gross total area affected (plus reversals):
PADDD_Areaaffected_summed_plus_rev <- PADDD_events_plus_rev %>%
  group_by(EventType, EnactedPro) %>%
  summarise(
    number_PADDD_ID = n_distinct(PADDD_ID),
    number_primarynam = n_distinct(primarynam),
    summed_combined_area = round(sum(as.numeric(combined_area)), 2),
    min_year = min(YearPADDD),
    max_year = max(YearPADDD))
# note: gross total area affected ignores that some PAs are affected by multiple PADDD events and some events are later reversed
  PADDD_Areaaffected_summed_plus_rev_years <- PADDD_events_plus_rev %>%
    filter(YearPADDD != "unk") %>%
    group_by(EventType, EnactedPro) %>%
    summarise(
      min_year = min(YearPADDD),
      max_year = max(YearPADDD))

#######################################################################################
#######################################################################################

## following PADDD tracker Technical Guide V2.1 for calculating area affected by PADDD
# split df into proposed and enacted
enact_rev <- PADDD_events_plus_rev %>%
  filter(EnactedPro == "Enacted") %>% #is 3105 PADDD events in total
  filter(!(PADDD_ID %in% partial_rev$PADDD_ID)) #take out 8 PADDD events that were partially reversed

pro_rev <- PADDD_events_plus_rev %>%
  filter(EnactedPro == "Proposed")

## filter out PAs that were affected by events without polygon data

###############################################################
###             NO POLY PAs      +REVERSALS
###############################################################

PA_enact_no_poly_rev <- enact_rev %>%
  filter(is.na(desic_area)) %>%
  group_by(primarynam) %>%
  summarise(no_PA = length(primarynam))

# filter for events in PAs only affected by events with polygon data
paddd_enact_with_poly_rev <- enact_rev %>%
  left_join(all_geo_ready_polygons %>% 
              filter(PADDD_ID %in% enact_rev$PADDD_ID) %>%
              select(PADDD_ID, geometry), 
                     by = "PADDD_ID") %>%
  filter(!(primarynam %in% PA_enact_no_poly_rev$primarynam)) %>%
  select(-Study_Link, -Legal_Type)

#### reexport these for analysis in ArcGIS
st_write(paddd_enact_with_poly_rev, "path_to_PADDD_file_directory/paddd_enact_with_poly_rev2.shp",
         driver = "ESRI Shapefile")

## paddd_enact_with_poly_rev2.shp export was dissolved (flattened) in ArcGIS by country and marine status
#paddd_enact_poly_area_rev <- st_area(st_union(paddd_enact_with_poly_rev$geometry))/1000/1000

paddd_enact_poly_area_rev <- 2250843.91986 
# value obtained from ArcGIS after dissolving all the geometries and calculating geodesic area

#####################

paddd_enact_no_poly_rev <- enact_rev %>%
  filter(primarynam %in% PA_enact_no_poly_rev$primarynam)
### apply function to handle events without spatial data
paddd_enact_no_poly_rev_area <- paddd_enact_no_poly_rev %>%
  group_by(primarynam, ISO3166, Marine) %>%
  summarise(
    total_area_affected = handle_overlaps(across(everything())))

absolute_total_enact_area <-as.numeric(paddd_enact_poly_area_rev) + 
  sum(paddd_enact_no_poly_rev_area$total_area_affected)+
  sum(partial_rev$Areaaffect)

########################################################################
PA_pro_no_poly_rev <- pro_rev %>%
  filter(GeoDataTyp != "Polygon") %>%
  group_by(primarynam) %>%
  summarise(no_PA = length(primarynam))

####################
# filter for events in PAs only affected by events with polygon data
paddd_pro_with_poly_rev <- pro_rev %>%
  left_join(all_geo_ready_polygons %>% 
              filter(PADDD_ID %in% pro_rev$PADDD_ID) %>%
              select(PADDD_ID, geometry), 
            by = "PADDD_ID") %>%
  filter(!(primarynam %in% PA_pro_no_poly_rev$primarynam)) %>%
  select(-Study_Link, -Legal_Type)
#### note: reexport these for analysis in ArcGIS 
st_write(paddd_pro_with_poly_rev, "path_to_PADDD_file_directory/paddd_pro_with_poly_rev.shp",
         driver = "ESRI Shapefile")
## paddd_pro_with_poly_rev.shp export was dissolved (flattened) in ArcGIS by country and marine status

paddd_pro_poly_area_rev <- 777338.148781
# value obtained from ArcGIS after dissolving all the geometries and calculating geodesic area

paddd_pro_no_poly_rev <- pro_rev %>%
  filter(primarynam %in% PA_pro_no_poly_rev$primarynam)
### apply function to handle events without spatial data
paddd_pro_no_poly_rev_area <- paddd_pro_no_poly_rev %>%
  group_by(primarynam, ISO3166, Marine) %>%
  summarise(
    total_area_affected = handle_overlaps(across(everything())))

absolute_total_pro_area <- as.numeric(paddd_pro_poly_area_rev) + sum(paddd_pro_no_poly_rev_area$total_area_affected)

###########################################################################
###########################################################################
### Analysis without reversals, accounting for partial reversals
###########################################################################

# filter out events that were reversed
PADDD_events_filtered <- PADDD_ready %>%
  filter(!(PADDD_ID %in% PADDD_events_no_area$PADDD_ID), Reversal.x != "Y", Reversal.y != "Y")
#for now only filter out definite reversals and leave unk in (there were events that had Y in poly data reversal)

# filter for events that were partially reversed
partial_rev <- PADDD_ready %>%
  filter(Rev_Type.y == "Partial") %>%
  filter(Rev_Area.y != "-99")

partial_rev <- partial_rev %>%
  mutate(
    partial_area = Areaaffect - Rev_Area.y
  )
##### add the sum of the partial reversal area to the enduring_total_enact_area

enact <- PADDD_events_filtered %>%
  filter(EnactedPro == "Enacted") %>% #2619 events
  filter(!(PADDD_ID %in% partial_rev$PADDD_ID))

pro <- PADDD_events_filtered %>%
  filter(EnactedPro == "Proposed")

## filter out PAs that were affected by events without polygon data
PA_enact_no_poly <- enact %>%
  filter(is.na(desic_area)) %>%
  group_by(primarynam) %>%
  summarise(no_PA = length(primarynam))


# filter for events in PAs only affected by events with polygon data
paddd_enact_with_poly <- enact %>%
  left_join(all_geo_ready_polygons %>% 
              filter(PADDD_ID %in% enact$PADDD_ID) %>%
              select(PADDD_ID, geometry), 
            by = "PADDD_ID") %>%
  filter(!(primarynam %in% PA_enact_no_poly$primarynam)) %>%
  select(-Study_Link, -Legal_Type)
### note: reexport these for analysis in ArcGIS (total but also per country)
st_write(paddd_enact_with_poly, "path_to_PADDD_file_directory/paddd_enact_with_poly.shp",
         driver = "ESRI Shapefile")
#paddd_enact_with_poly.shp export was dissolved (flattened) in ArcGIS by country and marine status
paddd_enact_poly_area <- 2129438.30342
# value obtained from ArcGIS after dissolving all the geometries and calculating geodesic area

#####################

paddd_enact_no_poly <- enact %>%
  filter(primarynam %in% PA_enact_no_poly$primarynam)
### apply function to handle events without spatial data
paddd_enact_no_poly_area <- paddd_enact_no_poly %>%
  group_by(primarynam) %>%
  summarise(
    total_area_affected = handle_overlaps(across(everything())))

enduring_total_enact_area <-as.numeric(paddd_enact_poly_area) + 
  sum(paddd_enact_no_poly_area$total_area_affected) +
  sum(partial_rev$Rev_Area.y)

########################################################################

PA_pro_no_poly <- pro %>%
  filter(is.na(desic_area)) %>%
  group_by(primarynam) %>%
  summarise(no_PA = length(primarynam))

# filter for events in PAs only affected by events with polygon data
paddd_pro_with_poly <- pro %>%
  left_join(all_geo_ready_polygons %>% 
              filter(PADDD_ID %in% pro$PADDD_ID) %>%
              select(PADDD_ID, geometry), 
            by = "PADDD_ID") %>%
  filter(!(primarynam %in% PA_pro_no_poly$primarynam)) %>%
  select(-Study_Link, -Legal_Type)
#### note: reexport these for analysis in ArcGIS (total but also per country)
st_write(paddd_pro_with_poly, "path_to_PADDD_file_directory/paddd_pro_with_poly.shp",
         driver = "ESRI Shapefile")
#paddd_pro_with_poly.shp export was dissolved (flattened) in ArcGIS by country and marine status
paddd_pro_poly_area <- 42782.7376033

#####################

paddd_pro_no_poly <- pro %>%
  filter(primarynam %in% PA_pro_no_poly$primarynam)
### apply function to handle events without spatial data
paddd_pro_no_poly_area <- paddd_pro_no_poly %>%
  group_by(primarynam) %>%
  summarise(
    total_area_affected = handle_overlaps(across(everything())))

enduring_total_pro_area <- as.numeric(paddd_pro_poly_area) + 
  sum(paddd_pro_no_poly_area$total_area_affected)

####################################################################################
