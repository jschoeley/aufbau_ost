install.packages('osmdata')
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

# Function ----------------------------------------------------------------

DrawDistrict <-
  function (district,
            what = 'brw',
            col_background = 'black',
            col_buildings = 'white',
            col_roads = '#212121',
            col_water = '#81C6E2') {

    what <- unlist(strsplit(what, split = ''))

    # bounds
    bounds <-
      getbb(district)
    bounds_sf <-
      getbb(district, format_out = 'sf_polygon')

    # buildings
    if ('b' %in% what) {
      buildings <-
        opq(bounds) %>%
        add_osm_feature(key = 'building') %>%
        osmdata_sf()
    }
    if ('w' %in% what) {
      water <-
        c(
          opq(bounds) %>%
            add_osm_feature(key = 'waterway') %>%
            osmdata_sf(),
          opq(bounds) %>%
            add_osm_feature(key = 'natural', value = 'water') %>%
            osmdata_sf()
        )
    }
    if ('r' %in% what) {
      roads <-
        opq(bounds) %>%
        add_osm_feature(key = 'highway') %>%
        osmdata_sf()
    }

    ggplot(data.frame()) +
      list(
        # background
        geom_sf(
          data = bounds_sf,
          fill = col_background,
          color = NA
        ),
        # buildings
        if ('b' %in% what) {
          list(
            if (!is.null(buildings$osm_multipolygons)) {
              geom_sf(
                data =
                  buildings$osm_multipolygons %>%
                  st_buffer(0) %>% # fixes self-intersection errors
                  st_intersection(bounds_sf),
                color = NA,
                fill = col_buildings
              )
            },
            if (!is.null(buildings$osm_polygons)) {
              geom_sf(
                data =
                  buildings$osm_polygons %>%
                  st_intersection(bounds_sf),
                color = NA,
                fill = col_buildings
              )
            }
          )
        },
        # roads
        if ('r' %in% what) {
          list(
            if (!is.null(roads$osm_lines)) {
              geom_sf(
                data =
                  roads$osm_lines %>%
                  st_intersection(bounds_sf),
                color = col_roads,
                size = 0.1
              )
            }
          )
        },
        # water
        if ('w' %in% what) {
          list(
            if (!is.null(water$osm_multipolygons)) {
              # water
              geom_sf(
                data =
                  water$osm_multipolygons %>%
                  st_buffer(0) %>% # fixes self-intersection errors
                  st_intersection(bounds_sf),
                color = NA,
                fill = col_water
              )
            },
            if (!is.null(water$osm_polygons)) {
              geom_sf(
                data =
                  water$osm_polygons %>%
                  st_intersection(bounds_sf),
                color = NA,
                fill = col_water
              )
            },
            if (!is.null(water$osm_lines)) {
              geom_sf(
                data =
                  water$osm_lines %>%
                  st_intersection(bounds_sf),
                color = col_water,
                size = 0.1
              )
            }
          )
        },
        theme_void(),
        coord_sf(datum = NA),
        labs(title = district)
      )
  }

DrawMultiDistrict <-
  function (districts,
            what = 'br',
            fill_district = NA,
            col_district = 'black',
            fill_buildings = 'black',
            col_buildings = NA,
            col_roads = '#212121') {

    DownloadOSM <- function (bounds, key, value = NULL) {
      opq(bounds) %>%
        add_osm_feature(key, value) %>%
        osmdata_sf()
    }

    CropToDistrict <- function (sf, district_bounds, district_name, buffer = FALSE) {

      # add zero fixes self-intersection errors in polygons
      if (buffer) sf <- st_buffer(sf, 0)

      sf %>%
        # crop sf to district bounds
        st_intersection(district_bounds) %>%
        # add district name
        mutate(district = district_name) %>%
        # only select district name and geo
        select(district, geometry)

    }

    CenterDistrictCoordinates <- function (sf, bounds_sf) {
      sf_3395 <- sf %>% st_transform(3395)
      bounds_sf_3395 <- bounds_sf %>% st_transform(3395)
      st_set_geometry(
        sf_3395,
        st_geometry(sf_3395) -
          ((bounds_sf_3395 %>% st_centroid() %>% st_coordinates()))
      ) %>%
        st_set_crs(3395)
    }

    what <- unlist(strsplit(what, split = ''))

    # initialize loop variables
    district_names <- districts
    district_n <- length(districts)
    district_bounds <- vector('list', district_n)
    district_area <- vector('list', district_n)
    district_buildings <- vector('list', district_n)
    district_buildings_poly <- vector('list', district_n)
    district_buildings_mpoly <- vector('list', district_n)
    district_roads <- vector('list', district_n)
    district_roads_line <- vector('list', district_n)

    # download data
    for (i in 1:district_n) {

      # bounds
      district_bounds[[i]] <- getbb(district_names[i])
      district_area[[i]] <-
        getbb(district_names[i], format_out = 'sf_polygon') %>%
        mutate(district = district_names[i])

      # buildings
      if ('b' %in% what) {

        district_buildings[[i]] <-
          DownloadOSM(
            district_bounds[[i]],
            key = 'building'
          )
        if (!is.null(district_buildings[[i]]$osm_polygons)) {
          district_buildings_poly[[i]] <-
            CropToDistrict(
              district_buildings[[i]]$osm_polygons,
              district_area[[i]],
              district_names[i]
            ) %>%
            CenterDistrictCoordinates(
              district_area[[i]]
            )
        }
        if (!is.null(district_buildings_mpoly[[i]]$osm_multipolygons)) {
          district_buildings_mpoly[[i]] <-
            CropToDistrict(
              district_buildings[[i]]$osm_multipolygons,
              district_area[[i]],
              district_names[i]
            ) %>%
            CenterDistrictCoordinates(
              district_area[[i]]
            )
        }

      }

      # roads & railways
      if ('r' %in% what) {

        district_roads[[i]] <-
          c(
            DownloadOSM(
              district_bounds[[i]],
              key = 'highway'
            ),
            DownloadOSM(
              district_bounds[[i]],
              key = 'railway'
            )
          )
        if (!is.null(district_roads[[i]]$osm_lines)) {
          district_roads_line[[i]] <-
            CropToDistrict(
              district_roads[[i]]$osm_lines,
              district_area[[i]],
              district_names[i]
            ) %>%
            CenterDistrictCoordinates(
              district_area[[i]]
            )
        }

      }

      # area
      district_area[[i]] <-
        CenterDistrictCoordinates(district_area[[i]], district_area[[i]])

    }

    # convert list to dataframe
    district_area <- do.call(rbind, district_area)
    district_buildings_poly <- do.call(rbind, district_buildings_poly)
    district_buildings_mpoly <- do.call(rbind, district_buildings_mpoly)
    district_roads_line <- do.call(rbind, district_roads_line)

    # plot districts small multiples
    ggplot(district_area) +
      list(
        # district
        geom_sf(
          fill = fill_district,
          color = col_district
        ),
        # buildings
        if ('b' %in% what) {
          list(
            if (length(district_buildings_mpoly) > 0) {
              geom_sf(
                data =
                  district_buildings_mpoly,
                color = col_buildings,
                fill = fill_buildings
              )
            },
            if (length(district_buildings_poly) > 0) {
              geom_sf(
                data =
                  district_buildings_poly,
                color = col_buildings,
                fill = fill_buildings
              )
            }
          )
        },
        # roads
        if ('r' %in% what) {
          if (length(district_roads_line) > 0) {
            geom_sf(
              data =
                district_roads_line,
              color = col_roads,
              fill = NA,
              size = 0.1
            )
          }
        },
        coord_sf(datum = NA),
        facet_wrap(~district),
        theme_void()
      )
  }

# Rostock -----------------------------------------------------------------

aufbau_ost <-
  DrawMultiDistrict(
    c(
      # Rostock
      'Evershagen, Rostock',
      'Lütten Klein, Rostock',
      'Groß Klein, Rostock',
      'Lichtenhagen, Rostock',
      'Toitenwinkel, Rostock',
      #'Dierkow, Rostock',
      'Schmarl, Rostock',
      'Südstadt, Rostock',
      # Berlin
      'Marzahn, Berlin',
      #'Ernst-Thälmann-Park, Berlin',
      'Fennpfuhl, Berlin',
      'Hellersdorf, Berlin',
      'Neu-Hohenschönhausen, Berlin',
      # Dresden
      #'Gorbitz, Dresden',
      #Halle
      #'Neustadt, Halle',
      'Silberhöhe, Halle',
      #'Heide-Nord, Halle',
      # Wernigerode
      #'Burgbreite, Wernigerode',
      #'Stadtfeld, Wernigerode',
      #'Harzblick, Wernigerode'
      # Jena
      'Winzerla, Jena',
      'Neulobeda, Jena',
      # Erfurt
      'Berliner Platz, Erfurt',
      'Johannesplatz, Erfurt',
      'Melchendorf, Erfurt',
      'Moskauer Platz, Erfurt',
      'Rieth, Erfurt',
      'Wiesenhügel, Erfurt',
      'Roter Berg, Erfurt',
      'Herrenberg, Erfurt',
      # Leipzig
      'Grünau, Leipzig'
      # Gera
      #'Bieblach, Gera',
      #'Lusan, Gera',
    ), col_buildings = 'black', fill_buildings = NA
  )

ggsave('aufbau_ost.svg', aufbau_ost,
       units = 'cm',
       width = 130, height = 100,
       limitsize = FALSE)
