#' US Base Map
#'
#' Create an empty US base map in ggplot
#'
#' @param incl Str vector to include AK, HI , and PR
#' @param highlight_states character vector of state FIPS codes
#' @param agg_county logical, should counties be aggregated tot he state-level? Defaults to \code{TRUE}
#'
#' @import ggplot2
#'
#' @importFrom ggthemes theme_map
#' @importFrom maptools elide spRbind unionSpatialPolygons
#' @importFrom rgdal readOGR
#' @importFrom rgeos gUnaryUnion
#' @importFrom rlang .data
#' @importFrom sp CRS bbox proj4string spTransform
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a base map of the US with options for including AK, HI, and PR.
#' This function was developed from a blog post by Bob Rudis (https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/)
#'
#' @examples
#' \dontrun{
#'
#' ##Just the national map
#' us_base_map()
#'
#' ##National map highlighting states with NERRS
#' nerr_states <- c('01', '02', '06', '10', '12', '13', '15'
#' , '23', '24', '25', '27', '28', '33', '34', '36', '37', '39'
#' , '41', '44', '45', '48', '51', '53', '55', '72')
#'
#' us_base_map(highlight_states = nerr_states)
#' }
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @author Bob Rudis, Julie Padilla
#'
#' @concept mapping
#'
us_base_map <- function(incl = c('contig', 'AK', 'HI', 'PR')
                        , highlight_states = NULL
                        , agg_county = T) {

  get_US_county_2010_shape <- function() {
    dir <- tempdir()
    utils::download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
    unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
    rgdal::readOGR(file.path(dir, "gz_2010_us_050_00_20m.shp"))
  }

  us <- get_US_county_2010_shape()

  # convert it to Albers equal area
  us_aea <- sp::spTransform(us, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  us_aea@data$id <- rownames(us_aea@data)

  # remove old states and put new ones back in; note the different order
  # we're also removing puerto rico in this example but you can move it
  # between texas and florida via similar methods to the ones we just used
  us_aea_mod <- us_aea[!us_aea$STATE %in% c("02", "15", "72"),]

  if('AK' %in% incl) {
    # extract, then rotate, shrink & move alaska (and reset projection)
    # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
    alaska <- us_aea[us_aea$STATE == "02", ]
    alaska <- maptools::elide(alaska, rotate = -50)
    alaska <- maptools::elide(alaska, scale = max(apply(sp::bbox(alaska), 1, diff)) / 2.3)
    alaska <- maptools::elide(alaska, shift = c(-2100000, -2500000))
    sp::proj4string(alaska) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, alaska)
  }

  if('HI' %in% incl) {
    # extract, then rotate & shift hawaii
    hawaii <- us_aea[us_aea$STATE == "15",]
    hawaii <- maptools::elide(hawaii, rotate = -35)
    hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
    sp::proj4string(hawaii) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, hawaii)
  }

  if('PR' %in% incl) {
    # extract, then rotate & shift pr
    pr <- us_aea[us_aea$STATE == "72", ]
    pr <- maptools::elide(pr, shift = c(-1400000,2000))
    sp::proj4string(pr) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, pr)
  }

  if(agg_county) {
    us_aea.diss <- maptools::unionSpatialPolygons(us_aea_mod, IDs = us_aea_mod@data$STATE)
    us_aea_mod <- us_aea.diss
  }

  # get ready for ggplotting it... this takes a cpl seconds ----
  map <- ggplot2::fortify(us_aea_mod, region = "GEO_ID")

  # plot it----

  # highlight some states
  gg <- ggplot()
  gg <- gg + coord_equal()
  gg <- gg + ggthemes::theme_map()
  gg <- gg + theme(plot.margin = unit(c(0, 0, 0, 0), "points")) #trbl

  if(is.null(highlight_states)) {
    gg <- gg + geom_map(data = map, map = map
                        , aes(map$long, map$lat, map_id = map$id)
                        , fill = '#f8f8f8', color = '#999999'
                        , size = 0.15, show.legend = F)


  } else {
    map$flag <- ifelse(map$id %in% highlight_states, TRUE, FALSE)

    gg <- gg + geom_map(data = map, map = map
                        , aes(map$long, map$lat, map_id = map$id, fill = map$flag)
                        , color = '#999999', size = 0.15, show.legend = F) +
      scale_fill_manual(values = c('#f8f8f8', '#cccccc'))

  }

  # return(map)
  return(gg)
}


