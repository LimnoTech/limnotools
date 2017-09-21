#' US Base Map
#'
#' Create an empty US base map in ggplot
#'
#' @param incl ...
#' @param agg_county logical, should counties be aggregated tot he state-level? Defaults to \code{TRUE}
#'
#' @import ggplot2
#'
#' @importFrom maptools elide
#' @importFrom rgdal readOGR
#' @importFrom rgeos gUnaryUnion
#' @importFrom sp proj4string spTransform
#' @importFrom utils unzip
#'
#' @export
#'
#' @details Creates a base map of the US with options
#'
#' @examples
#' \dontrun{
#' x <- c('0.24', '>0.25', '2', '>0.5', '<2.5', '<6')
#' nd_classifier(x)
#'
#' x <- c('  0.24', '> 0.25', '2 ', '>.5', '<2.5', '<  6')
#' nd_classifier(x)
#' }
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{nd_remover}
#'
#' @author Bob Rudis, Julie Padilla
#'
#' @concept mapping
#'
us_base_map <- function(incl = c('contig', 'AK', 'HI', 'PR'), agg_county = T) {

  get_US_county_2010_shape <- function() {
    dir <- tempdir()
    download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
    unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
    readOGR(file.path(dir, "gz_2010_us_050_00_20m.shp"))
  }

  us <- get_US_county_2010_shape()

  # convert it to Albers equal area
  us_aea <- sp::spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  us_aea@data$id <- rownames(us_aea@data)

  # remove old states and put new ones back in; note the different order
  # we're also removing puerto rico in this example but you can move it
  # between texas and florida via similar methods to the ones we just used
  us_aea <- us_aea[!us_aea$STATE %in% c("02", "15", "72"),]

  if('AK' %in% incl) {
    # extract, then rotate, shrink & move alaska (and reset projection)
    # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
    alaska <- us_aea[us_aea$STATE == "02", ]
    alaska <- maptools::elide(alaska, rotate = -50)
    alaska <- maptools::elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
    alaska <- maptools::elide(alaska, shift=c(-2100000, -2500000))
    proj4string(alaska) <- proj4string(us_aea)

    us_aea <- rbind(us_aea, alaska)
  }

  if('HI' %in% incl) {
    # extract, then rotate & shift hawaii
    hawaii <- us_aea[us_aea$STATE == "15",]
    hawaii <- maptools::elide(hawaii, rotate = -35)
    hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
    proj4string(hawaii) <- proj4string(us_aea)

    us_aea <- rbind(us_aea, hawaii)
  }

  if('PR' %in% incl) {
    # extract, then rotate & shift pr
    pr <- us_aea[us_aea$STATE == "72", ]
    pr <- maptools::elide(pr, shift = c(-1400000,2000))
    proj4string(pr) <- proj4string(us_aea)

    us_aea <- rbind(us_aea, pr)
  }

  if(agg_county) {
    us_aea.diss <- rgeos::gUnaryUnion(us_aea, id = us_aea@data$STATE)
    us_aea <- us_aea.diss
  }

  # get ready for ggplotting it... this takes a cpl seconds ----
  map <- fortify(us_aea, region="GEO_ID")

  # plot it----
  gg <- ggplot()
  gg <- gg + geom_map(data = map, map = map,
                      aes(x=long, y=lat, map_id=id, group = group)
                      , fill = '#f8f8f8', color="#999999", size=0.15, show.legend = F)

  gg <- gg + coord_equal()
  gg <- gg + theme_map()
  gg <- gg + theme(plot.margin = unit(c(0, 0, 0, 0), "points")) #trbl

  gg
}


