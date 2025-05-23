###############################################################
### Function to generate hatching
###############################################################

#' Return a SpatialLinesDataframe as hatched area for a SpatialPolygons
#' Adapted from https://github.com/statnmap/HatchedPolygons
#'
#' @inheritParams graphics::polygon
#' @param x SpatialPolygons* from library sp
#' @import sp
#' @importFrom methods is as
#' @importFrom methods slot
#' @importFrom sf st_is
#' @importFrom sf st_as_sf
#'
#' @noRd

hatched.SpatialPolygons <-
  function(x,
           density = 10, angle = 45,
           fillOddEven = FALSE) {

    type <- NULL


    if (is(x, "SpatialPolygons")) {
      n <- length(slot(x, "polygons"))
      polys <- slot(x, "polygons")
      pO <- slot(x, "plotOrder")
      type <- "sp"
    } else if (st_is(x, c("POLYGON", "MULTIPOLYGON"))[1]) {
      # n <- length(x)
      # To do
      x <- as(x, "Spatial")
      n <- length(slot(x, "polygons"))
      polys <- slot(x, "polygons")
      pO <- slot(x, "plotOrder")
      type <- "sf"
    } else {
      stop("Not a sp::SpatialPolygons or sf::*POLYGON object")
    }


    if (length(density) != n)
      density <- rep(density, n, n)
    if (length(angle) != n)
      angle <- rep(angle, n, n)
    all.Lines <- list()
    all.Lines.ID <- numeric(0)

    for (j in pO) {
      all.Lines.tmp <- polygonRingHolesLines(
        polys[[j]],
        density = density[j], angle = angle[j],
        ID = polys[[j]]@ID,
        fillOddEven = fillOddEven
      )
      if(length(all.Lines.tmp)==0)
        next()

      all.Lines.ID <- c(all.Lines.ID, rep(polys[[j]]@ID, length(all.Lines.tmp)))
      all.Lines[length(all.Lines) + 1:length(all.Lines.tmp)] <- all.Lines.tmp
    }
    # Correct ID
    SpatialLinesDF <- SpatialLinesDataFrame(
      SpatialLines(all.Lines),
      data = data.frame(ID = all.Lines.ID),
      match.ID = FALSE)

    if (type == "sf") {
      SpatialLinesDF_sf <- st_as_sf(SpatialLinesDF)
      return(SpatialLinesDF_sf)
    } else {
      return(SpatialLinesDF)
    }
  }

###############################################################
### Function to generate hatching, individual lines
###############################################################

#' Get SpatialLines of one Polygons feature
#' Adapted from https://github.com/statnmap/HatchedPolygons
#'
#' @inheritParams graphics::polygon
#' @param Sr An object of class Polygons
#' @param ID Number or string identifying the Polygon inside Polygons
#'
#' @import sp
#' @importFrom methods as
#' @importFrom methods is
#' @importFrom methods slot
#' @importFrom sf st_intersection
#' @importFrom sf st_as_sfc
#' @noRd

polygonRingHolesLines <- function(Sr,
                                  density = 0.5,
                                  angle = 45,
                                  ID = 1,
                                  fillOddEven = FALSE) {
  if (!is(Sr, "Polygons"))
    stop("Not an Polygons object")

  if (!is.null(density)) hatch <- TRUE
  else hatch <- FALSE
  pO <- slot(Sr, "plotOrder")
  polys <- slot(Sr, "Polygons")

  if (hatch) {
    all.Lines <- list()
    for (i in pO) {
      if (!slot(polys[[i]], "hole")) {
        # Transform polygon as parallel lines
        lines.hatch <- polygon.fullhatch(slot(polys[[i]], "coords"),
                                         density = density, angle = angle, fillOddEven = fillOddEven)

        if(length(lines.hatch)==0)
        {
          warning("Polygon too small to contain any lines.  Consider increasing 'density'.")
          next()
        }

        # Transform as SpatialLines
        Lines.i <- SpatialLines(list(Lines(
          apply(lines.hatch, 1,
                function(x) Line(cbind(c(x[1], x[3]), c(x[2], x[4])))),
          ID = i)))

        # Clean Lines if over a "hole"
        #
        # Lines.i.holes <- rgeos::gIntersection(Lines.i, SpatialPolygons(list(Sr)),
        #                                       drop_lower_td = TRUE)
        Lines.i.holes <- st_intersection(
          Lines.i  |> st_as_sfc(),
          SpatialPolygons(list(Sr)) |> st_as_sfc()
        ) |> as("Spatial")

        if (!is.null(Lines.i.holes)) {
          Lines.i.holes@lines[[1]]@ID <- paste0(ID, ".", i)
          all.Lines[[length(all.Lines) + 1]] <- Lines.i.holes@lines[[1]]
        }
      }
    }
  }
  return(all.Lines)
}



###############################################################
### helper funtion to hatch area of one polygon
###############################################################

#' Create hatch area of one polygon
#' Adapted from https://github.com/statnmap/HatchedPolygons
#'
#' @inheritParams graphics::polygon
#' @param ..debug.hatch for drawing when debugging function
#'
#' @examples
#' \dontrun{
#' res <- polygon.fullhatch(x, density = 10, angle = 45)
#' arrows(res$lx1, res$ly1, res$lx2, res$ly2, col = "red", code = 0)
#' }
#'
#' @importFrom dplyr bind_rows
#'
#' @noRd

polygon.fullhatch <- function(x, y = NULL, density, angle, ..debug.hatch = FALSE,
                              fillOddEven = FALSE,
                              ...) {
  if (is.null(y)) {
    y <- x[,2]
    x <- x[,1]
  }
  if (x[1] != x[length(x)] | y[1] != y[length(y)]) {
    x <- c(x, x[1L])
    y <- c(y, y[1L])
  }
  angle <- angle%%180
  # if (par("xlog") || par("ylog")) {
  #   warning("cannot hatch with logarithmic scale active")
  #   return()
  # }
  # usr <- par("usr")
  # pin <- par("pin")
  # upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
  #if (upi[1L] < 0)
  #  angle <- 180 - angle
  #if (upi[2L] < 0)
  #  angle <- 180 - angle
  # upi <- abs(upi)
  res <- NULL
  xd <- cos(angle/180 * pi) #* upi[1L]
  yd <- sin(angle/180 * pi) #* upi[2L]
  if (angle < 45 || angle > 135) {
    if (angle < 45) {
      first.x <- max(x)
      last.x <- min(x)
    }
    else {
      first.x <- min(x)
      last.x <- max(x)
    }
    # y.shift <- upi[2L]/density/abs(cos(angle/180 * pi))
    y.shift <- 1/density/abs(cos(angle/180 * pi))
    x0 <- 0
    y0 <- floor((min(y) - first.x * yd/xd)/y.shift) *
      y.shift
    y.end <- max(y) - last.x * yd/xd
    while (y0 < y.end) {
      res.tmp <- polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch,
                                  fillOddEven = fillOddEven,
                                  ...)
      if (!is.null(res.tmp)) {
        res <- bind_rows(res, res.tmp)
      }
      y0 <- y0 + y.shift
    }
  } else {
    if (angle < 90) {
      first.y <- max(y)
      last.y <- min(y)
    } else {
      first.y <- min(y)
      last.y <- max(y)
    }
    # x.shift <- upi[1L]/density/abs(sin(angle/180 * pi))
    x.shift <- 1/density/abs(sin(angle/180 * pi))
    x0 <- floor((min(x) - first.y * xd/yd)/x.shift) * x.shift
    y0 <- 0
    x.end <- max(x) - last.y * xd/yd
    while (x0 < x.end) {
      # Get lines
      res.tmp <- polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch,
                                  fillOddEven = fillOddEven,
                                  ...)
      if (!is.null(res.tmp)) {
        res <- bind_rows(res, res.tmp)
      }
      x0 <- x0 + x.shift
    }
    # arrows(res$lx1, res$ly1, res$lx2, res$ly2, col = "red", code = 0)
  }
  return(res)
}



###############################################################
### Function to create one line for hatch area of one polygon
###############################################################

#' Create one line for hatch area of one polygon
#' Adapted from https://github.com/statnmap/HatchedPolygons
#'
#' @inheritParams graphics::polygon
#'
#' @param ..debug.hatch for drawing when debugging function
#' @param x0 parameter as issued from \code{\link{polygon.fullhatch}}
#' @param y0 parameter as issued from \code{\link{polygon.fullhatch}}
#' @param xd parameter as issued from \code{\link{polygon.fullhatch}}
#' @param yd parameter as issued from \code{\link{polygon.fullhatch}}
#'
#' @importFrom graphics points
#' @importFrom graphics arrows
#' @noRd

polygon.onehatch <- function(x, y, x0, y0, xd, yd, ..debug.hatch = FALSE,
                             fillOddEven = FALSE,
                             ...) {
  if (..debug.hatch) {
    graphics::points(x0, y0)
    graphics::arrows(x0, y0, x0 + xd, y0 + yd)
  }
  halfplane <- as.integer(xd * (y - y0) - yd * (x -
                                                  x0) <= 0)
  cross <- halfplane[-1L] - halfplane[-length(halfplane)]
  does.cross <- cross != 0
  if (!any(does.cross))
    return()
  x1 <- x[-length(x)][does.cross]
  y1 <- y[-length(y)][does.cross]
  x2 <- x[-1L][does.cross]
  y2 <- y[-1L][does.cross]
  t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 -
                                                x1))/(xd * (y2 - y1) - yd * (x2 - x1)))
  o <- order(t)
  tsort <- t[o]
  crossings <- cumsum(cross[does.cross][o])
  if (fillOddEven)
    crossings <- crossings%%2
  drawline <- crossings != 0
  lx <- x0 + xd * tsort
  ly <- y0 + yd * tsort
  lx1 <- lx[-length(lx)][drawline]
  ly1 <- ly[-length(ly)][drawline]
  lx2 <- lx[-1L][drawline]
  ly2 <- ly[-1L][drawline]
  # segments(lx1, ly1, lx2, ly2, ...)
  # get lines
  data.frame(lx1 = lx1, ly1 = ly1, lx2 = lx2, ly2 = ly2)
}
