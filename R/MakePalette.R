utils::globalVariables(c("kmeans", "rect"))
#'
#' Make your palette of colors
#'
#' @description Creates a color palette from an image, using the KMeans algorithm
#'
#' @import raster
#' @import rgdal
#' @import grDevices
#' @import prismatic
#'
#' @param photo Image location path. It can also be a URL address.
#' @param n Number of elements to be generated in the color palette. The default value is 4.
#'
#' @return A palette of colors
#' @export
#'
#' @examples
#' MakePalette(system.file("extdata", "picture02.jpg", package="MakeYourPalette"))
#' MakePalette(system.file("extdata", "picture04.png", package="MakeYourPalette"))
#' MakePalette(system.file("extdata", "picture06.png", package="MakeYourPalette"))
#'

MakePalette <- function(photo, n=4){
  col_R <- getValues(raster(photo, band=1))
  col_G <- getValues(raster(photo, band=2))
  col_B <- getValues(raster(photo, band=3))
  df <- data.frame(col_R, col_G, col_B)
  df <- unique(df)
  df2 <- rgb2hsv(r=df$col_R, g=df$col_G, b=df$col_B)
  df2 <- data.frame(t(df2))
  modelK <- kmeans(df2, centers=n)
  centers <- modelK$centers
  colors <- vector(length=nrow(centers))
  for (i in 1:nrow(centers)){
    colors[i] = hsv(h=centers[i,1], s=centers[i,2], v=centers[i,3])
  }
  return(prismatic::color(colors))
}
