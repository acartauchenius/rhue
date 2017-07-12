#' Convert a CSS color to Hue XY colorspace
#'
#' @param color A CSS hex color definition, e.g. \code{"#FF0000"}
#' @return A numeric vector of length 2, defining a color in XY colorspace
#' @examples
#' convert_xy("#FF0000")
convert_xy <- function(color){

  #validate color
  if(!grepl("^#([A-Fa-f0-9]{6})$", color)){
    stop("Malformed color definition")
  }

  if (color == "#000000") {
    color <- "#010101"
  }

  color %>%
    {colorspace::hex2RGB(.)} %>%
    # strip colorspace class metadata
    {colorspace::coords(.)} %>%
    # apply gamma correction
    sapply(function(col_component){
      if (col_component > 0.04045){
        ((col_component + 0.055) / (1.055))^2.4
      } else {
        col_component / 12.92
      }
    }) %>%
    # convert to CIE 1931 XYZ colorspace
    {c(
      .[1] * 0.664511 + .[2] * 0.154324 + .[3] * 0.162028,
      .[1] * 0.283881 + .[2] * 0.668433 + .[3] * 0.047685,
      .[1] * 0.000088 + .[2] * 0.072310 + .[3] * 0.986039
    )} %>%
    # convert to Philipps Hue xy colorspace
    {c(
      .[1] / sum(.[1], .[2], .[3]),
      .[2] / sum(.[1], .[2], .[3])
    )}
}
