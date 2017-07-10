#' Dump available data about all lights paired with the bridge.
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @return A list of data.frames containing metadata. One for each type of light in use.
#' @export
dump_lights_data <- function(bridge_url, user){

  response <- httr::GET(bridge_url, path = sprintf("api/%s/lights", user))

  response %>%
    {httr::stop_for_status(.)} %>%
    {httr::content(.)} %>%
    lapply(function(light){
      if (!is.null(light$state$xy)){
        names(light$state$xy) <- c("x", "y")
      }
      return(as.data.frame(light))
    }) %>%
    split(., sapply(., function(light){light$type})) %>%
    lapply(function(type){
      lights_table <- do.call(rbind.data.frame, type)
      lights_table$light_id <- rownames(lights_table)
      rownames(lights_table) <- NULL
      return(lights_table)
    })
}

#' Get state and metadata of any light paired with the bridge.
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @param light_id used to address a specific light. Can be looked up by \code{dump_lights_data},
#' @return A list containing state and metadata of a hue light.
#' @export
get_light_data <- function(bridge_url, user, light_id){

  response <- httr::GET(bridge_url, path = sprintf("api/%s/lights/%s", user, light_id))

  light_data <- response %>%
  {httr::stop_for_status(.)} %>%
  {httr::content(.)}

  if(!is.null(light_data[[1]]$error)){
    stop("light_id not found.")
  }

  if (!is.null(light_data$state$xy)){
    names(light_data$state$xy) <- c("x", "y")
  }

  return(light_data)
}
