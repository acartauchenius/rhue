#' Switch a light on / off
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @param light_id used to address a specific light. Can be looked up by \code{dump_lights_data}
#' @param on_off boolean, signal to turn the light on or off
#' @export
switch_light <- function(bridge_url, user, light_id, on_off){

  response <- httr::PUT(bridge_url, path = sprintf("api/%s/lights/%s/state", user, light_id),
                          body = list("on" = jsonlite::unbox(on_off)),
                          encode = 'json'
  )

  httr::stop_for_status(response)

  if(!is.null(httr::content(response)[[1]]$error)){
    stop(sprintf("api error: %s", jsonlite::toJSON(httr::content(response)[[1]], auto_unbox = TRUE)))
  }
}

#' Blink a light for a few seconds
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @param light_id used to address a specific light. Can be looked up by \code{dump_lights_data}
#' @export
alert_light <- function(bridge_url, user, light_id){
  response <- httr::PUT(bridge_url, path = sprintf("api/%s/lights/%s/state", user, light_id),
                          body = list("alert" = jsonlite::unbox("lselect")),
                          encode = 'json'
  )

  httr::stop_for_status(response)

  if(!is.null(content(response)[[1]]$error)){
    stop(sprintf("api error: %s", jsonlite::toJSON(content(response)[[1]], auto_unbox = TRUE)))
  }
}

#' Set a light to a specific color temperature and brightness
#'
#' This function does not check if the light actually has the capability to change color temperature,
#' Lamp metadata, obtainable by \code{get_light_data} should contain that information.
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @param light_id used to address a specific light. Can be looked up by \code{dump_lights_data}
#' @param kelvin color temperature in Kelvin
#' @param brightness brightness (number between 0 and 1)
#' @export
set_light_temp <- function(bridge_url, user, light_id, kelvin, brightness){

   if ((brightness < 0) | (brightness > 1)) {
     stop("brightness out of range")
   }

  response <- httr::PUT(bridge_url, path = sprintf("api/%s/lights/%s/state", user, light_id),
                          body = list(
                            "ct"  = jsonlite::unbox(round(1e6/kelvin, 0)),
                            "bri" = jsonlite::unbox(round(brightness * 255, 0))
                          ),
                          encode = 'json'
  )

  httr::stop_for_status(response)

  if(!is.null(httr::content(response)[[1]]$error)){
    stop(sprintf("api error: %s", jsonlite::toJSON(httr::content(response)[[1]], auto_unbox = TRUE)))
  }
}

#' Change light color
#'
#' This function does not check if the light actually has the capability to change color,
#' Lamp metadata, obtainable by \code{get_light_data} should contain that information.
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param user user id, to be obtained by \code{create_user}
#' @param light_id used to address a specific light. Can be looked up by \code{dump_lights_data}
#' @param color hex-encoded color, e.g. "#FF0000"
#' @export
set_light_color <- function(bridge_url, user, light_id, color){

  response <- httr::PUT(bridge_url, path = sprintf("api/%s/lights/%s/state", user, light_id),
                        body = list(
                          "xy" = convert_xy(color)
                        ),
                        encode = 'json'
  )

  httr::stop_for_status(response)

  if(!is.null(httr::content(response)[[1]]$error)){
    stop(sprintf("api error: %s", jsonlite::toJSON(httr::content(response)[[1]], auto_unbox = TRUE)))
  }
}
