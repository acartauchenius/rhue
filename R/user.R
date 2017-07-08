#' Register a new user with a hue bridge.
#'
#' \strong{Make sure the bridge's link button was pressed no longer than 30s prior to calling this function!}
#'
#' @param bridge_url Hue bridge URL E.g. "http://192.168.1.100"
#' @param app_name Arbitrary name. Default: \code{"rhue"} (See Hue API Documentation)
#' @param device_name Arbitrary name (See Hue API Documentation)
#' @return A new username (character)
#' @examples
#' \dontrun{
#' create_user("http://192.168.1.100", "rhue", "test")
#' }
#' @export
create_user <- function(bridge_url, app_name = "rhue", device_name){

    combined_name <- sprintf("%s#%s", app_name, device_name)

    response <- httr::POST(bridge_url, path = "api",
      body = list("devicetype" = jsonlite::unbox(combined_name)),
      encode = 'json'
    )

    httr::stop_for_status(response)

    if(!is.null(httr::content(response)[[1]]$error)){
      stop(sprintf("api error: %s", jsonlite::toJSON(httr::content(response)[[1]], auto_unbox = TRUE)))
    }

    return(httr::content(response)[[1]]$success$username)
}
