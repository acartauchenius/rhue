% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/switch_lights.R
\name{set_light_temp}
\alias{set_light_temp}
\title{Set a light to a specific color temperature and brightness}
\usage{
set_light_temp(bridge_url, user, light_id, kelvin, brightness)
}
\arguments{
\item{bridge_url}{Hue bridge URL E.g. "http://192.168.1.100"}

\item{user}{user id, to be obtained by \code{create_user}}

\item{light_id}{used to address a specific light. Can be looked up by \code{dump_lights_data}}

\item{kelvin}{color temperature in Kelvin}

\item{brightness}{brightness (number between 0 and 1)}
}
\description{
This function does not check if the light actually has the capability to change color temperature,
Lamp metadata, obtainable by \code{get_light_data} should contain that information.
}
