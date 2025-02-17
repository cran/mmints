% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authModule.R
\name{authUI}
\alias{authUI}
\title{Create UI elements for Authentication Shiny Module}
\usage{
authUI(id)
}
\arguments{
\item{id}{A character string that uniquely identifies this module instance}
}
\value{
A list containing UI elements for authentication
}
\description{
This function generates the UI components for the Authentication module,
including login, signup, and guest access options.
}
\examples{
shiny::fluidPage(
  authUI("auth_module")
)
}
