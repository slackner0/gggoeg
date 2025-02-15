% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gggoeg.R
\name{scale_fill_goeg}
\alias{scale_fill_goeg}
\title{A discrete scale for filling with GÖG colours}
\usage{
scale_fill_goeg(n = NULL, set = "normal", grey = TRUE,
  order = NULL, ...)
}
\arguments{
\item{n}{The number of GÖG colours to be produced}

\item{set}{The set of GÖG colours from which to draw. Possible values are "normal", "paired" or "tripled".}

\item{grey}{Whether or not grey should be included. Default: TRUE}

\item{order}{Optional vector of ordering of GÖG colours}

\item{...}{additional parameters passed to scale_fill_manual}
}
\value{
a ggplot2 discrete fill scale
}
\description{
A discrete scale for filling with GÖG colours
}
\seealso{
goeg.colours(), scale_fill_manual()
}
