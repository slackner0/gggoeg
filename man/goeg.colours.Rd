% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gggoeg.R
\name{goeg.colours}
\alias{goeg.colours}
\title{Return a palette of GÖG colours}
\usage{
goeg.colours(n = NULL, set = NULL, grey = TRUE, order = NULL)
}
\arguments{
\item{n}{Number of colours to be returned. Defaults to the size of the chosen set. If n is larger than the size of the chosen set, palette() will be used to interpolate additional colours.}

\item{set}{Chooses the set of colours to be returned. Possible values are "normal", "paired" or "tripled".}

\item{grey}{Include grey? Default = TRUE}

\item{order}{Optional, rearranges the order of the returned palette.}
}
\value{
a vector of GÖG colours
}
\description{
Return a palette of GÖG colours
}
