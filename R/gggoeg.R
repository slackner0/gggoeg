# a ggplot theme for goeg


.onLoad <- function(libname, pkgname) {

  requireNamespace("grDevices")

  grDevices::windowsFonts(LucidaSansUnicode = grDevices::windowsFont("TT Lucida Sans Unicode"))

}

#### GOEG COLOURS ####


#' Return a palette of GÖG colours
#'
#' @param n Number of colours to be returned. Defaults to the size of the chosen set. If n is larger than the size of the chosen set, palette() will be used to interpolate additional colours.
#' @param set Chooses the set of colours to be returned. Possible values are "normal", "paired" or "tripled".
#' @param grey Include grey? Default = TRUE
#' @param order Optional, rearranges the order of the returned palette.
#'
#' @return a vector of GÖG colours
goeg.colours <- function(n=NULL, set=NULL, grey=TRUE, order=NULL) {

  set <- match.arg(set, c('normal', 'paired', 'tripled'))

  palette <- if (grey) {
    c("#A3ABA6","#E9B500","#4FA9CB","#E53517","#79B51C")
  } else {
    c("#E9B500","#4FA9CB","#E53517","#79B51C")
  }

  if (set == "paired")
    palette <- if (grey) {
      c("#67726B","#A3ABA6",
        "#E9B500","#FFDA59",
        "#4FA9CB","#95CBDF",
        "#E53517","#F18572",
        "#79B51C","#B1E761")
    } else {
      c("#E9B500","#FFDA59",
        "#4FA9CB","#95CBDF",
        "#E53517","#F18572",
        "#79B51C","#B1E761")
    }

  if (set == "tripled")
    palette <- if(grey) {
      c("#67726B","#A3ABA6","#C0C7C2",
        "#E9B500","#FFDA59","#FFE791",
        "#4FA9CB","#95CBDF","#B9DCEA",
        "#E53517","#F18572","#F5ADA0",
        "#79B51C","#B1E761","#CBEF96")
    } else {
      c("#E9B500","#FFDA59","#FFE791",
        "#4FA9CB","#95CBDF","#B9DCEA",
        "#E53517","#F18572","#F5ADA0",
        "#79B51C","#B1E761","#CBEF96")
    }

  if (is.null(n)) {
    n <- length(palette)
  }

  if (n > length(palette)) {

    warning(paste("Selected palette has less than",n,"entries, interpolating values.\nConsider changing the set."))

    palette <- colorRampPalette(palette)(n)

  }

  if (is.null(order)) {
    order <- 1:length(palette)
  }

  return(palette[order])

}

#### GOEG THEME ####

#' ggplot2 theme parameters that modify plots
#'
#' @param ... additional ggplot theme parameters that overwrite or supplement the GÖG theme
#'
#' @return an (incomplete) ggplot2 theme
theme_goeg <- function(...) {

  theme_get() +

  theme(text = element_text(family="LucidaSansUnicode",size=7),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="black",size=0.5),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0,size=7),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.line = element_blank(),
        legend.text = element_text(size=7),
        plot.title = element_text(size=9, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=7, hjust=0.5)) +

    theme(...)

}

#### GOEG SCALES ####

#' A discrete scale for filling with GÖG colours
#'
#' @param n The number of GÖG colours to be produced
#' @param set The set of GÖG colours from which to draw. Possible values are "normal", "paired" or "tripled".
#' @param grey Whether or not grey should be included. Default: TRUE
#' @param order Optional vector of ordering of GÖG colours
#' @param ... additional parameters passed to scale_fill_manual
#'
#' @return a ggplot2 discrete fill scale
#'
#' @seealso goeg.colours(), scale_fill_manual()
scale_fill_goeg <- function(n=NULL, set='normal', grey=TRUE, order=NULL, ...) {
  scale_fill_manual(values=goeg.colours(n=n, set=set, grey=grey, order=order), ...)
}

#' A discrete scale for colouring with GÖG colours
#'
#' @param n The number of GÖG colours to be produced
#' @param set The set of GÖG colours from which to draw. Possible values are "normal", "paired" or "tripled".
#' @param grey Whether or not grey should be included. Default: TRUE
#' @param order Optional vector of ordering of GÖG colours
#' @param ... additional parameters passed to scale_colour_manual
#'
#' @return a ggplot2 discrete colour scale
#'
#' @seealso goeg.colours(), scale_colour_manual()
scale_colour_goeg <- function(n=NULL, set='normal', grey=TRUE, order=NULL, ...) {
  scale_colour_manual(values=goeg.colours(n=n, set=set, grey=grey, order=order), ...)
}

#' A discrete scale for colouring with GÖG colours
#'
#' @param n The number of GÖG colours to be produced
#' @param set The set of GÖG colours from which to draw. Possible values are "normal", "paired" or "tripled".
#' @param grey Whether or not grey should be included. Default: TRUE
#' @param order Optional vector of ordering of GÖG colours
#' @param ... additional parameters passed to scale_colour_manual
#'
#' @return a ggplot2 discrete colour scale
#'
#' @seealso goeg.colours(), scale_colour_manual()
scale_color_goeg <- function(n=NULL, set='normal', grey=TRUE, order=NULL, ...) {
  scale_colour_goeg(values=goeg.colours(n=n, set=set, grey=grey, order=order), ...)
}
