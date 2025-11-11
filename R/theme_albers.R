#' Albers Minimalist Visual System for Vignettes
#'
#' @description
#' A collection of ggplot2 themes and color scales following the Albers
#' minimalist design principles. These are primarily intended for use in
#' package vignettes to ensure consistent, accessible, and clean visualizations.
#'
#' @name theme_albers
#' @keywords internal
NULL

#' Okabe-Ito color palette for accessibility
#'
#' @description
#' Returns the Okabe-Ito color palette, optimized for color-blind accessibility.
#' This is the default discrete palette for Albers visualizations.
#'
#' @return Character vector of 8 hex color codes
#' @keywords internal
#' @examples
#' \dontrun{
#' # Display the palette
#' scales::show_col(albers_okabe_ito())
#' }
albers_okabe_ito <- function() {
  c(
    "#000000", # Black
    "#E69F00", # Orange
    "#56B4E9", # Sky Blue
    "#009E73", # Bluish Green
    "#F0E442", # Yellow
    "#0072B2", # Blue
    "#D55E00", # Vermillion
    "#CC79A7"  # Reddish Purple
  )
}

#' Albers color scale for discrete data
#'
#' @description
#' Color scale using the Okabe-Ito palette for accessible discrete colors.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_color_manual()]
#' @return A ggplot2 scale object
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_albers()
#' }
scale_color_albers <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale_color_albers()")
  }
  ggplot2::scale_color_manual(values = albers_okabe_ito(), ...)
}

#' Albers fill scale for discrete data
#'
#' @description
#' Fill scale using the Okabe-Ito palette for accessible discrete colors.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()]
#' @return A ggplot2 scale object
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_albers()
#' }
scale_fill_albers <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale_fill_albers()")
  }
  ggplot2::scale_fill_manual(values = albers_okabe_ito(), ...)
}

#' Albers minimalist ggplot2 theme
#'
#' @description
#' A clean, minimalist ggplot2 theme following Albers design principles:
#' - Generous white space
#' - Subtle grid lines
#' - Clear hierarchy
#' - Accessible contrast
#' - Legend on top
#'
#' @param base_size Base font size in pts (default: 11)
#' @param base_family Base font family (default: "")
#' @return A ggplot2 theme object
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' theme_set(theme_albers())
#'
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(size = 2.2) +
#'   scale_color_albers() +
#'   labs(
#'     title = "Fuel efficiency vs. weight",
#'     subtitle = "Example with Albers theme",
#'     x = "Weight (1000 lbs)",
#'     y = "MPG"
#'   )
#' }
theme_albers <- function(base_size = 11, base_family = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for theme_albers()")
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Grid
      panel.grid.major = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # Axes
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),

      # Plot labels
      plot.title = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 10),
        color = "#374151"
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = "#6b7280",
        margin = ggplot2::margin(t = 10)
      ),

      # Legend
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold")
    )
}
