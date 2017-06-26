#' Tell ggplot to display dollar-value abbreviations on axis labels
#'
#' @return a label-formatting function to be used with
#' scale_y_continuous or scale_x_continuous
#' @usage scale_y_continuous(labels = money_labels)
#' @examples
#' ggplot(
#'  data = FPDS %>%
#'    group_by(Fiscal.Year, Vendor.Size) %>%
#'    summarize(Action.Obligation = sum(Action.Obligation)),
#'  aes(
#'    x = Fiscal.Year,
#'    y = Action.Obligation,
#'    color = Vendor.Size)) +
#' geom_line() +
#' scale_y_continuous(labels = money_labels)
#'
#' @export

money_labels <- function(axis_values){

  if(class(axis_values) == "character"){
    warning(
      paste("money_labels() expects the axis to be a numeric variable",
            "but the axis is a character variable.  Coercing to numeric."))
    axis_values <- as.numeric(axis_values)
  } else if(class(axis_values) != "numeric" & class(axis_values)!= "integer"){
    stop(paste(
      "money_labels() expected a numeric axis, but got:",
      class(axis_values)))
  }
  axis_range <- max(axis_values, na.rm = TRUE) - min(axis_values, na.rm = TRUE)
  sig_digits <-  floor(log10(max(abs(axis_values), na.rm = TRUE))) -
    round(log10(axis_range))

  label_one_value <- function(a_value, max_value, sig) {
    if(is.na(a_value)) return(NULL)
    if(max_value > 1e7){
      if(max_value > 1e11){
        if(max_value > 1e13){
          if(max_value > 1e15){
            return(as.character(a_value))
          } else if(max_value > 1e14){
            y_lab <- paste0(
              "$", formatC(a_value/1e12, max(sig, 0), format = "f"), "T")
          } else {
            y_lab <- paste0(
              "$", formatC(a_value/1e12, max(sig, 1), format = "f"), "T")
          }
        } else if(max_value > 1e12){
          y_lab <- paste0(
            "$", formatC(a_value/1e12, max(sig, 2), format = "f"), "T")
        } else {
          y_lab <- paste0(
            "$", formatC(a_value/1e9, max(sig, 0), format = "f"), "B")
        }
      } else if(max_value > 1e9){
        if(max_value > 1e10){
          y_lab <- paste0(
            "$", formatC(a_value/1e9, max(sig, 1), format = "f"), "B")
        } else {
          y_lab <- paste0(
            "$", formatC(a_value/1e9, max(sig, 2), format = "f"), "B")
        }
      } else {
        if(max_value > 1e8){
          y_lab <- paste0(
            "$", formatC(a_value/1e6, max(sig, 0), format = "f"), "M")
        } else {
          y_lab <- paste0(
            "$", formatC(a_value/1e6, max(sig, 1), format = "f"), "M")
        }
      }
    } else if(max_value > 1e3){
      if(max_value > 1e5){
        if(max_value > 1e6){
          y_lab <- paste0(
            "$", formatC(a_value/1e6, max(sig, 2), format = "f"), "M")
        } else {
          y_lab <- paste0(
            "$", formatC(a_value/1e3, max(sig, 0), format = "f"), "k")
        }
      } else if(max_value > 1e4){
        y_lab <- paste0(
          "$", formatC(a_value/1e3, max(sig, 1), format = "f"), "k")
      } else {
        y_lab <- paste0(
          "$", formatC(a_value/1e3, max(sig, 2), format = "f"), "k")
      }
    } else if(max_value > 10){
      if(max_value > 100){
        y_lab <- paste0("$", formatC(a_value, max(sig, 0), format = "f"))
      } else {
        y_lab <- paste0("$", formatC(a_value, max(sig, 1), format = "f"))
      }
    } else {
      y_lab <- paste0("$", formatC(a_value, max(sig, 2), format = "f"))
    }
    return(y_lab)
  }

  return(sapply(
    axis_values,
    label_one_value,
    max(abs(axis_values), na.rm = TRUE),
    sig_digits))
}

#' Create a style for a tooltip wellPanel
#'
#' @param background_color A hex value, as quoted string, to form the
#' background color of the tooltip
#' @param preferred_side One of c("bottom_right", "top_right", "bottom_left",
#' "top_left")
#' @param flip_on_squish_h How close, in pixels, the mouse can get to the
#' (horizontal) edge of the chart before the tooltip flips to the other
#' side to give more space
#' @param flip_on_squish_v How close, in pixels, the mouse can get to the
#' (vertical) edge of the chart before the tooltip flips to the other
#' side to give more space
#'
#' @return A formatted string to include for wellPanel's style argument
#' within an output renderUI function for hover tips
#'
#' @usage wellPanel(style = hover_style(), "tip here")
#'
#' @examples
#' output$hover_info <- renderUI({
#'   if(is.null(input$plot_hover)){return()}
#'   year <- round(input$plot_hover$x)
#'   wellPanel(
#'     style = hover_style(),
#'     p(HTML(paste("hovering over year:", year))))
#' })
#'
#' @export

hover_style <- function(
  background_color = "#f5f5f5",
  preferred_side = "bottom_right",
  flip_on_squish_h = 100,
  flip_on_squish_v = 100){


  left_pct <- (hover$x - hover$domain$left) /
    (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) /
    (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct *
    (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct *
    (hover$range$bottom - hover$range$top)


  # Use HTML/CSS to change style of tooltip panel here

  style <- paste0(
    "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

  return(style)
}

