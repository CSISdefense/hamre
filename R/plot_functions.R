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

#' Create a tooltip for a ggplot2 graph in Shiny
#'
#'
#' @param html_content The content to display in the tooltip.
#' @param chart_data A data frame.  The same data used to create the chart.
#' @param hover_object The Shiny input object containing the hover info.
#' @param x_var The quoted name of the variable on the X axis. NEVER MIND
#' YOU CAN DO THIS WITH hover$mapping$x
#' @param y_var The quoted name of the variable on the Y axis. NEVER MIND
#' YOU CAN DO THIS WITH hover$mapping$x
#' @param chart_type One of c("line", "bar", "stacked bar", "area").
#' Other types are not currently supported and will require manual tips.
#' @param ... Additional arguments to customize the look of the tooltip;
#' will be passed through to hover_style()
#'
#' @return A wellPanel-based on-hover tooltip.
#'
#' @examples
#' \dontrun{
#' output$hover_info <- renderUI({
#'   hover_tip(
#'     html_content = p(html("hello world")),
#'     chart_data = dataset(),
#'     hover_object = input$plot_hover,
#'     chart_type = "stacked bar")
#' })
#' }
#' @export

hover_tip <- function(
  chart_data,
  hover_object,
  html_content,
  x_var = "Fiscal.Year",
  y_var = "Amount",
  chart_type = "line",
  ...
  ){

  if(is.null(hover_object$x)) return()
  if(is.null(hover_object$y)) return()


}

#' Define the tooltip visual style for a tooltip on a ggplot2 graph
#' in a Shiny app
#'
#' @param hover_object The Shiny input object containing the hover info.
#' @param background_color A hex value, as quoted string, to form the
#' background color of the tooltip
#' @param alpha A number between 0 and 1, to set the opacity of the
#' tooltip background
#' @param preferred_side One of c("bottom_right", "top_right", "bottom_left",
#' "top_left")
#' @param minimum_h The minimum horizonal width, in pixels, of the tooltip
#' panel.  The panel will "squish" to be smaller as the cursor approaches
#' the edge of the plotting area, but will not squish smaller than minimum_h.
#' @param minimum_v The minimum vertical width, in pixels, of the tooltip
#' panel.  The panel will "squish" to be smaller as the cursor approaches
#' the edge of the plotting area, but will not squish smaller than minimum_v.
#'
#' @return A wellPanel-based on-hover tooltip
#'
#' @usage wellPanel(style = hover_style(input$plot_hover), "tip here")
#'
#' @examples
#'
#'

hover_style <- function(
  hover_object,
  background_color = "#f5f5f5",
  alpha = 0.85,
  preferred_side = "bottom_right",
  minimum_h = 100,
  minimum_v = 100
  ){

  hover <- hover_object
  rgb <- col2rgb(background_color)[,1]
  if(alpha > 1 | alpha < 0) stop("alpha must be between 0 and 1")

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left +
    (((hover$x - hover$domain$left) /
    (hover$domain$right - hover$domain$left)) *
    (hover$range$right - hover$range$left))
  top_px <- hover$range$top +
    (((hover$domain$top - hover$y) /
    (hover$domain$top - hover$domain$bottom)) *
    (hover$range$bottom - hover$range$top))

  switch(
    preferred_side,

    "bottom_right" = {
      if((hover$range$right - left_px) < minimum_h){
        left_px <- (hover$range$right - minimum_h)}
      if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
        top_px <- (hover$range$bottom - mimimum_v)}
      return(paste0(
        "position:absolute; z-index:100; background-color: ",
        "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
        "left:", left_px, "px; top:", top_px, "px;"))
      },

    "bottom_left" = {
      if((left_px - hover$range$left) < minimum_h){
        left_px <- hover$range$left + minimum_h}
      if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
        top_px <- (hover$range$top - (hover$range$bottom + mimimum_v))}
      return(paste0(
        "position:absolute; z-index:100; background-color: ",
        "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
        "right:", hover$range$right - left_px, "px; top:", top_px, "px;"))
      },

    "top_right" = {
      if((hover$range$right - left_px) < minimum_h){
        left_px <- (hover$range$right - minimum_h)}
      if((top_px - hover$range$top) < minimum_v){
        top_px <- (hover$range$top + mimimum_v)}
      return(paste0(
        "position:absolute; z-index:100; background-color: ",
        "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
        "left:", left_px, "px; bottom:", hover$range$top - top_px, "px;"))
    },

    "top_left" = {
      if((left_px - hover$range$left) < minimum_h){
        left_px <- hover$range$left + minimum_h}
      if((top_px - hover$range$top) < minimum_v){
        top_px <- (hover$range$top + mimimum_v)}

    },
    stop(paste0(
      "preferred_side argument must be one of the following:\n",
      "'bottom_right', 'bottom_left','top_right', 'top_left'\n"))
  )
}


