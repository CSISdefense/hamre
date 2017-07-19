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




#' Retrieve the data for the current hover location
#'
#' @param chart_data A data frame.  The same data used to create the chart.
#' @param hover_object The Shiny input object containing the hover info.
#' @param chart_type Either "line" or "bar".  For scatterplot, use
#' "line", for area or stacked bar use "bar". Other types are not currently
#' supported and will require manual tips.
#' @param max_distance For line and scatter charts: a distance, in pixels.  If
#' the mouse is more than this distance from any point, the function will
#' return a null data frame.
#'
#' @return A data frame of
#'
#' @examples
#' \dontrun{
#' output$hover_info <- renderUI({
#'
#'   hover_data(
#'     chart_data = dataset(),
#'     hover_object = input$plot_hover,
#'     group_by = c("Vendor.Size", "Simple"),
#'     chart_type = "stacked bar")
#' })
#' }
#' @export

hover_data <- function(
  chart_data,
  hover_object,
  chart_type = "line",
  max_distance = 10
){

  if(is.null(hover_object$x)) return(NULL)
  if(is.null(hover_object$y)) return(NULL)

  if(chart_type == "line"){
    row <- nearPoints(
      chart_data,
      hover_object,
      xvar = hover_object$mapping$x,
      yvar = hover_object$mapping$y,
      threshold = max_distance,
      maxpoints = 1)
    if(nrow(row) == 0) return(NULL)

  } else if(chart_type == "bar"){

    # filter to rows with the correct value of the X axis variable
    x_class <- class(chart_data[[hover$mapping$x]])
    if(x_class == "factor"){
      row <- chart_data %>%
        filter(
          chart_data[[hover_object$mapping$x]] ==
            levels(chart_data[[hover_object$mapping$x]])[round(hover_object$x)])
    } else if(x_class == "character"){
      row <- chart_data %>%
        filter(
          chart_data[[hover_object$mapping$x]] ==
            levels(factor(chart_data[[hover_object$mapping$x]]))[
              round(hover_object$x)])
    } else if(x_class == "numeric" | x_class == "integer"){
      row <- chart_data %>%
        filter_(
          paste(
            hover_object$mapping$x,
            "==",
            round(hover_object$x)))
    } else stop(paste0("Error in hover_data: X variable must be factor,",
                       "character, numeric, or integer"))

    # filter to rows with the correct level of fill breakout
    # (for area or stacked bar)
    if("fill" %in% hover_object$mapping){

      breakout <- chart_data[[hover_object$mapping$fill]]
      if(!(class(breakout) == "factor")) breakout %<>% factor()

      # find which breakout section the cursor is in
      y_breaks <- row %>%
        group_by_(hover_object$mapping$fill) %>%
        summarize_(interp(
          ~sum(var, na.rm = TRUE), var = as.name(hover_object$mapping$y)))

      y_breaks <- append(
        cumsum(rev(pull(y_breaks, 2))),
        hover_object$domain$bottom,
        after = 0)

      cursor_level <- Position(
        function(x) x < hover_object$y,
        y_breaks)

      if(cursor_level > length(levels(breakout))) return(NULL)
      row %<>% filter_(
        paste0(
          hover_object$mapping$fill,
          " == '",
          levels(breakout)[cursor_level],
          "'"))
    }

    # filter to rows in the correct facet(s)
    if(with(hover_object$mapping, exists("panelvar1"))){
      if(!with(hover_object, exists("panelvar1"))) return(NULL)

      if(with(hover_object$mapping, exists("panelvar2"))){
        if(!with(hover_object, exists("panelvar2"))) return(NULL)
        row %<>% filter_(
          paste0(
            hover_object$mapping$panelvar2,
            " == '",
            hover_object$panelvar2,
            "'"))
      }

      row %<>% filter_(
        paste0(
          hover_object$mapping$panelvar1,
          " == '",
          hover_object$panelvar1,
          "'"))
    }

  } else stop("chart_type must be 'line' or 'bar'")


  ##### should I just return row here?  What's the point of aggregation?

  return(row)

  # chart_data <- suppressMessages(
  #     inner_join(
  #       select_(row, .dots = group_by),
  #       chart_data))
  #
  # chart_data %<>%
  #   ungroup() %>%
  #   group_by_(.dots = group_by) %>%
  #   summarize_(
  #     temp_ = interp(
  #       ~sum(var, na.rm = TRUE), var = as.name(hover_object$mapping$y)))
  #
  # names(chart_data)[which(names(chart_data) == "temp_")] <-
  #   hover_object$mapping$y
  #
  # return(chart_data)
}


#' Define the tooltip visual style for a tooltip on a ggplot2 graph
#' in a Shiny app
#'
#' @param hover_object The Shiny input object containing the hover info.
#' @param background_color A hex value, as quoted string, to form the
#' background color of the tooltip
#' @param alpha A number between 0 and 1, to set the opacity of the
#' tooltip background
#' @param preferred_side One of c("right", "left")
#' @param h_just The horizontal distance, in pixels, to adjust the tooltip
#' @param v_just The vertical distance, in pixels, to adjust the tooltop
#' @param minimum_h The minimum horizonal width, in pixels, of the tooltip
#' panel.  The panel will squish to be smaller as the cursor approaches
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
  preferred_side = "right",
  h_just = 0,
  v_just = 0,
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

  if(preferred_side == "right"){
      if((hover$range$right - left_px) < minimum_h){
        left_px <- (hover$range$right - minimum_h)}
      if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
        top_px <- (hover$range$bottom - mimimum_v)}
      return(paste0(
        "position:absolute; z-index:100; background-color: ",
        "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
        "left:", left_px, "px; top:", top_px, "px;"))
      } else if (tolower(preferred_side) == "left") {
      if((left_px - hover$range$left) < minimum_h){
        left_px <- hover$range$left + minimum_h}
      if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
        top_px <- (hover$range$top - (hover$range$bottom + mimimum_v))}
      return(paste0(
        "position:absolute; z-index:100; background-color: ",
        "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
        "right:", hover$range$right - left_px, "px; top:", top_px, "px;"))
      } else stop("preferred_side must be 'right' or 'left'")

}


