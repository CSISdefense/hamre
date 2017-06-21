#' Tell ggplot to display dollar-value abbreviations on axis labels
#'
#' @return a label-formatting function to be used with
#' scale_y_continuous or scale_x_continuous
#' @export
#'
#' @examples geom_bar() + scale_y_continuous(labels = money_labels())
money_labels <- function(){
  formatter <- function(axis_values){

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
    axis_range <- max(axis_values) - min(axis_values)
    sig_digits <-  floor(log10(max(abs(axis_values)))) -
      round(log10(axis_range))

    label_one_value <- function(value, sig) {
      if(abs(value) > 1e7){
        if(abs(value) > 1e11){
          if(abs(value) > 1e13){
            if(abs(value) > 1e15){
              return(as.character(value))
            } else if(abs(value) > 1e14){
              y_lab <- paste0(
                "$", formatC(value/1e12, max(sig, 0), format = "f"), "T")
            } else {
              y_lab <- paste0(
                "$", formatC(value/1e12, max(sig, 1), format = "f"), "T")
            }
          } else if(abs(value) > 1e12){
            y_lab <- paste0(
              "$", formatC(value/1e12, max(sig, 2), format = "f"), "T")
          } else {
            y_lab <- paste0(
              "$", formatC(value/1e9, max(sig, 0), format = "f"), "B")
          }
        } else if(abs(value) > 1e9){
          if(abs(value) > 1e10){
            y_lab <- paste0(
              "$", formatC(value/1e9, max(sig, 1), format = "f"), "B")
          } else {
            y_lab <- paste0(
              "$", formatC(value/1e9, max(sig, 2), format = "f"), "B")
          }
        } else {
          if(abs(value) > 1e8){
            y_lab <- paste0(
              "$", formatC(value/1e6, max(sig, 0), format = "f"), "M")
          } else {
            y_lab <- paste0(
              "$", formatC(value/1e6, max(sig, 1), format = "f"), "M")
          }
        }
      } else if(abs(value) > 1e3){
        if(abs(value) > 1e5){
          if(abs(value) > 1e6){
            y_lab <- paste0(
              "$", formatC(value/1e6, max(sig, 2), format = "f"), "M")
          } else {
            y_lab <- paste0(
              "$", formatC(value/1e3, max(sig, 0), format = "f"), "k")
          }
        } else if(abs(value) > 1e4){
          y_lab <- paste0(
            "$", formatC(value/1e3, max(sig, 1), format = "f"), "k")
        } else {
          y_lab <- paste0(
            "$", formatC(value/1e3, max(sig, 2), format = "f"), "k")
        }
      } else if(abs(value) > 10){
        if(abs(value) > 100){
          y_lab <- paste0("$", formatC(value, max(sig, 0), format = "f"))
        } else {
          y_lab <- paste0("$", formatC(value, max(sig, 1), format = "f"))
        }
      } else {
        y_lab <- paste0("$", formatC(value, max(sig, 2), format = "f"))
      }
      return(y_lab)
    }

    return(sapply(axis_values, label_one_value, sig_digits))
  }
  return(formatter)
}




