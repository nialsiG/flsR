# lvd.plot----
#' @title lvd.plot
#' @description Plot load vs. displacement curve
#' @param x An object of class 'lvd'
#' @param auc A logical indicating whether the area under curve should be filled
#' @return A ggplot2 object
#' @examples
#'
#' @export
lvd.plot <- function(x, auc = TRUE, size = 1) {
  if (class(x)[1] == "lvd") {
    Mydata <- data.frame(Load = x@load, Displacement = x@displacement)
    Myplot <- ggplot2::ggplot(data = Mydata, ggplot2::aes(x = Displacement, y = Load))
    if (auc == TRUE) {
      Myplot <- Myplot + ggplot2::geom_area(fill = "red", alpha = 0.2)
    }
    Myplot <- Myplot + ggplot2::geom_line(color = "red", size = size)
    return(Myplot)
  }
}

# lvd.crop----
#' @title lvd.crop
#' @description Crop the range of lvd values to keep using an interactive plot
#' @param x An object of class 'lvd'
#' @param method The method for cropping data, either 'before.max', 'after.max'
#'   or 'manual'.
#' @return A new 'lvd' object
#' @examples
#'
#' @export
lvd.crop <- function(x, method = "after.max") {
  if(method == "after.max"){
    #first get the max and crop to the right of the lvd graph:
    indexMaxLoad <- which(x@load == max(x@load))
    x@load <- x@load[indexMaxLoad:length(x@load)]
    x@displacement <- x@displacement[indexMaxLoad:length(x@load)]
    x@time <- x@time[indexMaxLoad:length(x@load)]
    #then get the min of the cropped data and crop to the left of the lvd graph:
    indexMinLoad <- which(x@load == min(x@load))
    x@load <- x@load[1:indexMinLoad]
    x@displacement <- x@displacement[1:indexMinLoad]
    x@time <- x@time[1:indexMinLoad]
    return(x)
  }
  else if (method == "before.max"){
    #get the max and crop to the left of the lvd graph:
    indexMaxLoad <- which(x@load == max(x@load))
    x@load <- x@load[1:indexMaxLoad]
    x@displacement <- x@displacement[1:indexMaxLoad]
    x@time <- x@time[1:indexMaxLoad]
    return(x)
  }
  else if (method == "manual"){
    #TO DO (see: plotly)
    #a <- plotly::ggplotly(lvd.plot(x), originalData = FALSE)
    #plotly::highlight(a, "plotly_selected", dynamic = TRUE, persistent = FALSE)
    return(x)
  }
  else {
    return(x)
    warning("Argument 'method' should be of type 'auto' or 'manual'.")
  }
}
