# lvd-class----
#' Load versus displacement object
#'
#' Creates an object of class lvd containing the load, the displacement and the
#' time for each observation.
#'
#' @name lvd
#' @rdname lvd
#' @slot load A vector of numeric values specifying the load.
#' @slot displacement A vector of numeric values specifying the displacement.
#' @slot time An optional vector of numeric values specifying the time.
#' @details This function creates an *lvd* object which can be used for
#'   calculating most parameters available in the flsr package.
#' @seealso \code{\link{lvd}}
#' @seealso \code{\link{read.lvm}}
#' @export
setClass("lvd", slots = c(load = "numeric", displacement = "numeric", time = "numeric"))

# build.lvd----
#' @title build.lvd
#' @description Builds an object of class .lvd from specified vectors
#' @param load A vector of numeric values specifying the load.
#' @param displ A vector of numeric values specifying the displacement.
#' @param time An optional vector of numeric values specifying the time.
#' @return An object of class lvd (load versus displacement)
#' @examples
#' l <- c(0,2,4,8,0)
#' d <- c(0,1,2,3,4)
#' t <- c(0,0.1,0.2,0.3,0.4)
#' myLvd <- build.lvd(load = l, displ = d, time = t)
#' @export
build.lvd <- function(load, displ, time = NULL){
  Mylvd <- new("lvd", load = load, displacement = displ, time = time)
  return(Mylvd)
}

# read.lvm----
#' @title read.lvm
#' @description Loads a .lvm file (from Lucas' mechanical tester FLS2) into
#'   memory
#' @param file An .lvm file
#' @param center Logical indicating whether to move lvd data above 0
#' @param inv.displ Logical indicating whether the displacement axis should be
#'   reversed (TRUE) or not (FALSE)
#' @param inv.load Logical indicating whether the load axis should be reversed
#'   (TRUE) or not (FALSE)
#' @return An object of class lvd (load versus displacement)
#' @examples
#' #myLvd <- read.lvm(file = <path>, center = TRUE)
#' @export
read.lvm <- function(file, center = FALSE, inv.displ = FALSE, inv.load = FALSE) {
  Mylvm <- read.table(file)
    Mylvd <- build.lvd(load = Mylvm[, 3], displ = Mylvm[, 2], time = Mylvm[, 1])
    if (inv.displ) Mylvd@displacement <- -(Mylvd@displacement)
    if (inv.load) Mylvd@load <- -(Mylvd@load)
    if (center == TRUE) {
      Mylvd@displacement <- Mylvd@displacement - min(Mylvd@displacement)
      Mylvd@load <- Mylvd@load - min(Mylvd@load)
    }
  return(Mylvd)
}

