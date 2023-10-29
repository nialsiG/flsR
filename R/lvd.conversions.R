# lvd.to.strain----
#' @title lvd.to.str
#' @description Convert load data from an lvd object into strain
#' @param x An object of class 'lvd', or a numeric vector of displacement values
#' @param lng A numeric indicating the initial specimen length
#' @param unit The unit in which the measure was taken. Default is mm.
#' @return A numeric vector for strain
#' @examples
#'
#' @export
lvd.to.strain <- function(x, lng, unit = "mm") {
  if (is.numeric (x) == TRUE & is.vector(x) == TRUE) {
    Strain <- x/lng
  }

  if (is.vector(x) == FALSE) {
    if (class(x)[1] == "lvd") {
      Strain <- x@displacement/lng
    }
    else {
      stop("x should be a numeric vector, or an object of class 'lvd'")
    }
  }
  return(Strain)
}

# lvd.to.stress----
#' @title lvd.to.stress
#' @description Convert load data from an lvd object into stress
#' @param x A vector of load data or an object of class 'lvd'
#' @param area A numeric indicating the initial specimen cross-section area
#' @param unit The unit in which the measure was taken. Default is square mm
#'   ("mm2").
#' @return A numeric vector for stress
#' @examples
#'
#' @export
lvd.to.stress <- function(x, area, unit = "mm2") {
  Stress <- NULL

  if (is.numeric (x) == TRUE & is.vector(x) == TRUE) {
    Stress <- x/area
  }

  if (is.vector(x) == FALSE) {
    if (class(x)[1] == "lvd") {
      Stress <- x@load/area
    }
    else {
      stop("x should be a numeric vector, or an object of class 'lvd'")
    }
  }
  return(Stress)
}

# hardness----
#' @title hardness
#' @description Compute Vicker's hardness from an lvd object
#' @param x An object of class 'lvd'
#' @param area A numeric indicating the area of the deformation
#' @param unit.area The unit in which the cross area was taken. Default is
#'   square mm ("mm2")
#' @param unit.load The unit in which the load was measured. Default is N
#' @param crop Logical indicating whether the lvd.crop function should be
#'   launched before the computation starts
#' @param zero.out Logical indicating whether the stress-strain curve should be
#'   "zero-outed" before the conversion
#' @return A numeric value for Young's elasticity modulus
#' @examples
#'
#' @export
hardness <- function(x, area, unit.area = "mm2", unit.load = "N", crop = FALSE, zero.out = FALSE, alpha = 0.05) {

  if (class(x)[1] == "lvd") {
    if(crop == TRUE) {
      x <- lvd.crop(x, method = "before.max")
    }
    Load <- x@load
    if(zero.out == TRUE) {
     Load <- Load[which(Load > (max(Load)*alpha))]
    }
    # Conversion
    # Si cross-area en mm2 et stress en N, cela nous donne des N/mm2, soit des MPa
    # Facteur de conversion à calculer en fonction des unités
    if (unit.load == "N") {L = 9.80665}
    if (unit.load == "mN") {L = 9.80665 / 1000}
    if (unit.load == "kgf") {L = 1}
    if (unit.area == "mm2") {A = 1}
    if (unit.area == "cm2") {A = 100}
    if (unit.area == "m2") {A = 1000000}

    Load <- Load / L
    area <- area * A

    # Hardness
    Hardness <-  1.854 * max(Load) / area
    return(Hardness)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}

# vickers.area----
#' @title vickers.area
#' @description Converts dimensions of indentation marks left by Vicker's test
#'   into surface area
#' @param d the average diagonal length of the indentation mark
#' @return A numeric approximation of the surface area of the indentation mark
#' @examples
#'
#' @export
vickers.area <- function(d) {
  VA <- d^2 / (2*sin((136/2)*pi/180))
  return(VA)
}


# young.modulus----
#' @title young.modulus
#' @description Compute Young's modulus from an lvd object
#' @param x An object of class 'lvd'
#' @param area A numeric indicating the fracture cross-section area
#' @param lng A numeric indicating the initial specimen length
#' @param unit.area The unit in which the cross area was taken. Default is
#'   square mm ("mm2")
#' @param unit.load The unit in which the load was measured. Default is N
#' @param crop Logical indicating whether the lvd.crop function should be
#'   launched before the computation starts
#' @param zero.out Logical indicating whether the stress-strain curve should be
#'   "zero-outed" before the conversion.
#' @param slope.percentile The percentile of slopes used to compute the actual slope of the strain/stress curve
#' @return A numeric value for Young's elasticity modulus
#' @examples
#'
#' @export
young.modulus <- function(x, lng, area, unit.area = "mm2", unit.load = "N", crop = FALSE, zero.out = FALSE, alpha = 0.05, slope.percentile = 0.95) {

  if (class(x)[1] == "lvd") {
    if(crop == TRUE) {
      x <- lvd.crop(x, method = "before.max")
    }
    Stress <- x@load/area
    Strain <- x@displacement/lng
    if(zero.out == TRUE) {
      Stress <- Stress[which(Stress > (max(Stress)*alpha))]
      Strain <- Strain[which(Stress > (max(Stress)*alpha))]
    }
    # Points for the smooth curve
    temp <- loess.smooth(x = Strain, y = Stress, evaluation = 50, degree = 2)
    # Slopes from the smooth curve, the max slope is the initial slope
    Slopes <- diff(temp$y)/diff(temp$x)
    Slope0 <- quantile(Slopes, slope.percentile)
    # Conversion
    # Si cross-area en mm2 et stress en N, cela nous donne des N/mm2, soit des MPa
    # Facteur de conversion à calculer en fonction des unités
    if (unit.load == "N") {L = 1}
    if (unit.load == "mN") {L = 0.001}
    if (unit.load == "kgf") {L = 1/9.80665}
    if (unit.area == "mm2") {A = 1}
    if (unit.area == "cm2") {A = 100}
    if (unit.area == "m2") {A = 1000000}
    K <- L/A
    Modulus <- Slope0/K
    return(Modulus)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}

#  energy release rate----
#' @title energy.rate
#' @description Compute the energy release rate in J/m2
#' @param x An object of class 'lvd' for the first pass (= with specimen)
#' @param y An object of class 'lvd' for the second pass (= empty)
#' @param blade A character string indicating the type of blade used for the
#'   test ('wedge' or 'scissors'). Default is 'wedge.'
#' @param width For the wedge test, a numeric indicating the width of the specimen
#' @param depth For the wedge test, a numeric indicating the depth of the fracture
#' @param unit.area The unit in which the cross area was taken. Default is
#'   square mm ("mm2")
#' @param unit.load The unit in which the load was measured. Default is N
#' @param crop Character string indicating whether the lvd data should be
#'   cropped automatically ('auto') or manually ('manual'). Default is 'auto', which crops after the maximum was reached.
#' @return A numeric value for the specimen's energy release rate G
#' @examples
#'
#' @export
energy.rate <- function(x, y, blade = "wedge", width, depth, crop = "auto", unit.area = "mm2", unit.load = "N") {

  if (class(x)[1] == "lvd") {
    if(crop == "auto") {

      indexMaxLoad <- which(x@load == max(x@load))
      x@load <- x@load[indexMaxLoad:length(x@load)]
      x@displacement <- x@displacement[indexMaxLoad:length(x@load)]
      x@time <- x@time[indexMaxLoad:length(x@load)]
      y@load <- y@load[indexMaxLoad:length(x@load)]
      y@displacement <- y@displacement[indexMaxLoad:length(x@load)]
      y@time <- y@time[indexMaxLoad:length(x@load)]
      #then get the min of the cropped data and crop to the left of the lvd graph:
      indexMinLoad <- which(x@load == min(x@load))
      x@load <- x@load[1:indexMinLoad]
      x@displacement <- x@displacement[1:indexMinLoad]
      x@time <- x@time[1:indexMinLoad]
      y@load <- y@load[1:indexMinLoad]
      y@displacement <- y@displacement[1:indexMinLoad]
      y@time <- y@time[1:indexMinLoad]


    }
    else if(crop == "manual") {
      warning("Sorry, 'manual' crop is still work in progress")
    }

    #Area under curve
    Load <- x@load
    Displacement <- x@displacement
    Auc <- sum(tis::lintegrate(Load, Displacement, xint = Load))
    #Remove 2nd pass
    Load <- y@load
    Displacement <- y@displacement
    Auc <- Auc - sum(tis::lintegrate(Load, Displacement, xint = Load))

    if (blade == "wedge"){
      G <- Auc / (width * depth)
    }
    else if (blade == "scissors"){
      warning("Sorry, 'scissors' are still work in progress")
      return()
    }
    else{
      warning("Blade argument should be of type 'wedge' or 'scissors'")
      return()
    }

    #Conversion
    if (unit.load == "N") {L = 1}
    if (unit.load == "mN") {L = 0.001}
    if (unit.load == "kgf") {L = 1/9.80665}
    if (unit.area == "mm2") {A = 0.000001}
    if (unit.area == "cm2") {A = 0.0001}
    if (unit.area == "m2") {A = 1}
    K <- L / A
    G <- G / K

    return(G)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}
