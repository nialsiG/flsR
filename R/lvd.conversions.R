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
#' @description Compute Young's modulus from an lvd object
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
    # Hardness
    Hardness <- max(Load)/area
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
    Hardness <- Hardness/K
    return(Hardness)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}

# strength----
#' @title strength
#' @description Convert data from an lvd object into strength
#' @param x An object of class 'lvd'
#' @return A numeric value for strength
#' @examples
#'
#' @export
strength <- function() {
 #TO DO
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

# toughness----
#' @title toughness
#' @description Compute the energy per unit volume, or toughness, in J/m3
#' @param x An object of class 'lvd'
#' @param area A numeric indicating the initial specimen cross-section area
#' @param lng A numeric indicating the initial specimen length
#' @param unit.area The unit in which the cross area was taken. Default is
#'   square mm ("mm2")
#' @param unit.load The unit in which the load was measured. Default is N
#' @param crop Character string indicating whether the lvd data should be
#'   cropped automatically ('auto') or manually ('manual').
#' @param zero.out Logical indicating whether the stress-strain curve should be
#'   "zero-outed" before the conversion. NOTE : INTRODUIRE UN ALPHA POUR DECIDER
#'   QUEL POURCENTAGE EXCLURE ENTRE 1% et 5%
#' @return A numeric value for Young's elasticity modulus
#' @examples
#'
#' @export
toughness <- function(x, lng, area, crop = "auto", unit.area = "mm2", unit.load = "N", zero.out = FALSE, alpha = 0.05) {

  if (class(x)[1] == "lvd") {
    if(crop == "auto") {
      x <- lvd.crop(x, method = "before.max")
    }
    else if(crop == "manual") {
      x <- lvd.crop(x, method = "manual")
    }
    Stress <- x@load/area
    Strain <- x@displacement/lng
    if(zero.out) {
      Stress <- Stress[which(Stress > (max(Stress) * alpha))]
      Strain <- Strain[which(Stress > (max(Stress) * alpha))]
    }
    #Area under curve
    Auc <- sum(tis::lintegrate(Stress, Strain, xint = Stress))
    #Conversion?
    #
    return(Auc)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}


#  energy release rate----
#' @title energy.release.rate
#' @description Compute the energy release rate in J/m2
#' @param x An object of class 'lvd' for the first pass (= with specimen)
#' @param y An object of class 'lvd' for the second pass (= empty)
#' @param blade A character string indicating the type of blade used for the
#'   test (wedge or scissors)
#' @param width For the wedge test, a numeric indicating the width of the specimen
#' @param unit.area The unit in which the cross area was taken. Default is
#'   square mm ("mm2")
#' @param unit.load The unit in which the load was measured. Default is N
#' @param crop Character string indicating whether the lvd data should be
#'   cropped automatically ('auto') or manually ('manual').
#' @return A numeric value for Young's elasticity modulus
#' @examples
#'
#' @export
release.rate <- function(x, y, blade = "wedge", width, crop = "auto", unit.area = "mm2", unit.load = "N") {

  if (class(x)[1] == "lvd") {
    if(crop == "auto") {
      x <- lvd.crop(x, method = "before.max")
    }
    else if(crop == "manual") {
      x <- lvd.crop(x, method = "manual")
    }
    Load <- x@load
    Displacement <- x@displacement

    #Area under curve
    Auc <- sum(tis::lintegrate(Stress, Strain, xint = Stress))

    if (blade == "wedge"){
      Errate <- Auc / width
    }
    else if (blade == "scissors"){
      warning("Sorry, 'scissors' are still work in progress")
      return()
    }
    else{
      warning("Blade argument should be of type 'wedge' or 'scissors'")
      return()
    }
    #Conversion?
    #
    return(Errate)
  }

  else {
    stop("x should be a numeric vector, or an object of class 'lvd'")
  }
}
