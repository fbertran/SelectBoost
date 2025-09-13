#' Network confidence class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' \describe{
#'    \item{network.confidence}{Matrix of confidence indices.}
#'
#'    \item{name}{Vector.}
#'
#'    \item{array}{F array}
#'
#'    \item{time_pt}{Vector}
#'
#'    \item{cv.subjects}{Logical. Was crossvalidation carried out subjectwise?}
#'  }
#' @name network.confidence-class
#' @rdname network.confidence-class
#' @exportClass network.confidence
setClass(Class = "network.confidence",
         slots = c(network.confidence="matrix",
                        name="character",
                        F="array",
                        time_pt="numeric",
                        cv.subjects="logical")
         #,cv.fun="function"
         #,cv.fun.name="vector"
)
