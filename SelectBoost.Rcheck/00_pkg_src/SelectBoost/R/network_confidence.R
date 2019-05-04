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
         representation(network.confidence="matrix",name="vector",F="array",time_pt="vector",cv.subjects="logical")#,cv.fun="function",cv.fun.name="vector"
)
