#' @title plot_Selectboost_cascade
#'
#' @description Plot result of Selectboost for Cascade inference.
#'
#' @name plot_selectboost_cascade
#'
#' @param x A \code{network.confidence} object to be plotted.
#' @param col Colors for the plot.
#' @param ... Additionnal arguments passed to the heatmap function.
#'
#' @return Nothing.
#' @family Selectboost functions
#' @author Frederic Bertrand, \email{frederic.bertrand@@utt.fr}
#' @references \emph{selectBoost: a general algorithm to enhance the performance of variable selection methods in correlated datasets}, Frédéric Bertrand, Ismaïl Aouadi, Nicolas Jung, Raphael Carapito, Laurent Vallat, Seiamak Bahram, Myriam Maumy-Bertrand, Bioinformatics, 2020. \doi{10.1093/bioinformatics/btaa855}
#' @details Extending results from the Cascade package: providing confidence indices for the reverse engineered links.
#'
#' Reference for the Cascade modelling
#' Vallat, L., Kemper, C. a., Jung, N., Maumy-Bertrand, M., Bertrand, F.,
#' Meyer, N., Pocheville, A., Fisher, J. W., Gribben, J. G. et Bahram, S.
#' (2013). Reverse-engineering the genetic circuitry of a cancer cell with predicted
#' intervention in chronic lymphocytic leukemia. Proceedings of the National
#' Academy of Sciences of the United States of America, 110(2), 459-64.
#'
#' Reference for the Cascade package
#' Jung, N., Bertrand, F., Bahram, S., Vallat, L. et Maumy-Bertrand, M. (2014).
#' Cascade : A R package to study, predict and simulate the diffusion of a signal
#' through a temporal gene network. Bioinformatics. ISSN 13674803..
#' @seealso \code{\link{boost}}, \code{\link{fastboost}}, \code{\link{selectboost}}, \code{\link[Cascade]{inference}}
#' @docType methods
NULL
#> NULL

#' @rdname plot_selectboost_cascade
#' @aliases plot,network.confidence,network.confidence-method
#' @examples
#' data(net_confidences)
#' plot(net_confidence)
#' plot(net_confidence_.5)
#' plot(net_confidence_thr)
#'
#' @export

setMethod(f="plot"
          ,signature=c("network.confidence")
          ,definition=function(x,
                               col=gray((1:99)/100, alpha = NULL)
                               ,...
          ){
            stats::heatmap(x@network.confidence, Rowv = NA, Colv = NA, col=col, scale="none", revC=TRUE,...)
          }
)

