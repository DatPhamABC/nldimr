library(tsne)
library(R6)


R_tsne <- R6Class(classname = "t-SNE",

                  inherit = Dimension_reduction,

                 # public attributes and methods
                 public = list(
                   data = NULL,
                   group = NULL,
                   k = NULL,
                   initial_config = NULL,
                   initial_dims = NULL,
                   perplexity = NULL,
                   max_iter = NULL,
                   min_cost = NULL,
                   epoch_callback = NULL,
                   whiten = NULL,
                   epoch = NULL,
                   result = NULL,

#' Initialize function for R_tsne class
#'
#' @param data The matrix of similarities/distances between data points.
#' @param group The groups of data points. Used for visualization. Can be NULL if it is not known
#' @param initial_config An argument providing matrix specifying the initial embedding for data.
#' @param k The dimension of the resulting embedding.
#' @param initial_dims The number of dimensions to use in reduction methods.
#' @param perplexity Perplexity parameter.
#' @param max_iter Maximum number of iterations to perform.
#' @param min_cost Minimum cost value (error) to halt iteration.
#' @param epoch_callback A callback function used after each epoch.
#' @param whiten A boolean value indicating whether the matrix data should be whitened.
#' @param epoch The number of iterations in between update messages.
#'
#' @returns None
#' @export
#'
#' @examples
#' tsne_iris = R_tsne$new(data=iris[,1:4], group=iris$Species, k=2, perplexity=50)
                   initialize = function(data, group = NULL,
                                         initial_config = NULL, k = 2,
                                         initial_dims = 30, perplexity = 30,
                                         max_iter = 1000, min_cost = 0,
                                         epoch_callback = NULL, whiten = TRUE,
                                         epoch=100) {
                     self$data <- data
                     self$group <- group
                     self$k <- k
                     self$initial_config <- initial_config
                     self$initial_dims <- initial_dims
                     self$perplexity <- perplexity
                     self$max_iter <- max_iter
                     self$min_cost <- min_cost
                     self$epoch_callback <- epoch_callback
                     self$whiten <- whiten
                     self$epoch <- epoch
                   },

                   getResult = function(){
                     tryCatch({
                       self$result = tsne(self$data,
                                          initial_config = self$initial_config,
                                          k = self$k,
                                          initial_dims = self$initial_dims,
                                          perplexity = self$perplexity,
                                          max_iter = self$max_iter,
                                          min_cost = self$min_cost,
                                          epoch_callback = self$epoch_callback,
                                          whiten = self$whiten,
                                          epoch=self$epoch)
                       self$result = as.data.frame(self$result)
                       }
                       ,
                       # error occurs
                       error=function(e) {
                         message('t-SNE class error:')
                         print(e)
                         },
                       # warning occurs
                       warning=function(w) {
                         message('t-SNE class waring:')
                         print(w)
                         }
                     )
                     }

                 )

                 )
