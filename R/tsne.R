library(tsne)
library(R6)
library(ggplot2)
library(rgl)

R_tsne <- R6Class(classname = "t-SNE",

                 # public attributes and methods
                 public = list(
                   coordinates = NULL,
                   k = NULL,
                   initial_config = NULL,
                   initial_dims = 30,
                   perplexity = 30,
                   max_iter = 1000,
                   min_cost = 0,
                   epoch_callback = NULL,
                   whiten = TRUE,
                   epoch = 100,
                   result = NULL,

                   initialize = function(coordinates, initial_config = NULL, k = 2,
                                         initial_dims = 30, perplexity = 30,
                                         max_iter = 1000, min_cost = 0,
                                         epoch_callback = NULL, whiten = TRUE,
                                         epoch=100) {
                     self$coordinates <- coordinates
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
                     tryCatch(
                       self$result = tsne(self$coordinates,
                                          initial_config = self$initial_config,
                                          k = self$k,
                                          initial_dims = self$initial_dims,
                                          perplexity = self$perplexity,
                                          max_iter = self$max_iter,
                                          min_cost = self$min_cost,
                                          epoch_callback = self$epoch_callback,
                                          whiten = self_whiten,
                                          epoch=self$epoch),
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
                         return(NA)
                         }
                     )
                     },

                   plotResult = function(){
                     tryCatch(
                       if(self$k != 2 || self$k != 3){
                         stop(paste("Unable to visualize ", k, " dimension."))
                       } else if (self$k == 2) {
                         if (is.NULL(self$result)){
                           stop(paste("No dimensionality reduction result found.
                                      Please run getResult() first."))
                         } else {
                           plt = ggplot(data = self$data,
                                        mapping=aes(x = X1, y=X2)) +
                             geom_point(size=3)
                           return(plt)
                         }
                       } else {
                         if (is.NULL(self$result)){
                           stop(paste("No dimensionality reduction result found.
                                      Please run getResult() first."))
                         } else {
                           plt = plot3d(x = self$data$X1,
                                        y = self$data$X2,
                                        z = self$data$X3,
                                        type = 's',
                                        col = NULL,
                                        radius = .1,
                                        xlab = 'Dimension 1',
                                        ylab = 'Dimension 2',
                                        zlab = 'Dimension 3')
                         }
                       }
                     )
                   }

                 )






                 )
