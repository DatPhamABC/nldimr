library(R6)
library(ggplot2)
library(rgl)
source('R/colors.R')


Dimension_reduction <- R6Class(classname = "dimension reduction",

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    isDistance = FALSE,
                    group = NULL,
                    result = NULL,

                    plotResult = function(){
                      tryCatch({
                        if (is.null(self$result)){
                          stop(paste("No dimensionality reduction result found.
                                      Please run getResult() first."))
                        }

                        if (ncol(self$result) != 2 & ncol(self$result) != 3){
                          stop(paste("Unable to visualize", ncol(self$result),
                                     "dimension. The number of dimensions must be 2 or 3."))
                        } else if ( ncol(self$result) == 2 ) {

                          plt = private$plot2d()

                          print(plt)
                          return(plt)

                         } else {
                           private$plot3d()
                           }
                        },

                        # error occurs
                        error=function(e) {
                          message('Plot error:')
                          print(e)
                        }
                      )
                    },


                    fitPlot = function(){
                      if (self$isDistance){
                        full_dist = data.frame(as.vector(self$data),
                                               as.vector(dist(self$result)))
                      } else {
                        full_dist = data.frame(as.vector(dist(self$data)),
                                               as.vector(dist(self$result)))
                      }
                      colnames(full_dist) <- c('high_dim', 'low_dim')
                      plt = ggplot(data = full_dist, aes(x = high_dim, y = low_dim)) +
                        geom_point(size = 2) +
                        labs(x = 'High-dimensional Distance',
                             y = 'Low-dimensional Distance')

                      print(plt)
                      return(plt)

                    }

                  ),

                  # private attributes and methods
                  private = list(
                    plot2d = function(){
                      # separate plot with group and without group
                      if (is.null(self$group)) {

                        full_result = self$result
                        colnames(full_result) <- c('Dimension_1', 'Dimension_2')
                        plt = ggplot(data = full_result,
                                     mapping=aes(x = Dimension_1, y=Dimension_2)) +
                          geom_point(size=2) +
                          labs(x='Dimension 1', y='Dimension 2')

                      } else {
                        full_result = as.data.frame(cbind(self$result, self$group))
                        colnames(full_result) <- c('Dimension_1', 'Dimension_2', 'Group')
                        plt = ggplot(data = full_result,
                                     mapping=aes(x = Dimension_1, y=Dimension_2,
                                                 col = as.factor(Group))) +
                          geom_point(size=2) +
                          labs(x='Dimension 1', y='Dimension 2', color='Group')
                      }

                      return(plt)
                    },

                    plot3d = function(){
                      full_result = self$result
                      colnames(full_result) <- c('Dimension_1', 'Dimension_2',
                                                 'Dimension_3')

                      # separate plot with group and without group
                      if(is.null(self$group)){
                        plot3d(x = self$result$Dimension_1,
                               y = self$result$Dimension_2,
                               z = self$result$Dimension_3,
                               type = 'p',
                               col = '#000000',
                               size = 5,
                               xlab = 'Dimension 1',
                               ylab = 'Dimension 2',
                               zlab = 'Dimension 3')
                      } else {
                        plot3d(x = self$result$Dimension_1,
                               y = self$result$Dimension_2,
                               z = self$result$Dimension_3,
                               type = 'p',
                               col = get_colors(self$group),
                               size = 5,
                               xlab = 'Dimension 1',
                               ylab = 'Dimension 2',
                               zlab = 'Dimension 3')
                      }
                    }
                  ),


                  lock_class = TRUE
)
