library(R6)
library(ggplot2)
library(plotly)
# source('R/colors.R')


Dimension_reduction <- R6Class(classname = "dimension reduction",

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    isDistance = FALSE,
                    group = NULL,

                    ############################################################
                    plotResult = function(){
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                      Please run getResult() first."))
                        }

                        if (ncol(private$result) != 2 & ncol(private$result) != 3){
                          stop(paste("Unable to visualize", ncol(private$result),
                                     "dimension. The number of dimensions must be 2 or 3."))
                        }

                        if ( ncol(private$result) == 2 ) {
                          full_data <- data.frame(private$result)
                          cols <- c('dim_1', 'dim_2')

                          if(!is.null(self$group)){
                            full_data <- cbind(full_data, self$group)
                            cols <- append(cols, 'group')
                          }

                          colnames(full_data) <- cols

                          plt <- ggplot(data = full_data) +
                            geom_point(size = 2) +
                            aes(x=dim_1, y=dim_2) +
                            labs(x='Dimension 1', y='Dimension 2')

                          if(!is.null(self$group)){
                            plt <- plt +
                              aes(col = as.factor(group)) +
                              labs(col = 'Groups')
                          }

                          plt <- plt + coord_fixed()

                          print(plt)
                          return(plt)

                         }

                        if ( ncol(private$result) == 3 ) {
                           private$plot3d(private$result, as.factor(self$group))
                          }
                        },

                        # error occurs
                        error = function(e) {
                          message('Plot error:')
                          print(e)
                        }
                      )
                    },

                    ############################################################
                    fitPlot = function(){
                      if (self$isDistance){
                        full_dist <- data.frame(as.vector(self$data),
                                                as.vector(dist(private$result)))
                      } else {
                        full_dist <- data.frame(as.vector(dist(self$data)),
                                                as.vector(dist(private$result)))
                      }
                      colnames(full_dist) <- c('high_dim', 'low_dim')
                      plt <- ggplot(data = full_dist, aes(x = high_dim, y = low_dim)) +
                        geom_point(size = 2) +
                        labs(x = 'High-dimensional Distance',
                             y = 'Low-dimensional Distance') +
                        coord_fixed()

                      print(plt)
                      return(plt)

                    },

                    ############################################################
                    getJaccardSimilarity = function(k=5, method='euclidean'){
                      tryCatch({
                        if(is.null(k) | k>=nrow(self$data)){
                          stop("Invalid number of neighbors. The number of neighbors must be less than the number of observations.")
                        }

                        if(self$isDistance){
                          high.dist <- self$data
                        } else {
                          high.dist <- as.matrix(dist(self$data, method = method))
                        }

                        low.dist <- as.matrix(dist(private$result, method = method))

                        jaccard_sim <- c()

                        for (row in 1:nrow(self$data)){
                          jaccard_sim <- append(jaccard_sim,
                                               private$jacc(as.vector(order(high.dist[row,])[2:(k+1)]),
                                                            as.vector(order(low.dist[row,])[2:(k+1)])))
                        }

                        return(jaccard_sim)

                      },
                      error = function(e) {
                        message('Jaccard Similarity error:')
                        print(e)
                      })
                    },

                    ############################################################
                    plotJaccardSimilarity = function(k=5, method='euclidean') {
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                      Please run getResult() first."))
                        }

                        if (ncol(private$result) != 2 & ncol(private$result) != 3){
                          stop(paste("Unable to visualize Jaccard Similarity in", ncol(private$result),
                                     "dimension. The number of dimensions must be 2 or 3."))
                        }


                        jaccard <- self$getJaccardSimilarity(k, method)
                        full_data <- data.frame(private$result, jaccard)

                        if ( ncol(private$result) == 2 ) {
                          cols <- c('dim_1', 'dim_2', 'jaccard')
                          colnames(full_data) <- cols

                          plt <- ggplot(data = full_data) +
                            geom_point(size = 2) +
                            aes(x=dim_1, y=dim_2, col=jaccard) +
                            labs(x='Dimension 1', y='Dimension 2', col='Jaccard Similarity') +
                            scale_color_gradient2(low='darkred', mid='white', high='darkblue', midpoint = 0.5) +
                            coord_fixed()

                          print(plt)
                          return(plt)

                        }

                        if ( ncol(private$result) == 3) {
                          cols <- c('dim_1', 'dim_2', 'dim_3')
                          colnames(full_data) <- cols
                          private$plot3d(full_data, jaccard)
                        }

                      },error = function(e) {
                        message('Jaccard Similarity error:')
                        print(e)
                      })
                    },

                    ############################################################
                    plotJaccardPerNeighbor = function(max_k, method='euclidean'){
                      result <- data.frame()

                      if (max_k >= nrow(self$data)){
                        message('The number of the maximum nearest neighbors cannot exceed the number of observations. Setting the maximum number of nearest neighbors to the number of observations - 1')
                        max_k <- nrow(self$data) - 1
                      }

                      for(k in c(1:max_k)){
                        result <- rbind(result, data.frame(rep(k, nrow(self$data)),
                                                           mean(self$getJaccardSimilarity(k, method))))
                      }

                      colnames(result) <- c('k', 'jaccard_similarity')

                      plt <- ggplot(result, aes(x=k, y=jaccard_similarity)) +
                        geom_line() +
                        labs(x = 'k', y = 'Average Jaccard Similarity')

                      print(plt)
                      return(plt)
                    }

                  ),




                  # private attributes and methods
                  private = list(
                    result = NULL,

                    # plot2d <- function(result, ...){
                    #
                    #   full_data <- copy(result)
                    #   cols <- c('Dimension_1', 'Dimension_2')
                    #
                    #   args <- list(...)
                    #   nms <- names(args)
                    #
                    #   for (i in seq_along(args)){
                    #     if (!is.null(nms) && nms[i] != ""){
                    #       cols <- append(cols, nms[i])
                    #       full_data <- cbind(full_data, args[[i]])
                    #     }
                    #   }
                    #
                    #
                    #
                    #   # separate plot with group and without group
                    #   # if (is.null(self$group)) {
                    #   #
                    #   #   full_result = private$result
                    #   #   colnames(full_result) <- c('Dimension_1', 'Dimension_2')
                    #   #   plt = ggplot(data = full_result,
                    #   #                mapping=aes(x = Dimension_1, y=Dimension_2)) +
                    #   #     geom_point(size=2) +
                    #   #     labs(x='Dimension 1', y='Dimension 2')
                    #   #
                    #   # } else {
                    #   #   full_result = as.data.frame(cbind(private$result, self$group))
                    #   #   colnames(full_result) <- c('Dimension_1', 'Dimension_2', 'Group')
                    #   #   plt = ggplot(data = full_result,
                    #   #                mapping=aes(x = Dimension_1, y=Dimension_2,
                    #   #                            col = as.factor(Group))) +
                    #   #     geom_point(size=2) +
                    #   #     labs(x='Dimension 1', y='Dimension 2', color='Group')
                    #   # }
                    #
                    #   return(plt)
                    # },

                    ############################################################
                    plot3d = function(data, group = NULL){
                      full_data <- data.frame(data)
                      cols <- c('dim_1', 'dim_2', 'dim_3')

                      if(!is.null(group)){
                        full_data <- cbind(full_data, self$group)
                        cols <- append(cols, 'group')
                        colnames(full_data) <- cols

                        fig <- plot_ly(full_data,
                                       x = ~dim_1, y = ~dim_2, z = ~dim_3,
                                       color = ~group)
                      } else {
                        colnames(full_data) <- cols

                        fig <- plot_ly(full_data,
                                       x = ~dim_1, y = ~dim_2, z = ~dim_3,
                                       colors = c('#000'))
                      }

                      fig <- fig %>% add_markers()
                      fig <- fig %>% layout(scene = list(xaxis = list(title = 'Dimension 1'),
                                                         yaxis = list(title = 'Dimension 2'),
                                                         zaxis = list(title = 'Dimension 3')))

                      return(fig)

                    },

                    ############################################################
                    jacc = function(x, y) {
                      intersection <- length(intersect(x, y))
                      union <- length(x) + length(y) - intersection
                      return (intersection/union)
                    }


                  ),


                  lock_class = TRUE
)
