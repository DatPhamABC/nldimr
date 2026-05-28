Dimension_reduction <- R6Class(classname = "dimension reduction",

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    isDistance = FALSE,
                    group = NULL,

                  ##############################################################
                    plot_result = function(save=FALSE, filename=NULL,
                                           width=NA, height=NA,
                                           units=c("in", "cm", "mm", "px"),
                                           ratio=1,
                                           display_legend=FALSE){
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                      Please run get_Result() first."))
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
                            geom_point(size = 2, alpha=0.5) +
                            aes(x=dim_1, y=dim_2) +
                            labs(x='Dimension 1', y='Dimension 2') +
                            coord_fixed(ratio=ratio) +
                            scale_color_brewer(palette="Paired")

                          if(!display_legend) plt <- plt + theme(legend.position="none")

                          if(!is.null(self$group)){
                            plt <- plt +
                              aes(col = as.factor(group)) +
                              labs(col = 'Groups')
                          }

                          print(plt)

                          # if (save){
                          #   ggsave(filename=filename,
                          #          plot=egg::set_panel_size(p=plt,
                          #                                   width = grid::unit(width, units = units),
                          #                                   height = grid::unit(height, units = units))
                          #   )
                          # }

                          if(save){
                            ggsave(filename = filename,
                                   plot=plt,
                                   width=width,
                                   height=height,
                                   units=units)
                          }

                          return(plt)

                         }

                        if ( ncol(private$result) == 3 ) {
                           private$plot_3d(private$result, as.factor(self$group))
                          }
                        },

                        # error occurs
                        error = function(e) {
                          message('Plot error:')
                          print(e)
                        }
                      )
                    },

                  ##############################################################
                    shepard_diagram = function(sampling = NULL,
                                               save=FALSE, filename=NULL,
                                               width=NA, height=NA,
                                               units=c("in", "cm", "mm", "px")){
                      if (self$isDistance){
                        full_dist <- data.frame(as.vector(self$data),
                                                as.vector(dist(private$result)))
                      } else {
                        full_dist <- data.frame(as.vector(dist(self$data)),
                                                as.vector(dist(private$result)))
                      }
                      colnames(full_dist) <- c('high_dim', 'low_dim')

                      if(!is.null(sampling)){
                        full_dist <- full_dist[sample(nrow(full_dist), size = sampling), ]
                      }

                      plt <- ggplot(data = full_dist, aes(x = high_dim, y = low_dim)) +
                        geom_point(size = 2, alpha = 0.5) +
                        labs(x = 'High-dimensional Distances',
                             y = 'Low-dimensional Distances')


                      if(save){
                        ggsave(filename = filename,
                               plot=plt,
                               width=width,
                               height=height,
                               units=units)
                      }

                      print(plt)
                      return(plt)

                    },

                  ##############################################################
                    get_Jaccard_similarity = function(k=5, method='euclidean'){
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

                  ##############################################################
                    plot_Jaccard_similarity = function(k=5, method='euclidean',
                                                       save=FALSE, filename=NULL,
                                                       width=NA, height=NA,
                                                       units=c("in", "cm", "mm", "px"),
                                                       ratio=1) {
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                      Please run get_Result() first."))
                        }

                        if (ncol(private$result) != 2 & ncol(private$result) != 3){
                          stop(paste("Unable to visualize Jaccard Similarity in", ncol(private$result),
                                     "dimension. The number of dimensions must be 2 or 3."))
                        }


                        jaccard <- self$get_Jaccard_similarity(k, method)
                        full_data <- data.frame(private$result, jaccard)

                        if ( ncol(private$result) == 2 ) {
                          cols <- c('dim_1', 'dim_2', 'jaccard')
                          colnames(full_data) <- cols

                          plt <- ggplot(data = full_data) +
                            geom_point(size = 2, alpha=0.6) +
                            aes(x=dim_1, y=dim_2, col=jaccard) +
                            labs(x='Dimension 1', y='Dimension 2', col='Jaccard Similarity') +
                            scale_color_gradient(low='red', high='green')

                          if(ratio){
                            plt <- plt + coord_fixed(ratio=ratio)
                          }

                          if (save){
                            # ggsave(filename=filename,
                            #        plot=egg::set_panel_size(p=plt,
                            #                                 width = grid::unit(width, units = units),
                            #                                 height = grid::unit(height, units = units))
                            # )
                            ggsave(filename = filename,
                                   plot = plt,
                                   width = width,
                                   height = height,
                                   units = units)
                          }

                          print(plt)
                          return(plt)

                        }

                        if ( ncol(private$result) == 3) {
                          cols <- c('dim_1', 'dim_2', 'dim_3')
                          colnames(full_data) <- cols
                          private$plot_3d(full_data, jaccard)
                        }

                      },error = function(e) {
                        message('Jaccard Similarity error:')
                        print(e)
                      })
                    },

                  # ############################################################
                  #   plot_Jaccard_per_neighbor = function(low_k, high_k, method='euclidean',
                  #                                        save=FALSE, filename=NULL,
                  #                                        width=NA, height=NA,
                  #                                        units=c("in", "cm", "mm", "px")
                  #                                        ){
                  #     result <- data.frame()
                  #
                  #     if (high_k >= nrow(self$data)){
                  #       message('The number of the maximum nearest neighbors cannot exceed the number of observations. Setting the maximum number of nearest neighbors to the number of observations - 1')
                  #       high_k <- nrow(self$data) - 1
                  #     }
                  #
                  #     for(k in c(low_k:high_k)){
                  #       result <- rbind(result, data.frame(rep(k, nrow(self$data)),
                  #                                          mean(self$get_Jaccard_similarity(k, method))))
                  #     }
                  #
                  #     colnames(result) <- c('k', 'jaccard_similarity')
                  #
                  #     plt <- ggplot(result, aes(x=k, y=jaccard_similarity)) +
                  #       geom_line() +
                  #       labs(x = 'k', y = 'Average Jaccard Similarity')
                  #
                  #     if (save){
                  #       ggsave(filename = filename,
                  #              plot = plt,
                  #              width = width,
                  #              height = height,
                  #              units = units)
                  #     }
                  #
                  #     print(plt)
                  #     return(plt)
                  #   },

                  ##############################################################
                    trustworthiness = function(k=5) {
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                        Please run get_Result() first."))
                        }
                        n <- nrow(self$data)

                        # Compute distances
                        dist_X <- as.matrix(dist(self$data))
                        dist_Y <- as.matrix(dist(private$result))

                        # Order distances
                        rank_X <- apply(dist_X, 1, function(row) rank(row, ties.method = "average"))
                        rank_X <- t(rank_X)

                        rank_Y <- apply(dist_Y, 1, function(row) rank(row, ties.method = "average"))
                        rank_Y <- t(rank_Y)

                        total_penalty <- 0

                        for (i in 1:n) {
                          # Get k nearest neighbors in low dimensional space
                          neighbors_Y <- order(rank_Y[i, ])[2:(k + 1)]

                          for (j in neighbors_Y) {
                            if (rank_X[i, j] > k) {
                              total_penalty <- total_penalty + (rank_X[i, j] - k)
                            }
                          }
                        }

                        # Normalization
                        normalization <- 2/(n*k * (2*n - 3*k - 1))

                        tw <- 1 - normalization * total_penalty

                        return(tw)
                      }, error = function(e) {
                          message('Trustworthiness error:')
                          print(e)
                      })
                    },

                  ##############################################################
                    continuity = function(k=5) {
                      tryCatch({
                        if (is.null(private$result)){
                          stop(paste("No dimensionality reduction result found.
                                          Please run get_Result() first."))
                        }
                        n <- nrow(self$data)

                        # Compute distances
                        dist_X <- as.matrix(dist(self$data))
                        dist_Y <- as.matrix(dist(private$result))

                        # order distances
                        rank_X <- apply(dist_X, 1, function(row) rank(row, ties.method = "average"))
                        rank_X <- t(rank_X)

                        rank_Y <- apply(dist_Y, 1, function(row) rank(row, ties.method = "average"))
                        rank_Y <- t(rank_Y)

                        total_penalty <- 0

                        for (i in 1:n) {
                          # k nearest neighbors in high dimensional space
                          neighbors_X <- order(rank_X[i, ])[2:(k + 1)]

                          for (j in neighbors_X) {
                            if (rank_Y[i, j] > k) {
                              total_penalty <- total_penalty + (rank_Y[i, j] - k)
                            }
                          }
                        }

                        # Normalization
                        normalization <- 2/(n*k * (2*n - 3*k - 1))

                        cont <- 1 - normalization * total_penalty

                        return(cont)
                      }, error = function(e) {
                        message('Continuity error:')
                        print(e)
                      })
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
                    plot_3d = function(data, group = NULL){
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
                                       colors = c('#00000020'))
                      }

                      print(data)
                      fig <- fig %>% add_markers()
                      fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'Dimension 1'),
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
