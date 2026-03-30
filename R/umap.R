
#' umap_iris = R_umap$new(data=iris[,1:4], group=iris$Species, n_neighbors=5)

R_umap <- R6Class(classname = "umap",

                  inherit = Dimension_reduction,

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    group = NULL,
                    isDistance = FALSE,
                    method = NULL,
                    n_neighbors = NULL,
                    n_components = NULL,
                    metric = NULL,
                    n_epochs = NULL,
                    input = NULL,
                    min_dist = NULL,
                    bandwidth = NULL,
                    alpha = NULL,
                    gamma = NULL,


                    initialize = function(data, group = NULL,
                                          method = "naive",
                                          n_neighbors = 15, n_components = 2,
                                          metric = 'euclidean', n_epochs = 200,
                                          input = 'data', min_dist = 0.1,
                                          bandwidth = 1, alpha = 1, gamma = 1
                                          ) {
                      self$data <- data
                      self$group <- group
                      self$method <- method
                      self$n_neighbors <- n_neighbors
                      self$n_components <- n_components
                      self$metric <- metric
                      self$n_epochs <- n_epochs
                      self$input <- input
                      self$min_dist <- min_dist
                      self$bandwidth <- bandwidth
                      self$alpha <- alpha
                      self$gamma <- gamma
                    },

                    getResult = function(...){
                      tryCatch({
                        result = umap(self$data, config = umap.defaults,
                                      method = self$method,
                                      n_neightbors = self$n_neightbors,
                                      n_components = self$n_components,
                                      input = self$input,...)

                        private$result = as.data.frame(result$layout)

                        return(private$result)
                      }
                      ,
                      # error occurs
                      error=function(e) {
                        message('UMAP running error:')
                        print(e)
                      },

                      # warning occurs
                      warning = function(w) {
                        message('UMAP running warning:')
                        print(w)
                      }
                      )
                    }

                  )

)
