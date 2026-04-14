
# umap_iris = R_umap$new(data=iris[!duplicated(iris),1:4], isDistance=FALSE, group=iris[!duplicated(iris),]$Species, n_neighbors=8, n_components = 2)

R_umap <- R6Class(classname = "umap",

                  inherit = Dimension_reduction,

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    group = NULL,
                    isDistance = NULL,
                    metric = NULL,
                    n_neighbors = NULL,
                    n_components = NULL,
                    min_dist = NULL,
                    spread = NULL,



                    ############################################################
                    initialize = function(data,
                                          isDistance = FALSE,
                                          group = NULL,
                                          sampling = NULL,
                                          metric = "euclidean",
                                          n_neighbors = 15,
                                          n_components = 2,
                                          min_dist = 0.1,
                                          spread = 1
                                          ) {

                      if(!is.null(sampling)){
                        sample_index <- sample(nrow(data), size=sampling)

                        self$data <- data[sample_index,]
                        self$group <- group[sample_index,]
                      } else {
                        self$data <- data
                        self$group <- group
                      }

                      self$isDistance <- isDistance
                      self$metric <- metric
                      self$n_neighbors <- n_neighbors
                      self$n_components <- n_components
                      self$min_dist <- min_dist
                      self$spread <- spread
                    },

                    ############################################################
                    get_Result = function(...){
                      tryCatch({
                        result = umap2(X = self$data,
                                       n_neighbors = self$n_neighbors,
                                       n_components = self$n_components,
                                       metric = self$metric,
                                       min_dist = self$min_dist,
                                       spread = self$spread,
                                       ret_extra = c('fgraph'),
                                       ...)

                        private$result <- result$embedding

                        private$V <- result$fgraph

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
                    },

                    ##############################################################
                    get_V = function(){
                      if(is.null(private$V)) stop('Result has not been calculated. Please run get_Result() first.')
                      v <- as.matrix(private$V)
                      diag(v) <- 0
                      return(c(v[upper.tri(v)]))
                    },

                    ##############################################################
                    get_W = function(){
                      if(is.null(private$W)) private$W <- private$compute_W()
                      return(private$W)
                    },

                    ##############################################################
                    plot_VW = function(sampling = NULL){
                      vw_data <- data.frame(self$get_V(), self$get_W())

                      if(!is.null(sampling)){
                        vw_data <- vw_data[sample(nrow(vw_data), size = sampling), ]
                      }

                      colnames(vw_data) <- c('v_probs', 'w_probs')

                      plt <- ggplot(vw_data, aes(v_probs, w_probs)) +
                        geom_point(alpha=0.5) +
                        labs(x='Smooth distance normalization\n(High dimensionality)',
                             y='Weight representation\n(Low dimentionality)')

                      print(plt)
                      return(plt)
                    }

                  ),


                  # private attributes and methods
                  private = list(
                    result = NULL,
                    V = NULL,
                    W = NULL,

                  ##############################################################
                    compute_W = function(){
                      if(is.null(private$V)) stop('Result has not been calculated. Please run get_Result() first.')


                      ab <- uwot:::find_ab_params(spread = self$spread,
                                                  min_dist = self$min_dist)

                      dist_sq <- as.matrix(dist(private$result, method=self$metric))^2

                      q_val <- 1/(1 + ab['a']*(dist_sq)^(2*ab['b']))
                      q_val <- c(q_val[upper.tri(q_val)])

                      return(q_val)
                    }
                  )

)
