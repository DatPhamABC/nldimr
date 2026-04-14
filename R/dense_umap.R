dense_umap <- R6Class(classname = "dense-UMAP",

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
                        dens_frac = NULL,
                        dens_lambda = NULL,

                        #' densmap_iris = dense_umap$new(data=iris[!duplicated(iris),1:4], group=iris[!duplicated(iris),]$Species, n_neighbors=8, n_components = 2)
                        initialize = function(data,
                                              group = NULL,
                                              isDistance = FALSE,
                                              sampling = NULL,
                                              metric = "euclidean",
                                              n_neighbors = 15,
                                              n_components = 2,
                                              min_dist = 0.1,
                                              spread = 1,
                                              dens_frac = 0.3,
                                              dens_lambda = 0.1
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
                          self$dens_frac <- dens_frac
                          self$dens_lambda <- dens_lambda
                        },

                        get_Result = function(...){
                          tryCatch({
                            private$result <- densmap(x = data.frame(self$data),
                                                      n_neighbors = self$n_neighbors,
                                                      n_components = self$n_components,
                                                      metric = self$metric,
                                                      min_dist = self$min_dist,
                                                      spread = self$spread,
                                                      dens_frac = self$dens_frac,
                                                      dens_lambda = self$dens_lambda,
                                                      ...
                                                      )
                            return(private$result)
                          }
                          ,
                          # error occurs
                          error=function(e) {
                            message('Dense UMAP class error:')
                            print(e)
                          },
                          # warning occurs
                          warning=function(w) {
                            message('Dense UMAP class waring:')
                            print(w)
                          }
                          )
                        },

                        ##############################################################
                        get_V = function(){
                          if(is.null(private$V)) private$V <- private$compute_V()
                          return(private$V)
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
                        compute_V = function(){

                        },

                        ##############################################################
                        compute_W = function(){

                        }

                      )
)
