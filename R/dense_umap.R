dense_umap <- R6Class(classname = "dense-UMAP",

                      inherit = Dimension_reduction,

                      # public attributes and methods
                      public = list(
                        data = NULL,
                        name = NULL,
                        group = NULL,
                        isDistance = NULL,
                        metric = NULL,
                        n_neighbors = NULL,
                        n_components = NULL,
                        min_dist = NULL,
                        spread = NULL,
                        dens_lambda = NULL,

                        #' densmap_iris = dense_umap$new(data=iris[!duplicated(iris),1:4], group=iris[!duplicated(iris),]$Species, n_neighbors=8, n_components = 2)
                        initialize = function(data,
                                              name = 'dense UMAP',
                                              group = NULL,
                                              isDistance = FALSE,
                                              metric = "euclidean",
                                              n_neighbors = 15,
                                              n_components = 2,
                                              min_dist = 0.1,
                                              spread = 1,
                                              dens_lambda = 0.1,
                                              sampling = NULL,
                                              print_result = FALSE,
                                              ...
                                              ) {
                          if(!is.null(sampling)){
                            sample_index <- sample(nrow(data), size=sampling)

                            self$data <- data[sample_index,]
                            self$group <- group[sample_index]
                          } else {
                            self$data <- data
                            self$group <- group
                          }

                          self$name <- name
                          self$isDistance <- isDistance
                          self$metric <- metric
                          self$n_neighbors <- n_neighbors
                          self$n_components <- n_components
                          self$min_dist <- min_dist
                          self$spread <- spread
                          self$dens_lambda <- dens_lambda

                          private$result <- self$get_result(print_result=FALSE, ...)
                        },

                        get_result = function(print_result=FALSE, ...){
                          tryCatch({
                            if(is.null(private$result)){
                              private$result <- uwot::umap2(X = data.frame(self$data),
                                                            n_neighbors = self$n_neighbors,
                                                            n_components = self$n_components,
                                                            metric = self$metric,
                                                            min_dist = self$min_dist,
                                                            spread = self$spread,
                                                            dens_scale = self$dens_lambda,
                                                            ...
                                                            )
                            }

                            if (print_result) {
                              return(private$result)
                            } else {
                              return(invisible(private$result))
                            }
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
                        plot_VW = function(sampling = NULL,
                                           save=FALSE, filename=NULL,
                                           width=NA, height=NA,
                                           units=c("in", "cm", "mm", "px"),
                                           display_legend=FALSE){
                          vw_data <- data.frame(self$get_V(), self$get_W())

                          if(!is.null(sampling)){
                            vw_data <- vw_data[sample(nrow(vw_data), size = sampling), ]
                          }

                          colnames(vw_data) <- c('v_probs', 'w_probs')

                          plt <- ggplot(vw_data, aes(v_probs, w_probs)) +
                            geom_point(alpha=0.5) +
                            labs(x='Smooth distance normalization\n(High dimensionality)',
                                 y='Weight representation\n(Low dimentionality)')

                          if(!display_legend) plt <- plt + theme(legend.position="none")
                          print(plt)

                          if(save){
                            ggsave(filename = filename,
                                   plot=plt,
                                   width=width,
                                   height=height,
                                   units=units)
                          }
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
                          # nn <- FNN::get.knn(self$data, k = self$n_neighbors)
                          # distances <- nn$nn.dist
                          # indices <- nn$nn.index
                          #
                          # rho <- apply(distances, 1, function(row) {
                          #   pos_r <- row[row > 0]
                          #   if (length(pos_r) == 0) 0 else min(pos_r)
                          # })
                          #
                          # find_sigma <- function(dist_row, rho_i, target = log2(self$n_neighbors)) {
                          #   sigma <- 1
                          #   sigma_min <- 0
                          #   sigma_max <- Inf
                          #
                          #   for (iter in 1:50) {
                          #     val <- sum(exp(-pmax(dist_row - rho_i,0) / sigma))
                          #
                          #     if (abs(val - target) < 1e-5) break
                          #
                          #     if (val > target) {
                          #       sigma_max <- sigma
                          #       sigma <- if (is.infinite(sigma_min)) sigma / 2 else (sigma + sigma_min)/2
                          #     } else {
                          #       sigma_min <- sigma
                          #       sigma <- if (is.infinite(sigma_max)) sigma * 2 else (sigma + sigma_max)/2
                          #     }
                          #   }
                          #   return(sigma)
                          # }
                          #
                          # sigma <- sapply(1:nrow(distances), function(i) {
                          #   find_sigma(distances[i, ], rho[i])
                          # })
                          #
                          # V_condition <- matrix(0, nrow(distances), ncol(distances))
                          #
                          # for (i in 1:nrow(distances)) {
                          #   V_condition[i, ] <- exp(-pmax(distances[i, ] - rho[i], 0) / sigma[i])
                          # }
                          #
                          # n <- nrow(self$data)
                          # V <- matrix(0, n, n)
                          #
                          # for (i in 1:n) {
                          #   for (j in 1:self$n_neighbors) {
                          #     j_indice <- indices[i, j]
                          #     V[i, j_indice] <- V_condition[i, j]
                          #   }
                          # }
                          #

                          method <- 'fnn'
                          n_trees <- 50
                          search_k <- 2*self$n_neighbors*n_trees
                          nn_args <- list()
                          tmpdir <- tempdir()
                          n_threads <- NULL
                          n_build_threads <- NULL
                          grain_size <- 1
                          ret_model <- FALSE
                          sparse_is_distance <- TRUE
                          verbose <- FALSE

                          set_op_mix_ratio = 1.0
                          local_connectivity = 1.0
                          bandwidth = 1.0
                          ret_sigma = 'sigma'

                          nn <- uwot:::find_nn(
                            self$data,
                            self$n_neighbors,
                            # method = method,
                            metric = self$metric,
                            # n_trees = n_trees,
                            # search_k = 2 * n_neighbors * n_trees,
                            # nn_args = nn_args,
                            # tmpdir = tmpdir,
                            # n_threads = n_threads,
                            # grain_size = grain_size,
                            # ret_index = ret_model,
                            # sparse_is_distance = sparse_is_distance,
                            verbose = verbose
                          )

                          res <- uwot:::fuzzy_simplicial_set(
                            nn = nn,
                            set_op_mix_ratio = set_op_mix_ratio,
                            local_connectivity = local_connectivity,
                            # bandwidth = bandwidth,
                            # ret_sigma = ret_sigma,
                            # n_threads = n_threads,
                            # grain_size = grain_size,
                            verbose = verbose
                          )

                          V <- as.matrix(res)
                          diag(V) <- 0
                          return(c(V[upper.tri(V)]))
                        },

                        ##############################################################
                        compute_W = function(){
                          if(is.null(private$result)) stop('Result has not been calculated. Please run get_Result() first.')

                          ab <- uwot:::find_ab_params(spread = self$spread,
                                                      min_dist = self$min_dist)

                          dist_sq <- as.matrix(dist(private$result, method=self$metric))^2

                          q_val <- 1/(1 + ab['a']*(dist_sq)^(2*ab['b']))
                          q_val <- c(q_val[upper.tri(q_val)])

                          return(q_val)
                        }

                      )
)
