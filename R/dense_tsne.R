R_dense_tsne <- R6Class(classname = "dense-tSNE",

                  inherit = Dimension_reduction,

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    name = NULL,
                    group = NULL,
                    isDistance = NULL,
                    dims = NULL,
                    perplexity = NULL,
                    theta = NULL,
                    initial_dims = NULL,
                    momentum = NULL,
                    dens_lambda = NULL,

                    #' densne_iris = dense_tsne$new(data=iris[!duplicated(iris),1:4], group=iris[!duplicated(iris),]$Species, dims=2, perplexity=20)
                    initialize = function(data,
                                          name = 'dense t-SNE',
                                          group = NULL,
                                          isDistance = FALSE,
                                          dims = 2,
                                          perplexity = 50,
                                          theta = 0.5,
                                          dens_lambda = 0.1,
                                          sampling = NULL,
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
                      self$dims <- dims
                      self$perplexity <- perplexity
                      self$theta <- theta
                      self$dens_lambda <- dens_lambda

                      private$result <- self$get_result(print_result=FALSE, ...)
                    },

                    get_result = function(print_result=FALSE, ...){
                      tryCatch({
                        # if(is.null(private$result)){
                          private$result <- densne(X = self$data,
                                            dims = self$dims,
                                            perplexity = self$perplexity,
                                            theta = self$theta,
                                            dens_lambda = self$dens_lambda,
                                            ...
                                            )
                        # }

                        if (print_result) {
                          return(private$result)
                        } else {
                          return(invisible(private$result))
                        }
                      }
                      ,
                      # error occurs
                      error=function(e) {
                        message('Dense t-SNE class error:')
                        print(e)
                      },
                      # warning occurs
                      warning=function(w) {
                        message('Dense t-SNE class waring:')
                        print(w)
                      }
                      )
                    },

                  ##############################################################
                    get_P = function(){
                      if(is.null(private$P)) private$P <- private$compute_P()
                      return(private$P)
                    },

                  ##############################################################
                    get_Q = function(){
                      if(is.null(private$Q)) private$Q <- private$compute_Q()
                      return(private$Q)
                    },

                  ##############################################################
                    plot_PQ = function(sampling = NULL,
                                       save=FALSE, filename=NULL,
                                       width=NA, height=NA,
                                       units=c("in", "cm", "mm", "px"),
                                       display_legend=FALSE){
                      pq_data <- data.frame(self$get_P(), self$get_Q())

                      if(!is.null(sampling)){
                        pq_data <- pq_data[sample(nrow(pq_data), size = sampling), ]
                      }

                      colnames(pq_data) <- c('p_probs', 'q_probs')

                      plt <- ggplot(pq_data, aes(p_probs, q_probs)) +
                        geom_point(alpha=0.5) +
                        labs(x='P probabilities', y='Q probabilities')

                      if(!display_legend) plt <- plt + theme(legend.position="none")
                      # print(plt)

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
                    P = NULL,
                    Q = NULL,

                  ##############################################################
                    compute_P = function(){
                      if (!self$isDistance) D = as.matrix(dist(self$data))^2
                      else D = as.matrix(self$data)^2

                      n <- nrow(D)
                      P <- matrix(0, n, n)

                      for (i in 1:n) {
                        beta <- 1
                        Di <- D[i, -i]

                        # Calculate H_beta for binary search
                        Hbeta <- function(D, beta){
                          P <- exp(-Di * beta)
                          sumP <- sum(P)
                          if (sumP == 0){
                            H <- 0
                            P <- D * 0
                          } else {
                            H <- log(sumP) + beta * sum(D %*% P) / sumP
                            P <- P/sumP
                          }
                          return(list(H = H, P = P))
                        }

                        # Binary search for beta
                        for (iter in 1:50) {
                          res <- Hbeta(Di, beta)
                          Hdiff <- res$H - log(self$perplexity)

                          if (abs(Hdiff) < 1e-5) break
                          if (Hdiff > 0) {
                            beta <- beta * 2
                          } else {
                            beta <- beta / 2
                          }
                        }

                        P[i, -i] <- res$P
                      }

                      P <- (P + t(P)) / (2 * n)

                      P <- P[upper.tri(P)]
                      return(c(P))
                    },

                  ##############################################################
                    compute_Q = function(){
                      if(is.null(private$result)) stop('Result have not been calculated. Please run get_Result() first.')

                      n <- nrow(private$result)
                      low_dist <- as.matrix(dist(private$result))^2

                      Q <- 1 / (1 + low_dist)
                      diag(Q) <- 0
                      Q <- Q / sum(Q)

                      Q <- Q[upper.tri(Q)]
                      return(c(Q))
                    }

                  )
)
