R_tsne <- R6Class(classname = "t-SNE",

                  inherit = Dimension_reduction,

                  # public attributes and methods
                  public = list(
                    data = NULL,
                    group = NULL,
                    isDistance = NULL,
                    dims = NULL,
                    perplexity = NULL,
                    theta = NULL,


                  #' Initialize function for R_tsne class
                  #'
                  #' @param data (matrix) The matrix of similarities/distances between data points.
                  #' @param group (matrix) The groups of data points. Used for visualization. Can be NULL if it is not known
                  #' @param dims (integer) The dimension of the resulting embedding.
                  #' @param perplexity (numeric) Perplexity parameter.
                  #' @param theta (numeric) Speed/accuracy trade-off (default: 0.5).
                  #'
                  #' @returns (matrix) The k dimensional embedding result of t-SNE.
                  #' @export
                  #'
                  #' @examples
                  #' tsne_iris = R_tsne$new(data=iris[!duplicated(iris),1:4], group=iris$Species, dims=2, perplexity=20)
                   initialize = function(data, group = NULL, isDistance = FALSE,
                                         dims = 2, perplexity = 30, theta = 0.5) {
                     self$data <- data
                     self$group <- group
                     self$isDistance <- isDistance
                     self$dims <- dims
                     self$perplexity <- perplexity
                     self$theta <- theta
                   },

                   getResult = function(...){
                     tryCatch({
                       results <- Rtsne(self$data,
                                        dims = self$dims,
                                        perplexity = self$perplexity,
                                        theta = self$theta,
                                        is_distance = self$isDistance,
                                        ...)
                       private$result <- as.data.frame(results$Y)
                       return(private$result)
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
                   plot_PQ = function(){
                     pq_data <- data.frame(self$get_P(), self$get_Q())
                     colnames(pq_data) <- c('p_probs', 'q_probs')

                     plt <- ggplot(pq_data, aes(p_probs, q_probs)) +
                       geom_point() +
                       labs(x='P probabilities', y='Q probabilities') +
                       coord_fixed()

                     print(plt)
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

                        log_perp <- log(self$perplexity)

                        for (iter in 1:50) {
                          res <- Hbeta(Di, beta)
                          Hdiff <- res$H - log_perp

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
                      return(P)
                      },

                  ##############################################################
                    compute_Q = function(){
                      n <- nrow(private$result)
                      low_dist <- as.matrix(dist(private$result))^2

                      Q <- 1 / (1 + low_dist)
                      diag(Q) <- 0
                      Q <- Q / sum(Q)
                      return(Q)
                    }


                  )
)
