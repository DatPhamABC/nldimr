compare_trustworthiness <- function(methods = c(), k = NULL, plot = FALSE, print_result = FALSE) {

  qm_results <- list()
  index <- 1

  for (method in methods) {
    for (k_i in k) {
      qm_results[[index]] <- list(
        Method = method$name,
        k = k_i,
        trustworthiness = method$trustworthiness(k_i)
      )
      index <- index + 1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))

  qm_results$k <- as.numeric(qm_results$k)
  qm_results$trustworthiness <- as.numeric(qm_results$trustworthiness)
  qm_results$Method <- as.character(qm_results$Method)

  if (print_result){
    print(qm_results)
  }

  if (plot){
    plt <- ggplot(data = qm_results,
                  aes(x=k, y=trustworthiness, group=Method, color=Method)) +
        geom_point() +
        geom_line() +
        labs(x='k', y='Trustworthiness')
    print(plt)
  }
}

################################################################################

compare_continuity <- function(methods = c(), k = NULL, plot = FALSE, print_result = FALSE) {

  qm_results <- list()
  index <- 1

  for (method in methods) {
    for (k_i in k) {
      qm_results[[index]] <- list(
        Method = method$name,
        k = k_i,
        continuity = method$continuity(k_i)
      )
      index <- index + 1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))

  qm_results$k <- as.numeric(qm_results$k)
  qm_results$continuity <- as.numeric(qm_results$continuity)
  qm_results$Method <- as.character(qm_results$Method)

  if (print_result){
    print(qm_results)
  }

  if (plot){
    plt <- ggplot(data = qm_results,
                  aes(x=k, y=continuity, group=Method, color=Method)) +
      geom_point() +
      geom_line() +
      labs(x='k', y='Continuity')
    print(plt)
  }
}

################################################################################
compare_kNN_Jaccard_similarity <- function(methods = c(), k = NULL,
                                           plot = FALSE, print_result = FALSE) {

  # Initialized variable
  qm_results <- list()
  index <- 1
  lowest_max_row <- nrow(methods[1]$data)

  # Adjusting the maximum k
  for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
  print(lowest_max_row)
  if (tail(k, 1) >= lowest_max_row){
    message('The number of the maximum nearest neighbors cannot exceed the number of observations.
            Setting the maximum number of nearest neighbors to the number of observations - 1')
    k <- c(k[1]:(lowest_max_row - 1))
  }

  for (method in methods) {
    for(k_i in k){
      qm_results[[index]] <-list(
        Method = method$name,
        k = k_i,
        average_jacc = mean(method$get_Jaccard_similarity(k, method))
      )
      index <- index+1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))

  qm_results$k <- as.numeric(qm_results$k)
  qm_results$average_jacc <- as.numeric(qm_results$average_jacc)
  qm_results$Method <- as.character(qm_results$Method)

  if (print_result){
    print(qm_results)
  }

  if (plot){
    plt <- ggplot(data = qm_results,
                  aes(x=k, y=average_jacc, group=Method, color=Method)) +
      geom_point() +
      geom_line() +
      labs(x='k', y='Average Jaccard Similarity')
    print(plt)
  }
}
