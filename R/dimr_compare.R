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
