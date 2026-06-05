compare_trustworthiness <- function(methods = c(), list_k = NULL,
                                    plot = FALSE, print_result = FALSE,
                                    method_groups = NULL, sd = FALSE,
                                    filename=NULL,
                                    width=NA, height=NA,
                                    units=c("in", "cm", "mm", "px")
                                    ) {

  qm_results <- list()
  index <- 1

  if (is.null(methods) || length(methods) == 0) {
    stop("methods cannot be NULL or empty.")
  }

  if (is.null(list_k) || length(list_k) == 0) {
    stop("list_k cannot be NULL or empty.")
  }

  # Adjusting the maximum k
  for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
  if (tail(list_k, 1) >= lowest_max_row){
    message('The number of the maximum nearest neighbors cannot exceed the number of observations.
            Setting the maximum number of nearest neighbors to the number of observations - 1')
    list_k <- list_k[list_k<lowest_max_row]

    if (length(list_k) == 0) {
      stop("All values in list_k exceed the number of observations.
           Please provide smaller k values.")
    }
  }

  for (method in methods) {
    if (!is.null(method_groups) && method$name %in% names(method_groups)) hyperparam_group <- method_groups[[method$name]]
    else hyperparam_group <- method$name
    for (k_i in list_k) {
      qm_results[[index]] <- list(
        Method = method$name,
        Hyperparam_group = hyperparam_group,
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
  qm_results$Hyperparam_group  <- as.character(qm_results$Hyperparam_group)

  qm_avg <- aggregate(trustworthiness ~ Hyperparam_group + k, data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(trustworthiness ~ Hyperparam_group + k, data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'trustworthiness'] <- 'trustworthiness_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = c('Hyperparam_group', 'k'))

  if (print_result){
    print(qm_summary)
  }

  if (plot){
    plt <- ggplot(data = qm_summary, aes(x=k, y=trustworthiness,
                                         group=Hyperparam_group,
                                         color=Hyperparam_group)) +
      geom_line() +
      scale_color_brewer(palette="Set3") +
      labs(x='k', y='Trustworthiness')

    if(!is.null(method_groups) && !length(method_groups)==0 && sd==TRUE){
      plt <- plt +
        geom_ribbon(aes(ymin = trustworthiness - trustworthiness_sd,
                        ymax = trustworthiness + trustworthiness_sd,
                        fill = Hyperparam_group), alpha = 0.2, color = NA) +
        scale_fill_brewer(palette="Set3")
    }

    print(plt)

    if (!is.null(filename)){
      ggsave(filename = filename,
             plot = plt,
             width = width,
             height = height,
             units = units)
    }
  }

  return(list(results = qm_results, plot = plt))
}

################################################################################

compare_continuity <- function(methods = c(), list_k = NULL,
                               plot = FALSE, print_result = FALSE,
                               method_groups = NULL, sd = FALSE,
                               filename=NULL,
                               width=NA, height=NA,
                               units=c("in", "cm", "mm", "px")) {

  qm_results <- list()
  index <- 1

  if (is.null(methods) || length(methods) == 0) {
    stop("methods cannot be NULL or empty.")
  }

  if (is.null(list_k) || length(list_k) == 0) {
    stop("list_k cannot be NULL or empty.")
  }

  # Adjusting the maximum k
  for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
  if (tail(list_k, 1) >= lowest_max_row){
    message('The number of the maximum nearest neighbors cannot exceed the number of observations.
            Setting the maximum number of nearest neighbors to the number of observations - 1')
    list_k <- list_k[list_k<lowest_max_row]

    if (length(list_k) == 0) {
      stop("All values in list_k exceed the number of observations.
           Please provide smaller k values.")
    }
  }

  for (method in methods) {
    for (k_i in list_k) {
      if (!is.null(method_groups) && method$name %in% names(method_groups)) hyperparam_group <- method_groups[[method$name]]
      else hyperparam_group <- method$name
      qm_results[[index]] <- list(
        Method = method$name,
        Hyperparam_group = hyperparam_group,
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
  qm_results$Hyperparam_group  <- as.character(qm_results$Hyperparam_group)

  qm_avg <- aggregate(continuity ~ Hyperparam_group + k, data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(continuity ~ Hyperparam_group + k, data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'continuity'] <- 'continuity_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = c('Hyperparam_group', 'k'))

  if (print_result){
    print(qm_summary)
  }

  if (plot){
    plt <- ggplot(data = qm_summary, aes(x=k, y=continuity,
                                         group=Hyperparam_group,
                                         color=Hyperparam_group)) +
      geom_line() +
      scale_color_brewer(palette="Set3") +
      labs(x='k', y='Continuity')

    if(!is.null(method_groups) && !length(method_groups)==0 && sd==TRUE){
      plt <- plt +
        geom_ribbon(aes(ymin = continuity - continuity_sd,
                        ymax = continuity + continuity_sd,
                        fill = Hyperparam_group), alpha = 0.2, color = NA) +
        scale_fill_brewer(palette="Set3")
    }

    print(plt)

    if (!is.null(filename)){
      ggsave(filename = filename,
             plot = plt,
             width = width,
             height = height,
             units = units)
    }
  }

  return(list(results = qm_results, plot = plt))
}

################################################################################
compare_kNN_Jaccard_similarity <- function(methods = c(), list_k = NULL,
                                           plot = FALSE, print_result = FALSE,
                                           method_groups = NULL, sd = FALSE,
                                           filename=NULL,
                                           width=NA, height=NA,
                                           units=c("in", "cm", "mm", "px"),
                                           verbose=F) {

  # Initialized variable
  qm_results <- list()
  plt <- NULL
  index <- 1

  if (is.null(methods) || length(methods) == 0) {
    stop("methods cannot be NULL or empty.")
  }
  lowest_max_row <- nrow(methods[1]$data)

  if (is.null(list_k) || length(list_k) == 0) {
    stop("list_k cannot be NULL or empty.")
  }
  list_k <- sort(list_k, decreasing = F)

  # Adjusting the maximum k
  for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
  if (tail(list_k, 1) >= lowest_max_row){
    message('The number of the maximum nearest neighbors cannot exceed the number of observations.
            Setting the maximum number of nearest neighbors to the number of observations - 1')
    list_k <- list_k[list_k<lowest_max_row]

    if (length(list_k) == 0) {
      stop("All values in list_k exceed the number of observations.
           Please provide smaller k values.")
    }
  }

  for (method in methods) {
    if(verbose) print(paste('Calculating', method$name, sep=" "))
    for(k_i in list_k){
      if (!is.null(method_groups) && method$name %in% names(method_groups)) hyperparam_group <- method_groups[[method$name]]
      else hyperparam_group <- method$name
      jacc <- method$get_Jaccard_similarity(k_i)
      qm_results[[index]] <- list(
        Method = method$name,
        Hyperparam_group = hyperparam_group,
        k = k_i,
        mean_jacc = mean(jacc)
      )
      index <- index+1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))

  qm_results$k <- as.numeric(qm_results$k)
  qm_results$mean_jacc <- as.numeric(qm_results$mean_jacc)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Hyperparam_group  <- as.character(qm_results$Hyperparam_group)

  qm_avg <- aggregate(mean_jacc ~ Hyperparam_group + k, data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(mean_jacc ~ Hyperparam_group + k, data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'mean_jacc'] <- 'mean_jacc_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = c('Hyperparam_group', 'k'))

  if (print_result){
    print(qm_summary)
  }

  if (plot){
    # plt <- ggplot(data = qm_summary, aes(x=k, y=mean_jacc, group=Method, color=Method)) +
    #   geom_line() +
    #   scale_color_brewer(palette="Set3") +
    #   labs(x='k', y='Mean Jaccard Similarity') +
    #   theme(legend.spacing.y = unit(1, 'cm'))
    #
    # if(show_sd){
    #   plt <- plt +
    #     geom_ribbon(aes(ymin = mean_jacc - sd_jacc,
    #                     ymax = mean_jacc + sd_jacc,
    #                     fill = Method), alpha = .2) +
    #     scale_fill_brewer(palette="Set3")
    # }
    #
    # plt <- ggplot(data = qm_summary, aes(x=k, y=lcmc,
    #                                      group=Hyperparam_group,
    #                                      color=Hyperparam_group)) +
    #   geom_line() +
    #   scale_color_brewer(palette="Set3")+
    #   labs(x='k', y='LCMC') +
    #   theme(legend.spacing.y = unit(1, 'cm'))
    #
    # if(!is.null(method_groups) && !length(method_groups)==0 && sd==TRUE){
    #   plt <- plt +
    #     geom_ribbon(aes(ymin = lcmc - lcmc_sd,
    #                     ymax = lcmc + lcmc_sd,
    #                     fill = Hyperparam_group), alpha = 0.2, color = NA) +
    #     scale_fill_brewer(palette="Set3")
    # }

    plt <- ggplot(data = qm_summary, aes(x=k, y=mean_jacc,
                                         group=Hyperparam_group,
                                         color=Hyperparam_group)) +
      geom_line() +
      scale_color_brewer(palette="Set3") +
      labs(x='k', y='Mean Jaccard similarity')

    if(!is.null(method_groups) && !length(method_groups)==0 && sd==TRUE){
      plt <- plt +
        geom_ribbon(aes(ymin = mean_jacc - mean_jacc_sd,
                        ymax = mean_jacc + mean_jacc_sd,
                        fill = Hyperparam_group), alpha = 0.2, color = NA) +
        scale_fill_brewer(palette="Set3")
    }

    print(plt)

    if (!is.null(filename)){
      ggsave(filename = filename,
             plot = plt,
             width = width,
             height = height,
             units = units)
    }
  }
  return(list(results = qm_results, plot = plt))
}


################################################################################
# compare_Jaccard_similarity <- function(methods = c(), k = NULL,
#                                        plot = FALSE, print_result = FALSE,
#                                        filename=NULL,
#                                        width=NA, height=NA,
#                                        units=c("in", "cm", "mm", "px")) {
#
#   # Initialized variable
#   qm_results <- list()
#   plt <- NULL
#   index <- 1
#   lowest_max_row <- nrow(methods[1]$data)
#
#   # Adjusting the maximum k
#   for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
#   if (k >= lowest_max_row){
#     message('The number of the maximum nearest neighbors cannot exceed the number of observations.
#             Setting the k number of nearest neighbors to the minimum dataset size - 1')
#     k <- lowest_max_row - 1
#   }
#
#   for (method in methods) {
#     jacc <- method$get_Jaccard_similarity(k)
#     qm_results <- append(qm_results, mapply(function(x, i) list(method = method$name, jacc = x),
#                                           jacc, SIMPLIFY = FALSE))
#   }
#
#   qm_results <- data.frame(do.call(rbind, qm_results))
#
#   qm_results$method <- as.character(qm_results$method)
#   qm_results$jacc <- as.numeric(qm_results$jacc)
#
#   if (print_result){
#     print(qm_results)
#   }
#
#   if (plot){
#     plt <- ggplot(data = qm_results, aes(x=method, y=jacc)) +
#       geom_boxplot() +
#       scale_color_brewer(palette="Set3") +
#       labs(x='Method', y='Jaccard Similarity') +
#       theme(legend.spacing.y = unit(1, 'cm'))
#
#     print(plt)
#
#     if (!is.null(filename)){
#       ggsave(filename = filename,
#              plot = plt,
#              width = width,
#              height = height,
#              units = units)
#     }
#   }
#   return(c(results = qm_results, plot = plt))
# }


################################################################################
compare_lcmc <- function(methods = NULL, list_k = NULL,
                         method_groups = NULL, sd = FALSE,
                         plot = FALSE, print_result = FALSE,
                         filename=NULL,
                         width=NA, height=NA,
                         units=c("in", "cm", "mm", "px")) {

  # Initialized variable
  qm_results <- list()
  plt <- NULL
  index <- 1

  if (is.null(methods) || length(methods) == 0) {
    stop("methods cannot be NULL or empty.")
  }
  lowest_max_row <- nrow(methods[[1]]$data)

  if (is.null(list_k) || length(list_k) == 0) {
    stop("list_k cannot be NULL or empty.")
  }
  list_k <- sort(list_k, decreasing = F)

  # Adjusting the maximum k
  for(method in methods[-1]) lowest_max_row <- min(lowest_max_row, nrow(method$data))
  if (tail(list_k, 1) >= lowest_max_row){
    message('The number of the maximum nearest neighbors cannot exceed the number of observations.
            Setting the maximum number of nearest neighbors to the number of observations - 1')
    list_k <- list_k[list_k<lowest_max_row]

    if (length(list_k) == 0) {
      stop("All values in list_k exceed the number of observations.
           Please provide smaller k values.")
    }
  }

  for (method in methods) {
    if (!is.null(method_groups) && method$name %in% names(method_groups)) hyperparam_group <- method_groups[[method$name]]
    else hyperparam_group <- method$name
    for(k_i in list_k){
      qm_results[[index]] <- list(
        Method = method$name,
        Hyperparam_group = hyperparam_group,
        k = k_i,
        lcmc = method$lcmc(k_i)
      )
      index <- index+1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))

  qm_results$k <- as.numeric(qm_results$k)
  qm_results$lcmc <- as.numeric(qm_results$lcmc)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Hyperparam_group  <- as.character(qm_results$Hyperparam_group)

  qm_avg <- aggregate(lcmc ~ Hyperparam_group + k, data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(lcmc ~ Hyperparam_group + k, data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'lcmc'] <- 'lcmc_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = c('Hyperparam_group', 'k'))

  if (print_result) print(qm_summary)

  if (plot){
    plt <- ggplot(data = qm_summary, aes(x=k, y=lcmc,
                                         group=Hyperparam_group,
                                         color=Hyperparam_group)) +
      geom_line() +
      scale_color_brewer(palette="Set3")+
      labs(x='k', y='LCMC') +
      theme(legend.spacing.y = unit(1, 'cm'))

    if(!is.null(method_groups) && !length(method_groups)==0 && sd==TRUE){
      plt <- plt +
        geom_ribbon(aes(ymin = lcmc - lcmc_sd,
                        ymax = lcmc + lcmc_sd,
                        fill = Hyperparam_group), alpha = 0.2, color = NA) +
        scale_fill_brewer(palette="Set3")
    }

    print(plt)

    if (!is.null(filename)){
      ggsave(filename = filename,
             plot = plt,
             width = width,
             height = height,
             units = units)
    }
  }
  return(list(results = qm_results, plot = plt))
}
