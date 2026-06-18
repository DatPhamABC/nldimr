
#' @export
compare_trustworthiness <- function(methods = c(), list_k = NULL,
                                    method_groups = NULL,
                                    extra_attrs = NULL,
                                    group_attrs = c('Params'),
                                    verbose=F) {

  qm_results <- list()
  index <- 1

  if (is.null(methods) || length(methods) == 0) {
    stop("methods cannot be NULL or empty.")
  }
  lowest_max_row <- nrow(methods[[1]]$data)

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
    if (verbose) print(paste('Calculating', method$name))

    if (!is.null(method_groups) && method$name %in% names(method_groups)) param_group <- method_groups[[method$name]]
    else param_group <- method$name

    extra_vals <- list()
    if (!is.null(extra_attrs)){
      for (attr in extra_attrs){
        if (attr %in% names(method)) {
          extra_vals[[attr]] <- method[[attr]]
        } else {
          warning(paste("attribute", attr, "not found in method", method$name, ", setting to NA"))
          extra_vals[[attr]] <- NA
        }
      }
    }

    for (k_i in list_k) {
      qm_results[[index]] <- c(
        list(Method = method$name,
             Params = param_group,
             k = k_i,
             trustworthiness = method$trustworthiness(k_i)
             ),
        extra_vals
      )
      index <- index + 1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))
  qm_results$k <- as.numeric(qm_results$k)
  qm_results$trustworthiness <- as.numeric(qm_results$trustworthiness)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Params  <- as.character(qm_results$Params)

  if (!is.null(extra_attrs)) {
    for (attr in extra_attrs) {
      if (is.numeric(method[[attr]])) {
        qm_results[[attr]] <- as.numeric(qm_results[[attr]])
      } else {
        qm_results[[attr]] <- as.character(qm_results[[attr]])
      }
    }
  }

  # Return raw result if there is no group
  if (is.null(group_attrs)) {
    return(list(results = qm_results, summary = NULL))
  }

  # Checking validity of groups
  invalid_attrs <- group_attrs[!group_attrs %in% colnames(qm_results)]
  if (length(invalid_attrs) > 0) {
    stop(paste("group_attrs not found in results:", paste(invalid_attrs, collapse = ', ')))
  }


  formula_str <- paste('trustworthiness ~', paste(group_attrs, collapse = ' + '))
  qm_avg <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'trustworthiness'] <- 'trustworthiness_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = group_attrs)

  return(list(results = qm_results, summary = qm_summary))
}

################################################################################

#' @export
compare_continuity <- function(methods = c(),
                               list_k = NULL,
                               method_groups = NULL,
                               extra_attrs = NULL,
                               group_attrs = c('Params'),
                               verbose=F) {

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

    if (!is.null(method_groups) && method$name %in% names(method_groups)) param_group <- method_groups[[method$name]]
    else param_group <- method$name

    extra_vals <- list()
    if (!is.null(extra_attrs)){
      for (attr in extra_attrs){
        if (attr %in% names(method)) {
          extra_vals[[attr]] <- method[[attr]]
        } else {
          warning(paste("attribute", attr, "not found in method", method$name, ", setting to NA"))
          extra_vals[[attr]] <- NA
        }
      }
    }

    for (k_i in list_k) {
      qm_results[[index]] <- c(
        list(Method = method$name,
             Params = param_group,
             k = k_i,
             continuity = method$continuity(k_i)
             ),
        extra_vals
        )
      index <- index + 1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))
  qm_results$k <- as.numeric(qm_results$k)
  qm_results$continuity <- as.numeric(qm_results$continuity)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Params  <- as.character(qm_results$Params)

  if (!is.null(extra_attrs)) {
    for (attr in extra_attrs) {
      if (is.numeric(method[[attr]])) {
        qm_results[[attr]] <- as.numeric(qm_results[[attr]])
      } else {
        qm_results[[attr]] <- as.character(qm_results[[attr]])
      }
    }
  }

  # Return raw result if there is no group
  if (is.null(group_attrs)) {
    return(list(results = qm_results, summary = NULL))
  }

  formula_str <- paste('continuity ~', paste(group_attrs, collapse = ' + '))
  qm_avg <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'continuity'] <- 'continuity_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = group_attrs)

  return(list(results = qm_results, summary = qm_summary))
}

################################################################################
#' @export
compare_kNN_Jaccard_similarity <- function(methods = c(), list_k = NULL,
                                           method_groups = NULL,
                                           extra_attrs = NULL,
                                           group_attrs = c('Params'),
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
    if (!is.null(method_groups) && method$name %in% names(method_groups)) param_group <- method_groups[[method$name]]
    else param_group <- method$name

    extra_vals <- list()
    if (!is.null(extra_attrs)){
      for (attr in extra_attrs){
        if (attr %in% names(method)) {
          extra_vals[[attr]] <- method[[attr]]
        } else {
          warning(paste("attribute", attr, "not found in method", method$name, ", setting to NA"))
          extra_vals[[attr]] <- NA
        }
      }
    }

    for(k_i in list_k){
      jacc <- method$get_Jaccard_similarity(k_i)
      qm_results[[index]] <- c(
        list(Method = method$name,
             Params = param_group,
             k = k_i,
             njs = mean(jacc)
             ),
        extra_vals
        )
      index <- index+1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))
  qm_results$k <- as.numeric(qm_results$k)
  qm_results$njs <- as.numeric(qm_results$njs)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Params  <- as.character(qm_results$Params)

  if (!is.null(extra_attrs)) {
    for (attr in extra_attrs) {
      if (is.numeric(method[[attr]])) {
        qm_results[[attr]] <- as.numeric(qm_results[[attr]])
      } else {
        qm_results[[attr]] <- as.character(qm_results[[attr]])
      }
    }
  }

  # Return raw result if there is no group
  if (is.null(group_attrs)) {
    return(list(results = qm_results, summary = NULL))
  }

  formula_str <- paste('njs ~', paste(group_attrs, collapse = ' + '))
  qm_avg <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'njs'] <- 'njs_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = group_attrs)

  return(list(results = qm_results, summary = qm_summary))
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
#' @export
compare_lcmc <- function(methods = c(), list_k = NULL,
                         method_groups = NULL,
                         extra_attrs = NULL,
                         group_attrs = c('Params'),
                         verbose=F) {

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
    if(verbose) print(paste('Calculating', method$name, sep=" "))
    if (!is.null(method_groups) && method$name %in% names(method_groups)) param_group <- method_groups[[method$name]]
    else param_group <- method$name

    extra_vals <- list()
    if (!is.null(extra_attrs)){
      for (attr in extra_attrs){
        if (attr %in% names(method)) {
          extra_vals[[attr]] <- method[[attr]]
        } else {
          warning(paste("attribute", attr, "not found in method", method$name, ", setting to NA"))
          extra_vals[[attr]] <- NA
        }
      }
    }

    for(k_i in list_k){
      qm_results[[index]] <- c(
        list(Method = method$name,
             Params = param_group,
             k = k_i,
             lcmc = method$lcmc(k_i)
             ),
        extra_vals
        )
      index <- index+1
    }
  }

  qm_results <- data.frame(do.call(rbind, qm_results))
  qm_results$k <- as.numeric(qm_results$k)
  qm_results$lcmc <- as.numeric(qm_results$lcmc)
  qm_results$Method <- as.character(qm_results$Method)
  qm_results$Params  <- as.character(qm_results$Params)

  if (!is.null(extra_attrs)) {
    for (attr in extra_attrs) {
      if (is.numeric(method[[attr]])) {
        qm_results[[attr]] <- as.numeric(qm_results[[attr]])
      } else {
        qm_results[[attr]] <- as.character(qm_results[[attr]])
      }
    }
  }

  # Return raw result if there is no group
  if (is.null(group_attrs)) {
    return(list(results = qm_results, summary = NULL))
  }

  formula_str <- paste('lcmc ~', paste(group_attrs, collapse = ' + '))
  qm_avg <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'mean')
  qm_sd  <- aggregate(as.formula(formula_str), data = qm_results, FUN = 'sd')
  colnames(qm_sd)[colnames(qm_sd) == 'lcmc'] <- 'lcmc_sd'
  qm_summary <- merge(qm_avg, qm_sd, by = group_attrs)

  return(list(results = qm_results, summary = qm_summary))
}
