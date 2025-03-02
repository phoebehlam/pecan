#' table 1
#' tired of making table 1's
#'
#' @param data dataframe; containing numeric and/or logical columns to calculate correlations for
#' @return a correlation matrix
#' @export
table1 <- function(data) {
  
  # separate numeric vs categorical
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # frame
  descriptives <- tibble::tibble(Variable = names(data), Descriptive = NA_character_)
  
  # mean and sd
  if (length(numeric_vars) > 0) {
    num_desc <- psych::describe(data[numeric_vars]) %>%
      dplyr::select(mean, sd) %>%
      dplyr::mutate(Descriptive = sprintf("%.2f (%.2f)", mean, sd)) %>%
      tibble::rownames_to_column(var = "Variable")
    
    descriptives <- dplyr::left_join(descriptives, num_desc, by = "Variable") %>%
      dplyr::mutate(`Descriptive Statistics` = coalesce(Descriptive.y, Descriptive.x)) %>%
      dplyr::select(Variable, `Descriptive Statistics`)
  }
  
  # frequencies
  if (length(categorical_vars) > 0) {
    cat_desc <- data[categorical_vars] %>%
      dplyr::summarise(dplyr::across(everything(), ~ {
        counts <- table(.)
        levels <- names(counts)
        props <- round(100 * counts / sum(counts), 1)
        desc <- paste0(levels, "=", counts, " (", props, "%)")
        paste(desc, collapse = "; ")
      })) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "Descriptive")
    
    descriptives <- dplyr::left_join(descriptives, cat_desc, by = "Variable") %>%
      dplyr::mutate(Descriptives = coalesce(Descriptive.y, Descriptive.x)) %>%
      dplyr::select(Variable, Descriptives)
  }
  
  # test
  if (length(numeric_vars) == 0) {
    return(descriptives)
  }
  
  corr_results <- psych::corr.test(data[numeric_vars])
  
  # r and p
  cor_matrix <- round(corr_results$r, 2)
  p_matrix <- corr_results$p
  
  # just below diagonal
  formatted_matrix <- matrix(NA, nrow = ncol(cor_matrix), ncol = ncol(cor_matrix))
  colnames(formatted_matrix) <- rownames(cor_matrix)
  rownames(formatted_matrix) <- rownames(cor_matrix)
  
  for (i in 1:ncol(cor_matrix)) {
    formatted_matrix[i, i] <- "-"  
    for (j in 1:(i-1)) {
      if (!is.na(cor_matrix[i, j]) && !is.null(cor_matrix[i, j]) && length(cor_matrix[i, j]) > 0 && cor_matrix[i, j] == 0) {
        formatted_matrix[i, j] <- ".00"
      } else {
        cor_value <- sub("^0", "", sprintf("%.2f", cor_matrix[i, j]))
        p_value <- p_matrix[i, j]
        formatted_matrix[i, j] <- ifelse(p_value < 0.05, paste0(cor_value, "*"), cor_value)
      }
      
    }
  }
  
  # make df
  cor_df <- as.data.frame(formatted_matrix) %>%
    tibble::rownames_to_column(var = "Variable")
  
  # merge with descriptives
  final_table <- dplyr::left_join(descriptives, cor_df, by = "Variable")
  
  final_table <- dplyr::select(final_table, -ncol(final_table))
  return(final_table)
}

