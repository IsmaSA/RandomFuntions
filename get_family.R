

# Script to help to identify which family is the best based on your dataset

get_family <- function(response.var, type.data = c("count", "probability", "success_failure", "binary", "continuous"), random.factor = FALSE) {
  require(broom)
  require(parameters)
  
  family_mapping <- list(
    count = c("poisson", "quasipoisson", "negativebinomial"),
    probability = c("binomial", "beta"),
    success_failure = "binomial",
    binary = "binomial",
    continuous = c("gaussian", "Gamma", "lognormal")
  )
  
  # Examples of distributions
  examples <- list(
    binomial = "Number surviving, number killed",
    poisson = "Seeds per quadrat, settlers (variance ≈ mean)",
    quasipoisson = "Overdispersed count data",
    negativebinomial = "Seeds per quadrat, settlers (variance > mean)",
    Gamma = "Survival time, distance to nearest edge",
    gaussian = "Normal distribution",
    lognormal = "Size, mass (exponential growth)",
    beta = "Cover proportion or bounded probabilities (0-1)"
  )
  
    check_variance_mean_ratio <- function(column) {
      mean_val <- mean(column, na.rm = TRUE)
      var_val <- var(column, na.rm = TRUE)
      ratio <- var_val / mean_val
      if (ratio < 1.2) {
        family <- "poisson"
      } else if (ratio >= 1.2 && ratio < 2) {
        family <- "quasipoisson"
      } else {
        family <- "negativebinomial"
      }
      return(list(family = family, mean_val = mean_val, var_val = var_val, ratio = ratio))
    }
    
    select_continuous_family <- function(column) {
      # Ensure the input is a numeric vector and handle potential issues
      column <- as.numeric(column)
      column <- column[!is.na(column)]  # Remove NA values
      
      # Calculate skewness of the column
      skewness_val <- e1071::skewness(column, na.rm = TRUE)
      
      # Check log-transformed skewness for strictly positive values
      if (all(column > 0)) {
        log_transformed_skewness <- e1071::skewness(log(column), na.rm = TRUE)
      } else {
        log_transformed_skewness <- Inf  # Set to infinity if log transformation is not possible
      }
      
      # Determine the best family based on skewness
      if (skewness_val > 0) {
        if (log_transformed_skewness < 0.5) {
          return("lognormal")  # Log transformation normalizes the data
        } else {
          return("Gamma")  # Positively skewed data
        }
      } else {
        return("gaussian")  # Symmetric data
      }
    }
    
    # Process each data type
    result <- lapply(type.data, function(type) {
      if (type %in% names(family_mapping)) {
        families <- family_mapping[[type]]
        if(type=="continuos") {
          best_family <- select_continuous_family(response.var)
          families <- best_family
          mean_val <- mean(response.var, na.rm = TRUE)
          var_val <- var(response.var, na.rm = TRUE)
          skewness_val <- skewness(response.var, na.rm = TRUE)
        }  else {
          mean_val <- NA
          var_val <- NA
          skewness_val <- NA
        }
        
        # If type is count, check variance-to-mean ratio
        if (type == "count" && !is.null(data)) {
          metrics <- check_variance_mean_ratio(response.var)
          families <- metrics$family
          mean_val <- metrics$mean_val
          var_val <- metrics$var_val
          ratio <- metrics$ratio
        } else {
          mean_val <- NA
          var_val <- NA
          ratio <- NA
        }
        
        associated_examples <- examples[families]
        return(list(
          data_type = type,
          families = families,
          examples = associated_examples,
          mean_val = mean_val,
          var_val = var_val,
          ratio = ratio
        ))
      } else {
        return(list(
          data_type = type,
          families = "unknown",
          examples = "No examples available",
          mean_val = NA,
          var_val = NA,
          ratio = NA
        )) }
    })
    
    if(is.numeric(response.var) ==TRUE) {hist(response.var) }
    names(result) <- type.data
    return(result)
}

# Example usage
set.seed(123)
data <- data.frame(
  count_column = rpois(100, lambda = 5),  # Replace with your actual data
  other_column = rnorm(100)
)

type.data <- c("count", "probability", "success_failure", "binary", "continuous")
result <- get_family(response.var = data$count_column, type.data = "continuous", random.factor = TRUE)



