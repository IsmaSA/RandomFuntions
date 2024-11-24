get_family <- function(response.var, type.data = c("count", "probability","binary", "continuous"), random.factor = FALSE) {
  require(broom)
  require(parameters)
  require(e1071)  
  
  family_mapping <- list(
    count = c("poisson", "quasipoisson", "negativebinomial"),
    probability = c("binomial", "beta"),
    success_failure = "binomial",
    binary = "binomial",
    continuous = c("gaussian", "Gamma", "lognormal")
  )
  
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
  
  # Check variance-to-mean ratio for count data
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
    column <- as.numeric(column[!is.na(column)])  # Ensure numeric and remove NAs
    
    skewness_val <- skewness(column, na.rm = TRUE)
    if (all(column > 0)) {
      log_transformed_skewness <- skewness(log(column), na.rm = TRUE)
    } else {
      log_transformed_skewness <- Inf
    }
    
    # Determine family
    if (skewness_val > 0) {
      if (log_transformed_skewness < 0.5) {
        return("lognormal")
      } else {
        return("Gamma")
      }
    } else {
      return("gaussian")
    }
  }
  select_probability_family <- function(column) {
    if (all(column %in% c(0, 1))) {
      return("binomial")  # Binary outcomes
    } else if (all(column > 0 & column < 1)) {
      return("beta")  
    } else {
      return("unknown") 
    }
  }
  # Process each data type
  result <- lapply(type.data, function(type) {
    if (type %in% names(family_mapping)) {
      if (type == "continuous") {
        # Continuous data
        mean_val <- mean(response.var, na.rm = TRUE)
        var_val <- var(response.var, na.rm = TRUE)
        skewness_val <- skewness(response.var, na.rm = TRUE)
        best_family <- select_continuous_family(response.var)
        families <- best_family
        ratio <- NA
      } else if (type == "count") {
        # Count data
        metrics <- check_variance_mean_ratio(response.var)
        families <- metrics$family
        mean_val <- metrics$mean_val
        var_val <- metrics$var_val
        ratio <- metrics$ratio
      } else if (type == "probability") {
        # Probability data
        families <- select_probability_family(response.var)
        mean_val <- mean(response.var, na.rm = TRUE)
        var_val <- var(response.var, na.rm = TRUE)
        skewness_val <- skewness(response.var, na.rm = TRUE)
        ratio <- NA
      } else {
        # Other types
        families <- family_mapping[[type]]
        mean_val <- NA
        var_val <- NA
        skewness_val <- NA
        ratio <- NA
      }
      
      associated_examples <- examples[[families]]
      return(list(
        data_type = type,
        families = families,
        examples = associated_examples,
        mean_val = mean_val,
        var_val = var_val,
        skewness = skewness_val,
        ratio = ratio
      ))
    } else {
      return(list(
        data_type = type,
        families = "unknown",
        examples = "No examples available",
        mean_val = NA,
        var_val = NA,
        skewness = NA,
        ratio = NA
      ))
    }
  })
  
  # Plot histogram if numeric
  if (is.numeric(response.var)) {
    hist(response.var, main = "Histogram of the response sariable", 
         xlab = "Values", col = "lightblue")
  }
  
  names(result) <- type.data
  return(result)
}

data <- data.frame(
  count_column = rpois(100, lambda = 5),
  continuous_column = rgamma(100, shape = 2, scale = 2),
  probability_column = runif(100, min = 0, max = 1)  # Random probabilities
)

# Test for probabilities
type.data <- c("probability")
result <- get_family(response.var = data$probability_column, type.data = "probability", random.factor = FALSE)
print(result)
