# =============================================================================
# Scaled Mass Index (SMI) Calculation
# Based on Peig & Green 2009
# =============================================================================
#
# Purpose: Calculate a more sophisticated measure of body condition than
# simple weight, using the Scaled Mass Index approach.
#
# Formula: SMI = Mi × (L0/Li)^bSMA
#   - Mi = individual rat mass (weight)
#   - Li = individual rat body length
#   - L0 = mean body length across entire dataset
#   - bSMA = scaling exponent from SMA regression of lnM ~ lnL
#
# References:
#   - Peig & Green 2009: https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0706.2009.17643.x
# =============================================================================

# Load required packages (install if needed)
if (!require("smatr")) install.packages("smatr", repos = "https://cloud.r-project.org")
library(smatr)

# =============================================================================
# MAIN FUNCTION: Calculate SMI
# =============================================================================

calculate_smi <- function(data,
                          mass_col = "weight",
                          length_col = "body_length",
                          method = "smatr") {

  #' Calculate Scaled Mass Index for each individual

  #'

  #' @param data A data frame containing mass and body length measurements

  #' @param mass_col Name of the column containing mass/weight data (default: "weight")
  #' @param length_col Name of the column containing body length data (default: "body_length")

  #' @param method Method to calculate bSMA: "smatr" (recommended) or "manual" (bOLS/r)
  #' @return The input data frame with additional columns: LnMass, LnBL, and SMI

  # Validate inputs
  if (!mass_col %in% names(data)) {
    stop(paste("Mass column '", mass_col, "' not found in data", sep = ""))
  }
  if (!length_col %in% names(data)) {
    stop(paste("Length column '", length_col, "' not found in data", sep = ""))
  }

  # Extract mass and length vectors
  mass <- data[[mass_col]]
  body_length <- data[[length_col]]

  # Check for valid (non-missing, positive) values
  valid_idx <- !is.na(mass) & !is.na(body_length) & mass > 0 & body_length > 0

  if (sum(valid_idx) < 3) {
    stop("Insufficient valid data points (need at least 3 non-missing, positive values)")
  }

  cat("Processing", sum(valid_idx), "valid observations out of", nrow(data), "total rows\n")

  # ---------------------------------------------------------------------------
  # Step 2: Log-transform the data
  # ---------------------------------------------------------------------------
  data$LnMass <- ifelse(mass > 0, log(mass), NA)
  data$LnBL <- ifelse(body_length > 0, log(body_length), NA)

  cat("Step 2 complete: Added LnMass and LnBL columns\n")

  # ---------------------------------------------------------------------------
  # Step 3: Calculate the scaling exponent bSMA
  # ---------------------------------------------------------------------------
  ln_mass_valid <- data$LnMass[valid_idx]
  ln_bl_valid <- data$LnBL[valid_idx]

  if (method == "smatr") {
    # Method 3a: Use smatr package for proper SMA regression
    sma_model <- sma(ln_mass_valid ~ ln_bl_valid)
    bSMA <- coef(sma_model)[2]  # Slope from SMA regression
    cat("Step 3 complete: bSMA calculated via smatr package =", round(bSMA, 4), "\n")

  } else if (method == "manual") {
    # Method 3b: Calculate as bOLS / r
    ols_model <- lm(ln_mass_valid ~ ln_bl_valid)
    bOLS <- coef(ols_model)[2]  # Slope from OLS regression
    r <- cor(ln_mass_valid, ln_bl_valid)  # Pearson correlation
    bSMA <- bOLS / r
    cat("Step 3 complete: bSMA calculated manually (bOLS/r)\n")
    cat("  - bOLS =", round(bOLS, 4), "\n")
    cat("  - r (Pearson) =", round(r, 4), "\n")
    cat("  - bSMA =", round(bSMA, 4), "\n")

  } else {
    stop("Invalid method. Use 'smatr' or 'manual'")
  }

  # ---------------------------------------------------------------------------
  # Step 4: Calculate L0 (mean body length across entire dataset)
  # ---------------------------------------------------------------------------
  L0 <- mean(body_length[valid_idx])
  cat("Step 4 complete: L0 (mean body length) =", round(L0, 2), "\n")

  # ---------------------------------------------------------------------------
  # Step 5: Calculate SMI for each individual
  # Formula: SMI = Mi × (L0/Li)^bSMA
  # ---------------------------------------------------------------------------
  data$SMI <- ifelse(
    valid_idx,
    mass * ((L0 / body_length) ^ bSMA),
    NA
  )

  cat("Step 5 complete: SMI calculated for all valid observations\n")

  # Store parameters as attributes for reference
  attr(data, "smi_parameters") <- list(
    bSMA = bSMA,
    L0 = L0,
    method = method,
    n_valid = sum(valid_idx)
  )

  return(data)
}

# =============================================================================
# HELPER FUNCTION: Get SMI summary statistics
# =============================================================================

smi_summary <- function(data, group_col = NULL) {
  #' Get summary statistics for SMI
  #'
  #' @param data Data frame with SMI column (output from calculate_smi)
  #' @param group_col Optional: column name to group by (e.g., "city")
  #' @return Summary statistics for SMI

  if (!"SMI" %in% names(data)) {
    stop("SMI column not found. Run calculate_smi() first.")
  }

  # Get stored parameters
  params <- attr(data, "smi_parameters")

  cat("\n=== SMI Calculation Parameters ===\n")
  cat("bSMA (scaling exponent):", round(params$bSMA, 4), "\n")
  cat("L0 (mean body length):", round(params$L0, 2), "\n")
  cat("Method:", params$method, "\n")
  cat("Valid observations:", params$n_valid, "\n")

  if (is.null(group_col)) {
    # Overall summary
    cat("\n=== Overall SMI Summary ===\n")
    summary_stats <- data.frame(
      n = sum(!is.na(data$SMI)),
      mean = mean(data$SMI, na.rm = TRUE),
      sd = sd(data$SMI, na.rm = TRUE),
      median = median(data$SMI, na.rm = TRUE),
      min = min(data$SMI, na.rm = TRUE),
      max = max(data$SMI, na.rm = TRUE)
    )
    print(summary_stats)
    return(invisible(summary_stats))

  } else {
    # Grouped summary
    cat("\n=== SMI Summary by", group_col, "===\n")
    summary_stats <- aggregate(
      SMI ~ data[[group_col]],
      data = data,
      FUN = function(x) c(
        n = length(x),
        mean = mean(x),
        sd = sd(x),
        median = median(x)
      )
    )
    names(summary_stats)[1] <- group_col
    print(summary_stats)
    return(invisible(summary_stats))
  }
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# Uncomment and modify the code below to use with your data:

# # Load your data
# rat_data <- read.csv("your_rat_data.csv")
#
# # Calculate SMI (assuming columns are named "weight" and "body_length")
# rat_data <- calculate_smi(
#   data = rat_data,
#   mass_col = "weight",
#   length_col = "body_length",
#   method = "smatr"  # or "manual" for bOLS/r method
# )
#
# # View summary statistics
# smi_summary(rat_data)
#
# # View summary by city
# smi_summary(rat_data, group_col = "city")
#
# # Save results
# write.csv(rat_data, "rat_data_with_smi.csv", row.names = FALSE)

# =============================================================================
# DEMONSTRATION WITH SIMULATED DATA
# =============================================================================

demo_smi <- function() {
  #' Run a demonstration with simulated rat data

  cat("\n")
  cat("=============================================================\n")
  cat("       DEMONSTRATION: Scaled Mass Index Calculation          \n")
  cat("=============================================================\n\n")

  # Simulate rat data (realistic values based on Norway rat measurements)
  set.seed(42)
  n <- 100

  # Body length in mm (adult rats typically 200-270mm)
  body_length <- rnorm(n, mean = 220, sd = 25)

  # Weight in grams (correlated with body length, typically 200-500g)
  # Using allometric relationship: Mass ~ Length^3 (approximately)
  weight <- exp(log(body_length) * 2.8 - 10 + rnorm(n, 0, 0.15))

  # Create simulated city assignments
  cities <- sample(c("New York", "London", "Tokyo", "Sydney"), n, replace = TRUE)

  demo_data <- data.frame(
    rat_id = 1:n,
    city = cities,
    weight = round(weight, 1),
    body_length = round(body_length, 1)
  )

  cat("Simulated data preview (first 6 rows):\n")
  print(head(demo_data))
  cat("\n")

  # Calculate SMI using both methods for comparison
  cat("--- Method 1: Using smatr package ---\n")
  result_smatr <- calculate_smi(demo_data, method = "smatr")

  cat("\n--- Method 2: Manual calculation (bOLS/r) ---\n")
  result_manual <- calculate_smi(demo_data, method = "manual")

  # Show results
  cat("\n--- Results Preview (first 6 rows) ---\n")
  print(head(result_smatr[, c("rat_id", "city", "weight", "body_length",
                               "LnMass", "LnBL", "SMI")]))

  # Summary statistics
  smi_summary(result_smatr)

  cat("\n=== Comparison: Weight vs SMI ===\n")
  cat("Weight mean:", round(mean(demo_data$weight), 2), "g\n")
  cat("SMI mean:", round(mean(result_smatr$SMI, na.rm = TRUE), 2), "\n")
  cat("Correlation (Weight, SMI):",
      round(cor(demo_data$weight, result_smatr$SMI, use = "complete.obs"), 4), "\n")

  return(invisible(result_smatr))
}

# Run demo if this script is sourced directly
# demo_smi()
