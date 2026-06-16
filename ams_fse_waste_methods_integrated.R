# ams_fse_waste_methods_integrated.R
#
# Builds per-FSE monthly waste-weight estimates for Amsterdam food-service
# establishments (FSEs) across the Jun–Oct 2020 rat-capture window.
#
# DATA PROVENANCE — read before editing:
#   National waste TONNAGE: researcher-supplied (WRAP / Dutch Monitor / WUR).
#     CBS does NOT publish commercial food-service waste tonnage by subsector.
#     Do NOT attempt to fetch tonnage from CBS — it is not there.
#   Turnover seasonality:   CBS table 85828ENG (uncorrected, monthly, 2020).
#   Establishment counts:   CBS table 81578NED (COROP Groot-Amsterdam, 2020).
#   FSE establishment counts per subsector: OSM-derived (separate pipeline),
#     loaded from amsfood/ams_fse_counts_by_subsector.csv or in-memory object.
#
# OUTPUT:
#   amsfood/ams_method1_per_fse_monthly_kg.csv
#   amsfood/ams_method2_per_fse_monthly_kg.csv
#   amsfood/ams_waste_method_comparison.csv
#
# MANUSCRIPT NOTE (see "what to check" block at the bottom):
#   State in your methods that national tonnage is researcher-supplied from
#   WRAP/Dutch Monitor, NOT from CBS. The localization scalar and turnover
#   seasonality ARE from CBS.

suppressPackageStartupMessages({
  library(cbsodataR)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
})

# ── 0.  CONFIG ────────────────────────────────────────────────────────────────

CFG <- list(
  out_dir        = "amsfood",
  year           = 2020,          # matches rat-capture year — DO NOT change to 2024
  months         = 6:10,          # Jun–Oct capture window (Bug 3 fix)
  TONNES_TO_KG   = 1000,          # unit conversion constant — applied exactly once
  turnover_id    = "85828ENG",
  turnover_topic = "UncorrectedProductionTurnover_1",  # keeps seasonal signal
  estab_id       = "81578NED",
  estab_topic    = "Vestigingen_1",
  region_ams     = "CR23",        # Groot-Amsterdam COROP
  region_nl      = "NL01",        # NL national (excludes Buitenland — use NL01 not NL00)
  estab_period   = "2020JJ00",
  # Confirmed SBI branch keys — identical block used for BOTH turnover and
  # establishment pulls so the localization scalar aligns with the seasonal
  # apportionment (Bug 5 fix).
  # convenience intentionally shares grocery key 371800: CBS 81578NED has no
  # separable convenience-store SBI class. Do NOT invent a different key.
  branch_keys = c(
    restaurant_fastfood = "390700",   # SBI 5610 restaurants/eetgelegenheden
    barpub              = "391500",   # SBI 5630 cafes
    convenience         = "371800",   # SBI 4711 supermarkten (shared with grocery)
    grocery             = "371800"    # SBI 4711 supermarkten (shared with convenience)
  )
)

dir.create(CFG$out_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1.  NATIONAL TONNAGE SLOT (researcher-supplied, NOT CBS) ─────────────────
#
# Paste WRAP / Dutch Monitor (WUR) national annual food-service waste figures
# here in TONNES. These are NOT from CBS. CBS does not publish this.
# Values remain NA_real_ until you fill them in — the script hard-stops on NA.
#
# Source example:
#   WRAP (2023). "Food Waste in the Netherlands." / Dutch Monitor Voedselverspilling.
#   Values represent national annual food-service waste by SBI subsector, in metric
#   tonnes, for the reference year closest to 2020.

national_tonnage_raw <- tribble(
  ~fse_subsector,       ~waste_tonnes,
  "restaurant_fastfood", NA_real_,    # <-- paste national tonnes here (WRAP/Dutch Monitor)
  "barpub",              NA_real_,    # <-- paste national tonnes here
  "convenience",         NA_real_,    # <-- paste national tonnes here
  "grocery",             NA_real_     # <-- paste national tonnes here
)

# Hard-stop on placeholder NA (Bug 6 fix) — refusing to run on missing tonnage
if (any(is.na(national_tonnage_raw$waste_tonnes))) {
  stop(
    "HARD STOP: national waste tonnage contains NA values.\n",
    "Paste researcher-supplied WRAP/Dutch Monitor figures (in TONNES) into the\n",
    "national_tonnage_raw tribble before running. CBS does not supply this.\n",
    "Subsectors missing values: ",
    paste(national_tonnage_raw$fse_subsector[is.na(national_tonnage_raw$waste_tonnes)],
          collapse = ", ")
  )
}

# Unit conversion: tonnes → kg, exactly once (Bug 1 fix)
national_tonnage <- national_tonnage_raw %>%
  mutate(waste_kg_national = waste_tonnes * CFG$TONNES_TO_KG)

cat("National tonnage (researcher-supplied, WRAP/Dutch Monitor):\n")
print(national_tonnage)

# ── 2.  LOCALIZATION SCALAR (CBS 81578NED, COROP Groot-Amsterdam) ─────────────

loc_file <- file.path(CFG$out_dir, "ams_localization_scalars.csv")

if (!file.exists(loc_file)) {
  stop(
    "HARD STOP: localization scalar file not found at: ", loc_file, "\n",
    "Run the localization script first (CBS 81578NED establishment-share pull)."
  )
}

loc_scalars <- read_csv(loc_file, show_col_types = FALSE)

required_loc_cols <- c("fse_subsector", "loc_scalar")
missing_loc_cols  <- setdiff(required_loc_cols, names(loc_scalars))
if (length(missing_loc_cols) > 0) {
  stop("HARD STOP: ams_localization_scalars.csv is missing columns: ",
       paste(missing_loc_cols, collapse = ", "))
}

if (any(is.na(loc_scalars$loc_scalar))) {
  stop(
    "HARD STOP: loc_scalar is NA for subsectors: ",
    paste(loc_scalars$fse_subsector[is.na(loc_scalars$loc_scalar)], collapse = ", "),
    "\nCheck subsector name spelling — joins are case-sensitive."
  )
}

cat("\nLocalization scalars (CBS 81578NED, CR23/NL01 establishment share):\n")
print(loc_scalars)

# ── 3.  AMSTERDAM ANNUAL WASTE (kg) ───────────────────────────────────────────

waste_ams <- national_tonnage %>%
  left_join(loc_scalars, by = "fse_subsector") %>%
  mutate(waste_kg_ams_annual = waste_kg_national * loc_scalar)

if (any(is.na(waste_ams$waste_kg_ams_annual))) {
  stop(
    "HARD STOP: waste_kg_ams_annual is NA — subsector name mismatch between\n",
    "national_tonnage_raw and ams_localization_scalars.csv.\n",
    "Check that fse_subsector spelling matches exactly in both.\n",
    "Problem subsectors: ",
    paste(waste_ams$fse_subsector[is.na(waste_ams$waste_kg_ams_annual)],
          collapse = ", ")
  )
}

cat("\nAmsterdam annual waste (kg) after localization:\n")
print(waste_ams %>% select(fse_subsector, waste_tonnes, waste_kg_national,
                           loc_scalar, waste_kg_ams_annual))

# ── 4.  OSM FSE ESTABLISHMENT COUNT ──────────────────────────────────────────
#
# Prefer an in-memory object named `fse_counts` if already loaded, else read CSV.

if (exists("fse_counts") && is.data.frame(fse_counts)) {
  cat("\nUsing in-memory fse_counts object.\n")
  n_estab_df <- fse_counts
} else {
  fse_count_file <- file.path(CFG$out_dir, "ams_fse_counts_by_subsector.csv")
  if (!file.exists(fse_count_file)) {
    stop(
      "HARD STOP: OSM FSE counts not found.\n",
      "Expected in-memory object `fse_counts` OR file: ", fse_count_file, "\n",
      "Run the OSM FSE pipeline first."
    )
  }
  n_estab_df <- read_csv(fse_count_file, show_col_types = FALSE)
  cat("\nLoaded FSE counts from: ", fse_count_file, "\n")
}

required_estab_cols <- c("fse_subsector", "n_estab_subsector")
missing_estab_cols  <- setdiff(required_estab_cols, names(n_estab_df))
if (length(missing_estab_cols) > 0) {
  stop("HARD STOP: FSE count data is missing columns: ",
       paste(missing_estab_cols, collapse = ", "))
}

cat("FSE establishment counts (OSM-derived):\n")
print(n_estab_df %>% select(fse_subsector, n_estab_subsector))

# ── 5.  TURNOVER SEASONALITY PULL (CBS 85828ENG, 2020, Method 2) ─────────────

cat("\n--- Pulling CBS turnover table", CFG$turnover_id, "for year", CFG$year, "---\n")

turnover_raw <- cbs_get_data(
  CFG$turnover_id,
  Branches  = as.list(unique(CFG$branch_keys)),
  Perioden  = has_substring(as.character(CFG$year)),
  select    = c("Branches", "Perioden", CFG$turnover_topic)
)

# Filter to monthly periods only — Perioden mixes MM/KW/JJ codes
turnover_monthly <- turnover_raw %>%
  filter(str_detect(Perioden, paste0(CFG$year, "MM"))) %>%
  mutate(
    cal_month = as.integer(str_extract(Perioden, "(?<=MM)\\d+")),
    branch_key = Branches
  ) %>%
  rename(turnover_value = !!sym(CFG$turnover_topic))

# Map branch key → subsector(s)
# Note: 371800 maps to BOTH convenience and grocery — create a row for each
key_map <- tibble(
  branch_key  = CFG$branch_keys,
  fse_subsector = names(CFG$branch_keys)
)

turnover_mapped <- turnover_monthly %>%
  inner_join(key_map, by = "branch_key", relationship = "many-to-many") %>%
  select(fse_subsector, branch_key, Perioden, cal_month, turnover_value)

# Aggregate: when multiple CBS series share a key (convenience + grocery both
# use 371800), the CBS turnover for 371800 is the same value — keep distinct
# rows per subsector, not summed.
turnover_agg <- turnover_mapped %>%
  group_by(fse_subsector, cal_month) %>%
  summarise(turnover_value = sum(turnover_value, na.rm = TRUE), .groups = "drop")

# Assert we have 12 months for each subsector
month_counts <- turnover_agg %>%
  group_by(fse_subsector) %>%
  summarise(n_months = n_distinct(cal_month))
if (any(month_counts$n_months != 12)) {
  warning("Not all subsectors have 12 monthly turnover observations for ",
          CFG$year, ":\n", paste(capture.output(print(month_counts)), collapse = "\n"))
}

# Print 2020 monthly turnover wide — inspect COVID dips before they feed shares
cat("\n2020 monthly turnover (uncorrected) — inspect for COVID lockdown dips",
    "(NL horeca closed ~mid-Mar to Jun 2020, and again late 2020):\n")
turnover_wide <- turnover_agg %>%
  pivot_wider(names_from = fse_subsector,
              values_from = turnover_value,
              names_prefix = "turn_") %>%
  arrange(cal_month)
print(turnover_wide, n = 12)

cat(
  "\n[METHODS CAVEAT] 2020 horeca turnover is atypical due to COVID-19 lockdowns.",
  "\n  Dutch hospitality closures: ~mid-March to 1 June 2020; late Oct–Dec 2020.",
  "\n  These dips are present in the uncorrected series and propagate into Method 2",
  "\n  throughput shares. Method 1 (equal split) is unaffected — note this asymmetry",
  "\n  in the manuscript methods and sensitivity discussion.\n"
)

# ── 6.  THROUGHPUT SHARES — normalized over 12 months (Bug 7 fix) ─────────────

turnover_shares <- turnover_agg %>%
  group_by(fse_subsector) %>%
  mutate(
    annual_turnover   = sum(turnover_value, na.rm = TRUE),
    throughput_share  = turnover_value / annual_turnover  # share of full-year
  ) %>%
  ungroup()

# Assert shares sum to 1 per subsector over 12 months
share_sums <- turnover_shares %>%
  group_by(fse_subsector) %>%
  summarise(share_sum = sum(throughput_share))
if (any(abs(share_sums$share_sum - 1) > 1e-6)) {
  stop("HARD STOP: throughput_share does not sum to 1 for all subsectors.\n",
       paste(capture.output(print(share_sums)), collapse = "\n"))
}

cat("\nThroughput share sums (must all equal 1.000):\n")
print(share_sums)

# Subset to capture window — shares for Jun–Oct should sum to ~5/12 ≈ 0.417
shares_capture <- turnover_shares %>%
  filter(cal_month %in% CFG$months) %>%
  select(fse_subsector, cal_month, turnover_value, throughput_share)

capture_share_sums <- shares_capture %>%
  group_by(fse_subsector) %>%
  summarise(capture_share_sum = sum(throughput_share))
cat("\nThroughput share sums for Jun–Oct (expect ~0.417 per subsector):\n")
print(capture_share_sums)

# ── 7.  JOIN SHARED INPUTS ───────────────────────────────────────────────────

# Base table: subsector × capture month, annotated with ams annual kg and n_estab
base_capture <- expand_grid(
  fse_subsector = names(CFG$branch_keys),
  capture_month = CFG$months
) %>%
  left_join(
    waste_ams %>% select(fse_subsector, waste_kg_ams_annual),
    by = "fse_subsector"
  ) %>%
  left_join(
    n_estab_df %>% select(fse_subsector, n_estab_subsector),
    by = "fse_subsector"
  )

# ── 8.  METHOD 1 — EQUAL SPLIT ────────────────────────────────────────────────

method1 <- base_capture %>%
  mutate(
    waste_kg_equal = (waste_kg_ams_annual / 12) / n_estab_subsector
  ) %>%
  select(fse_subsector, capture_month, waste_kg_ams_annual, waste_kg_equal)

# Guard: no NA weights
if (any(is.na(method1$waste_kg_equal))) {
  warning("Method 1 has NA per-FSE weights for: ",
          paste(unique(method1$fse_subsector[is.na(method1$waste_kg_equal)]),
                collapse = ", "),
          "\nCheck n_estab_subsector and waste_kg_ams_annual for these subsectors.")
}

cat("\nMethod 1 (equal split) per-FSE monthly kg — static across months by design:\n")
print(method1)

# ── 9.  METHOD 2 — THROUGHPUT-WEIGHTED ────────────────────────────────────────

method2 <- base_capture %>%
  left_join(
    shares_capture %>% select(fse_subsector, cal_month = cal_month,
                               turnover_value, throughput_share),
    by = c("fse_subsector", "capture_month" = "cal_month")
  ) %>%
  mutate(
    waste_kg_subsector_month = waste_kg_ams_annual * throughput_share,
    waste_kg_throughput      = waste_kg_subsector_month / n_estab_subsector
  ) %>%
  select(fse_subsector, capture_month, turnover_value, throughput_share,
         waste_kg_ams_annual, waste_kg_throughput)

if (any(is.na(method2$waste_kg_throughput))) {
  warning("Method 2 has NA per-FSE weights for: ",
          paste(unique(method2$fse_subsector[is.na(method2$waste_kg_throughput)]),
                collapse = ", "),
          "\nCheck turnover join, n_estab_subsector, and waste_kg_ams_annual.")
}

cat("\nMethod 2 (throughput-weighted) per-FSE monthly kg:\n")
print(method2)

# ── 10.  COMPARISON ──────────────────────────────────────────────────────────

comparison <- method1 %>%
  select(fse_subsector, capture_month, waste_kg_equal) %>%
  left_join(
    method2 %>% select(fse_subsector, capture_month, waste_kg_throughput,
                        throughput_share),
    by = c("fse_subsector", "capture_month")
  ) %>%
  left_join(
    n_estab_df %>% select(fse_subsector, n_estab_subsector),
    by = "fse_subsector"
  ) %>%
  mutate(
    ratio_thru_to_equal = waste_kg_throughput / waste_kg_equal,
    pct_diff            = 100 * (waste_kg_throughput - waste_kg_equal) / waste_kg_equal
  ) %>%
  select(fse_subsector, capture_month, n_estab_subsector,
         waste_kg_equal, waste_kg_throughput, throughput_share,
         ratio_thru_to_equal, pct_diff) %>%
  arrange(fse_subsector, capture_month)

cat("\nMethod comparison (ratio ≈ 1 means weighting changed little):\n")
print(comparison, n = nrow(comparison))

# ── 11.  WRITE OUTPUTS ───────────────────────────────────────────────────────

write_csv(method1,     file.path(CFG$out_dir, "ams_method1_per_fse_monthly_kg.csv"))
write_csv(method2,     file.path(CFG$out_dir, "ams_method2_per_fse_monthly_kg.csv"))
write_csv(comparison,  file.path(CFG$out_dir, "ams_waste_method_comparison.csv"))

cat("\nOutputs written to", CFG$out_dir, ":\n")
cat("  ams_method1_per_fse_monthly_kg.csv\n")
cat("  ams_method2_per_fse_monthly_kg.csv\n")
cat("  ams_waste_method_comparison.csv\n")

# ── WHAT TO CHECK WHEN IT RUNS ───────────────────────────────────────────────
#
# 1. TONNAGE PROVENANCE (manuscript methods):
#      State explicitly that national waste tonnage is researcher-supplied from
#      WRAP / Dutch Monitor (WUR), NOT from CBS. CBS does not publish commercial
#      food-service waste tonnage by subsector. The localization scalar (CBS
#      81578NED) and turnover seasonality (CBS 85828ENG) ARE from CBS.
#
# 2. UNIT CHECK:
#      national_tonnage_raw → waste_tonnes (t) → × TONNES_TO_KG (1000) →
#      waste_kg_national → × loc_scalar → waste_kg_ams_annual (kg).
#      Conversion happens exactly once. Confirm waste_kg_ams_annual is ~1000×
#      the input tonnes for a sanity check.
#
# 3. KEY ALIGNMENT:
#      Both the turnover pull and the establishment pull use CFG$branch_keys.
#      The scalar denominator and the seasonal apportionment describe the same
#      CBS sectors. If you ever update one key, update CFG$branch_keys — both
#      pulls inherit it automatically.
#
# 4. COVID INSPECTION:
#      Look at the 2020 monthly turnover wide table printed to console. Months
#      3–5 (Mar–May) and 10–12 (Oct–Dec) for restaurant_fastfood and barpub
#      should show depressed values. If they look flat or identical to 2019,
#      something is wrong with the pull. This is expected for grocery/convenience
#      (supermarkten were not closed). Document the COVID atypicality in methods.
#
# 5. CONVENIENCE = GROCERY SCALAR, DISTINCT WEIGHTS:
#      convenience and grocery share CBS SBI key 371800, so their loc_scalar and
#      throughput_share will be identical. Their waste_kg_equal and
#      waste_kg_throughput will differ IF their n_estab_subsector or
#      waste_tonnes differ. This is correct and intentional — do not collapse them.
#      Disclose the shared key limitation in the manuscript.
#
# 6. METHOD 2 SHARE SUM CHECK:
#      capture_share_sums printed above should be ≈ 0.417 per subsector (5/12).
#      Values well above 0.417 suggest 2020 Jun–Oct turnover was unusually HIGH
#      relative to the full year (plausible for post-lockdown surge in summer 2020).
#      Values well below suggest the window was locked down — inspect the wide table.
#
# 7. METHOD 1 STATIC BY DESIGN:
#      waste_kg_equal is IDENTICAL across all five capture months for a given
#      subsector. This is correct — equal split does not vary by month. The
#      comparison CSV will show this as ratio = waste_kg_throughput / constant.
#
# 8. NA WEIGHT GUARD:
#      Both method guards warn if per-FSE weights are NA. NA most likely means
#      a subsector name mismatch between fse_subsector fields. All four names
#      must be spelled exactly: restaurant_fastfood, barpub, convenience, grocery.
#
# 9. RATIO INTERPRETATION:
#      ratio_thru_to_equal ≈ 1 → seasonal weighting changed little for that
#      subsector × month (expected for grocery/convenience, which CBS doesn't
#      track as a distinct seasonal sector).
#      ratio > 1 → month had above-average throughput (e.g., summer peak).
#      ratio < 1 → month had below-average throughput (e.g., COVID dip).
#
# 10. DOWNSTREAM USE:
#      The waste_kg_equal (Method 1) and waste_kg_throughput (Method 2) values
#      are per-FSE monthly weights. Attach the relevant weight to each OSM FSE
#      point for the capture month, then pass the weighted point layer to the
#      spatial KDE (`kde_wt_*`). Do NOT compute the KDE in this script.
