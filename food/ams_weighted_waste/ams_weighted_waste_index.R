# =============================================================================
# ams_weighted_waste_index.R
# Amsterdam weighted food-waste index for rat capture buffers
# -----------------------------------------------------------------------------
# Builds a per-rat "food waste availability" score by:
#   1. Extracting food service establishments (FSE) from OSM (ohsome).
#   2. Benchmarking OSM completeness against live CBS establishment counts.
#   3. Allocating national food-waste tonnage down to metro -> city -> per-shop.
#   4. Attaching per-shop annual (and seasonal) waste weights to each FSE point.
#   5. Summing weights within each rat's capture buffer -> wwindex_kg_year.
#
# GEOGRAPHY DISCIPLINE (load-bearing, do not relax):
#   CBS establishment counts are at CR23 = Groot-Amsterdam COROP -> compare to
#   OSM points inside `grootams` ONLY. Never against ams_city directly.
#
# NOTE ON NAMING: fse_subsector "grocery" = supermarkets + OSM shop=convenience
# (SBI 4711). fse_subsector "convenience" = specialist food shops -- bakery,
# deli, butcher, greengrocer, pastry, cheese (SBI 472). These do NOT line up
# 1:1 with the OSM tag `shop=convenience`, which lands in "grocery" -- this is
# intentional (see classify_subsector()) but easy to misread at a glance.
#
# CLEANUP NOTES (this revision):
#   - Removed ad hoc CBS-API exploration code (undefined objects, typos) that
#     was interleaved with the production pipeline in the prior draft.
#   - Consolidated the two duplicate "write outputs" blocks into one, at the
#     very end, after every output object exists.
#   - Fixed a stray un-commented note appended to the st_write() call that
#     would raise a parse error if the file were sourced top to bottom.
#   - 2020 turnover shape is now fetched once and reused for both the primary
#     seasonal weighting and the 2019-vs-2020 comparison (was fetched twice).
# =============================================================================

suppressPackageStartupMessages({
  library(sf); library(ohsome); library(dplyr); library(tidyr)
  library(stringr); library(readr); library(tibble)
  library(cbsodataR); library(lubridate); library(purrr)
})

# ── 0.  CONFIG ────────────────────────────────────────────────────────────────
CFG <- list(
  base_dir = "C:/heart/Dr R 2025/finalsem_spring26/food",
  out_dir  = "C:/heart/Dr R 2025/finalsem_spring26/food/amsfood",

  WGS84 = 4326, AMS_CRS = 28992,

  grootams_gpkg = "grootamsterdam_cr23_2020.gpkg",
  city_source   = "ams_borough.csv",
  city_wkt_col  = "WKT_LNG_LAT",

  fse_snapshot = "2020-11-01",     # complete-October 2020 OSM snapshot
  loc_file     = "ams_localization_scalars.csv",
  rat_sj_file  = "rnorv_sj_all.csv",

  # CBS tables
  count_id    = "81578NED",        # Vestigingen; bedrijfstak, regio (COUNTS)
  count_topic = "Vestigingen_1",
  count_region_label = "Groot-Amsterd",  # matches "Groot-Amsterdam (CR)" -> CR23
  count_period = "2020JJ00",

  turnover_id    = "85828ENG",     # SHAPE ONLY (monthly/quarterly turnover)
  turnover_topic = "UncorrectedProductionTurnover_1",
  turnover_year  = 2020,
  turnover_compare_year = 2019,    # methods justification for using flat weights

  months = 6:10,                   # rat trapping window this index must cover
  TONNES_TO_KG = 1000
)

dir.create(CFG$out_dir, showWarnings = FALSE, recursive = TRUE)
setwd(CFG$base_dir)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Canonical FSE filter -- byte-identical to main pipeline.
food_filter <- paste(
  "(",
  "amenity in (restaurant, cafe, fast_food, bar, pub, food_court, biergarten, ice_cream, juice_bar)",
  "or",
  "shop in (convenience, bakery, deli, supermarket, butcher, greengrocer, pastry, cheese)",
  ")",
  "and amenity != fuel"
)

# Subsector classifier. cafe -> restaurant_fastfood by DEFAULT; flip via toggle
# below if you want the "cafe = NL cafe/drinking" reading. OSM shop=convenience
# -> "grocery" (general retail), NOT the fse_subsector called "convenience".
CFG$cafe_as_barpub <- FALSE  # TRUE => cafe counts in barpub bucket (SBI 5630)

classify_subsector <- function(amenity, shop, cafe_as_barpub = FALSE) {
  rf_amenity  <- c("restaurant", "fast_food", "food_court", "ice_cream", "juice_bar")
  bar_amenity <- c("bar", "pub", "biergarten")
  if (cafe_as_barpub) bar_amenity <- c(bar_amenity, "cafe")
  else                rf_amenity  <- c(rf_amenity, "cafe")

  dplyr::case_when(
    amenity %in% rf_amenity                               ~ "restaurant_fastfood",
    amenity %in% bar_amenity                              ~ "barpub",
    shop == "supermarket"                                 ~ "grocery",
    shop == "convenience"                                 ~ "grocery",      # general retail side
    shop %in% c("bakery", "deli", "butcher", "greengrocer",
                "pastry", "cheese")                        ~ "convenience",  # specialist 472
    TRUE                                                   ~ NA_character_
  )
}

# =============================================================================
#  CBS HELPERS -- resolve keys from meta by LABEL, never hardcode.
#  (CBS keys carry trailing whitespace, e.g. "390700 ", that silently breaks
#  == filters. Matching on Title and passing the meta Key back verbatim avoids
#  that, and avoids hardcoding counts that CBS could revise.)
# =============================================================================

cbs_key_by_label <- function(meta_dim, pattern, exact_code = NULL) {
  hit <- meta_dim[grepl(pattern, meta_dim$Title, ignore.case = TRUE), c("Key", "Title")]
  if (!is.null(exact_code)) {
    hit <- hit[grepl(paste0("^", exact_code, "\\b"), trimws(hit$Title)), , drop = FALSE]
  }
  if (nrow(hit) == 0) {
    stop(sprintf("No CBS category matched: %s / %s", pattern, exact_code %||% ""))
  }
  if (nrow(hit) > 1) {
    message(sprintf("Multiple matches for '%s' -- using first: %s",
                     exact_code %||% pattern, trimws(hit$Title[1])))
  }
  hit$Key[1]
}

# ── 1.  LOAD BOUNDARIES ───────────────────────────────────────────────────────
grootams <- st_read(file.path(CFG$out_dir, CFG$grootams_gpkg), quiet = TRUE) |>
  st_transform(CFG$AMS_CRS) |> st_make_valid()

city_raw <- read.csv2(file.path(CFG$out_dir, CFG$city_source), stringsAsFactors = FALSE)
stopifnot(CFG$city_wkt_col %in% names(city_raw))
ams_city <- st_as_sf(city_raw, wkt = CFG$city_wkt_col, crs = CFG$WGS84) |>
  st_transform(CFG$AMS_CRS) |> st_make_valid() |> summarise(.groups = "drop")

a_city  <- as.numeric(sum(st_area(ams_city))) / 1e6
a_metro <- as.numeric(sum(st_area(grootams))) / 1e6
cat(sprintf("City area:  %.1f km^2\nMetro area: %.1f km^2\nratio: %.3f\n",
            a_city, a_metro, a_city / a_metro))
if (a_city >= a_metro) warning("City >= metro -- boundaries swapped or CRS wrong.")

# ── 2.  OHSOME FSE EXTRACTION ─────────────────────────────────────────────────
fetch_fse <- function(boundary_sf, label) {
  bpoly <- boundary_sf |>
    st_simplify(dTolerance = 25, preserveTopology = TRUE) |>  # 25 m in 28992
    st_transform(CFG$WGS84)
  q <- ohsome_elements_centroid(boundary = bpoly, filter = food_filter,
                                 time = CFG$fse_snapshot,
                                 properties = c("tags", "metadata"))
  res <- ohsome_post(q)
  if ("@osmId" %in% names(res)) res <- dplyr::rename(res, osm_id = `@osmId`)
  res <- st_transform(res, CFG$AMS_CRS)
  if (!"amenity" %in% names(res)) res$amenity <- NA_character_
  if (!"shop"    %in% names(res)) res$shop    <- NA_character_
  res <- res |>
    mutate(fse_subsector = classify_subsector(amenity, shop, CFG$cafe_as_barpub)) |>
    filter(!is.na(fse_subsector))
  cat(sprintf("[%s] ohsome returned %d classified FSE points\n", label, nrow(res)))
  res
}
fse_metro_all <- fetch_fse(grootams, "Groot-Amsterdam")
fse_city_all  <- fetch_fse(ams_city, "Amsterdam city (pre-clip)")

fse_metro <- st_filter(fse_metro_all, grootams, .predicate = st_within)
fse_city  <- st_filter(fse_city_all,  ams_city, .predicate = st_within)
cat(sprintf("After st_within clip: metro=%d  city=%d\n", nrow(fse_metro), nrow(fse_city)))

# ── 3.  OSM COUNTS BY SUBSECTOR ───────────────────────────────────────────────
osm_metro_counts <- st_drop_geometry(fse_metro) |> count(fse_subsector, name = "n_osm_metro")
osm_city_counts  <- st_drop_geometry(fse_city)  |> count(fse_subsector, name = "n_osm_city")
osm_counts <- full_join(osm_metro_counts, osm_city_counts, by = "fse_subsector") |>
  mutate(across(c(n_osm_metro, n_osm_city), ~replace_na(., 0L)))
cat("\nOSM FSE counts by subsector:\n"); print(osm_counts)

# ── 4.  CBS ESTABLISHMENT COUNTS -- LIVE, NO HARDCODING ───────────────────────
# 4a. meta + resolve region key (padded) by label.
meta_cnt <- cbs_get_meta(CFG$count_id)
stopifnot("RegioS" %in% names(meta_cnt))
region_key <- cbs_key_by_label(meta_cnt$RegioS, CFG$count_region_label)
stopifnot(length(region_key) == 1L, !is.na(region_key))
cat(sprintf("Resolved CBS region key: '%s' (%s)\n", region_key,
            trimws(meta_cnt$RegioS$Title[meta_cnt$RegioS$Key == region_key])))

# 4b. Map each subsector -> the SBI parent code we benchmark against.
#     grocery              -> 4711 Supermarkten           (OSM supermarket + convenience)
#     convenience          -> 472  Winkels in voedingsmiddelen (specialist food shops)
#     restaurant_fastfood  -> 5610 Restaurants, andere eetgelegenheden
#     barpub               -> 563  Cafes
sbi_targets <- tribble(
  ~fse_subsector,        ~sbi_code, ~label_pattern,
  "restaurant_fastfood", "5610",    "Restaurants, andere eetgelegenheden",
  "barpub",              "563",     "Caf",
  "grocery",             "4711",    "Supermarkt",
  "convenience",         "472",     "Winkels in voedingsmiddelen"
)

stopifnot("BedrijfstakkenBranchesSBI2008" %in% names(meta_cnt))
sbi_targets <- sbi_targets |>
  mutate(cbs_key = map2_chr(label_pattern, sbi_code,
                             ~cbs_key_by_label(meta_cnt$BedrijfstakkenBranchesSBI2008,
                                                .x, exact_code = .y)))
if (any(is.na(sbi_targets$cbs_key))) {
  stop("HARD STOP: unresolved SBI branch key for: ",
       paste(sbi_targets$fse_subsector[is.na(sbi_targets$cbs_key)], collapse = ", "))
}
cat("\nResolved SBI branch keys (verbatim, padding preserved):\n")
print(sbi_targets |> mutate(cbs_key_show = sprintf("'%s'", cbs_key)) |>
        select(fse_subsector, sbi_code, cbs_key_show))

# 4c. Pull counts for CR23 x 2020 across ALL branches, then join by trimmed key
#     (pull-all-then-subset avoids per-key padded-filter fragility).
cbs_all <- cbs_get_data(
  CFG$count_id,
  RegioS   = region_key,
  Perioden = CFG$count_period,
  select   = c("BedrijfstakkenBranchesSBI2008", "RegioS", "Perioden", CFG$count_topic)
)
cbs_counts <- cbs_all |>
  transmute(cbs_key_trim = trimws(BedrijfstakkenBranchesSBI2008),
            cbs_count    = .data[[CFG$count_topic]]) |>
  right_join(sbi_targets |> mutate(cbs_key_trim = trimws(cbs_key)), by = "cbs_key_trim") |>
  select(fse_subsector, sbi_code, cbs_count)

if (any(is.na(cbs_counts$cbs_count))) {
  warning("CBS count NA (disclosure suppression or key mismatch) for: ",
          paste(cbs_counts$fse_subsector[is.na(cbs_counts$cbs_count)], collapse = ", "))
}
cat("\n[CBS] CR23 establishment counts (live):\n"); print(cbs_counts)

# ── 5.  DELIVERABLE 2: OSM (metro) vs CBS (CR23) completeness ─────────────────
osm_vs_cbs <- osm_counts |>
  left_join(cbs_counts, by = "fse_subsector") |>
  mutate(osm_minus_cbs  = n_osm_metro - cbs_count,
         osm_pct_of_cbs = 100 * n_osm_metro / cbs_count)
cat("\n[Deliverable 2] OSM metro vs CBS CR23 (geography-matched):\n")
print(osm_vs_cbs |> select(fse_subsector, n_osm_metro, cbs_count, osm_minus_cbs, osm_pct_of_cbs))
cat("Interpretation: pct < 100 => OSM under-maps that subsector vs census.\n",
    "Report RANK ORDER across subsectors, not absolute pct (SBI vs OSM tag\n",
    "definitions are approximate). Geography = CR23 for BOTH sides.\n")

# ── 6.  DELIVERABLE 3: OSM city share of metro (per subsector) ────────────────
osm_share <- osm_counts |>
  mutate(osm_city_share = if_else(n_osm_metro > 0, n_osm_city / n_osm_metro, NA_real_))
cat("\n[Deliverable 3] OSM city share of metro:\n"); print(osm_share)
if (any(osm_share$osm_city_share > 1, na.rm = TRUE)) {
  warning("osm_city_share > 1 -- boundary overlap or border point.")
}

# ── 7.  NATIONAL TONNAGE (researcher-supplied, Wageningen -- DO NOT EDIT) ─────
# Retail food-waste is reported by the national monitor as ONE aggregate; it
# does NOT separate supermarkets (4711) from specialist food shops (472). We
# treat grocery + convenience as a SINGLE "retail" tier sharing one per-shop
# waste intensity -- a disclosed DATA LIMITATION, not a claim they're
# ecologically identical. Horeca splits into two real subsectors.
retail_pool_kt <- 210
horeca_pool_kt <- 83

subsector_to_tier <- tribble(
  ~fse_subsector,        ~waste_tier,
  "restaurant_fastfood", "restaurant_fastfood",
  "barpub",              "barpub",
  "grocery",             "retail",
  "convenience",         "retail"
)

nat_estab_share <- tribble(
  ~waste_tier,           ~nat_share,
  "restaurant_fastfood", 591835 / 809980,
  "barpub",              218145 / 809980
)
national_tonnage <- bind_rows(
  nat_estab_share |>
    mutate(waste_tonnes = horeca_pool_kt * 1000 * nat_share) |>
    select(waste_tier, waste_tonnes),
  tibble(waste_tier = "retail", waste_tonnes = retail_pool_kt * 1000)
) |>
  mutate(waste_kg_national = waste_tonnes * CFG$TONNES_TO_KG)

if (any(is.na(national_tonnage$waste_tonnes))) stop("HARD STOP: national tonnage NA -- check shares.")
cat("\n[National tonnage] by TIER (retail = single pool):\n"); print(national_tonnage)

# ── 8.  CHAIN C LOCALIZATION (national -> metro -> city), by TIER ─────────────
# All localizers aggregate to TIER so retail stays internally consistent:
# grocery+convenience combine for loc_scalar (mean), OSM counts (sum), CBS (sum).
loc <- read_csv(file.path(CFG$out_dir, CFG$loc_file), show_col_types = FALSE)
stopifnot("fse_subsector" %in% names(loc), "loc_scalar" %in% names(loc))

loc_tier <- loc |>
  left_join(subsector_to_tier, by = "fse_subsector") |>
  group_by(waste_tier) |>
  # grocery & convenience share the same loc_scalar today, so mean == that
  # value; revisit with a weighted mean if they ever diverge.
  summarise(loc_scalar = mean(loc_scalar, na.rm = TRUE), .groups = "drop")

osm_share_tier <- osm_share |>
  left_join(subsector_to_tier, by = "fse_subsector") |>
  group_by(waste_tier) |>
  summarise(n_osm_metro = sum(n_osm_metro), n_osm_city = sum(n_osm_city), .groups = "drop") |>
  mutate(osm_city_share = if_else(n_osm_metro > 0, n_osm_city / n_osm_metro, NA_real_))

cbs_counts_tier <- cbs_counts |>
  left_join(subsector_to_tier, by = "fse_subsector") |>
  group_by(waste_tier) |>
  summarise(cbs_count = sum(cbs_count, na.rm = TRUE), .groups = "drop")

city_waste_pool <- national_tonnage |>
  left_join(loc_tier,                                            by = "waste_tier") |>
  left_join(osm_share_tier |> select(waste_tier, osm_city_share), by = "waste_tier") |>
  mutate(waste_kg_grootams = waste_kg_national * loc_scalar,
         waste_kg_city     = waste_kg_grootams * osm_city_share)
if (any(is.na(city_waste_pool$waste_kg_city))) {
  stop("HARD STOP: waste_kg_city NA -- tier name mismatch across tables.")
}
cat("\n[Chain C] city waste pool by TIER (kg/yr):\n")
print(city_waste_pool |> select(waste_tier, waste_tonnes, loc_scalar,
                                 osm_city_share, waste_kg_grootams, waste_kg_city))

# ── 9.  PER-FSE ANNUAL WASTE (per-census-shop, by TIER) ───────────────────────
# Divide each tier's city pool by its CENSUS count scaled to city via the OSM
# city-share ratio -- i.e. we assume real establishments thin out from metro
# to city at roughly the same rate OSM's mapped points do. Retail divides by
# (grocery + convenience) census combined, so the per-shop rate reflects ALL
# real food-retail shops, not just OSM-mapped ones: OSM under-mapping lowers
# KDE density downstream, not per-point mass (Herfort et al.).
per_tier_annual <- city_waste_pool |>
  left_join(cbs_counts_tier, by = "waste_tier") |>
  mutate(cbs_city_equiv  = cbs_count * osm_city_share,
         per_fse_kg_year = if_else(cbs_city_equiv > 0, waste_kg_city / cbs_city_equiv, NA_real_))
if (any(is.na(per_tier_annual$per_fse_kg_year))) {
  warning("per_fse_kg_year NA for tier: ",
          paste(per_tier_annual$waste_tier[is.na(per_tier_annual$per_fse_kg_year)], collapse = ", "))
}
cat("\n[Per-TIER annual waste, kg/yr]:\n")
print(per_tier_annual |> select(waste_tier, waste_kg_city, cbs_count,
                                 osm_city_share, cbs_city_equiv, per_fse_kg_year))

# Expand tier rate back to SUBSECTOR labels so it joins to OSM points (which
# carry grocery / convenience, not "retail"). Both retail subsectors inherit
# the SAME rate -- intended and disclosed.
per_fse_annual <- subsector_to_tier |>
  left_join(per_tier_annual |> select(waste_tier, per_fse_kg_year), by = "waste_tier")
cat("\n[Per-FSE annual waste, kg/yr] (subsector-keyed, retail tiers identical):\n")
print(per_fse_annual |> arrange(desc(per_fse_kg_year)))

# ── 10.  SEASONALITY -- turnover SHAPE by TIER (SBI-code anchored) ────────────
# 85828ENG: dim 'Periods', English labels, QUARTERLY (KW) only for these
# branches. Resolve branch keys by SBI code, aggregate shape to TIER so retail
# has ONE seasonal curve matching its ONE waste value.
meta_turn <- cbs_get_meta(CFG$turnover_id)
stopifnot("Branches" %in% names(meta_turn), "Periods" %in% names(meta_turn))

turn_targets <- tribble(
  ~fse_subsector,        ~sbi_code, ~label_pattern,
  "restaurant_fastfood", "561",     "^561 Restaurants$",
  "barpub",              "563",     "^563 Bars$",
  "grocery",             "4711",    "Supermarkets and similar",
  "convenience",         "472",     "Specialised shops selling food"
) |>
  mutate(branch_key = map2_chr(label_pattern, sbi_code,
                                ~tryCatch(cbs_key_by_label(meta_turn$Branches, .x, exact_code = .y),
                                          error = function(e) NA_character_)))
cat("\n[85828ENG] resolved turnover branch keys:\n")
print(turn_targets |> mutate(k = sprintf("'%s'", branch_key)) |> select(fse_subsector, sbi_code, k))

turn_ok <- turn_targets |> filter(!is.na(branch_key))
if (nrow(turn_ok) < 4L) {
  warning("Only ", nrow(turn_ok), "/4 turnover keys resolved (missing: ",
          paste(setdiff(turn_targets$fse_subsector, turn_ok$fse_subsector), collapse = ", "), ")")
}
stopifnot(!any(duplicated(trimws(turn_ok$branch_key))))

# Quarterly share-of-year, aggregated to TIER, cached by year so the same year
# is never re-fetched from the CBS API twice in one run.
.turnover_cache <- new.env(parent = emptyenv())
quarterly_share_for_year <- function(yr) {
  yr <- as.character(yr); stopifnot(grepl("^[0-9]{4}$", yr))
  if (!is.null(.turnover_cache[[yr]])) return(.turnover_cache[[yr]])

  out <- cbs_get_data(CFG$turnover_id, Periods = has_substring(yr),
                       select = c("Branches", "Periods", CFG$turnover_topic)) |>
    mutate(branch_trim = trimws(Branches)) |>
    inner_join(turn_ok |> mutate(branch_trim = trimws(branch_key)), by = "branch_trim") |>
    left_join(subsector_to_tier, by = "fse_subsector") |>
    filter(str_detect(Periods, paste0(yr, "KW"))) |>
    mutate(cal_quarter    = as.integer(str_extract(Periods, "(?<=KW)\\d+")),
           turnover_value = .data[[CFG$turnover_topic]]) |>
    filter(!is.na(turnover_value)) |>
    group_by(waste_tier, cal_quarter) |>
    summarise(turnover_value = sum(turnover_value, na.rm = TRUE), .groups = "drop") |>
    group_by(waste_tier) |>
    mutate(quarter_share_of_year = turnover_value / sum(turnover_value, na.rm = TRUE)) |>
    ungroup() |>
    mutate(turnover_year = yr)

  .turnover_cache[[yr]] <- out
  out
}

turn_year <- as.character(CFG$turnover_year)
stopifnot(grepl("^[0-9]{4}$", turn_year))
turnover_share_tier <- quarterly_share_for_year(turn_year)
if (nrow(turnover_share_tier) == 0L) stop("HARD STOP: no quarterly turnover resolved -- check table frequency.")

# Expand tier shape back to subsector labels for the point join.
turnover_share <- subsector_to_tier |>
  left_join(turnover_share_tier |> select(waste_tier, cal_quarter, quarter_share_of_year),
            by = "waste_tier")
cat("\n[85828ENG] quarterly seasonal shape by subsector (retail tiers identical):\n")
print(turnover_share)

# ── 10b.  2019 vs 2020 SEASONAL COMPARISON (methods justification) ────────────
# Reuses the CFG$turnover_year fetch above via the cache when it matches.
share_2019_tier <- quarterly_share_for_year(CFG$turnover_compare_year)
share_2020_tier <- quarterly_share_for_year(2020)
if (nrow(share_2019_tier) == 0L) stop("HARD STOP: no comparison-year quarterly turnover.")

seasonal_compare <- share_2019_tier |>
  select(waste_tier, cal_quarter, share_2019 = quarter_share_of_year) |>
  full_join(share_2020_tier |> select(waste_tier, cal_quarter, share_2020 = quarter_share_of_year),
            by = c("waste_tier", "cal_quarter")) |>
  arrange(waste_tier, cal_quarter) |>
  mutate(across(c(share_2019, share_2020), ~replace_na(., 0)),
         delta_2020_minus_2019 = share_2020 - share_2019)
cat("\n[Seasonal shape] 2019 vs 2020 by TIER:\n"); print(seasonal_compare, n = Inf)

# ── 11.  PRIMARY (flat monthly) vs SENSITIVITY (seasonal) MONTHLY WEIGHTS ─────
# Primary: flat monthly weight, robust to the 2020 COVID seasonal distortion
# shown in seasonal_compare above.
per_fse_month_flat <- per_fse_annual |>
  select(fse_subsector, per_fse_kg_year) |>
  mutate(per_fse_kg_month_flat = per_fse_kg_year / 12)

# Sensitivity: quarterly shape broadcast evenly across each quarter's 3 months.
month_to_quarter <- tibble(cal_month = 1:12, cal_quarter = rep(1:4, each = 3))
per_fse_month_season <- per_fse_annual |>
  select(fse_subsector, per_fse_kg_year) |>
  left_join(turnover_share |> select(fse_subsector, cal_quarter, quarter_share_of_year),
            by = "fse_subsector") |>
  left_join(month_to_quarter, by = "cal_quarter",
            relationship = "many-to-many") |>  # intentional: 1 quarter -> 3 months
  mutate(month_share_of_year     = quarter_share_of_year / 3,
         per_fse_kg_month_season = per_fse_kg_year * month_share_of_year) |>
  select(fse_subsector, cal_month, cal_quarter, month_share_of_year, per_fse_kg_month_season)

# ── 12.  ATTACH WEIGHTS TO EACH CITY OSM POINT ────────────────────────────────
fse_city_weighted <- fse_city |>
  left_join(per_fse_month_flat |> select(fse_subsector, per_fse_kg_month_flat), by = "fse_subsector") |>
  left_join(per_fse_annual |> select(fse_subsector, per_fse_kg_year), by = "fse_subsector")

season_wide <- per_fse_month_season |>
  filter(cal_month %in% CFG$months) |>
  transmute(fse_subsector,
            colname = paste0("per_fse_kg_month_", sprintf("%02d", cal_month)),
            per_fse_kg_month_season) |>
  pivot_wider(names_from = colname, values_from = per_fse_kg_month_season)

fse_city_weighted <- fse_city_weighted |> left_join(season_wide, by = "fse_subsector")
cat("\nWeighted point layer built. Weight columns:\n")
print(names(fse_city_weighted)[grepl("per_fse_kg", names(fse_city_weighted))])

# ── 13.  AMSTERDAM RAT JOIN + PER-RAT WEIGHTED WASTE INDEX ────────────────────
# rnorv_sj_all.csv is produced upstream (spatial join of rats to nearby FSE
# points per buffer_label) -- not built in this script.
sj_all <- read_csv(file.path(CFG$out_dir, CFG$rat_sj_file), show_col_types = FALSE)

# Columns entirely empty for the Amsterdam subset (join artifacts + non-AMS
# enrichment block + full "_1" duplicate block). Confirmed 0/5041 non-empty
# before dropping -- re-verify this if the upstream join file changes shape.
drop_cols <- c(
  "X", "id", "dataset", "city", "country", "site", "species", "date", "UR_ID", "sex", "age",
  "hels_age", "grams", "bodyL", "totalL", "tailL", "footL", "condition", "habitat", "grade",
  "updater", "second_age", "city_group", "lat", "lon",
  paste0("bio", 1:19), "pop_dens",
  "urbanization_1km", "urbanization_5km", "urbanization_10km", "urbanization_20km",
  "abs_lat", "gsbr_ageclass", "mean_wt", "sd_wt", "z_score", "og_lat", "og_lon",
  "capture_date", "all_tags", "snapshot_timestamp",
  paste0(c("X", "id", "dataset", "city", "country", "site", "species", "date", "UR_ID",
           "Rat_ID", "sex", "age", "hels_age", "grams", "bodyL", "totalL", "tailL", "footL",
           "condition", "habitat", "grade", "updater", "second_age", "city_group",
           "lat", "lon", paste0("bio", 1:19), "pop_dens",
           "urbanization_1km", "urbanization_5km", "urbanization_10km", "urbanization_20km",
           "abs_lat", "gsbr_ageclass", "mean_wt", "sd_wt", "z_score", "og_lat", "og_lon",
           "capture_date", "BUFF_DIST", "ORIG_FID", "Shape_Length", "Shape_Area"), "_1")
)

sj_ams <- sj_all |>
  filter(city_id == "Amsterdam") |>
  dplyr::select(-dplyr::any_of(drop_cols)) |>
  mutate(fse_subsector = classify_subsector(amenity, shop, CFG$cafe_as_barpub)) |>
  left_join(per_fse_annual |> select(fse_subsector, per_fse_kg_year), by = "fse_subsector") |>
  mutate(per_fse_kg_year = case_when(
    Join_Count == 0      ~ 0,
    is.na(fse_subsector) ~ 0,
    TRUE                 ~ per_fse_kg_year
  ))
cat(sprintf("sj_ams: %d rows, %d cols\n", nrow(sj_ams), ncol(sj_ams)))
stopifnot(sum(is.na(sj_ams$Rat_ID) | sj_ams$Rat_ID == "") == 0)  # must be 0 for AMS

# Seasonal weight per row: match each rat's capture month (from month_key,
# e.g. "6/1/2020") to that FSE subsector's seasonal monthly weight.
sj_ams_seasonal <- sj_ams |>
  mutate(cal_month = as.integer(stringr::str_extract(month_key, "^\\d+"))) |>
  left_join(per_fse_month_season |> select(fse_subsector, cal_month, per_fse_kg_month_season),
            by = c("fse_subsector", "cal_month")) |>
  mutate(per_fse_kg_month_season = case_when(
    Join_Count == 0      ~ 0,
    is.na(fse_subsector) ~ 0,
    TRUE                 ~ per_fse_kg_month_season
  ))

# Per-rat-per-buffer weighted waste index. SUM = total waste availability in
# buffer (not a mean) -- overlapping FSEs of the same subsector are meant to
# add up, since availability is additive.
rat_buffer_index <- sj_ams_seasonal |>
  group_by(Rat_ID, buffer_label) |>
  summarise(
    n_fse             = sum(!is.na(osm_id) & osm_id != ""),
    wwindex_kg_year   = sum(per_fse_kg_year, na.rm = TRUE),          # flat, PRIMARY
    wwindex_kg_season = sum(per_fse_kg_month_season, na.rm = TRUE),  # turnover, SENSITIVITY
    .groups = "drop"
  )
cat("\n[Amsterdam] weighted waste index (flat + seasonal) per rat per buffer:\n")
print(head(rat_buffer_index, 12))

# Optional QA: coverage of rat_buffer_index against the comprehensive adult
# rat set, if it's already loaded in this session (built upstream, not here).
if (exists("rat_ams")) {
  n_index_rats  <- dplyr::n_distinct(rat_buffer_index$Rat_ID)
  n_adult_rats  <- dplyr::n_distinct(rat_ams$Rat_ID)
  n_overlap     <- length(intersect(rat_ams$Rat_ID, rat_buffer_index$Rat_ID))
  cat(sprintf(
    "\n[QA] rats with a food index: %d | adult rats w/ coords: %d | overlap: %d\n",
    n_index_rats, n_adult_rats, n_overlap))
  if (n_index_rats != n_overlap) {
    warning(sprintf(
      "%d rat(s) have a weighted waste index but are NOT in rat_ams -- check upstream filters.",
      n_index_rats - n_overlap))
  }
}

# ── 14.  WRITE OUTPUTS (single consolidated block) ────────────────────────────
ww_dir <- file.path(CFG$out_dir, "ams_wwindex")
if (!dir.exists(ww_dir)) dir.create(ww_dir, recursive = TRUE)

write_csv(osm_counts,       file.path(ww_dir, "ams_osm_fse_counts.csv"))
write_csv(osm_vs_cbs,       file.path(ww_dir, "ams_osm_vs_cbs_cr23.csv"))
write_csv(cbs_counts,       file.path(ww_dir, "ams_cbs_cr23_counts.csv"))
write_csv(city_waste_pool,  file.path(ww_dir, "ams_city_waste_pool.csv"))
write_csv(per_fse_annual,   file.path(ww_dir, "ams_per_fse_annual.csv"))
write_csv(sj_ams,           file.path(ww_dir, "ams_sj_all_weighted.csv"))
write_csv(rat_buffer_index, file.path(ww_dir, "ams_rat_buffer_wwindex.csv"))
st_write(fse_city_weighted, file.path(ww_dir, "ams_fse_city_weighted.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

cat(sprintf("\nAll outputs written to: %s\n", ww_dir))
