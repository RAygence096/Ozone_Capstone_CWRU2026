library(RAQSAPI)
library(dplyr)
library(purrr)
library(readr)

# ── Credentials ──────────────────────────────────────────────────────────────
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"),
  key = Sys.getenv("AQS_KEY")
)

# ── Date range ───────────────────────────────────────────────────────────────
bdate <- as.Date("2024-01-01")
edate <- as.Date("2024-12-31")

# ── Site definitions ─────────────────────────────────────────────────────────
sites <- tribble(
  ~name,               ~state, ~county, ~site,
  "GT_Craig",          "39",   "035",   "0060",   # NCore/PAMS – main target
  "Near_Road",         "39",   "035",   "0073",   # NO2 + CO
  "Galleria",          "39",   "035",   "0051",   # CO
  "St_Theodosius",     "39",   "035",   "0038"    # Canister VOCs
)

# ── Parameter codes ──────────────────────────────────────────────────────────
# NOx family (GT Craig + Near Road)
nox_params <- c(
  "42600",  # NOy (total reactive nitrogen)
  "42601",  # NO
  "42602"   # True NO2 (CAPS spectroscopy)
)

# CO (GT Craig, Near Road, Galleria)
co_params <- c("42101")  # CO (standard / trace)

# Carbonyls – GT Craig only (PAMS carbonyl canister)
carbonyl_params <- c(
  "43502",  # Formaldehyde (HCHO)
  "43503"   # Acetaldehyde
)

# VOCs – Auto GC continuous at GT Craig (PAMS target compound list)
# These are the standard 57 PAMS compounds; request by group below
voc_params_autogc <- c(
  # C2-C3 alkanes/alkenes
  "43202", "43203", "43204", "43205", "43206",
  # C4 compounds
  "43212", "43214", "43216", "43217", "43218",
  # C5 compounds
  "43220", "43221", "43224", "43226", "43227",
  # C6+ alkanes
  "43228", "43230", "43231", "43232", "43233",
  # C7-C10 alkanes
  "43235", "43238", "43243", "43247", "43248",
  # Alkenes / dienes
  "43280", "43285", "43291",
  # Aromatics (BTEX + others)
  "45201",  # Benzene
  "45202",  # Toluene
  "45203",  # Ethylbenzene
  "45204",  # m/p-Xylene
  "45207",  # o-Xylene
  "45208",  # Styrene
  "45209",  # Isopropylbenzene (Cumene)
  "45210",  # n-Propylbenzene
  "45212",  # 1,3,5-Trimethylbenzene
  "45213",  # 1,2,4-Trimethylbenzene
  "45220"   # 1,2,3-Trimethylbenzene
)

# Canister VOCs at St. Theodosius (1-in-12 days)
# Same compound codes, but will return fewer samples
voc_params_canister <- voc_params_autogc  # same codes, different site

# ── Helper: download one parameter × one site ────────────────────────────────
fetch_site <- function(state, county, site, param, bdate, edate) {
  message(glue::glue("  Fetching param {param} @ {state}-{county}-{site}..."))
  tryCatch(
    aqs_sampledata_by_site(
      parameter   = param,
      bdate       = bdate,
      edate       = edate,
      stateFIPS   = state,
      countycode  = county,
      sitenum     = site
    ),
    error = function(e) {
      warning(glue::glue("  FAILED: {param} @ {state}-{county}-{site}: {e$message}"))
      NULL
    }
  )
}

# ── Download NOx (GT Craig + Near Road) ─────────────────────────────────────
message("=== Downloading NOx ===")
nox_data <- sites %>%
  filter(name %in% c("GT_Craig", "Near_Road")) %>%
  pmap_dfr(function(name, state, county, site) {
    map_dfr(nox_params, ~ fetch_site(state, county, site, .x, bdate, edate))
  })

saveRDS(nox_data, "data_out/cleveland_2024_nox.rds")
message("Saved: cleveland_2024_nox.rds  (", nrow(nox_data), " rows)")

# ── Download CO (GT Craig + Near Road + Galleria) ───────────────────────────
message("=== Downloading CO ===")
co_data <- sites %>%
  filter(name %in% c("GT_Craig", "Near_Road", "Galleria")) %>%
  pmap_dfr(function(name, state, county, site) {
    map_dfr(co_params, ~ fetch_site(state, county, site, .x, bdate, edate))
  })

saveRDS(co_data, "data_out/cleveland_2024_co.rds")
message("Saved: cleveland_2024_co.rds  (", nrow(co_data), " rows)")

# ── Download Carbonyls (GT Craig only) ───────────────────────────────────────
message("=== Downloading Carbonyls ===")
carbonyl_data <- sites %>%
  filter(name == "GT_Craig") %>%
  pmap_dfr(function(name, state, county, site) {
    map_dfr(carbonyl_params, ~ fetch_site(state, county, site, .x, bdate, edate))
  })

saveRDS(carbonyl_data, "data_out/cleveland_2024_carbonyls.rds")
message("Saved: cleveland_2024_carbonyls.rds  (", nrow(carbonyl_data), " rows)")

# ── Download Auto-GC VOCs (GT Craig) ─────────────────────────────────────────
message("=== Downloading VOCs – Auto GC (GT Craig) ===")
voc_gtcraig <- sites %>%
  filter(name == "GT_Craig") %>%
  pmap_dfr(function(name, state, county, site) {
    map_dfr(voc_params_autogc, ~ fetch_site(state, county, site, .x, bdate, edate))
  })

saveRDS(voc_gtcraig, "data_out/cleveland_2024_voc_gtcraig.rds")
message("Saved: cleveland_2024_voc_gtcraig.rds  (", nrow(voc_gtcraig), " rows)")

# ── Download Canister VOCs (St. Theodosius, 1-in-12 days) ───────────────────
message("=== Downloading VOCs – Canister (St. Theodosius) ===")
voc_sttheo <- sites %>%
  filter(name == "St_Theodosius") %>%
  pmap_dfr(function(name, state, county, site) {
    map_dfr(voc_params_canister, ~ fetch_site(state, county, site, .x, bdate, edate))
  })

saveRDS(voc_sttheo, "data_out/cleveland_2024_voc_sttheo.rds")
message("Saved: cleveland_2024_voc_sttheo.rds  (", nrow(voc_sttheo), " rows)")

message("\n✅ All downloads complete.")