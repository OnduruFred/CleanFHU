# ============================================================
#  CleanFHU — Demographic & clinical classifier helpers
#  These are exported so analysts can call them independently
#  of the full prisma_pipeline().
# ============================================================


# ------------------------------------------------------------
#  Gravidity
# ------------------------------------------------------------

#' Classify gravidity from a raw pregnancy count
#'
#' Converts a numeric prior-pregnancy count (as stored in `ph_prevn_rporres`,
#' which represents *previous* pregnancies) into a three-level ordered factor.
#' The function adds 1 to the raw value before classifying to obtain the
#' *total* number of pregnancies including the current one.
#'
#' @param x Integer or numeric vector of **previous** pregnancy counts
#'   (i.e. the raw `ph_prevn_rporres` field, `NA` treated as 0).
#'
#' @return An ordered factor with levels:
#'   * `"1. Primigravida"` — first pregnancy
#'   * `"2. Been pregnant 2-4 times"` — second through fourth
#'   * `"3. Been pregnant >4 times"` — fifth or more
#'
#' @examples
#' classify_gravidity(c(NA, 0, 1, 3, 5))
#'
#' @export
classify_gravidity <- function(x) {
  x <- tidyr::replace_na(x, 0) + 1L

  out <- dplyr::case_when(
    x == 1L         ~ "1. Primigravida",
    dplyr::between(x, 2L, 4L) ~ "2. Been pregnant 2-4 times",
    x >= 5L         ~ "3. Been pregnant >4 times"
  )

  forcats::fct_relevel(
    factor(out),
    "1. Primigravida",
    "2. Been pregnant 2-4 times",
    "3. Been pregnant >4 times"
  )
}


# ------------------------------------------------------------
#  ANC visits
# ------------------------------------------------------------

#' Classify total ANC visits into frequency categories
#'
#' Converts a numeric total-ANC-visit count into a three-level ordered factor
#' aligned with the WHO recommended minimum of four visits.
#'
#' @param x Numeric vector of total ANC visit counts.
#'
#' @return An ordered factor with levels:
#'   * `"1. Less than 4 times"`
#'   * `"2. Between 4-5 times"`
#'   * `"3. Above 5 times"`
#'
#' @examples
#' classify_anc_visits(c(1, 4, 5, 7, NA))
#'
#' @export
classify_anc_visits <- function(x) {
  out <- dplyr::case_when(
    x < 4                      ~ "1. Less than 4 times",
    dplyr::between(x, 4, 5)   ~ "2. Between 4-5 times",
    x >= 6                     ~ "3. Above 5 times"
  )

  forcats::fct_relevel(
    factor(out),
    "1. Less than 4 times",
    "2. Between 4-5 times",
    "3. Above 5 times"
  )
}


# ------------------------------------------------------------
#  Maternal age
# ------------------------------------------------------------

#' Classify maternal age into clinical categories
#'
#' @param age Numeric vector of maternal ages in years.
#'
#' @return An ordered factor with levels:
#'   * `"Teenage (<20)"`
#'   * `"Adult (20-<35)"`
#'   * `"Advanced Maternal Age (35+)"`
#'
#' @examples
#' classify_maternal_age(c(17, 24, 36, NA))
#'
#' @export
classify_maternal_age <- function(age) {
  out <- dplyr::case_when(
    age < 20              ~ "Teenage (<20)",
    age >= 20 & age < 35  ~ "Adult (20-<35)",
    age >= 35             ~ "Advanced Maternal Age (35+)"
  )

  forcats::fct_relevel(
    factor(out),
    "Teenage (<20)",
    "Adult (20-<35)",
    "Advanced Maternal Age (35+)"
  )
}


# ------------------------------------------------------------
#  Job type
# ------------------------------------------------------------

#' Collapse raw job categories into three employment groups
#'
#' Collapses the granular `job_scorres` field into Formal Employment,
#' Informal Employment, Housewife, or Not Working.
#'
#' @param x Character or factor vector of raw job categories.
#'
#' @return A factor with four levels:
#'   `"Formal Employment"`, `"Informal Employment"`,
#'   `"House wife"`, `"Not working"`.
#'
#' @examples
#' classify_job_type(c("Salaried worker", "Small business", "Housewife",
#'                     "Not applicable/Not working"))
#'
#' @export
classify_job_type <- function(x) {
  forcats::fct_collapse(
    factor(x),
    "Formal Employment"   = "Salaried worker",
    "Informal Employment" = c(
      "Small business", "Business owner", "Skilled labor",
      "Unskilled labor", "Subsistence farming", "Commercial farming"
    ),
    "House wife"  = "Housewife",
    "Not working" = c("Not applicable/Not working", "Other")
  )
}


# ------------------------------------------------------------
#  Residence
# ------------------------------------------------------------

#' Classify facility location as rural or peri-urban
#'
#' Uses a pattern match on the facility name to assign a residence category.
#' Currently recognises Siaya / Tingwangi as Rural-Karemo and all other sites
#' as Peri-urban Manyata.
#'
#' @param facility_name Character vector of facility name strings
#'   (e.g. `scrn_fac_spfy_obsloc`).
#'
#' @return A factor with reference level `"Peri-urban Manyata"`.
#'
#' @examples
#' classify_residence(c("Siaya County Referral Hospital",
#'                      "Kisumu County Hospital",
#'                      "Tingwangi Health Centre"))
#'
#' @export
classify_residence <- function(facility_name) {
  out <- dplyr::if_else(
    stringr::str_detect(facility_name, "Siaya|Ting"),
    "Rural-Karemo",
    "Peri-urban Manyata"
  )
  forcats::fct_relevel(factor(out), "Peri-urban Manyata")
}


# ------------------------------------------------------------
#  Wall type
# ------------------------------------------------------------

#' Classify external wall material as dirt/mud or cement/brick
#'
#' @param x Character vector of raw wall material descriptions
#'   (e.g. `ext_wall_fcorres`).
#'
#' @return A character vector: `"Dirt/mud/dung"`, `"Cement/brick"`, or `NA`.
#'
#' @examples
#' classify_wall_type(c("Mud and dung", "Cement blocks", "Brick"))
#'
#' @export
classify_wall_type <- function(x) {
  x_lower <- stringr::str_to_lower(x)
  dplyr::case_when(
    stringr::str_detect(x_lower, "mud|dung|grass|wood|board") ~ "Dirt/mud/dung",
    stringr::str_detect(x_lower, "cement|brick")              ~ "Cement/brick"
  )
}


# ------------------------------------------------------------
#  Facility level
# ------------------------------------------------------------

#' Map facility names to Ministry of Health facility levels
#'
#' Translates a PRISMA facility name string into its corresponding MoH
#' tier (Level 2–6). The mapping is supplied as a named character vector
#' via `facility_map`, so studies at different sites can override the default.
#'
#' @param facility_name Character vector of facility name strings.
#' @param facility_map Named character vector where **names** are facility
#'   name strings and **values** are level labels (e.g. `"Level 4"`).
#'   Defaults to the six PRISMA Kenya sites.
#'
#' @return A character vector of facility level labels, or `NA` for
#'   unrecognised facilities.
#'
#' @examples
#' map_facility_level(c("JOOTRH", "Kuoyo Health Center"))
#'
#' @export
map_facility_level <- function(
    facility_name,
    facility_map = default_facility_map()
) {
  unname(facility_map[facility_name])
}


#' Default PRISMA Kenya facility-to-level lookup table
#'
#' Returns the built-in named vector mapping PRISMA Kenya facility names to
#' MoH facility levels. Pass your own named vector to [map_facility_level()]
#' to override for a different study site.
#'
#' @return A named character vector.
#' @export
default_facility_map <- function() {
  c(
    "JOOTRH"                         = "Level 6",
    "Kisumu County Hospital"          = "Level 5",
    "Kuoyo Health Center"             = "Level 2",
    "Lumumba Sub County Hospital"     = "Level 3",
    "Siaya County Referral Hospital"  = "Level 4",
    "Tingwangi Health Centre"         = "Level 3"
  )
}


# ------------------------------------------------------------
#  History of pregnancy loss
# ------------------------------------------------------------

#' Derive a binary indicator for history of pregnancy loss
#'
#' Returns `1` if the participant reported any prior stillbirth **or**
#' miscarriage, and `0` otherwise.
#'
#' @param stillbirth Character/factor vector coded `"Yes"` / `"No"`.
#' @param miscarriage Character/factor vector coded `"Yes"` / `"No"`.
#'
#' @return An integer vector of 0/1.
#'
#' @examples
#' derive_preg_loss(c("Yes", "No", "No"), c("No", "Yes", "No"))
#'
#' @export
derive_preg_loss <- function(stillbirth, miscarriage) {
  dplyr::if_else(
    stillbirth == "Yes" | miscarriage == "Yes",
    1L, 0L,
    missing = 0L
  )
}
