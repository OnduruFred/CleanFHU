# ============================================================
#  CleanFHU — Anemia and biomarker classifier helpers
# ============================================================


# ------------------------------------------------------------
#  Trimester derivation
# ------------------------------------------------------------

#' Derive pregnancy trimester from gestational age in weeks
#'
#' @param ga_weeks Numeric vector of gestational age in weeks at visit
#'   (e.g. `us_ga_visit`).
#'
#' @return A character vector: `"1st Trimester"`, `"2nd Trimester"`,
#'   `"3rd Trimester"`, or `NA` when `ga_weeks` is `NA`.
#'
#' @examples
#' classify_trimester(c(10, 20, 32, NA))
#'
#' @export
classify_trimester <- function(ga_weeks) {
  dplyr::case_when(
    ga_weeks < 14              ~ "1st Trimester",
    dplyr::between(ga_weeks, 14, 27.999) ~ "2nd Trimester",
    ga_weeks >= 28             ~ "3rd Trimester"
  )
}


# ------------------------------------------------------------
#  Anemia grading
# ------------------------------------------------------------

#' Classify anemia severity with trimester-specific haemoglobin thresholds
#'
#' Implements WHO trimester-adjusted anemia grading for pregnant women:
#'
#' | Severity | 1st / 3rd trimester Hb | 2nd trimester Hb |
#' |---|---|---|
#' | None | ≥ 11.0 | ≥ 10.5 |
#' | Mild | 10.0–10.9 | 9.5–10.4 |
#' | Moderate | 7.0–9.9 | 7.0–9.4 |
#' | Severe | < 7.0 | < 7.0 |
#'
#' @param hb Numeric vector of haemoglobin concentration in g/dL.
#' @param trimester Character vector of trimester labels as returned by
#'   [classify_trimester()], i.e. one of `"1st Trimester"`,
#'   `"2nd Trimester"`, `"3rd Trimester"`.
#'
#' @return A factor with levels ordered from least to most severe:
#'   `"None"`, `"Mild"`, `"Moderate"`, `"Severe"`.
#'
#' @examples
#' classify_anemia(c(12, 10.2, 8.5, 6.0), c("1st Trimester",
#'   "2nd Trimester", "3rd Trimester", "2nd Trimester"))
#'
#' @export
classify_anemia <- function(hb, trimester) {
  is_2nd <- trimester == "2nd Trimester"

  severity <- dplyr::case_when(
    hb < 7                                              ~ "Severe",
    is_2nd  & dplyr::between(hb, 7.0, 9.4)             ~ "Moderate",
    !is_2nd & dplyr::between(hb, 7.0, 9.9)             ~ "Moderate",
    is_2nd  & dplyr::between(hb, 9.5, 10.4)            ~ "Mild",
    !is_2nd & dplyr::between(hb, 10.0, 10.9)           ~ "Mild",
    is_2nd  & hb >= 10.5                                ~ "None",
    !is_2nd & hb >= 11.0                                ~ "None"
  )

  forcats::fct_relevel(factor(severity), "None", "Mild", "Moderate", "Severe")
}


#' Derive binary any-anemia indicator (trimester-adjusted)
#'
#' Returns `1L` if Hb is below the trimester-specific threshold, `0L`
#' otherwise. Thresholds: < 10.5 g/dL in 2nd trimester, < 11.0 g/dL
#' in 1st and 3rd trimesters.
#'
#' @inheritParams classify_anemia
#'
#' @return An integer vector of 0/1.
#'
#' @examples
#' derive_any_anemia(c(9.0, 11.5, 10.3), c("2nd Trimester",
#'   "1st Trimester", "3rd Trimester"))
#'
#' @export
derive_any_anemia <- function(hb, trimester) {
  dplyr::if_else(
    (trimester == "2nd Trimester" & hb < 10.5) |
    (trimester != "2nd Trimester" & hb < 11.0),
    1L, 0L
  )
}


# ------------------------------------------------------------
#  Micronutrient / biomarker classifiers
# ------------------------------------------------------------

#' Classify Vitamin B12 status from cobalamin concentration
#'
#' @param vitb12 Numeric vector of serum cobalamin in pmol/L.
#'
#' @return An ordered factor:
#'   `"deficient"` (< 150), `"insufficient"` (150–220), `"sufficient"` (> 220).
#'
#' @examples
#' classify_vitb12(c(100, 180, 300, NA))
#'
#' @export
classify_vitb12 <- function(vitb12) {
  out <- dplyr::case_when(
    vitb12 < 150                         ~ "deficient",
    dplyr::between(vitb12, 150, 220)     ~ "insufficient",
    vitb12 > 220                         ~ "sufficient"
  )
  forcats::fct_relevel(factor(out), "sufficient", "insufficient", "deficient")
}


#' Classify Vitamin A (retinol) status from RBP4 concentration
#'
#' @param rbp4 Numeric vector of retinol-binding protein 4 in µmol/L.
#'
#' @return An ordered factor:
#'   `"none deficient"` (≥ 1.05), `"mild deficient"` (0.70–1.049),
#'   `"moderate deficient"` (0.35–0.699), `"severe deficient"` (< 0.35).
#'
#' @examples
#' classify_vita(c(0.2, 0.5, 0.85, 1.1, NA))
#'
#' @export
classify_vita <- function(rbp4) {
  out <- dplyr::case_when(
    rbp4 < 0.35                            ~ "severe deficient",
    dplyr::between(rbp4, 0.35,  0.6999)   ~ "moderate deficient",
    dplyr::between(rbp4, 0.7,   1.0499)   ~ "mild deficient",
    rbp4 >= 1.05                           ~ "none deficient"
  )
  forcats::fct_relevel(
    factor(out),
    "none deficient", "mild deficient", "moderate deficient", "severe deficient"
  )
}


#' Classify iron status using soluble transferrin receptor (sTfR)
#'
#' Applies trimester-specific sTfR cut-offs to identify iron deficiency.
#'
#' | Trimester | sTfR threshold for iron deficiency |
#' |---|---|
#' | 1st | > 3.61 mg/L |
#' | 2nd | > 4.98 mg/L |
#' | 3rd | > 5.94 mg/L |
#'
#' @param stfr Numeric vector of sTfR in mg/L.
#' @param trimester Character vector of trimester labels (see [classify_trimester()]).
#'
#' @return An ordered factor: `"none deficient"` or `"iron deficient"`.
#'
#' @examples
#' classify_stfr(c(3.0, 5.5, 6.5),
#'               c("1st Trimester", "2nd Trimester", "3rd Trimester"))
#'
#' @export
classify_stfr <- function(stfr, trimester) {
  out <- dplyr::case_when(
    is.na(stfr)                                              ~ NA_character_,
    stfr > 3.61 & trimester == "1st Trimester"              ~ "iron deficient",
    stfr > 4.98 & trimester == "2nd Trimester"              ~ "iron deficient",
    stfr > 5.94 & trimester == "3rd Trimester"              ~ "iron deficient",
    TRUE                                                     ~ "none deficient"
  )
  forcats::fct_relevel(factor(out), "none deficient")
}


#' Classify iron deficiency using ferritin with inflammation correction
#'
#' Applies inflammation-adjusted ferritin thresholds:
#' * If **inflammation present** (CRP > 5 mg/L **or** AGP > 1 g/L):
#'   iron deficiency defined as ferritin < 70 µg/L.
#' * If **no inflammation**: iron deficiency defined as ferritin < 15 µg/L.
#'
#' @param ferritin Numeric vector of serum ferritin in µg/L.
#' @param crp Numeric vector of C-reactive protein in mg/L.
#' @param agp Numeric vector of alpha-1-acid glycoprotein in g/L.
#'
#' @return An ordered factor: `"none deficient"` or `"iron deficient"`.
#'
#' @examples
#' classify_iron_ferritin(
#'   ferritin = c(10, 50, 80),
#'   crp      = c(2, 8, 3),
#'   agp      = c(0.8, 1.2, 0.9)
#' )
#'
#' @export
classify_iron_ferritin <- function(ferritin, crp, agp) {
  inflamed <- crp > 5 | agp > 1

  out <- dplyr::case_when(
    is.na(ferritin) | is.na(inflamed)              ~ NA_character_,
    ferritin < 70 & inflamed                        ~ "iron deficient",
    ferritin < 15 & !inflamed                       ~ "iron deficient",
    TRUE                                            ~ "none deficient"
  )
  forcats::fct_relevel(factor(out), "none deficient")
}


#' Classify G6PD enzyme activity
#'
#' @param g6pd Numeric vector of G6PD activity in U/gHb.
#'   Values ≥ 6.1 are classified as normal.
#'
#' @return A character vector: `"normal G6PD"` or `"abnormal G6PD"`.
#'
#' @examples
#' classify_g6pd(c(7.0, 4.5, NA))
#'
#' @export
classify_g6pd <- function(g6pd) {
  dplyr::if_else(g6pd >= 6.1, "normal G6PD", "abnormal G6PD")
}


#' Derive any-thalassemia indicator from wide rbc_thala columns
#'
#' Scans across up to 18 binary `rbc_thala_*` columns and returns `"Yes"` if
#' any are coded `"Yes"`, otherwise `"No"`.
#'
#' @param df A data frame containing columns named `rbc_thala_1` through
#'   `rbc_thala_18` (or a subset thereof).
#'
#' @return A character vector: `"Yes"` or `"No"`.
#'
#' @examples
#' \dontrun{
#' df$thalassemia <- derive_thalassemia(df)
#' }
#'
#' @export
derive_thalassemia <- function(df) {
  thala_cols <- grep("^rbc_thala_", names(df), value = TRUE)

  if (length(thala_cols) == 0) {
    cli::cli_alert_warning(
      "No `rbc_thala_*` columns found. Returning NA."
    )
    return(rep(NA_character_, nrow(df)))
  }

  row_any_yes <- rowSums(
    dplyr::across(dplyr::all_of(thala_cols), ~ . == "Yes", .names = NULL),
    na.rm = TRUE
  ) > 0

  dplyr::if_else(row_any_yes, "Yes", "No")
}
