# ============================================================
#  CleanFHU — WASH classifier helpers
#  Water source, toilet, and cooking fuel classification
#  following JMP / WHO improved vs unimproved definitions.
# ============================================================


#' Classify drinking water source as improved or unimproved
#'
#' Applies the WHO/JMP definitions of improved and unimproved drinking water
#' sources to a free-text or coded water source field.
#'
#' **Improved sources** include piped water (into dwelling, yard, or public
#' standpipe), tube wells/boreholes, protected wells or springs, rainwater
#' collection, and bottled/sachet water.
#'
#' **Unimproved sources** include unprotected wells or springs, tanker trucks,
#' cart vendors, surface water, and water vendors.
#'
#' @param x Character vector of water source descriptions
#'   (e.g. `h2o_fcorres`).
#'
#' @return A character vector: `"Improved"`, `"Unimproved"`, or `NA`.
#'
#' @references
#' WHO/UNICEF Joint Monitoring Programme (JMP) for Water Supply, Sanitation
#' and Hygiene. <https://washdata.org/>
#'
#' @examples
#' classify_water_source(c("Piped water into dwelling",
#'                          "Unprotected well",
#'                          "Bottled water|sachets"))
#'
#' @export
classify_water_source <- function(x) {
  dplyr::case_when(
    # --- Improved --------------------------------------------------
    grepl("Piped water into dwelling",          x) ~ "Improved",
    grepl("Piped to yard/plot",                 x) ~ "Improved",
    grepl("Public tap|standpipe",               x) ~ "Improved",
    grepl("Tube well|borehole",                 x) ~ "Improved",
    grepl("Protected well",                     x) ~ "Improved",
    grepl("Water from spring \\(protected\\)",  x) ~ "Improved",
    grepl("Rainwater",                          x) ~ "Improved",
    grepl("Bottled water|sachets",              x) ~ "Improved",
    # --- Unimproved ------------------------------------------------
    grepl("Unprotected well",                   x) ~ "Unimproved",
    grepl("Water from spring \\(unprotected\\)",x) ~ "Unimproved",
    grepl("Tanker truck",                       x) ~ "Unimproved",
    grepl("Cart with small tank",               x) ~ "Unimproved",
    grepl("Surface water",                      x) ~ "Unimproved",
    grepl("Water vendor",                       x) ~ "Unimproved",
    grepl("Other",                              x) ~ "Unimproved",
    TRUE                                           ~ NA_character_
  )
}


#' Classify sanitation facility as improved or unimproved
#'
#' Applies the WHO/JMP definitions to a toilet/latrine type field.
#'
#' **Improved facilities** include flush/pour-flush toilets connected to a
#' sewer, septic tank, or pit; ventilated improved pit latrines (VIP);
#' pit latrines with slabs; and composting toilets.
#'
#' **Unimproved facilities** include flush toilets discharging to an unknown
#' or undesirable destination, open pits, pit latrines without slabs, bucket
#' toilets, hanging latrines, and open defecation.
#'
#' @param x Character vector of toilet type descriptions
#'   (e.g. `toilet_fcorres`).
#'
#' @return A character vector: `"Improved"`, `"Unimproved"`, or `NA`.
#'
#' @references
#' WHO/UNICEF JMP. <https://washdata.org/>
#'
#' @examples
#' classify_toilet(c("Pit latrine with slab",
#'                    "No facility|bush|field",
#'                    "Composting toilet"))
#'
#' @export
classify_toilet <- function(x) {
  dplyr::case_when(
    # --- Improved --------------------------------------------------
    grepl("Flush/pour flush to piped sewer system", x) ~ "Improved",
    grepl("Flush/pour flush to septic tank",        x) ~ "Improved",
    grepl("Flush/pour flush to pit latrine",        x) ~ "Improved",
    grepl("Ventilated improved pit latrine",        x) ~ "Improved",
    grepl("Pit latrine with slab",                  x) ~ "Improved",
    grepl("Composting toilet",                      x) ~ "Improved",
    # --- Unimproved ------------------------------------------------
    grepl("Flush/pour flush to somewhere else",     x) ~ "Unimproved",
    grepl("Flush/pour flush, don't know where",     x) ~ "Unimproved",
    grepl("Pit latrine without slab|open pit",      x) ~ "Unimproved",
    grepl("Bucket toilet",                          x) ~ "Unimproved",
    grepl("Hanging toilet|Hanging latrine",         x) ~ "Unimproved",
    grepl("No facility|bush|field",                 x) ~ "Unimproved",
    grepl("Other",                                  x) ~ "Unimproved",
    TRUE                                               ~ NA_character_
  )
}


#' Classify cooking fuel as improved (clean) or unimproved (polluting)
#'
#' Applies WHO household energy criteria. Clean fuels (electricity, solar,
#' LPG, piped natural gas, biogas) are classified as **Improved**. Solid,
#' liquid, and biomass fuels associated with household air pollution are
#' classified as **Unimproved**. Households that do not cook are returned
#' as `NA`.
#'
#' @param x Character vector of cooking stove/fuel descriptions
#'   (e.g. `stove_fcorres`).
#'
#' @return A character vector: `"Improved"`, `"Unimproved"`, or `NA`.
#'
#' @references
#' WHO. Household air pollution.
#' <https://www.who.int/news-room/fact-sheets/detail/household-air-pollution-and-health>
#'
#' @examples
#' classify_cooking_fuel(c("Electric stove", "Charcoal", "LPG", "Wood"))
#'
#' @export
classify_cooking_fuel <- function(x) {
  dplyr::case_when(
    # --- Improved (clean) ------------------------------------------
    grepl("Electric stove",                                x) ~ "Improved",
    grepl("Solar cooker",                                  x) ~ "Improved",
    grepl("Liqui[sf]ied petroleum gas|LPG",                x) ~ "Improved",
    grepl("Piped natural gas",                             x) ~ "Improved",
    grepl("Biogas",                                        x) ~ "Improved",
    # --- Unimproved (polluting) ------------------------------------
    grepl("Coal pot",                                      x) ~ "Unimproved",
    grepl("Charcoal",                                      x) ~ "Unimproved",
    grepl("Kerosene",                                      x) ~ "Unimproved",
    grepl("Wood",                                          x) ~ "Unimproved",
    grepl("Crop residue",                                  x) ~ "Unimproved",
    grepl("Animal dung",                                   x) ~ "Unimproved",
    grepl("Manufactured solid fuel",                       x) ~ "Unimproved",
    grepl("Traditional solid fuel",                        x) ~ "Unimproved",
    grepl("Three stone stove|Open fire",                   x) ~ "Unimproved",
    grepl("Liquid fuel stove",                             x) ~ "Unimproved",
    grepl("Other",                                         x) ~ "Unimproved",
    # --- Not applicable --------------------------------------------
    grepl("No food cooked in household",                   x) ~ NA_character_,
    TRUE                                                      ~ NA_character_
  )
}
