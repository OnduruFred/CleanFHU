# ============================================================
#  CleanFHU — prisma_pipeline()
#  Full enrollment + repeat-ANC cleaning pipeline for
#  PRISMA-aligned pregnancy study data.
# ============================================================


#' Clean and harmonise PRISMA study data into an analysis-ready tibble
#'
#' @description
#' `prisma_pipeline()` is the primary entry point of **CleanFHU**. It accepts
#' the raw, wide PRISMA data frame and returns a single tidy tibble that
#' combines:
#'
#' 1. **Enrollment visit** sociodemographic and clinical history variables
#'    (one row per pregnancy).
#' 2. **ANC visit** measurements joined onto the enrollment spine
#'    (one row per participant × visit).
#' 3. **IPC & PNC visit** measurements joined
#'
#'
#' @section Variables produced:
#' **Sociodemographic**
#' \describe{
#'   \item{`No_pregnancy`}{Gravidity category (factor, 3 levels).}
#'   \item{`No_ANC_visit`}{ANC visit frequency category (factor, 3 levels).}
#'   \item{`mum_age_cat_2`}{Maternal age group: Teenage / Adult / AMA.}
#'   \item{`Maternal_educ`}{Binary schooling: `"<10years"` / `"10+years"`.}
#'   \item{`job_type`}{Employment category (4 levels).}
#'   \item{`residence`}{`"Rural-Karemo"` or `"Peri-urban Manyata"`.}
#'   \item{`marital_scorres`}{Marital status collapsed to Married / Single/divorced/widowed.}
#'   \item{`wall_type`}{External wall material: Dirt/mud/dung or Cement/brick.}
#'   \item{`facility_level`}{MoH facility tier (Level 2–6).}
#'   \item{`water_source_class`}{WHO improved/unimproved water source.}
#'   \item{`toilet_class`}{WHO improved/unimproved sanitation.}
#'   \item{`cooking_fuel_class`}{WHO clean/polluting cooking fuel.}
#'   \item{`history_preg_loss`}{1/0 indicator of prior stillbirth or miscarriage.}
#' }
#'
#' **Clinical (ANC)**
#' \describe{
#'   \item{`trimester`}{Gestational trimester at each visit.}
#'   \item{`any_anemia`}{1/0 trimester-adjusted anemia indicator.}
#'   \item{`any_anemia_ovl`}{1/0 indicator: ever anaemic across all visits.}
#'   \item{`VitB_12`}{Vitamin B12 status (deficient / insufficient / sufficient).}
#'   \item{`VitA_def`}{Vitamin A status (4-level ordered factor).}
#'   \item{`STFR`}{Soluble transferrin receptor iron status.}
#'   \item{`iron_def`}{Ferritin + inflammation-adjusted iron deficiency.}
#'   \item{`inflamation_biom`}{Binary inflammation marker (CRP > 5 or AGP > 1).}
#'   \item{`G6PD`}{G6PD enzyme activity classification.}
#'   \item{`sickle_cell`}{Raw sickle cell field (passed through).}
#'   \item{`thalassemia`}{Any thalassemia indicator derived from `rbc_thala_*` columns.}
#' }
#'
#' @param data A factor data frame (or tibble) with lower case (clean_names) containing the raw PRISMA study data,
#'   with at minimum the columns used internally (see *Details*).
#'
#' @return An analytically tibble with women with documented IPC.
#'
#' @examples
#' \dontrun{
#' library(CleanFHU)
#'
#' # Importing data
#' Prisma_data <- readRDS("C:/Users/user/OneDrive - Kenya Medical Research Institute/Work Related/Work related - PRISMA 3/
#' Prisma-2-workload/Final Data/Harmonized Prisma df.rds") %>% clean_names() %>%
#' mutate_if(is.labelled, as_factor) %>% drop_na(momid) %>%
#' mutate_if(is.factor, ~ replace(.x, .x %in% c("77", "55", "Dont Know"), NA))
#'
#' # Basic usage
#' clean_data <- prisma_pipeline(Prisma_data)
#' }
#'
#' @seealso
#' Classifier helpers: [classify_gravidity()], [classify_anc_visits()],
#' [classify_maternal_age()], [classify_job_type()], [classify_residence()],
#' [classify_wall_type()], [map_facility_level()], [classify_water_source()],
#' [classify_toilet()], [classify_cooking_fuel()], [classify_anemia()],
#' [classify_vitb12()], [classify_vita()], [classify_stfr()],
#' [classify_iron_ferritin()], [classify_g6pd()], [derive_thalassemia()].
#'
#' @export

prisma_pipeline <- function(data) {
  # ── Input checks ──────────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a factor data frame or tibble with lowercase (clean_names).")
  }

  data %>%
    ## --- Demographic and Clinical section @ enrollment
    dplyr::mutate(
      ph_prevn_rporres = tidyr::replace_na(ph_prevn_rporres, 0),
      ph_prevn_rporres = ph_prevn_rporres + 1,
      No_pregnancy = case_when(
        ph_prevn_rporres == 1 ~ "1. Primigravida",
        ph_prevn_rporres %in% c(2:4) ~ "2. Been pregnant 2-4 times",
        ph_prevn_rporres >= 5 ~ "3. Been pregnant >4 times"
        #ph_prevn_rporres > 1 ~ "Multigravida"
      ) %>%
        forcats::fct_relevel(., "1. Primigravida"),
      No_ANC_visit = case_when(
        anc_tot_vists < 4 ~ "1. Less than 4times",
        between(anc_tot_vists, 4, 5) ~ "2. Between 4-5times",
        anc_tot_vists >= 6 ~ "3. Above 5times"
      ),
      job_type = forcats::fct_collapse(
        job_scorres,
        "Formal Employment" = "Salaried worker",
        "Informal Employment" = c(
          "Small business",
          "Business owner",
          "Skilled labor",
          "Unskilled labor",
          "Subsistence farming",
          "Commercial farming"
        ),
        "House wife" = "Housewife",
        "Not working" = c("Not applicable/Not working", "Other")
      ),
      residence = if_else(
        stringr::str_detect(scrn_fac_spfy_obsloc, "Siaya|Ting") == T,
        "Rural-Karemo",
        "Peri-urban Manyata"
      ) %>%
        forcats::fct_relevel(., "Peri-urban Manyata"),
      floor_fcorres = fct_lump_n(floor_fcorres, n = 4) %>%
        forcats::fct_relevel(., "Cement/concrete") %>%
        replace(., . == "Other", NA),
      wall_type = case_when(
        stringr::str_detect(
          str_to_lower(ext_wall_fcorres),
          "mud|dung|grass|wood|board"
        ) ~ "Dirt/mud/dung",
        stringr::str_detect(
          str_to_lower(ext_wall_fcorres),
          "cement|brick"
        ) ~ "Cement/brick"
      ),
      facility_level = case_when(
        scrn_fac_spfy_obsloc == "JOOTRH" ~ "Level 6",
        scrn_fac_spfy_obsloc == "Kisumu County Hospital" ~ "Level 5",
        scrn_fac_spfy_obsloc == "Kuoyo Health Center" ~ "Level 2",
        scrn_fac_spfy_obsloc == "Lumumba Sub County Hospital" ~ "Level 3",
        scrn_fac_spfy_obsloc == "Siaya County Referral Hospital" ~ "Level 4",
        scrn_fac_spfy_obsloc == "Tingwangi Health Centre" ~ "Level 3"
      ),
      marital_scorres_old = marital_scorres,
      marital_scorres = forcats::fct_collapse(
        marital_scorres,
        "Single/divorsed/widowed" = c(
          "Not married, but living with partner (cohabitating)",
          "Divorced or permanently separated",
          "Widowed",
          "Single, never married"
        )
      ),
      history_preg_loss = if_else(
        stillbirth_rporres == "Yes" |
          miscarriage_rporres == "Yes",
        1,
        0
      ),

      ##  Water sourse
      water_source_class = case_when(
        # Improved
        grepl("Piped water into dwelling", h2o_fcorres) ~ "Improved",
        grepl("Piped to yard/plot", h2o_fcorres) ~ "Improved",
        grepl("Public tap|standpipe", h2o_fcorres) ~ "Improved",
        grepl("Tube well|borehole", h2o_fcorres) ~ "Improved",
        grepl("Protected well", h2o_fcorres) ~ "Improved",
        grepl("Water from spring \\(protected\\)", h2o_fcorres) ~ "Improved",
        grepl("Rainwater", h2o_fcorres) ~ "Improved",
        grepl("Bottled water|sachets", h2o_fcorres) ~ "Improved",

        # Unimproved
        grepl("Unprotected well", h2o_fcorres) ~ "Unimproved",
        grepl(
          "Water from spring \\(unprotected\\)",
          h2o_fcorres
        ) ~ "Unimproved",
        grepl("Tanker truck", h2o_fcorres) ~ "Unimproved",
        grepl("Cart with small tank", h2o_fcorres) ~ "Unimproved",
        grepl("Surface water", h2o_fcorres) ~ "Unimproved",
        grepl("Water vendor", h2o_fcorres) ~ "Unimproved",
        grepl("Other", h2o_fcorres) ~ "Unimproved",

        TRUE ~ NA_character_
      ),
      ## Toilet
      toilet_class = case_when(
        # Improved sanitation facilities
        grepl(
          "Flush/pour flush to piped sewer system",
          toilet_fcorres
        ) ~ "Improved",
        grepl("Flush/pour flush to septic tank", toilet_fcorres) ~ "Improved",
        grepl("Flush/pour flush to pit latrine", toilet_fcorres) ~ "Improved",
        grepl("Ventilated improved pit latrine", toilet_fcorres) ~ "Improved",
        grepl("Pit latrine with slab", toilet_fcorres) ~ "Improved",
        grepl("Composting toilet", toilet_fcorres) ~ "Improved",

        # Unimproved / Open defecation
        grepl(
          "Flush/pour flush to somewhere else",
          toilet_fcorres
        ) ~ "Unimproved",
        grepl(
          "Flush/pour flush, don't know where",
          toilet_fcorres
        ) ~ "Unimproved",
        grepl(
          "Pit latrine without slab|open pit",
          toilet_fcorres
        ) ~ "Unimproved",
        grepl("Bucket toilet", toilet_fcorres) ~ "Unimproved",
        grepl(
          "Hanging toilet|Hanging latrine",
          toilet_fcorres
        ) ~ "Unimproved",
        grepl("No facility|bush|field", toilet_fcorres) ~ "Unimproved",
        grepl("Other", toilet_fcorres) ~ "Unimproved",

        TRUE ~ NA_character_
      ),
      cooking_fuel_class = case_when(
        # Clean / Improved cooking fuels
        grepl("Electric stove", stove_fcorres) ~ "Improved",
        grepl("Solar cooker", stove_fcorres) ~ "Improved",
        grepl(
          "Liquified petroleum gas|Liquefied petroleum gas|LPG",
          stove_fcorres
        ) ~ "Improved",
        grepl("Piped natural gas", stove_fcorres) ~ "Improved",
        grepl("Biogas", stove_fcorres) ~ "Improved",

        # Polluting / Unimproved cooking fuels
        grepl("Coal pot", stove_fcorres) ~ "Unimproved",
        grepl("Charcoal", stove_fcorres) ~ "Unimproved",
        grepl("Kerosene", stove_fcorres) ~ "Unimproved",
        grepl("Wood", stove_fcorres) ~ "Unimproved",
        grepl("Crop residue|Crop residues", stove_fcorres) ~ "Unimproved",
        grepl("Animal dung", stove_fcorres) ~ "Unimproved",
        grepl("Manufactured solid fuel", stove_fcorres) ~ "Unimproved",
        grepl("Traditional solid fuel", stove_fcorres) ~ "Unimproved",
        grepl("Three stone stove|Open fire", stove_fcorres) ~ "Unimproved",
        grepl("Liquid fuel stove", stove_fcorres) ~ "Unimproved",
        grepl("Other", stove_fcorres) ~ "Unimproved",

        # Not applicable
        grepl("No food cooked in household", stove_fcorres) ~ NA_character_,

        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate_at(vars(No_pregnancy, No_ANC_visit), as.factor) %>%
    group_by(momid) %>%
    fill(
      No_ANC_visit,
      .direction = "updown"
    ) %>%
    ungroup() %>%
    dplyr::mutate_if(is.labelled, as_factor) %>%
    filter(type_visit == "Enrollment") %>%
    select(
      momid, #type_visit,
      pregid,
      mum_age,
      mum_age_cat,
      singleton_peres,
      ph_prevn_rporres,
      No_pregnancy,
      anc_tot_vists,
      No_ANC_visit,
      job_type,
      residence,
      ses_quintile,
      house_occ_tot_fcorres,
      currepregn_desire_yn,
      marital_scorres_old,
      marital_scorres,
      school_yrs_scorres,
      school_level,
      mosquito_net_fcorres,
      floor_fcorres,
      ext_wall_fcorres,
      insect_lstnight_obsoccur,
      roof_fcorres,
      ## --- Previous Clinical hist
      previous_pph,
      previous_aph,
      previous_macrosomia,
      previous_preterm,
      previous_lowbirthwt,
      previous_interuter_growth,
      previous_premature_rupture,
      previous_stillbirth,
      previous_miscarriage,
      rh_factor_lborres,
      macrosomia_rporres,
      lowbirthwt_rporres,
      cesarian_rporres,
      unpl_cesarian_proccur,
      preterm_rporres,
      history_preg_loss,
      facility_level,
      water_source_class,
      toilet_class,
      cooking_fuel_class
    ) %>%
    dplyr::mutate(
      mum_age_cat_2 = case_when(
        mum_age < 20 ~ "Teenage (<20)",
        mum_age >= 20 & mum_age < 35 ~ "Adult (20-<35)",
        mum_age >= 35 ~ "Advanced Maternal Age (35+)"
      ) %>%
        forcats::fct_relevel(
          .,
          "Teenage (<20)",
          "Adult (20-<35)",
          "Advanced Maternal Age (35+)"
        ),
      Maternal_educ = if_else(school_yrs_scorres < 10, "<10years", "10+years") %>%
        fct_rev(.),
      school_level = fct_explicit_na(school_level, na_level = "None"),
      school_yrs_scorres = tidyr::replace_na(school_yrs_scorres, 0)
    ) %>%
    mutate(across(
      contains("previous_"),
      ~ fct_explicit_na(., na_level = "No")
    )) %>%

    inner_join(
      ## --- Pregnancy Outcomes section
      data %>%
        drop_na(pregnancy_outcome) %>%
        select(
          momid,
          pregid,
          pregnancy_outcome,
          preterm_birth,
          low_brth_weight_cat,
          lowbwt,
          macrosomia,
          apgar_5m_peres,
          still_birth,
          miscarriage,
          deliv_prroute,
          unplanned_cesarian,
          induced_proccur,
          term_deliv_status,
          apgar_1min_peres,
          apgar_5m_peres,
          con_malform_mhoccur,
          pph_ceoccur,
          hdp_htn_mhoccur_3,
          del_comp_mhoccur_2,
          shortbreath_ceoccur,
          obstructed_labor_ceoccur,
        )
    ) %>%
    dplyr::mutate(
      stillbirth = case_when(
        pregnancy_outcome == "Stillbirth" ~ 1,
        pregnancy_outcome == "Miscarriage" ~ NA,
        TRUE ~ 0
      ),
      miscarriage = tidyr::replace_na(miscarriage, "No"),
      miscarriage_n = if_else(
        pregnancy_outcome == "Miscarriage",
        1,
        0
      ),
      PTB = case_when(
        preterm_birth == "Yes" ~ 1,
        preterm_birth == "No" ~ 0,
        TRUE ~ NA_integer_
      ) %>%
        as.integer(.),
      LBW = case_when(
        lowbwt == "Yes" ~ 1,
        lowbwt == "No" ~ 0,
        TRUE ~ NA_integer_
      ) %>%
        as.integer(.),
      deliv_prroute = if_else(
        str_squish(deliv_prroute) == "Undelivered",
        NA,
        deliv_prroute
      ),
      current_preg_loss = if_else(
        pregnancy_outcome %in% c("Stillbirth", "Miscarriage"),
        "Yes",
        "No"
      ),
      ## Replace None CS to no
      unplanned_cesarian = tidyr::replace_na(unplanned_cesarian, "No")
    ) %>%


    left_join(
      ## --- Repeat ANC measurements section
      data %>%
        filter(stringr::str_detect(type_visit, regex("ANC|Enrol", ignore_case = TRUE))) %>%
        dplyr::mutate(
          trimester = case_when(
            us_ga_visit < 14 ~ "1st Trimester",
            us_ga_visit >= 14 & us_ga_visit < 28 ~ "2nd Trimester",
            us_ga_visit >= 28 ~ "3rd Trimester"
          )
        ) %>%
        select(
          momid,
          pregid,
          type_visit,
          trimester,
          anemia_poc,
          anemia_poc_yn,
          bmi,
          bmi_category,
          muac_peres,
          muac_category,
          hiv_poc_lborres,
          malaria_poc_lborres,
          bp_sys_vsorres,
          bp_dia_vsorres,
          cbc_hb_lborres,
          hb_poc_lborres,
          syph_poc_lborres,
          hbv_poc_lborres,
          hcv_poc_lborres,
          ctng_ct_lborres,
          ctng_ng_lborres,
          us_ga_visit,
          maternal_morbidity,
          any_hpd,
          chronic_hyper,
          gesta_hyp,
          preclampsia,
          eclampsia,
          hyperten_ga,
          vitb12_cob_lborres,
          rbp4_lborres,
          ferritin_lborres,
          crp_lborres,
          agp_lborres,
          transferrin_lborres,
          folate_rbc_nmoll_lborres,
          folate_plasma_nmoll_lborres,
          rbc_g6pd_lborres,
          rbc_sickle_lborres,
          contains("rbc_thala_")
        ),
      relationship = "many-to-many"
    ) %>%

    full_join(
      data %>%
        filter(
          stringr::str_detect(type_visit, regex("ANC|Enrol", ignore_case = TRUE)) == FALSE
        ) %>%
        group_by(pregid) %>%
        fill(pregnancy_outcome, .direction = "updown") %>%
        ungroup() %>%
        dplyr::mutate(
          death_cat = if_else(
            pregnancy_outcome != "Live Birth",
            NA_character_,
            death_cat
          ),
          early_neonate_dth = if_else(
            death_cat %in% c("Death <24hours", "Early neonatal death(1-7days)"),
            1,
            if_else(pregnancy_outcome == "Live Birth", 0, NA_real_)
          )
        ) %>%
        group_by(pregid, infantid) %>%
        fill(death_cat, early_neonate_dth, .direction = "downup") %>%
        ungroup() %>%
        drop_na(pregnancy_outcome) %>%
        filter(stringr::str_detect(type_visit, regex("PNC|IPC", ignore_case = TRUE))) %>%
        select(
          pregid,
          momid,
          infantid,
          #type_visit,
          neonatal_death,
          early_neonate_dth,
          death_cat,
          neonat_infant_death,
          neonatal_death_date,
          nbu_admission
        ) %>%
        drop_na(infantid) %>%
        distinct(pregid, infantid, .keep_all = T),
      relationship = "many-to-many"
    ) %>%
    group_by(pregid) %>%
    dplyr::mutate(
      Chlamydia = if_else(ctng_ct_lborres == "Positive", 1, NA_integer_),
      gonorrhea = if_else(ctng_ng_lborres == "Positive", 1, NA_integer_),
      co_chlym_gono = if_else(
        ctng_ng_lborres == "Positive" & ctng_ct_lborres == "Positive",
        1,
        NA_integer_
      ),
    ) %>%
    fill(
      preterm_birth,
      maternal_morbidity,
      nbu_admission,
      deliv_prroute,
      obstructed_labor_ceoccur,
      any_hpd,
      chronic_hyper,
      gesta_hyp,
      Chlamydia,
      gonorrhea,
      co_chlym_gono,
      .direction = "updown"
    ) %>%
    dplyr::mutate(
      Chlamydia = replace(
        Chlamydia,
        ctng_ct_lborres == "Negative" & is.na(Chlamydia),
        0
      ),
      gonorrhea = replace(
        gonorrhea,
        ctng_ng_lborres == "Negative" & is.na(gonorrhea),
        0
      ),
      co_chlym_gono = replace(
        gonorrhea,
        (ctng_ct_lborres == "Negative" | ctng_ng_lborres == "Negative") &
          is.na(co_chlym_gono),
        0
      ),
      chronic_hyper = tidyr::replace_na(chronic_hyper, 0),
      gesta_hyp = tidyr::replace_na(gesta_hyp, 0),
      any_hpd = tidyr::replace_na(any_hpd, 0)
    ) %>%
    fill(Chlamydia, gonorrhea, co_chlym_gono, .direction = "updown") %>%
    ungroup() %>%

    dplyr::mutate(
      apgar_1min_peres = as.integer(apgar_1min_peres),
      apgar_5m_peres = as.integer(as.character(apgar_5m_peres)) * 1.0,
      APGAR1min = if_else(
        apgar_1min_peres < 7,
        "APGAR score <7",
        "APGAR score ≥7"
      ) %>%
        forcats::fct_relevel(., "APGAR score ≥7"),
      APGAR5min = if_else(
        apgar_5m_peres < 7,
        "APGAR score <7",
        "APGAR score ≥7"
      ) %>%
        forcats::fct_relevel(., "APGAR score ≥7"),
      hiv_poc_result = if_else(hiv_poc_lborres == "Positive", 1, 0),
      age_cat = case_when(
        mum_age < 25 ~ "<25yrs",
        mum_age >= 25 & mum_age < 30 ~ "25 - <30",
        mum_age >= 30 & mum_age < 35 ~ "30 -<35",
        mum_age >= 35 ~ ">=35yrs"
      ),
      school_level_new = forcats::fct_collapse(
        school_level,
        Primary = c("Primary incomplete", "Primary complete"),
        "Secondary" = c(
          "Secondary Complete",
          "Secondary Incomplete"
        ),
        "Tertiary" = c(
          "Tertiary incomplete",
          "Tertiary complete"
        )
      ),
    ) %>%
    group_by(pregid, infantid) %>%
    fill(neonatal_death, early_neonate_dth, death_cat, .direction = "downup") %>%
    ungroup() %>%
    set_variable_labels(
      pregid = "pregid",
      unpl_cesarian_proccur = "Previous Emergent CS",
      history_preg_loss = "Previous Pregnancy loss",

      preterm_birth = "Current Preterm birth",
      lowbwt = "Current Lowbirth weight",
      macrosomia = "Current Macrosomia",
      unplanned_cesarian = "Current Emergent CS",
      miscarriage_n = "Current Miscarrage",
      current_preg_loss = "Current Pregnancy loss",
      stillbirth = "Current stillbirth",
      miscarriage = "Current miscarriage"
    ) %>%

    ## --- dplyr::mutate anemia by trimester
    dplyr::mutate(
      ##--- Incooperate trimester variance
      cbc_hb_lborres = coalesce(cbc_hb_lborres, hb_poc_lborres),
      any_mild = if_else(
        trimester == "2nd Trimester" & between(cbc_hb_lborres, 9.5, 10.4),
        "Yes",
        "No"
      ),
      any_moderate = if_else(
        trimester == "2nd Trimester" & between(cbc_hb_lborres, 7, 9.4),
        "Yes",
        "No"
      ),
      any_severe = if_else(cbc_hb_lborres < 7, "Yes", "No"),
      any_anemia = if_else(
        trimester == "2nd Trimester" & cbc_hb_lborres < 10.5,
        "Yes",
        "No"
      ),

      any_mild = if_else(
        trimester != "2nd Trimester" & between(cbc_hb_lborres, 10, 10.9),
        "Yes",
        any_mild
      ),
      any_moderate = if_else(
        trimester != "2nd Trimester" & between(cbc_hb_lborres, 7, 9.9),
        "Yes",
        any_moderate
      ),
      any_anemia = if_else(
        trimester != "2nd Trimester" & cbc_hb_lborres < 11,
        "Yes",
        any_anemia
      ),

      ## --- Biomarkers
      VitB_12 = case_when(
        vitb12_cob_lborres < 150 ~ "deficient",
        between(vitb12_cob_lborres, 150, 220) ~ "insufficient",
        vitb12_cob_lborres > 220 ~ "sufficient"
      ),
      VitA_def = case_when(
        rbp4_lborres < 0.35 ~ "severe deficient",
        between(rbp4_lborres, 0.35, 0.6999) ~ "moderate deficient",
        between(rbp4_lborres, 0.7, 1.0499) ~ "mild deficient",
        rbp4_lborres >= 1.05 ~ "none deficient"
      ),
      STFR = case_when(
        is.na(transferrin_lborres) ~ NA_character_,
        transferrin_lborres > 3.61 &
          trimester == "1st Trimester" ~ "iron deficient",
        transferrin_lborres > 4.98 &
          trimester == "2nd Trimester" ~ "iron deficient",
        transferrin_lborres > 5.94 &
          trimester == "3rd Trimester" ~ "iron deficient",
        TRUE ~ "none deficient"
      ),
      serum_folat_def = if_else(folate_plasma_nmoll_lborres < 10, "Yes (<10)", "No"),
      rbc_folat_les784 = if_else(folate_rbc_nmoll_lborres < 784, "Yes (<784)", "No"),
      inflamation_biom = if_else(crp_lborres > 5 | agp_lborres > 1, "Yes", "No"),
      inflammation_CRP = if_else(crp_lborres > 5, "Inflammation", "Normal"),
      inflammation_AGP = if_else(agp_lborres > 1, "Inflammation", "Normal"),
      iron_def_ferritin_stfr = if_else(
        ferritin_lborres < 15 | transferrin_lborres > 8.3,
        "iron deficiency",
        "none iron deficiency"
      ),
      iron_def = case_when(
        is.na(ferritin_lborres) | is.na(inflamation_biom) ~ NA_character_,
        (ferritin_lborres < 70 & inflamation_biom == "Yes") ~ "iron deficient",
        (ferritin_lborres < 15 & inflamation_biom == "No") ~ "iron deficient",
        TRUE ~ "none deficient"
      ),
      G6PD = if_else(rbc_g6pd_lborres >= 6.1, "normal G6PD", "abnormal G6PD"),
      sickle_cell = rbc_sickle_lborres
    ) %>%
    dplyr::mutate(
      thalassemia = if_else(
        rowSums(
          across(
            c(
              rbc_thala_1,
              rbc_thala_2,
              rbc_thala_3,
              rbc_thala_4,
              rbc_thala_5,
              rbc_thala_6,
              rbc_thala_7,
              rbc_thala_8,
              rbc_thala_9,
              rbc_thala_10,
              rbc_thala_11,
              rbc_thala_12,
              rbc_thala_13,
              rbc_thala_14,
              rbc_thala_15,
              rbc_thala_16,
              rbc_thala_17,
              rbc_thala_18
            ),
            ~ . == "Yes"
          ),
          na.rm = TRUE
        ) >
          0,
        "Yes",
        "No"
      )
    ) %>%
    dplyr::mutate(
      any_anemia_ovl = if_else(any_anemia == "Yes", 1, NA),
      .after = any_anemia,
      any_anemia = if_else(any_anemia == "Yes", 1, 0)
    ) %>%
    group_by(pregid) %>%
    fill(any_anemia_ovl, .direction = "updown") %>%
    ungroup() %>%
    dplyr::mutate(
      any_anemia_ovl = tidyr::replace_na(any_anemia_ovl, 0)
    ) %>%
    dplyr::mutate_at(
      vars(VitB_12, VitA_def, STFR, iron_def),
      ~ forcats::fct_relevel(.x, "none deficient", "sufficient")
    )
}

