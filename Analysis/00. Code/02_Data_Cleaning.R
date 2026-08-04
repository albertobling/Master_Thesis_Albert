# Data Cleaning ################################################################

# 00 Dependencies ##############################################################

# Loading packages
library(lubridate)
library(tidyverse)

# Clearing environment
rm(list = ls())

# Setting working directory to analysis folder. Must be replaced to reproduce the analysis
setwd('/Users/albertobling/Desktop/Statskundskab/Kandidat/Masters Thesis/Analysis')

# Underlying folder structure:
# Analysis
  # 00. Code
  # 01. Raw data
  # 02. Clean data
  # 03. Output
    # Appendix
      # Figures
      # Tables
    # Paper
      # Figures

# 01 Importing data ############################################################

# Importing raw data and design file with candidate profiles
data_raw <- read_csv('01. Raw data/data_raw.csv')
design   <- read_csv('01. Raw data/choice_questions.csv')

# 02 Helper functions ##########################################################

# Creating a helper function to recode candidate attributes and translate them into English.
# Used below when building the separate conjoint and vignette datasets.
recode_candidate_attrs <- function(df) {
  df %>%
    mutate(
      cand_scandal = factor(cand_scandal,
                            levels = c(
                              "Ingen omtale i medierne for nylig",
                              "Beskyldt for seksuelt upassende adfærd til en fest, men har benægtet",
                              "Beskyldt for seksuelt upassende adfærd til en fest og har undskyldt",
                              "Beskyldt for at misbruge offentlige midler til private formål, men har benægtet",
                              "Beskyldt for at misbruge offentlige midler til private formål og har undskyldt"
                            ),
                            labels = c(
                              "No scandal",
                              "Personal - denial",
                              "Personal - apology",
                              "Office-related - denial",
                              "Office-related - apology"
                            )
      ),
      scandal_any = factor(
        ifelse(cand_scandal == "No scandal", 0, 1),
        levels = c(0, 1),
        labels = c("No scandal", "Scandal")
      ),
      scandal_type = factor(
        case_when(
          cand_scandal == "No scandal"                                                        ~ "No scandal",
          cand_scandal %in% c("Personal - denial", "Personal - apology")                     ~ "Personal",
          cand_scandal %in% c("Office-related - denial", "Office-related - apology")         ~ "Office-related"
        ),
        levels = c("No scandal", "Personal", "Office-related")
      ),
      scandal_reaction = factor(
        case_when(
          cand_scandal == "No scandal"                                                        ~ "No scandal",
          cand_scandal %in% c("Personal - denial", "Office-related - denial")                ~ "Denial",
          cand_scandal %in% c("Personal - apology", "Office-related - apology")              ~ "Apology"
        ),
        levels = c("No scandal", "Denial", "Apology")
      ),
      cand_reputation = factor(cand_reputation,
                               levels = c(
                                 "Har hverken et negativt eller positivt omdømme",
                                 "Anses bredt som kompetent og i stand til at levere resultater"
                               ),
                               labels = c("Neutral", "Competent")
      ),
      cand_gender = factor(cand_gender,
                           levels = c("Mand", "Kvinde"),
                           labels = c("Man", "Woman")
      ),
      cand_age = factor(cand_age,
                        levels = c("35 år", "45 år", "55 år"),
                        labels = c("35", "45", "55")
      ),
      cand_immig = factor(cand_immig,
                          levels = c(
                            "Udlændingepolitikken bør fastholdes som i dag",
                            "Udlændingepolitikken bør være strammere end i dag",
                            "Udlændingepolitikken bør være mere åben end i dag"
                          ),
                          labels = c("Stay as today", "More restrictive", "Less restrictive")
      ),
      cand_econ = factor(cand_econ,
                         levels = c(
                           "Skatterne og den offentlige velfærd bør fastholdes på det nuværende niveau",
                           "Skatterne bør sænkes, selvom det betyder mindre offentlig velfærd",
                           "Den offentlige velfærd bør øges, selvom det betyder højere skatter"
                         ),
                         labels = c("Same level", "Lower taxes", "More public welfare")
      )
    )
}

# Creating a helper function to compute policy congruence between respondent and candidate.
# Used below when building the separate conjoint and vignette datasets.
add_congruence <- function(df) {
  df %>%
    mutate(
      immig_rank = case_when(
        cand_immig == "More restrictive" ~ immig_rank_strict,
        cand_immig == "Stay as today"    ~ immig_rank_statusquo,
        cand_immig == "Less restrictive" ~ immig_rank_open
      ),
      econ_rank = case_when(
        cand_econ == "Lower taxes"       ~ econ_rank_low,
        cand_econ == "Same level"     ~ econ_rank_statusquo,
        cand_econ == "More public welfare" ~ econ_rank_high
      ),
      congruence_sum = immig_rank + econ_rank,
      congruence = factor(
        case_when(
          congruence_sum <= 3 ~ "High",
          congruence_sum == 4 ~ "Medium",
          congruence_sum >= 5 ~ "Low"
        ),
        levels = c("Low", "Medium", "High")
      ),
      immig_match = factor(
        ifelse(immig_rank == 1, 1, 0),
        levels = c(0, 1),
        labels = c("No match (Immigration)", "Match (Immigration)")
      ),
      econ_match = factor(
        ifelse(econ_rank == 1, 1, 0),
        levels = c(0, 1),
        labels = c("No match (Economy)", "Match (Economy)")
      )
    )
}

# Creating a helper function to attach figure axis labels to the candidate attributes
attach_attr_labels <- function(df) {
  attr(df$cand_scandal,    "label") <- "Scandal"
  attr(df$scandal_any,     "label") <- "Scandal"
  attr(df$scandal_type,    "label") <- "Scandal type"
  attr(df$scandal_reaction,"label") <- "Scandal reaction"
  attr(df$cand_reputation, "label") <- "Competence"
  attr(df$cand_gender,     "label") <- "Gender"
  attr(df$cand_age,        "label") <- "Age"
  attr(df$cand_immig,      "label") <- "Immigration policy"
  attr(df$cand_econ,       "label") <- "Economic policy"
  attr(df$congruence,      "label") <- "Congruence"
  df
}

# 03 Basic cleaning steps ######################################################

# Selecting the relevant variables and deriving completion and ID
data <- data_raw %>%
  mutate(
    time_start = ymd_hms(time_start),
    time_end   = ymd_hms(time_end),
    time_total = as.numeric(time_end - time_start, units = "secs") / 60
  ) %>%
  select(
    session_id, time_total, current_page, respID,
    resp_gender    = koen,
    resp_birthyear = alder,
    resp_education = uddannelse,
    region,
    party_vote, trust,
    immigration, middleim, econ, middleecon,
    starts_with("cbc_q"),
    starts_with("candidate_rating_1_"),
    starts_with("candidate_rating_2_")
  ) %>%
  mutate(
    respID    = as.numeric(respID),
    ID        = seq_along(session_id),
    completed = ifelse(current_page == "end_normal", 1, 0)
  )


# Checking the total number of respondents and the completion rate

cat("Total respondents:", nrow(data), "\n")
cat("Completes:", sum(data$completed), "\n")
cat("Completion rate:", round(mean(data$completed), 2), "\n")

# 04 Recoding of background variables ##########################################

# Numeric conversions and age calculation
data <- data %>%
  mutate(
    trust          = as.numeric(trust),
    resp_birthyear = as.numeric(resp_birthyear),
    resp_age       = 2026 - resp_birthyear
  )


# Recoding gender, age, education, region and party
data <- data %>%
  mutate(
    resp_gender = factor(
      case_when(
        resp_gender == "Mand"         ~ "Man",
        resp_gender == "Kvinde"       ~ "Woman",
        resp_gender %in% c("Andet", "Onsker_ikke") ~ "Other/prefer not to say"
      ),
      levels = c("Man", "Woman", "Other/prefer not to say")
    ),
    resp_age_group = factor(
      case_when(
        resp_age >= 80 ~ "80+",
        resp_age >= 70 ~ "70-79",
        resp_age >= 60 ~ "60-69",
        resp_age >= 50 ~ "50-59",
        resp_age >= 40 ~ "40-49",
        resp_age >= 30 ~ "30-39",
        resp_age >= 18 ~ "18-29"
      ),
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
    ),
    resp_education = factor(
      case_when(
        resp_education == "grundskole"          ~ "Primary school",
        resp_education == "gymnasial"           ~ "Upper secondary",
        resp_education == "erhvervsuddannelse"  ~ "Vocational",
        resp_education == "kort_videre"         ~ "Short-cycle higher",
        resp_education == "mellemlang_videre"   ~ "Medium-cycle higher",
        resp_education == "lang_videre"         ~ "Long-cycle higher",
        resp_education == "forsker"             ~ "PhD/Researcher",
        resp_education == "anden"               ~ "Other",
        TRUE ~ NA_character_
      ),
      levels = c("Primary school", "Upper secondary", "Vocational",
                 "Short-cycle higher", "Medium-cycle higher", "Long-cycle higher",
                 "PhD/Researcher", "Other")
    ),
    resp_region = factor(
      case_when(
        region == "hovedstaden"  ~ "Capital Region",
        region == "midtjylland"  ~ "Central Jutland",
        region == "sjaelland"    ~ "Zealand",
        region == "syddanmark"   ~ "Southern Denmark",
        region == "nordjylland"  ~ "Northern Jutland",
        TRUE ~ NA_character_
      ),
      levels = c("Capital Region", "Zealand", "Southern Denmark",
                 "Central Jutland", "Northern Jutland")
    ),
    party_vote = factor(
      case_when(
        party_vote == "socialdemokratiet"  ~ "Socialdemokratiet",
        party_vote == "radikale"           ~ "Radikale Venstre",
        party_vote == "konservative"       ~ "Det Konservative Folkeparti",
        party_vote == "sf"                 ~ "SF - Socialistisk Folkeparti",
        party_vote == "borgernes_parti"    ~ "Borgernes Parti",
        party_vote == "liberal_alliance"   ~ "Liberal Alliance",
        party_vote == "moderaterne"        ~ "Moderaterne",
        party_vote == "dansk_folkeparti"   ~ "Dansk Folkeparti",
        party_vote == "venstre"            ~ "Venstre",
        party_vote == "danmarksdemokraterne" ~ "Danmarksdemokraterne",
        party_vote == "enhedslisten"       ~ "Enhedslisten",
        party_vote == "alternativet"       ~ "Alternativet",
        party_vote == "udenfor_parti"      ~ "Independent",
        party_vote %in% c("blankt", "stemte_ikke", "onsker_ikke") ~ "Other",
        TRUE ~ NA_character_
      ),
      levels = c(
        "Socialdemokratiet", "Radikale Venstre", "Det Konservative Folkeparti", "SF - Socialistisk Folkeparti",
        "Borgernes Parti", "Liberal Alliance", "Moderaterne",
        "Dansk Folkeparti", "Venstre", "Danmarksdemokraterne",
        "Enhedslisten", "Alternativet", "Independent", "Other"
      )
    )
  )

# Creating two binary trust groups, split at the sample median (6) and at the scale midpoint (5)
data <- data %>%
  mutate(
    high_trust_midpoint = factor(
      case_when(
        trust > 5 ~ 1,
        trust < 5 ~ 0,
        TRUE      ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("Low trust", "High trust")
    ),
    high_trust_median = factor(
      ifelse(trust > 6, 1, 0),
      levels = c(0, 1),
      labels = c("Low trust", "High trust")
    )
  )

# Computing the ranking of policy preferences
data <- data %>%
  mutate(
    immig_pref = factor(
      case_when(
        immigration == 1 ~ "More restrictive",
        immigration == 3 ~ "Less restrictive",
        immigration == 2 ~ "Stay as today"
      ),
      levels = c("More restrictive", "Stay as today", "Less restrictive")
    ),
    econ_pref = factor(
      case_when(
        econ == 1 ~ "Lower taxes",
        econ == 3 ~ "More public welfare",
        econ == 2 ~ "Stay as today"
      ),
      levels = c("Lower taxes", "Stay as today", "More public welfare")
    )
  )

# Creating policy rankings (1 = most preferred, 3 = least preferred)
data <- data %>%
  mutate(
    immig_rank_strict = case_when(
      immigration == 1 ~ 1,
      immigration == 2 & middleim == 1 ~ 2,
      immigration == 2 & middleim == 3 ~ 3,
      immigration == 3 ~ 3
    ),
    immig_rank_statusquo = case_when(
      immigration == 1 ~ 2,
      immigration == 2 ~ 1,
      immigration == 3 ~ 2
    ),
    immig_rank_open = case_when(
      immigration == 1 ~ 3,
      immigration == 2 & middleim == 1 ~ 3,
      immigration == 2 & middleim == 3 ~ 2,
      immigration == 3 ~ 1
    ),
    econ_rank_low = case_when(
      econ == 1 ~ 1,
      econ == 2 & middleecon == 1 ~ 2,
      econ == 2 & middleecon == 3 ~ 3,
      econ == 3 ~ 3
    ),
    econ_rank_statusquo = case_when(
      econ == 1 ~ 2,
      econ == 2 ~ 1,
      econ == 3 ~ 2
    ),
    econ_rank_high = case_when(
      econ == 1 ~ 3,
      econ == 2 & middleecon == 1 ~ 3,
      econ == 2 & middleecon == 3 ~ 2,
      econ == 3 ~ 1
    )
  )

# 05 Filtering respondents to analysis sample ##################################

# Keeping only respondents who completed at least one conjoint task
data <- data %>%
  filter(
    !is.na(cbc_q1))

# 06 Creating conjoint data ####################################################

# Pivoting the choice data to long format and merging in the conjoint design file
choice_data <- data %>%
  select(session_id, ID, respID, completed, time_total,
         resp_gender, resp_birthyear, resp_age, resp_age_group,
         resp_education, resp_region, party_vote,
         trust, high_trust_midpoint, high_trust_median,
         immigration, middleim, immig_pref,
         immig_rank_strict, immig_rank_statusquo, immig_rank_open,
         econ, middleecon, econ_pref,
         econ_rank_low, econ_rank_statusquo, econ_rank_high,
         cbc_q1:cbc_q5) %>%
  pivot_longer(
    cols      = cbc_q1:cbc_q5,
    names_to  = "qID",
    values_to = "choice"
  ) %>%
  mutate(
    qID    = parse_number(qID),
    choice = parse_number(choice)
  ) %>%
  left_join(
    design %>%
      filter(qID %in% 1:5) %>%
      select(respID, qID, altID, profileID, obsID,
             cand_scandal    = Skandale,
             cand_reputation = Kompetence,
             cand_gender     = Køn,
             cand_age        = Alder,
             cand_immig      = Indvandring,
             cand_econ       = Okonomi),
    by = c("respID", "qID"),
    relationship = "many-to-many"
  ) %>%
  mutate(selected = ifelse(choice == altID, 1, 0))

# Applying the helper functions to recode, add congruence and attach labels
choice_data <- choice_data %>%
  recode_candidate_attrs() %>%
  add_congruence() %>%
  attach_attr_labels()

# Ordering columns
choice_data <- choice_data %>%
  arrange(ID, qID, altID) %>%
  mutate(obsID = rep(seq(n() / 2), each = 2)) %>%
  select(
    # IDs
    session_id, ID, respID, obsID, qID, altID, profileID,
    completed, time_total,

    # Respondent demographics
    resp_gender, resp_birthyear, resp_age, resp_age_group,
    resp_education,
    resp_region, party_vote,

    # Respondent policy preferences and trust
    trust, high_trust_midpoint, high_trust_median,
    immigration, middleim, immig_pref,
    immig_rank, immig_rank_strict, immig_rank_statusquo, immig_rank_open, immig_match,
    econ, middleecon, econ_pref,
    econ_rank, econ_rank_low, econ_rank_statusquo, econ_rank_high, econ_match,
    congruence_sum, congruence,

    # Candidate attributes
    selected,
    cand_scandal, scandal_any, scandal_type, scandal_reaction,
    cand_reputation, cand_gender, cand_age, cand_immig, cand_econ
  )

# Removing rows with no recorded choice
choice_data <- choice_data %>%
  filter(!is.na(selected))

# Converting qID and altID to factors for conjoint analysis
choice_data <- choice_data %>%
  mutate(
    qID   = factor(qID),
    altID = factor(altID)
  )

# Checking the number of respondents and observations in the conjoint data
cat("Conjoint respondents:", n_distinct(choice_data$ID), "\n")
cat("Conjoint observations:", nrow(choice_data), "\n")

# 07 Creating data for descriptives (conjoint respondents) #####################

# Keeping one row per respondent for the descriptive data
descriptive_data <- choice_data %>%
  filter(qID == 1, altID == 1) %>%
  select(
    session_id, ID, respID, completed, time_total,
    resp_gender, resp_birthyear, resp_age, resp_age_group,
    resp_education, resp_region, party_vote,
    trust, high_trust_midpoint, high_trust_median,
    immigration, middleim, immig_pref,
    immig_rank_strict, immig_rank_statusquo, immig_rank_open,
    econ, middleecon, econ_pref,
    econ_rank_low, econ_rank_statusquo, econ_rank_high
  )

# Checking the number of respondents, which should equal the conjoint data
cat("Descriptive data respondents:", nrow(descriptive_data), "\n")


# 08 Creating vignette data ####################################################

# Selecting the relevant vignette profiles from the design file
single_design <- design %>%
  filter(qID %in% c(6, 7), altID == 1) %>%
  select(
    respID, qID,
    cand_gender     = Køn,
    cand_age        = Alder,
    cand_reputation = Kompetence,
    cand_immig      = Indvandring,
    cand_econ       = Okonomi,
    cand_scandal    = Skandale
  ) %>%
  mutate(profil = ifelse(qID == 6, 1, 2))

# Joining the design file to the first vignette, keeping only respondents who completed all five ratings on a task
vignette_1 <- data %>%
  filter(
    !is.na(candidate_rating_1_trust_1),
    !is.na(candidate_rating_1_competent_1),
    !is.na(candidate_rating_1_fitforoffice_1),
    !is.na(candidate_rating_1_decency_1),
    !is.na(candidate_rating_1_voteprop_1)
  ) %>%
  select(
    session_id, ID, respID, completed, time_total,
    resp_gender, resp_birthyear, resp_age, resp_age_group,
    resp_education, resp_region, party_vote,
    trust, high_trust_midpoint, high_trust_median,
    immigration, middleim, immig_pref,
    immig_rank_strict, immig_rank_statusquo, immig_rank_open,
    econ, middleecon, econ_pref,
    econ_rank_low, econ_rank_statusquo, econ_rank_high,
    trustworthy = candidate_rating_1_trust_1,
    competent   = candidate_rating_1_competent_1,
    fit_office  = candidate_rating_1_fitforoffice_1,
    decent      = candidate_rating_1_decency_1,
    vote_prop   = candidate_rating_1_voteprop_1
  ) %>%
  mutate(profil = 1) %>%
  left_join(single_design %>% filter(profil == 1), by = c("respID", "profil"))

# Joining the design file to the second vignette, keeping only respondents who completed all five ratings on a task
vignette_2 <- data %>%
  filter(
    !is.na(candidate_rating_2_trust_2),
    !is.na(candidate_rating_2_competent_2),
    !is.na(candidate_rating_2_fitforoffice_2),
    !is.na(candidate_rating_2_decency_2),
    !is.na(candidate_rating_2_voteprop_2)
  ) %>%
  select(
    session_id, ID, respID, completed, time_total,
    resp_gender, resp_birthyear, resp_age, resp_age_group,
    resp_education, resp_region, party_vote,
    trust, high_trust_midpoint, high_trust_median,
    immigration, middleim, immig_pref,
    immig_rank_strict, immig_rank_statusquo, immig_rank_open,
    econ, middleecon, econ_pref,
    econ_rank_low, econ_rank_statusquo, econ_rank_high,
    trustworthy = candidate_rating_2_trust_2,
    competent   = candidate_rating_2_competent_2,
    fit_office  = candidate_rating_2_fitforoffice_2,
    decent      = candidate_rating_2_decency_2,
    vote_prop   = candidate_rating_2_voteprop_2
  ) %>%
  mutate(profil = 2) %>%
  left_join(single_design %>% filter(profil == 2), by = c("respID", "profil"))

# Binding the two profiles into one dataset and converting outcomes to numeric
vignette_data <- bind_rows(vignette_1, vignette_2) %>%
  mutate(across(c(trustworthy, competent, fit_office, decent, vote_prop), as.numeric))

# Applying the helper functions to recode, add congruence and attach labels
vignette_data <- vignette_data %>%
  recode_candidate_attrs() %>%
  add_congruence() %>%
  attach_attr_labels()

# Ordering the columns
vignette_data <- vignette_data %>%
  arrange(ID, profil) %>%
  select(
    # IDs
    session_id, ID, respID, profil,
    completed, time_total,

    # Respondent demographics
    resp_gender, resp_birthyear, resp_age, resp_age_group,
    resp_education,
    resp_region, party_vote,

    # Respondent policy preferences and trust
    trust, high_trust_midpoint, high_trust_median,
    immigration, middleim, immig_pref,
    immig_rank, immig_match,
    econ, middleecon, econ_pref,
    econ_rank, econ_match,
    congruence_sum, congruence,

    # Candidate attributes
    cand_scandal, scandal_any, scandal_type, scandal_reaction,
    cand_reputation, cand_gender, cand_age, cand_immig, cand_econ,

    # Outcomes
    trustworthy, competent, fit_office, decent, vote_prop
  )

# Checking the number of respondents and observations in the vignette data
cat("Vignette respondents:", n_distinct(vignette_data$ID), "\n")
cat("Vignette observations:", sum(!is.na(vignette_data$trustworthy)), "\n")


# 09 Saving cleaned datasets ###################################################

write_rds(descriptive_data, '02. Clean data/descriptive_data.rds')
write_rds(choice_data,      '02. Clean data/conjoint_data.rds')
write_rds(vignette_data,    '02. Clean data/vignette_data.rds')

# Script end ###################################################################

