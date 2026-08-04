# Analysis of Conjoint Experiment ##############################################

# 00 Dependencies ##############################################################

# Loading packages
library(tidyverse)
library(cregg)
library(cjoint)
library(kableExtra)
library(knitr)
library(estimatr)
library(gridExtra)
library(marginaleffects)
library(lmtest)

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

# 01 Loading data ##############################################################

# Importing cleaned conjoint data
choice_data <- read_rds('02. Clean data/conjoint_data.rds')

# 02 Helper functions and formulas #############################################

# Defining the output paths for figures and tables
paper_fig_path    <- "03. Output/Paper/Figures"
paper_tab_path    <- "03. Output/Paper/Tables"
appendix_fig_path <- "03. Output/Appendix/Figures"
appendix_tab_path <- "03. Output/Appendix/Tables"

# Function to save a kable table as a .tex file
save_tex <- function(tbl, path) {
  writeLines(as.character(tbl), path)
}

# Function to format regression table columns
format_estimates <- function(df, est_col = "AMCE") {
  df %>%
    mutate(
      stars = case_when(
        is.na(p)  ~ "",
        p < 0.01  ~ "***",
        p < 0.05  ~ "**",
        p < 0.1   ~ "*",
        TRUE      ~ ""
      ),
      !!est_col := ifelse(is.na(.data[[est_col]]) | .data[[est_col]] == 0, "--",
                          sprintf("%.3f%s", .data[[est_col]], stars)),
      SE    = ifelse(is.na(SE)   | SE == 0,    "--", sprintf("%.3f", SE)),
      p     = ifelse(is.na(p),                 "--", sprintf("%.3f", p)),
      Lower = ifelse(is.na(Lower)| Lower == 0, "--", sprintf("%.3f", Lower)),
      Upper = ifelse(is.na(Upper)| Upper == 0, "--", sprintf("%.3f", Upper))
    ) %>%
    select(-stars)
}

# Function to tidy cregg output into a dataframe ready for kable
tidy_cregg <- function(cregg_obj, feature_name = NULL, feature_label = NULL, choice_data) {
  df <- cregg_obj
  if (!is.null(feature_name)) df <- df %>% filter(feature == feature_name)
  if (!is.null(feature_label)) df <- df %>% mutate(feature = feature_label)
  df %>%
    mutate(
      estimate = ifelse(estimate == 0 & is.na(std.error), NA_real_, estimate),
      p        = ifelse(is.na(std.error), NA_real_, p)
    ) %>%
    select(Feature = feature, Level = level,
           AMCE = estimate, SE = std.error, p,
           Lower = lower, Upper = upper) %>%
    format_estimates() %>%
    bind_rows(
      tibble(Feature = "N respondents",  Level = sprintf("%d", n_distinct(choice_data$ID)),
             AMCE = "", SE = "", p = "", Lower = "", Upper = ""),
      tibble(Feature = "N observations", Level = sprintf("%d", nrow(choice_data)),
             AMCE = "", SE = "", p = "", Lower = "", Upper = "")
    )
}

# Function to save a tidy cregg dataframe as a latex table
save_cregg_tex <- function(tidy_df, caption, label, path, col_name = "AMCE") {
  n_rows      <- nrow(tidy_df) - 2
  group_ends  <- which(!duplicated(tidy_df$Feature[1:n_rows], fromLast = TRUE))
  linesep_vec <- rep("", nrow(tidy_df))
  linesep_vec[group_ends] <- "\\addlinespace[0.3em]"
  linesep_vec[max(group_ends)] <- "\\midrule"
  names(tidy_df)[names(tidy_df) == "AMCE"]    <- col_name
  names(tidy_df)[names(tidy_df) == "Feature"] <- "Attribute"
  save_tex(
    kable(tidy_df, format = "latex", booktabs = TRUE,
          caption = caption, label = label,
          linesep = linesep_vec) %>%
      kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
      footnote(general           = "$^*$ $p<0.1$; $^{**}$ $p<0.05$; $^{***}$ $p<0.01$",
               general_title     = "Note: ",
               footnote_as_chunk = TRUE,
               escape            = FALSE),
    path
  )
}

# Function to tidy a cjoint output into a dataframe ready for kable
tidy_cjoint <- function(amce_obj, attrs = NULL) {
  est <- amce_obj$estimates
  if (!is.null(attrs)) est <- est[intersect(attrs, names(est))]
  rows <- lapply(names(est), function(nm) {
    mat     <- est[[nm]]
    est_row <- grep("^AMCE$|^ACIE$", rownames(mat), value = TRUE)[1]
    se_row  <- grep("Std\\..*Error|^SE$", rownames(mat), value = TRUE)[1]
    data.frame(
      Attribute = nm,
      Level     = colnames(mat),
      Estimate  = as.numeric(mat[est_row, ]),
      SE        = as.numeric(mat[se_row,  ]),
      row.names = NULL
    )
  })
  df          <- do.call(rbind, rows)
  df$z        <- df$Estimate / df$SE
  df$p        <- round(2 * (1 - pnorm(abs(df$z))), 3)
  df$Estimate <- round(df$Estimate, 3)
  df$SE       <- round(df$SE, 3)
  df$z        <- round(df$z, 3)
  df
}

# Function to save an interaction term table as a latex table
save_interaction_tex <- function(int_df, caption, label, path, linesep = NULL) {
  names(int_df)[names(int_df) == "AMCE"] <- "ACIE"
  if (is.null(linesep)) {
    linesep <- c(rep("", nrow(int_df) - 3), "\\midrule", "")
  }
  save_tex(
    kable(int_df, format = "latex", booktabs = TRUE,
          caption = caption, label = label,
          linesep = linesep,
          escape  = FALSE) %>%
      kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
      footnote(general           = "$^*$ $p<0.1$; $^{**}$ $p<0.05$; $^{***}$ $p<0.01$",
               general_title     = "Note: ",
               footnote_as_chunk = TRUE,
               escape            = FALSE),
    path
  )
}

# Function to save a conditional AMCE table as latex table
save_cond_tex <- function(cond_df, caption, label, path) {
  save_tex(
    kable(cond_df, format = "latex", booktabs = TRUE,
          caption = caption, label = label,
          linesep = c(rep("", nrow(cond_df) - 3), "\\midrule", "")) %>%
      kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
      footnote(general           = "$^*$ $p<0.1$; $^{**}$ $p<0.05$; $^{***}$ $p<0.01$",
               general_title     = "Note: ",
               footnote_as_chunk = TRUE,
               escape            = FALSE),
    path
  )
}

# Main conjoint formula used for overall analysis and robustness checks
conjoint_formula <- selected ~ cand_scandal + cand_reputation + congruence +
  cand_gender + cand_age


# 03 Plotting themes ###########################################################

# Shared theme for conjoint plots
conjoint_theme <- theme(
  legend.position    = "none",
  axis.text.y        = element_text(size = 10),
  axis.text.x        = element_text(size = 9),
  axis.title.x       = element_text(size = 10, margin = margin(t = 8)),
  strip.text         = element_text(size = 8),
  plot.margin        = margin(8, 12, 8, 8),
  panel.grid.major.x = element_line(color = "grey95", linewidth = 0.3),
  panel.grid.minor   = element_blank()
)

# Shared facet wrap for conjoint plots
conjoint_facet <- ggplot2::facet_wrap(~feature, ncol = 1L,
                                      scales = "free_y",
                                      strip.position = "left")

# Reference lines for AMCE (0) and MM (0.5) plots
vline_zero <- geom_vline(xintercept = 0,   linetype = "dashed")
vline_half <- geom_vline(xintercept = 0.5, linetype = "dashed")



# 04 Analysis ##################################################################

## 4a AMCE for all attributes ##################################################

# Overall AMCE for all candidate attributes
amce_overall <- cregg::amce(choice_data, conjoint_formula, id = ~ID)

# Plotting AMCE for all attributes (Figure 1 in Main Paper)
plot_amce_overall <- plot(amce_overall, vline_color = "white", feature_headers = FALSE) +
  xlab("AMCE (pr. candidate preferred)") +
  scale_colour_manual(values = rep("black", 10)) +
  vline_zero + conjoint_facet + conjoint_theme

ggsave(file.path(paper_fig_path, "amce_overall.pdf"),
       plot = plot_amce_overall, width = 6, height = 6)

# Saving regression table
save_cregg_tex(tidy_cregg(amce_overall, choice_data = choice_data),
               caption = "Effect of Attributes on Candidate Preference (Figure 1 in Main Paper)",
               label   = "tab:amce_overall",
               path    = file.path(appendix_tab_path, "amce_overall.tex"))

## 4b Marginal means for all attributes ########################################

# Overall marginal means for all candidate attributes
mm_overall <- mm(choice_data, conjoint_formula, id = ~ID, h0 = 0.5)

# Plotting marginal means for all attributes
plot_mm_overall <- plot(mm_overall, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (pr. candidate preferred)") +
  scale_colour_manual(values = rep("black", 10)) +
  vline_half + conjoint_facet + conjoint_theme

ggsave(file.path(appendix_fig_path, "mm_overall.pdf"),
       plot = plot_mm_overall, width = 6, height = 7)

# Saving regression table
save_cregg_tex(tidy_cregg(mm_overall, choice_data = choice_data),
               caption  = "Descriptive Candidate Preferences",
               label    = "tab:mm_overall",
               path     = file.path(appendix_tab_path, "mm_overall.tex"),
               col_name = "MM")

## 4c Hypothesis 1 #############################################################

# Recoding to set personal scandal as the reference level
choice_data <- choice_data %>%
  mutate(scandal_type = factor(scandal_type,
                               levels = c("Personal", "No scandal", "Office-related")))

# Estimating AMCE
amce_h1 <- cregg::amce(
  choice_data,
  selected ~ scandal_type + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID
)
# RESULT (H1): office-related vs. personal = 0.002, p = 0.909 -> H1 not supported

# Plotting AMCE (Figure 2 in main paper)
plot_h1 <- plot(subset(amce_h1, feature == "scandal_type" & level != "No scandal") %>%
                  mutate(feature = "Scandal type"),
                vline_color = "white", feature_headers = FALSE) +
  xlab("AMCE (pr. candidate preferred)") +
  scale_colour_manual(values = rep("black", 2)) +
  vline_zero + conjoint_facet + conjoint_theme

ggsave(file.path(paper_fig_path, "h1_scandal_type.pdf"),
       plot = plot_h1, width = 6, height = 2.5)

# Saving regression table
save_cregg_tex(
  tidy_cregg(amce_h1, feature_name = "scandal_type",
             feature_label = "Scandal type", choice_data = choice_data) %>%
    filter(!(Level == "No scandal")),
  caption = "Effect of Scandal Type on Candidate Preference (Figure 2 in Main Paper)",
  label   = "tab:h1",
  path    = file.path(appendix_tab_path, "h1_scandal_type.tex")
)

# Estimating marginal means
cj_h1_mm <- cregg::cj(
  choice_data %>%
    mutate(scandal_type = factor(scandal_type,
                                 levels = c("No scandal",
                                            "Personal",
                                            "Office-related"))),
  selected ~ scandal_type + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "mm"
)


# Plotting marginal means
plot_h1_mm <- plot(subset(cj_h1_mm, feature == "scandal_type") %>%
                     mutate(feature = "Scandal type"),
                   feature_headers = FALSE,
                   size = 3) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  scale_colour_manual(values = "black")

ggsave(file.path(appendix_fig_path, "h1_scandal_mm.pdf"),
       plot = plot_h1_mm, width = 6, height = 1.8)


## 4d Hypothesis 2 #############################################################

# Recoding to set denial as reference level
choice_data <- choice_data %>%
  mutate(scandal_reaction = factor(scandal_reaction,
                                   levels = c("Denial", "No scandal", "Apology")))

# Estimating AMCE
amce_h2 <- cregg::amce(
  choice_data,
  selected ~ scandal_reaction + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID
)
# RESULT (H2): apology vs. denial = 0.033, p = 0.017 -> H2 not supported, effect runs opposite to prediction

# Plotting AMCE (Figure 3 in main paper)
plot_h2 <- plot(subset(amce_h2, feature == "scandal_reaction" & level != "No scandal") %>%
                  mutate(feature = "Candidate Reaction"),
                vline_color = "white", feature_headers = FALSE) +
  xlab("AMCE (pr. candidate preferred)") +
  scale_colour_manual(values = rep("black", 2)) +
  vline_zero + conjoint_facet + conjoint_theme

ggsave(file.path(paper_fig_path, "h2_scandal_reaction.pdf"),
       plot = plot_h2, width = 6, height = 2.5)

# Saving regression table
save_cregg_tex(
  tidy_cregg(amce_h2, feature_name = "scandal_reaction",
             feature_label = "Candidate reaction", choice_data = choice_data) %>%
    filter(!(Level == "No scandal")),
  caption = "Effect of Candidate Reaction on Candidate Preference (Figure 3 in Main Paper)",
  label   = "tab:h2",
  path    = file.path(appendix_tab_path, "h2_scandal_reaction.tex")
)

# Estimating marginal means
cj_h2_mm <- cregg::cj(
  choice_data %>%
    mutate(scandal_reaction = factor(scandal_reaction,
                                 levels = c("No scandal",
                                            "Denial",
                                            "Apology"))),
  selected ~ scandal_reaction + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "mm"
)


# Plotting marginal means
plot_h2_mm <- plot(subset(cj_h2_mm, feature == "scandal_reaction") %>%
                     mutate(feature = "Candidate reaction"),
                   feature_headers = FALSE,
                   size = 3) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  scale_colour_manual(values = "black")

ggsave(file.path(appendix_fig_path, "h2_scandal_mm.pdf"),
       plot = plot_h2_mm, width = 6, height = 1.8)


## 4e Hypothesis 3 #############################################################

# Recoding scandal and competence attributes to have no scandal and neutral as reference levels
choice_data <- choice_data %>%
  mutate(
    scandal_any     = relevel(factor(scandal_any),     ref = "No scandal"),
    cand_reputation = relevel(factor(cand_reputation), ref = "Neutral"))


# Formally testing the interaction term
acie_h3 <- cjoint::amce(
  selected ~ scandal_any * cand_reputation + cand_gender + cand_age + congruence,
  data          = choice_data,
  respondent.id = "ID"
)

summary(acie_h3)
# RESULT (H3): scandal x competent = -0.043, p = 0.025 -> H3 not supported, punishment is stronger
# The negative effect of a scandal is 4 percentage points stronger for competent candidates

# Saving the interaction terms in a table
h3_raw <- tidy_cjoint(acie_h3) %>%
  filter(grepl(":", Attribute)) %>%
  transmute(
    Interaction = "Scandal × Competent",
    AMCE  = Estimate, SE = SE, p = p,
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  AMCE = sprintf("%d", n_distinct(choice_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", AMCE = sprintf("%d", nrow(choice_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_interaction_tex(h3_raw,
                     caption = "Interaction Between Scandal and Competence (Figure 4 in Main Paper)",
                     label   = "tab:h3",
                     path    = file.path(appendix_tab_path, "h3_scandal_reputation.tex"))

# Estimating conditional AMCEs
cj_h3 <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "amce", by = ~cand_reputation
)

# Saving the conditional AMCEs in a table
h3_cond <- cj_h3 %>%
  filter(feature == "scandal_any") %>%
  as.data.frame() %>%
  transmute(
    Competence = as.character(cand_reputation),
    Scandal    = as.character(level),
    AMCE = estimate, SE = std.error, p = p, Lower = lower, Upper = upper
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Competence = "N respondents",  Scandal = sprintf("%d", n_distinct(choice_data$ID)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Competence = "N observations", Scandal = sprintf("%d", nrow(choice_data)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_cond_tex(h3_cond,
              caption = "Effect of Scandal on Candidate Preference Conditional On Competence (Figure 4 in Main Paper)",
              label   = "tab:h3_cond",
              path    = file.path(appendix_tab_path, "h3_cond_amce.tex"))

# Plotting conditional effect and interaction effect together (Figure 4 in main paper)
# Extracting interaction terms
h3_tidy <- tidy_cjoint(acie_h3) %>%
  filter(grepl(":", Attribute))

h3_int_est <- h3_tidy$Estimate[1]
h3_int_se  <- h3_tidy$SE[1]
h3_int_z   <- h3_tidy$z[1]
h3_int_p   <- h3_tidy$p[1]

# Plotting interaction
plot_h3_int <- tibble(
  label    = "         Scandal × Competent",
  estimate = h3_int_est,
  lower    = h3_int_est - 1.96 * h3_int_se,
  upper    = h3_int_est + 1.96 * h3_int_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name   = "ACIE (pr. candidate preferred)",
                     limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() +
  conjoint_theme +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none",
    legend.title    = element_blank()
  )

# Creating labels for the conditional effect plot
cj_h3_labels <- subset(cj_h3, feature == "scandal_any") %>%
  as.data.frame() %>%
  filter(level == "Scandal") %>%
  mutate(
    cand_reputation = factor(cand_reputation, levels = c("Neutral", "Competent")),
    level           = "                          Scandal",
    feature         = "Scandal",
    nudge           = case_when(
      cand_reputation == "Neutral"   ~ -0.2,
      cand_reputation == "Competent" ~  0.2
    )
  )

# Plotting conditional effects
plot_h3_cond <- plot(subset(cj_h3, feature == "scandal_any") %>%
                       mutate(
                         feature         = "Scandal",
                         cand_reputation = factor(cand_reputation, levels = c("Neutral", "Competent")),
                         level           = case_when(
                           level == "Scandal"    ~ "                          Scandal",
                           level == "No scandal" ~ "                          No scandal",
                           TRUE                  ~ level
                         )
                       ),
                     group = "cand_reputation", feature_headers = FALSE, size = 3) +
  xlab("AMCE (pr. candidate preferred)") +
  vline_zero + conjoint_facet + conjoint_theme +
  aes(shape = cand_reputation) +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  coord_cartesian(xlim = c(-0.3, 0.3)) +
  geom_label(data        = cj_h3_labels,
            aes(x       = upper + 0.02,
                y       = level,
                label   = as.character(cand_reputation),
                group   = cand_reputation),
            nudge_y     = cj_h3_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Adding panel titles
plot_h3_cond_titled <- arrangeGrob(plot_h3_cond,
                                   top = textGrob("A) Conditional Effect",
                                                  x  = 0.44,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))
plot_h3_int_titled  <- arrangeGrob(plot_h3_int,
                                   top = textGrob("B) Interaction Effect",
                                                  x  = 0.44,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))

# Combining interaction plot and conditional effect plot (Figure 4 in main paper)
plot_h3_full <- grid.arrange(
  plot_h3_cond_titled,
  plot_h3_int_titled,
  ncol    = 1,
  heights = c(1.25, 1.25)
)

ggsave(file.path(paper_fig_path, "h3_full.pdf"),
       plot = plot_h3_full, width = 6, height = 5)


# Estimating conditional marginal means to check baselines
cj_h3_mm <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "mm", by = ~cand_reputation
)

# Plotting conditional marginal means
plot_h3_mm <- plot(subset(cj_h3_mm, feature == "scandal_any") %>%
                     mutate(feature = "Scandal"),
                   group = "cand_reputation", feature_headers = FALSE, size = 3) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  aes(shape = cand_reputation) +
  scale_colour_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(16, 17))

ggsave(file.path(appendix_fig_path, "h3_scandal_reputation_mm.pdf"),
       plot = plot_h3_mm, width = 6, height = 2.3)

## 4f Hypothesis 4 #############################################################

# Recoding congruence measure to have medium as reference level
choice_data <- choice_data %>%
  mutate(
    congruence      = factor(congruence, levels = c("Medium", "Low", "High"))
  )

# Formally testing the interaction term
acie_h4 <- cjoint::amce(
  selected ~ scandal_any * congruence + cand_reputation + cand_gender + cand_age,
  data          = choice_data,
  respondent.id = "ID"
)

summary(acie_h4)
# RESULT (H4): scandal x low congruence = 0.064, p = 0.008; scandal x high congruence = 0.004, p = 0.877 -> H4 not supported
# The negative effect of a scandal is 6 percentage points weaker at low congruence
# No difference between medium and high congruence


# Saving the interaction terms in a table
h4_raw <- tidy_cjoint(acie_h4) %>%
  filter(grepl(":", Attribute)) %>%
  transmute(
    Interaction = case_when(
      grepl("Low",  Level) ~ "Scandal × Low Congruence",
      grepl("High", Level) ~ "Scandal × High Congruence",
      TRUE                 ~ Level
    ),
    AMCE  = Estimate, SE = SE, p = p,
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  AMCE = sprintf("%d", n_distinct(choice_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", AMCE = sprintf("%d", nrow(choice_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_interaction_tex(h4_raw,
                     caption = "Interaction Between Scandal and Congruence (Figure 5 in Main Paper)",
                     label   = "tab:h4",
                     path    = file.path(appendix_tab_path, "h4_scandal_congruence.tex"))

# Estimating conditional AMCEs
cj_h4 <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age,
  id = ~ID, estimate = "amce", by = ~congruence
)

# Saving the conditional AMCEs in a table
h4_cond <- cj_h4 %>%
  filter(feature == "scandal_any") %>%
  as.data.frame() %>%
  mutate(congruence = factor(congruence, levels = c("Low", "Medium", "High"))) %>%
  arrange(congruence) %>%
  transmute(
    Congruence = as.character(congruence),
    Scandal    = as.character(level),
    AMCE = estimate, SE = std.error, p = p, Lower = lower, Upper = upper
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Congruence = "N respondents",  Scandal = sprintf("%d", n_distinct(choice_data$ID)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Congruence = "N observations", Scandal = sprintf("%d", nrow(choice_data)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_cond_tex(h4_cond,
              caption = "Effect of Scandal on Candidate Preference Conditional On Congruence (Figure 5 in Main Paper)",
              label   = "tab:h4_cond",
              path    = file.path(appendix_tab_path, "h4_cond_amce.tex"))


# Plotting conditional effect and interactions effect together (Figure 5 in main paper)
# Extracting interaction terms
h4_tidy <- tidy_cjoint(acie_h4) %>%
  filter(grepl(":", Attribute))

h4_low_est  <- h4_tidy$Estimate[grepl("Low",  h4_tidy$Level)]
h4_low_se   <- h4_tidy$SE[grepl("Low",  h4_tidy$Level)]
h4_low_z    <- h4_tidy$z[grepl("Low",  h4_tidy$Level)]
h4_low_p    <- h4_tidy$p[grepl("Low",  h4_tidy$Level)]

h4_high_est <- h4_tidy$Estimate[grepl("High", h4_tidy$Level)]
h4_high_se  <- h4_tidy$SE[grepl("High", h4_tidy$Level)]
h4_high_z   <- h4_tidy$z[grepl("High",  h4_tidy$Level)]
h4_high_p   <- h4_tidy$p[grepl("High",  h4_tidy$Level)]


# Plotting interaction
plot_h4_int <- tibble(
  panel    = "H4: Interaction",
  label    = c("Scandal × Low congruence", "Scandal × High congruence"),
  estimate = c(h4_low_est, h4_high_est),
  lower    = c(h4_low_est - 1.96 * h4_low_se, h4_high_est - 1.96 * h4_high_se),
  upper    = c(h4_low_est + 1.96 * h4_low_se, h4_high_est + 1.96 * h4_high_se)
) %>%
  mutate(
    label = factor(label, levels = c("Scandal × Low congruence", "Scandal × High congruence")),
    type  = "Interaction effect"
  ) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "ACIE (pr. candidate preferred)",
                     limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() +
  conjoint_theme +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none",
    legend.title    = element_blank()
  )


# Creating labels for the conditional effect plot
cj_h4_labels <- subset(cj_h4, feature == "scandal_any") %>%
  as.data.frame() %>%
  filter(level == "Scandal") %>%
  mutate(
    congruence = factor(congruence, levels = c("Low", "Medium", "High")),
    label = paste(congruence, "Congruence"),
    level = "                          Scandal",
    feature = "Scandal",
    nudge = case_when(
      congruence == "Low"    ~ -0.25,
      congruence == "Medium" ~  0,
      congruence == "High"   ~  0.25
    )
  )

# Plotting conditional effects
plot_h4_cond <- plot(subset(cj_h4, feature == "scandal_any") %>%
                       mutate(
                         feature    = "Scandal",
                         congruence = factor(congruence, levels = c("Low", "Medium", "High")),
                         level      = case_when(
                           level == "Scandal"    ~ "                          Scandal",
                           level == "No scandal" ~ "                          No scandal",
                           TRUE                  ~ level
                         )
                       ),
                     group = "congruence", feature_headers = FALSE, size = 3) +
  xlab("AMCE (pr. candidate preferred)") +
  vline_zero + conjoint_facet + conjoint_theme +
  aes(shape = congruence) +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 17, 15)) +
  coord_cartesian(xlim = c(-0.3, 0.3)) +
  geom_label(
    data        = cj_h4_labels,
    aes(
      x     = upper + 0.02,
      y     = level,
      label = label,
      group = congruence
    ),
    nudge_y    = cj_h4_labels$nudge,
    size       = 3,
    hjust      = 0,
    label.size = 0,
    fill       = "white",
    inherit.aes = FALSE
  )

# Adding panel titles
plot_h4_cond_titled <- arrangeGrob(plot_h4_cond,
                            top = textGrob("A) Conditional Effect",
                                           x    = 0.44,
                                           gp   = gpar(fontsize = 10,
                                                       fontface = "bold")))
plot_h4_int_titled <- arrangeGrob(plot_h4_int,
                           top = textGrob("B) Interaction Effect",
                                          x    = 0.44,
                                          gp   = gpar(fontsize = 10,
                                                      fontface = "bold")))

# Combining interaction plot and conditional effect (Figure 5 in main paper)
plot_h4_full <- grid.arrange(
  plot_h4_cond_titled,
  plot_h4_int_titled,
  ncol    = 1,
  heights = c(1.25, 1.25)
)

ggsave(file.path(paper_fig_path, "h4_full.pdf"),
       plot = plot_h4_full, width = 6, height = 5)


# Estimating conditional marginal means to check baselines
cj_h4_mm <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age,
  id = ~ID, estimate = "mm", by = ~congruence
)

# Plotting conditional marginal means
plot_h4_mm <- plot(subset(cj_h4_mm, feature == "scandal_any") %>%
                     mutate(
                       feature    = "Scandal",
                       congruence = factor(congruence, levels = c("Low", "Medium", "High"))
                     ),
                   group = "congruence", feature_headers = FALSE, size = 3) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  aes(shape = congruence) +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 17, 15))

ggsave(file.path(appendix_fig_path, "h4_scandal_congruence_mm.pdf"),
       plot = plot_h4_mm, width = 6, height = 2.3)


# Re-testing with low congruence as reference to compare low against medium and high
acie_h4_low <- cjoint::amce(
  selected ~ scandal_any * congruence + cand_reputation + cand_gender + cand_age,
  data          = choice_data %>% mutate(congruence = relevel(congruence, ref = "Low")),
  respondent.id = "ID")

summary(acie_h4_low)
# With low congruence as the reference, the effect is stronger for medium and high, p < 0.01


## 4g Hypothesis 5 #############################################################

# Recoding to have low trust as the reference level
choice_data <- choice_data %>%
  mutate(high_trust_median = factor(high_trust_median, levels = c("Low trust", "High trust")))

# Formally testing the interaction term
acie_h5 <- cjoint::amce(
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age + congruence +
    high_trust_median:scandal_any,
  data               = choice_data,
  respondent.id      = "ID",
  respondent.varying = "high_trust_median"
)


# Extracting interaction term from the model output
interaction_est <- acie_h5$cond.estimates$`hightrustmedian:scandalany`["Conditional Estimate", ]
interaction_se  <- acie_h5$cond.estimates$`hightrustmedian:scandalany`["Std. Error", ]
interaction_z   <- interaction_est / interaction_se
interaction_p   <- 2 * (1 - pnorm(abs(interaction_z)))
# RESULT (H5): scandal x high trust = 0.034, p = 0.096 -> H5 not supported


# Estimating conditional AMCEs
cj_h5 <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "amce", by = ~high_trust_median
)

# Saving the interaction term in a table
h5_raw <- tibble(
  Interaction = "Scandal × High trust",
  AMCE        = interaction_est,
  SE          = interaction_se,
  p           = interaction_p,
  Lower       = interaction_est - 1.96 * interaction_se,
  Upper       = interaction_est + 1.96 * interaction_se
) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  AMCE = sprintf("%d", n_distinct(choice_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", AMCE = sprintf("%d", nrow(choice_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_interaction_tex(h5_raw,
                     caption = "Interaction Between Scandal and Trust in Politicians (Figure 6 in Main Paper)",
                     label   = "tab:h5",
                     path    = file.path(appendix_tab_path, "h5_scandal_trust.tex"))

# Saving the conditional AMCEs in a table
h5_cond <- cj_h5 %>%
  filter(feature == "scandal_any") %>%
  as.data.frame() %>%
  transmute(
    Trust   = as.character(high_trust_median),
    Scandal = as.character(level),
    AMCE = estimate, SE = std.error, p = p, Lower = lower, Upper = upper
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Trust = "N respondents",  Scandal = sprintf("%d", n_distinct(choice_data$ID)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Trust = "N observations", Scandal = sprintf("%d", nrow(choice_data)),
           AMCE = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_cond_tex(h5_cond,
              caption = "Effect of Scandal on Candidate Preference Conditional On Trust in Politicians (Figure 6 in Main Paper)",
              label   = "tab:h5_cond",
              path    = file.path(appendix_tab_path, "h5_cond_amce.tex"))

# Plotting conditional effect and interaction effect together (Figure 6 in main paper)
# Plotting interaction
plot_h5_int <- tibble(
  label    = "            Scandal × High trust",
  estimate = interaction_est,
  lower    = interaction_est - 1.96 * interaction_se,
  upper    = interaction_est + 1.96 * interaction_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name   = "ACIE (pr. candidate preferred)",
                     limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() +
  conjoint_theme +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none",
    legend.title    = element_blank()
  )

# Creating labels for the conditional effect plot
cj_h5_labels <- subset(cj_h5, feature == "scandal_any") %>%
  as.data.frame() %>%
  filter(level == "Scandal") %>%
  mutate(
    high_trust_median = factor(high_trust_median, levels = c("Low trust", "High trust")),
    level             = "                          Scandal",
    feature           = "Scandal",
    nudge             = case_when(
      high_trust_median == "Low trust"  ~ -0.2,
      high_trust_median == "High trust" ~  0.2
    )
  )

# Plotting conditional effects
plot_h5_cond <- plot(subset(cj_h5, feature == "scandal_any") %>%
                       mutate(
                         feature           = "Scandal",
                         high_trust_median = factor(high_trust_median,
                                                    levels = c("Low trust", "High trust")),
                         level             = case_when(
                           level == "Scandal"    ~ "                          Scandal",
                           level == "No scandal" ~ "                          No scandal",
                           TRUE                  ~ level
                         )
                       ),
                     group = "high_trust_median", feature_headers = FALSE, size = 3) +
  xlab("AMCE (pr. candidate preferred)") +
  vline_zero + conjoint_facet + conjoint_theme +
  aes(shape = high_trust_median) +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  coord_cartesian(xlim = c(-0.3, 0.3)) +
  geom_label(data        = cj_h5_labels,
            aes(x       = upper + 0.02,
                y       = level,
                label   = as.character(high_trust_median),
                group   = high_trust_median),
            nudge_y     = cj_h5_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Adding panel titles
plot_h5_cond_titled <- arrangeGrob(plot_h5_cond,
                                   top = textGrob("A) Conditional Effect",
                                                  x  = 0.44,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))
plot_h5_int_titled  <- arrangeGrob(plot_h5_int,
                                   top = textGrob("B) Interaction Effect",
                                                  x  = 0.44,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))

# Combining interaction plot and conditional effect (Figure 6 in main paper)
plot_h5_full <- grid.arrange(
  plot_h5_cond_titled,
  plot_h5_int_titled,
  ncol    = 1,
  heights = c(1.25, 1.25)
)

ggsave(file.path(paper_fig_path, "h5_full.pdf"),
       plot = plot_h5_full, width = 6, height = 5)


# Estimating conditional marginal means to check baselines
cj_h5_mm <- cregg::cj(
  choice_data,
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age + congruence,
  id = ~ID, estimate = "mm", by = ~high_trust_median
)

# Plotting conditional marginal means
plot_h5_mm <- plot(subset(cj_h5_mm, feature == "scandal_any") %>%
                     mutate(feature = "Scandal"),
                   group = "high_trust_median", feature_headers = FALSE, size = 3) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  aes(shape = high_trust_median) +
  scale_colour_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(16, 17))

ggsave(file.path(appendix_fig_path, "h5_scandal_trust_mm.pdf"),
       plot = plot_h5_mm, width = 6, height = 2.3)


# 05 Robustness checks #########################################################


## 5a Carryover effects ########################################################

# Restoring low congruence as the reference level for the robustness figures
choice_data <- choice_data %>%
  mutate(
    congruence      = factor(congruence, levels = c("Low", "Medium", "High"))
  )

# Adding correct labels
attr(choice_data$congruence, "label") <- "Congruence"
attr(choice_data$cand_reputation, "label") <- "Competence"

# Formal F test for carryover effects across task order
carryover_test <- cj_anova(choice_data, conjoint_formula, id = ~ID, by = ~qID)
print(carryover_test)

# Estimating and plotting marginal means by task order to inspect carryover effects
cj_carryover <- cregg::cj(
  choice_data, conjoint_formula,
  id = ~ID, estimate = "mm", by = ~qID
)

plot_carryover <- plot(cj_carryover,
                       group           = "qID",
                       feature_headers = FALSE) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_discrete(labels = paste("Task", 1:5)) +
  scale_shape_discrete(labels = paste("Task", 1:5))

ggsave(file.path(appendix_fig_path, "carryover.pdf"),
       plot = plot_carryover, width = 6, height = 7)

## 5b Profile order effects ####################################################

# Formal F test for profile order effects
profile_order_test <- cj_anova(choice_data, conjoint_formula, id = ~ID, by = ~altID)
print(profile_order_test)

# Estimating and plotting marginal means by profile order to inspect order effects
cj_profile_order <- cregg::cj(
  choice_data, conjoint_formula,
  id = ~ID, estimate = "mm", by = ~altID
)

plot_profile_order <- plot(cj_profile_order,
                           group           = "altID",
                           feature_headers = FALSE) +
  xlab("Marginal mean (pr. candidate preferred)") +
  vline_half + conjoint_facet + conjoint_theme +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Profile A", "Profile B")) +
  scale_shape_discrete(labels = c("Profile A", "Profile B"))

ggsave(file.path(appendix_fig_path, "profile_order.pdf"),
       plot = plot_profile_order, width = 6, height = 7)

## 5c Attribute frequency distribution #########################################

# Recoding candidate policy positions with descriptive labels
choice_data <- choice_data %>%
  mutate(
    cand_immig = factor(cand_immig,
                        levels = c("Less restrictive", "Stay as today", "More restrictive"),
                        labels = c("Less restrictive", "Status quo", "More restrictive")),
    cand_econ  = factor(cand_econ,
                        levels = c("More public welfare", "Same level", "Lower taxes"),
                        labels = c("More public welfare", "Status quo ", "Lower taxes"))
  )

# Adding correct labels
attr(choice_data$cand_immig, "label") <- "Immigration"
attr(choice_data$cand_econ, "label") <- "Economy"

# Checking that candidate attributes are uniformly distributed, with congruence and issues shown separately
freq_check <- cj_freqs(choice_data, selected ~ cand_scandal + cand_reputation + congruence
                       + cand_gender + cand_age + cand_immig + cand_econ , id = ~ID)

# Plotting attribute frequency
plot_freq <- plot(freq_check, feature_headers = FALSE) +
  conjoint_theme + theme(legend.position = "none") +
  ylab("Distribution of attribute levels") + conjoint_facet +
  scale_fill_manual(values = rep("grey50", 20)) +
  scale_color_manual(values = rep("grey50", 20))

ggsave(file.path(appendix_fig_path, "attribute_frequencies.pdf"),
       plot = plot_freq, width = 6, height = 7)

## 5d Balance tests ############################################################

# Checking that randomisation holds across respondent gender
mm_bal_gender <- mm(
  choice_data %>% mutate(resp_gender_num = ifelse(resp_gender == "Woman", 1, 0)),
  resp_gender_num ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(ifelse(choice_data$resp_gender == "Woman", 1, 0), na.rm = TRUE)
)

# Plotting balance across gender
plot_bal_gender <- plot(mm_bal_gender, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (woman resp.)") + conjoint_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(ifelse(choice_data$resp_gender == "Woman", 1, 0), na.rm = TRUE),
             linetype = "dashed") + conjoint_facet


# Checking that randomisation holds across respondent age
mm_bal_age <- mm(
  choice_data %>% filter(!is.na(resp_age)),
  resp_age ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(choice_data$resp_age, na.rm = TRUE)
)

# Plotting balance across respondent age
plot_bal_age <- plot(mm_bal_age, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (resp. age)") + conjoint_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(choice_data$resp_age, na.rm = TRUE),
             linetype = "dashed") + conjoint_facet


# Checking that dropping out is unrelated to candidate attributes
mm_dropout <- mm(
  choice_data %>% mutate(completed_num = as.numeric(completed)),
  completed_num ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(choice_data$completed, na.rm = TRUE)
)

# Plotting balance across dropout
plot_dropout <- plot(mm_dropout, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (survey completion)") + conjoint_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(choice_data$completed, na.rm = TRUE),
             linetype = "dashed") + conjoint_facet


# Combining the plots into one figure
plot_balance <- grid.arrange(
  plot_bal_gender,
  plot_bal_age  + theme(axis.text.y = element_blank(),
                            strip.text.y = element_blank()),
  plot_dropout + theme(axis.text.y = element_blank(),
                            strip.text.y = element_blank()),
  ncol   = 3,
  widths = c(1.7, 1, 1)
)

ggsave(file.path(appendix_fig_path, "balance_tests.pdf"),
       plot = plot_balance, width = 8.6, height = 6.3)


# Extracting p-values from the balance tests to check whether any are significant
as.data.frame(mm_bal_gender) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()

as.data.frame(mm_bal_age) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()

as.data.frame(mm_dropout) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()

## 5e Hypothesis 4 robustness: full range of congruence scores #################
# Robustness check for H4 without pooling the congruence variable into three categories

# Testing interaction effect relative to the middle congruence score (4)
choice_data <- choice_data %>%
  mutate(congruence_sum_f = relevel(factor(congruence_sum), ref = "4"))

acie_cong_sum <- cjoint::amce(
  selected ~ scandal_any * congruence_sum_f + cand_reputation + cand_gender + cand_age,
  data          = choice_data,
  respondent.id = "ID"
)
summary(acie_cong_sum)
# Congruence score 6 vs. 4 = 0.09, p < 0.01; no other score differs from 4

# Extracting the interaction terms
cong_sum_raw <- tidy_cjoint(acie_cong_sum) %>%
  filter(grepl(":", Attribute)) %>%
  mutate(congruence_sum = as.numeric(gsub("\\D", "", Level))) %>%
  bind_rows(
    tibble(congruence_sum = 4, Estimate = 0, SE = 0)
  ) %>%
  arrange(congruence_sum) %>%
  mutate(
    Lower = Estimate - 1.96 * SE,
    Upper = Estimate + 1.96 * SE
  )

# Plotting interaction effects across congruence sum
plot_cong_sum_int <- cong_sum_raw %>%
  filter(congruence_sum != 4) %>%
  mutate(
    label = factor(paste("Scandal × Congruence score =", congruence_sum),
                   levels = paste("Scandal × Congruence score =", sort(unique(congruence_sum)))),
    type  = "Interaction effect"
  ) %>%
  ggplot(aes(x = Estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = Lower, xend = Upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name   = "ACIE (pr. candidate preferred)",
                     limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() +
  conjoint_theme +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(appendix_fig_path, "aux_cong_sum_int.pdf"),
       plot = plot_cong_sum_int, width = 6, height = 2.5)


## 5f Hypothesis 4 robustness: congruence treated as continuous ################
# Robustness check for H4 treating congruence as a continuous variable

# Fitting a linear model by treating congruence as a continuous moderator
fit_param <- lm_robust(
  selected ~ scandal_any * congruence_sum + cand_reputation + cand_gender + cand_age,
  data     = choice_data,
  clusters = ID
)

# Plotting the linear relationship
plot_param <- plot_slopes(fit_param,
                          variables  = "scandal_any",
                          condition  = "congruence_sum") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name   = "Congruence \n (2 = most congruent, 6 = least congruent)",
                     breaks = 2:6) +
  scale_y_continuous(name = "Scandal effect (AMCE)") +
  conjoint_theme +
  theme(
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.border       = element_rect(color = "grey70", fill = NA)
  )


# Fitting a model with a squared term, treating congruence as a continuous moderator
fit_param_sq <- lm_robust(
  selected ~ scandal_any * congruence_sum + scandal_any * I(congruence_sum^2) +
    cand_reputation + cand_gender + cand_age,
  data     = choice_data,
  clusters = ID,
)

# Plotting the non-linear relationship
plot_param_sq <- plot_slopes(fit_param_sq,
                             variables  = "scandal_any",
                             condition  = "congruence_sum") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name   = "Congruence \n (2 = most congruent, 6 = least congruent )",
                     breaks = 2:6) +
  scale_y_continuous(name = "Scandal effect (AMCE)") +
  conjoint_theme +
  theme(
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.border       = element_rect(color = "grey70", fill = NA)
  )



# Combining linear and quadratic plots side by side with panel headers
plot_param_titled    <- arrangeGrob(plot_param,
                                    top = textGrob("A) Linear model",
                                                   x  = 0.375,
                                                   gp = gpar(fontsize = 10,
                                                             fontface = "bold")))
plot_param_sq_titled <- arrangeGrob(plot_param_sq,
                                    top = textGrob("B) Quadratic model",
                                                   x  = 0.415,
                                                   gp = gpar(fontsize = 10,
                                                             fontface = "bold")))

plot_param_combined <- grid.arrange(
  plot_param_titled,
  plot_param_sq_titled,
  ncol = 2
)

ggsave(file.path(appendix_fig_path, "aux_cong_parametric.pdf"),
       plot = plot_param_combined, width = 6.1, height = 2.75)

# Formally comparing the fit of the two models
waldtest(fit_param, fit_param_sq, vcov = vcov(fit_param_sq))


## 5g Hypothesis 5 robustness: trust split at the scale midpoint ###############
# Robustness check for H5 using midpoint (5) split instead of median

# Formally testing the interaction term with the alternative trust moderator
acie_h5_midpoint <- cjoint::amce(
  selected ~ scandal_any + cand_reputation + cand_gender + cand_age + congruence +
    high_trust_midpoint:scandal_any,
  data               = choice_data %>% filter(!is.na(high_trust_midpoint)),
  respondent.id      = "ID",
  respondent.varying = "high_trust_midpoint"
)

# Extracting interaction term
interaction_est_midpoint <- acie_h5_midpoint$cond.estimates$`hightrustmidpoint:scandalany`["Conditional Estimate", ]
interaction_se_midpoint  <- acie_h5_midpoint$cond.estimates$`hightrustmidpoint:scandalany`["Std. Error", ]
interaction_z_midpoint   <- interaction_est_midpoint / interaction_se_midpoint
interaction_p_midpoint   <- 2 * (1 - pnorm(abs(interaction_z_midpoint)))
# Scandal x high trust not significant at the midpoint split, p = 0.17


# Plotting interaction term
plot_h5_midpoint_int <- tibble(
  label    = "Scandal × High trust",
  estimate = interaction_est_midpoint,
  lower    = interaction_est_midpoint - 1.96 * interaction_se_midpoint,
  upper    = interaction_est_midpoint + 1.96 * interaction_se_midpoint,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name   = "ACIE (pr. candidate preferred)",
                     limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() +
  conjoint_theme +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none"
  )

ggsave(file.path(appendix_fig_path, "aux_trust_midpoint.pdf"),
       plot = plot_h5_midpoint_int, width = 6, height = 1.8)


## 5h Hypothesis 5 robustness: trust treated as continuous #####################
# Robustness check for H5, treating trust in politicians as a continuous variable

# Fitting a linear model using trust as a continuous moderator
fit_param_trust <- lm_robust(
  selected ~ scandal_any * trust + cand_reputation + congruence + cand_gender + cand_age,
  data     = choice_data,
  clusters = ID,
)

# Plotting the linear relationship
plot_trust_para<-plot_slopes(fit_param_trust,
            variables = "scandal_any",
            condition = "trust") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name   = "Trust in politicians (0 = no trust, 10 = full trust)",
                     breaks = 0:10) +
  scale_y_continuous(name = "Scandal effect (AMCE)") +
  conjoint_theme +
  theme(
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.border       = element_rect(color = "grey70", fill = NA)
  )

ggsave(file.path(appendix_fig_path, "aux_trust_parametric.pdf"),
       plot = plot_trust_para, width = 6, height = 2.75)


# 06 Auxiliary analysis ########################################################

## 6a Heterogeneity by respondent gender #######################################
# Testing whether the scandal effect differs by respondent gender

# Recoding gender to keep only men and women
choice_data <- choice_data %>%
  mutate(resp_gender_h = factor(
    ifelse(resp_gender %in% c("Man", "Woman"), as.character(resp_gender), NA_character_),
    levels = c("Man", "Woman")
  ))

# Formally testing the interaction term for Scandal x Respondent Gender
acie_gender <- cjoint::amce(
  selected ~ scandal_any + resp_gender_h + scandal_any:resp_gender_h +
    cand_reputation + congruence + cand_age + cand_gender,
  data               = choice_data %>% filter(!is.na(resp_gender_h)),
  respondent.id      = "ID",
  respondent.varying = "resp_gender_h"
)

# Extracting the term
gender_int_est <- acie_gender$cond.estimates$`respgenderh:scandalany`["Conditional Estimate", ]
gender_int_se  <- acie_gender$cond.estimates$`respgenderh:scandalany`["Std. Error", ]
gender_int_z   <- gender_int_est / gender_int_se
gender_int_p   <- 2 * (1 - pnorm(abs(gender_int_z)))
# Scandal x respondent gender not significant, p = 0.42


# Formally testing the interaction term for Scandal Type x Respondent Gender
acie_gender_type <- cjoint::amce(
  selected ~ scandal_type + resp_gender_h + scandal_type:resp_gender_h +
    cand_reputation + congruence + cand_age + cand_gender,
  data               = choice_data %>% filter(!is.na(resp_gender_h)),
  respondent.id      = "ID",
  respondent.varying = "resp_gender_h"
)


# Extracting the term
gender_type_office_est <- acie_gender_type$cond.estimates$`respgenderh:scandaltype`["Conditional Estimate", "respgenderhWoman:scandaltypeOfficerelated"]
gender_type_office_se  <- acie_gender_type$cond.estimates$`respgenderh:scandaltype`["Std. Error",           "respgenderhWoman:scandaltypeOfficerelated"]
gender_type_office_z   <- gender_type_office_est / gender_type_office_se
gender_type_office_p   <- 2 * (1 - pnorm(abs(gender_type_office_z)))
# Scandal type x respondent gender not significant, p = 0.38


# Formally testing the interaction term for Candidate Reaction x Respondent Gender
acie_gender_reaction <- cjoint::amce(
  selected ~ scandal_reaction + resp_gender_h + scandal_reaction:resp_gender_h +
    cand_reputation + congruence + cand_age + cand_gender,
  data               = choice_data %>% filter(!is.na(resp_gender_h)),
  respondent.id      = "ID",
  respondent.varying = "resp_gender_h"
)

# Extracting the term
gender_reaction_apology_est <- acie_gender_reaction$cond.estimates$`respgenderh:scandalreaction`["Conditional Estimate", "respgenderhWoman:scandalreactionApology"]
gender_reaction_apology_se  <- acie_gender_reaction$cond.estimates$`respgenderh:scandalreaction`["Std. Error",           "respgenderhWoman:scandalreactionApology"]
gender_reaction_apology_z   <- gender_reaction_apology_est / gender_reaction_apology_se
gender_reaction_apology_p   <- 2 * (1 - pnorm(abs(gender_reaction_apology_z)))
# Candidate reaction x respondent gender not significant, p = 0.31


# Plotting interaction terms for respondent gender in individual panels
plot_gender_any <- tibble(
  label    = "         Scandal × Woman respondent",
  estimate = gender_int_est,
  lower    = gender_int_est - 1.96 * gender_int_se,
  upper    = gender_int_est + 1.96 * gender_int_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_gender_type <- tibble(
  label    = "Office-related × Woman respondent",
  estimate = gender_type_office_est,
  lower    = gender_type_office_est - 1.96 * gender_type_office_se,
  upper    = gender_type_office_est + 1.96 * gender_type_office_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_gender_reaction <- tibble(
  label    = "         Apology × Woman respondent",
  estimate = gender_reaction_apology_est,
  lower    = gender_reaction_apology_est - 1.96 * gender_reaction_apology_se,
  upper    = gender_reaction_apology_est + 1.96 * gender_reaction_apology_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "ACIE (pr. candidate preferred)", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

# Adding panel titles
plot_gender_any_titled      <- arrangeGrob(plot_gender_any,
                                           top = textGrob("A) Scandal",
                                                          x  = 0.495,
                                                          gp = gpar(fontsize = 10,
                                                                    fontface = "bold")))
plot_gender_type_titled     <- arrangeGrob(plot_gender_type,
                                           top = textGrob("B) Scandal type",
                                                          x  = 0.52,
                                                          gp = gpar(fontsize = 10,
                                                                    fontface = "bold")))
plot_gender_reaction_titled <- arrangeGrob(plot_gender_reaction,
                                           top = textGrob("C) Candidate reaction",
                                                          x  = 0.55,
                                                          gp = gpar(fontsize = 10,
                                                                    fontface = "bold")))

# Combining the panels and saving the figure
plot_gender_int <- grid.arrange(
  plot_gender_any_titled,
  plot_gender_type_titled,
  plot_gender_reaction_titled,
  ncol    = 1,
  heights = c(1, 1, 1)
)

ggsave(file.path(appendix_fig_path, "aux_gender_int.pdf"),
       plot = plot_gender_int, width = 6, height = 6)

## 6b Heterogeneity by candidate gender ########################################
# Testing whether the scandal effect differs by candidate gender

# Formally testing the interaction term for Scandal x Candidate Gender
acie_cand_gender <- cjoint::amce(
  selected ~ scandal_any * cand_gender + cand_reputation + congruence + cand_age,
  data          = choice_data,
  respondent.id = "ID"
)
summary(acie_cand_gender)
# Scandal x candidate gender not significant, p = 0.10


# Formally testing the interaction term for Scandal Type x Candidate Gender
acie_cand_gender_type <- cjoint::amce(
  selected ~ scandal_type * cand_gender + cand_reputation + congruence + cand_age,
  data          = choice_data,
  respondent.id = "ID"
)
summary(acie_cand_gender_type)
# Scandal type x candidate gender significant, p < 0.01
# The interaction effect is 8 percentage points

# Formally testing the interaction term for Candidate Reaction x Candidate Gender
acie_cand_gender_reaction <- cjoint::amce(
  selected ~ scandal_reaction * cand_gender + cand_reputation + congruence + cand_age,
  data          = choice_data,
  respondent.id = "ID"
)
summary(acie_cand_gender_reaction)
# Candidate reaction x candidate gender not significant, p = 0.85

# Extracting interaction terms for figure
cand_gender_any_int <- tidy_cjoint(acie_cand_gender) %>%
  filter(grepl(":", Attribute))
cand_gender_any_est <- cand_gender_any_int$Estimate[1]
cand_gender_any_se  <- cand_gender_any_int$SE[1]

cand_gender_type_int <- tidy_cjoint(acie_cand_gender_type) %>%
  filter(grepl(":", Attribute), !grepl("Noscandal", Level))
cand_gender_type_est <- cand_gender_type_int$Estimate[grepl("Office", cand_gender_type_int$Level)]
cand_gender_type_se  <- cand_gender_type_int$SE[grepl("Office", cand_gender_type_int$Level)]

cand_gender_reaction_int <- tidy_cjoint(acie_cand_gender_reaction) %>%
  filter(grepl(":", Attribute), !grepl("Noscandal", Level))
cand_gender_reaction_est <- cand_gender_reaction_int$Estimate[grepl("Apology", cand_gender_reaction_int$Level)]
cand_gender_reaction_se  <- cand_gender_reaction_int$SE[grepl("Apology", cand_gender_reaction_int$Level)]


# Testing the interaction for scandal type using no scandal as reference
acie_cand_gender_type2 <- cjoint::amce(
  selected ~ scandal_type * cand_gender + cand_reputation + congruence + cand_age,
  data          = choice_data %>% mutate(scandal_type = relevel(factor(scandal_type), ref = "No scandal")),
  respondent.id = "ID"
)
summary(acie_cand_gender_type2)
# Using no scandal as reference, men are punished 7 percentage points more than women for personal scandals, p < 0.01

# Extracting interaction terms
cand_gender_type2_int <- tidy_cjoint(acie_cand_gender_type2) %>%
  filter(grepl(":", Attribute), !grepl("Noscandal", Level))

cand_gender_type2_personal_est <- cand_gender_type2_int$Estimate[grepl("Personal", cand_gender_type2_int$Level)]
cand_gender_type2_personal_se  <- cand_gender_type2_int$SE[grepl("Personal", cand_gender_type2_int$Level)]
cand_gender_type2_office_est   <- cand_gender_type2_int$Estimate[grepl("Office", cand_gender_type2_int$Level)]
cand_gender_type2_office_se    <- cand_gender_type2_int$SE[grepl("Office", cand_gender_type2_int$Level)]

# Plotting interaction terms for candidate gender in individual panels
plot_cand_gender_any <- tibble(
  label    = "         Scandal × Woman candidate",
  estimate = cand_gender_any_est,
  lower    = cand_gender_any_est - 1.96 * cand_gender_any_se,
  upper    = cand_gender_any_est + 1.96 * cand_gender_any_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_cand_gender_type <- tibble(
  label    = "Office-related × Woman candidate",
  estimate = cand_gender_type_est,
  lower    = cand_gender_type_est - 1.96 * cand_gender_type_se,
  upper    = cand_gender_type_est + 1.96 * cand_gender_type_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_cand_gender_type2 <- tibble(
  label    = c("Personal × Woman candidate",
               "Office-related × Woman candidate"),
  estimate = c(cand_gender_type2_personal_est, cand_gender_type2_office_est),
  lower    = c(cand_gender_type2_personal_est - 1.96 * cand_gender_type2_personal_se,
               cand_gender_type2_office_est   - 1.96 * cand_gender_type2_office_se),
  upper    = c(cand_gender_type2_personal_est + 1.96 * cand_gender_type2_personal_se,
               cand_gender_type2_office_est   + 1.96 * cand_gender_type2_office_se),
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label, levels = rev(label))) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_cand_gender_reaction <- tibble(
  label    = "         Apology × Woman candidate",
  estimate = cand_gender_reaction_est,
  lower    = cand_gender_reaction_est - 1.96 * cand_gender_reaction_se,
  upper    = cand_gender_reaction_est + 1.96 * cand_gender_reaction_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "ACIE (pr. candidate preferred)", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

# Adding panel titles
plot_cand_gender_any_titled      <- arrangeGrob(plot_cand_gender_any,
                                                top = textGrob("A) Scandal",
                                                               x  = 0.475,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))
plot_cand_gender_type_titled     <- arrangeGrob(plot_cand_gender_type,
                                                top = textGrob("B1) Scandal type (Ref: Personal)",
                                                               x  = 0.59,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))
plot_cand_gender_type2_titled    <- arrangeGrob(plot_cand_gender_type2,
                                                top = textGrob("B2) Scandal type (Ref: No scandal)",
                                                               x  = 0.605,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))
plot_cand_gender_reaction_titled <- arrangeGrob(plot_cand_gender_reaction,
                                                top = textGrob("C) Candidate reaction",
                                                               x  = 0.535,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))

# Combining the panels and saving the figure
plot_cand_gender_int <- grid.arrange(
  plot_cand_gender_any_titled,
  plot_cand_gender_type_titled,
  plot_cand_gender_type2_titled,
  plot_cand_gender_reaction_titled,
  ncol    = 1,
  heights = c(1, 1, 1, 1)
)

ggsave(file.path(appendix_fig_path, "aux_cand_gender_int.pdf"),
       plot = plot_cand_gender_int, width = 6, height = 6.75)


## 6c Heterogeneity by gender congruence #######################################
# Testing whether the scandal effect differs when respondent and candidate gender match

# Creating a gender congruence variable
choice_data <- choice_data %>%
  mutate(gender_congruent = factor(
    ifelse(as.character(resp_gender_h) == as.character(cand_gender),
           "Gender match", "Gender mismatch"),
    levels = c("Gender mismatch", "Gender match")
  ))

# Formally testing the interaction term for Scandal x Gender Congruence
acie_gender_cong <- cjoint::amce(
  selected ~ scandal_any * gender_congruent + cand_reputation + congruence + cand_age + cand_gender,
  data          = choice_data %>% filter(!is.na(gender_congruent)),
  respondent.id = "ID"
)
summary(acie_gender_cong)
# Scandal x gender congruence not significant, p = 0.78

# Formally testing the interaction term for Scandal Type x Gender Congruence
acie_gender_cong_type <- cjoint::amce(
  selected ~ scandal_type * gender_congruent + cand_reputation + congruence + cand_age + cand_gender,
  data          = choice_data %>% filter(!is.na(gender_congruent)),
  respondent.id = "ID"
)
summary(acie_gender_cong_type)
# Scandal type x gender congruence not significant, p = 0.07


# Formally testing the interaction term for Candidate Reaction x Gender Congruence
acie_gender_cong_reaction <- cjoint::amce(
  selected ~ scandal_reaction * gender_congruent + cand_reputation + congruence + cand_age + cand_gender,
  data          = choice_data %>% filter(!is.na(gender_congruent)),
  respondent.id = "ID"
)
summary(acie_gender_cong_reaction)
# Candidate reaction x gender congruence not significant, p = 0.78

# Extracting the interaction terms
gender_cong_any_int <- tidy_cjoint(acie_gender_cong) %>%
  filter(grepl(":", Attribute))
gender_cong_any_est <- gender_cong_any_int$Estimate[1]
gender_cong_any_se  <- gender_cong_any_int$SE[1]

gender_cong_type_int <- tidy_cjoint(acie_gender_cong_type) %>%
  filter(grepl(":", Attribute), !grepl("Noscandal", Level))
gender_cong_type_est <- gender_cong_type_int$Estimate[grepl("Office", gender_cong_type_int$Level)]
gender_cong_type_se  <- gender_cong_type_int$SE[grepl("Office", gender_cong_type_int$Level)]

gender_cong_reaction_int <- tidy_cjoint(acie_gender_cong_reaction) %>%
  filter(grepl(":", Attribute), !grepl("Noscandal", Level))
gender_cong_reaction_est <- gender_cong_reaction_int$Estimate[grepl("Apology", gender_cong_reaction_int$Level)]
gender_cong_reaction_se  <- gender_cong_reaction_int$SE[grepl("Apology", gender_cong_reaction_int$Level)]

# Plotting interaction terms for gender congruence in individual panels
plot_gender_cong_any <- tibble(
  label    = "         Scandal × Gender match",
  estimate = gender_cong_any_est,
  lower    = gender_cong_any_est - 1.96 * gender_cong_any_se,
  upper    = gender_cong_any_est + 1.96 * gender_cong_any_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_gender_cong_type <- tibble(
  label    = "Office-related × Gender match",
  estimate = gender_cong_type_est,
  lower    = gender_cong_type_est - 1.96 * gender_cong_type_se,
  upper    = gender_cong_type_est + 1.96 * gender_cong_type_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_gender_cong_reaction <- tibble(
  label    = "         Apology × Gender match",
  estimate = gender_cong_reaction_est,
  lower    = gender_cong_reaction_est - 1.96 * gender_cong_reaction_se,
  upper    = gender_cong_reaction_est + 1.96 * gender_cong_reaction_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "ACIE (pr. candidate preferred)", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

# Adding panel titles
plot_gender_cong_any_titled      <- arrangeGrob(plot_gender_cong_any,
                                                top = textGrob("A) Scandal",
                                                               x  = 0.435,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))
plot_gender_cong_type_titled     <- arrangeGrob(plot_gender_cong_type,
                                                top = textGrob("B) Scandal type",
                                                               x  = 0.46,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))
plot_gender_cong_reaction_titled <- arrangeGrob(plot_gender_cong_reaction,
                                                top = textGrob("C) Candidate reaction",
                                                               x  = 0.495,
                                                               gp = gpar(fontsize = 10,
                                                                         fontface = "bold")))

# Combining the panels and saving the figure
plot_gender_cong_int <- grid.arrange(
  plot_gender_cong_any_titled,
  plot_gender_cong_type_titled,
  plot_gender_cong_reaction_titled,
  ncol    = 1,
  heights = c(1, 1, 1)
)

ggsave(file.path(appendix_fig_path, "aux_gender_cong_int.pdf"),
       plot = plot_gender_cong_int, width = 6, height = 6)


## 6d Heterogeneity by political bloc ##########################################
# Testing whether the scandal effect differs between left and right bloc voters

# Creating a bloc variable
choice_data <- choice_data %>%
  mutate(pol_bloc = factor(
    case_when(
      party_vote %in% c("Socialdemokratiet", "Radikale Venstre",
                        "SF - Socialistisk Folkeparti", "Enhedslisten",
                        "Alternativet")                           ~ "Left bloc",
      party_vote %in% c("Det Konservative Folkeparti", "Venstre",
                        "Liberal Alliance", "Dansk Folkeparti",
                        "Danmarksdemokraterne", "Borgernes Parti") ~ "Right bloc",
      TRUE                                                          ~ NA_character_
    ),
    levels = c("Left bloc", "Right bloc")
  ))

# Formally testing the interaction term for Scandal x Political Bloc
acie_bloc <- cjoint::amce(
  selected ~ scandal_any + pol_bloc + scandal_any:pol_bloc +
    cand_reputation + congruence + cand_gender + cand_age,
  data               = choice_data %>% filter(!is.na(pol_bloc)),
  respondent.id      = "ID",
  respondent.varying = "pol_bloc"
)

# Extracting the term
bloc_int_est <- acie_bloc$cond.estimates$`polbloc:scandalany`["Conditional Estimate", ]
bloc_int_se  <- acie_bloc$cond.estimates$`polbloc:scandalany`["Std. Error", ]
bloc_int_z   <- bloc_int_est / bloc_int_se
bloc_int_p   <- 2 * (1 - pnorm(abs(bloc_int_z)))
# Scandal x political bloc not significant, p = 0.54

# Formally testing the interaction term for Scandal Type x Political bloc
acie_bloc_type <- cjoint::amce(
  selected ~ scandal_type + pol_bloc + scandal_type:pol_bloc +
    cand_reputation + congruence + cand_gender + cand_age,
  data               = choice_data %>% filter(!is.na(pol_bloc)),
  respondent.id      = "ID",
  respondent.varying = "pol_bloc"
)

# Extracting the term
bloc_type_office_est <- acie_bloc_type$cond.estimates$`polbloc:scandaltype`["Conditional Estimate", "polblocRightbloc:scandaltypeOfficerelated"]
bloc_type_office_se  <- acie_bloc_type$cond.estimates$`polbloc:scandaltype`["Std. Error",           "polblocRightbloc:scandaltypeOfficerelated"]
bloc_type_office_z   <- bloc_type_office_est / bloc_type_office_se
bloc_type_office_p   <- 2 * (1 - pnorm(abs(bloc_type_office_z)))
# Scandal type x political bloc not significant, p = 0.74

# Formally testing the interaction term for Candidate Reaction x Political bloc
acie_bloc_reaction <- cjoint::amce(
  selected ~ scandal_reaction + pol_bloc + scandal_reaction:pol_bloc +
    cand_reputation + congruence + cand_gender + cand_age,
  data               = choice_data %>% filter(!is.na(pol_bloc)),
  respondent.id      = "ID",
  respondent.varying = "pol_bloc"
)

# Extracting the term
bloc_reaction_apology_est <- acie_bloc_reaction$cond.estimates$`polbloc:scandalreaction`["Conditional Estimate", "polblocRightbloc:scandalreactionApology"]
bloc_reaction_apology_se  <- acie_bloc_reaction$cond.estimates$`polbloc:scandalreaction`["Std. Error",           "polblocRightbloc:scandalreactionApology"]
bloc_reaction_apology_z   <- bloc_reaction_apology_est / bloc_reaction_apology_se
bloc_reaction_apology_p   <- 2 * (1 - pnorm(abs(bloc_reaction_apology_z)))
# Candidate reaction x political bloc not significant, p = 0.75


# Plotting interaction terms in individual panels
plot_bloc_any <- tibble(
  label    = "         Scandal × Right bloc",
  estimate = bloc_int_est,
  lower    = bloc_int_est - 1.96 * bloc_int_se,
  upper    = bloc_int_est + 1.96 * bloc_int_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_bloc_type <- tibble(
  label    = "Office-related × Right bloc",
  estimate = bloc_type_office_est,
  lower    = bloc_type_office_est - 1.96 * bloc_type_office_se,
  upper    = bloc_type_office_est + 1.96 * bloc_type_office_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

plot_bloc_reaction <- tibble(
  label    = "         Apology × Right bloc",
  estimate = bloc_reaction_apology_est,
  lower    = bloc_reaction_apology_est - 1.96 * bloc_reaction_apology_se,
  upper    = bloc_reaction_apology_est + 1.96 * bloc_reaction_apology_se,
  type     = "Interaction effect"
) %>%
  mutate(label = factor(label)) %>%
  ggplot(aes(x = estimate, y = label, shape = type)) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = lower, xend = upper, y = label, yend = label),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Interaction", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "ACIE (pr. candidate preferred)", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + conjoint_theme +
  theme(panel.grid = element_blank(), legend.position = "none")

# Adding panel titles
plot_bloc_any_titled      <- arrangeGrob(plot_bloc_any,
                                         top = textGrob("A) Scandal",
                                                        x  = 0.395,
                                                        gp = gpar(fontsize = 10,
                                                                  fontface = "bold")))
plot_bloc_type_titled     <- arrangeGrob(plot_bloc_type,
                                         top = textGrob("B) Scandal type",
                                                        x  = 0.42,
                                                        gp = gpar(fontsize = 10,
                                                                  fontface = "bold")))
plot_bloc_reaction_titled <- arrangeGrob(plot_bloc_reaction,
                                         top = textGrob("C) Candidate reaction",
                                                        x  = 0.455,
                                                        gp = gpar(fontsize = 10,
                                                                  fontface = "bold")))

# Combining the panels and saving the figure
plot_bloc_int <- grid.arrange(
  plot_bloc_any_titled,
  plot_bloc_type_titled,
  plot_bloc_reaction_titled,
  ncol    = 1,
  heights = c(1, 1, 1)
)

ggsave(file.path(appendix_fig_path, "aux_bloc_int.pdf"),
       plot = plot_bloc_int, width = 6, height = 6)

# Script end ###################################################################

