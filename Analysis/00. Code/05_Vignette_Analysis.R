# Analysis of Vignette Experiment ##############################################

# 00 Dependencies ##############################################################

# Loading packages
library(tidyverse)
library(estimatr)
library(kableExtra)
library(knitr)
library(ggpubr)
library(cregg)
library(gridExtra)
library(grid)

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

# Importing cleaned vignette data
vignette_data <- read_rds('02. Clean data/vignette_data.rds')

# Attaching the correct congruence axis label
attr(vignette_data$congruence, "label") <- "Congruence"

# 02 Helper functions ##########################################################

# Defining the output paths for figures and tables
paper_fig_path    <- "03. Output/Paper/Figures"
paper_tab_path    <- "03. Output/Paper/Tables"
appendix_fig_path <- "03. Output/Appendix/Figures"
appendix_tab_path <- "03. Output/Appendix/Tables"

# Function to save a kable table as a .tex file
save_tex <- function(tbl, path) {
  writeLines(as.character(tbl), path)
}


# Function to run OLS model with clustered standard errors and return tidy output
run_ols <- function(data, outcome, rhs) {
  formula <- as.formula(paste(outcome, "~", rhs))
  fit     <- lm_robust(formula, data = data, clusters = ID)
  tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(outcome = outcome_labels[outcome])
}

# Function to format regression table columns
format_estimates <- function(df, est_col = "Coefficient") {
  df %>%
    mutate(
      stars = case_when(
        is.na(p)  ~ "",
        p < 0.01  ~ "***",
        p < 0.05  ~ "**",
        p < 0.1   ~ "*",
        TRUE      ~ ""
      ),
      !!est_col := ifelse(is.na(.data[[est_col]]), "--",
                          sprintf("%.3f%s", .data[[est_col]], stars)),
      SE    = ifelse(is.na(SE),    "--", sprintf("%.3f", SE)),
      p     = ifelse(is.na(p),     "--", sprintf("%.3f", p)),
      Lower = ifelse(is.na(Lower), "--", sprintf("%.3f", Lower)),
      Upper = ifelse(is.na(Upper), "--", sprintf("%.3f", Upper))
    ) %>%
    select(-stars)
}


# Function to save a results table as latex
save_results_tex <- function(tidy_df, caption, label, path, midrule = TRUE) {
  n    <- nrow(tidy_df)
  lsep <- if (midrule) c(rep("", n - 3), "\\midrule", "") else rep("", n)
  save_tex(
    kable(tidy_df, format = "latex", booktabs = TRUE,
          caption = caption, label = label,
          linesep = lsep) %>%
      kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
      footnote(general           = "$^*$ $p<0.1$; $^{**}$ $p<0.05$; $^{***}$ $p<0.01$",
               general_title     = "Note: ",
               footnote_as_chunk = TRUE,
               escape            = FALSE),
    path
  )
}

# Shared theme for vignette plots
vignette_theme <- theme(
  legend.position    = "none",
  axis.text.y        = element_text(size = 10),
  axis.text.x        = element_text(size = 10),
  axis.title.x       = element_text(size = 10, margin = margin(t = 8)),
  strip.text         = element_text(size = 9),
  plot.margin        = margin(8, 12, 8, 8)
)

# Shared facet for vignette plots
vignette_facet <- ggplot2::facet_wrap(~feature, ncol = 1L,
                                      scales = "free_y",
                                      strip.position = "left")


# 03 Outcome labels and formulas ###############################################

# Labels for outcome variables
outcome_labels <- c(
  trustworthy = "Trustworthy",
  competent   = "Competent",
  fit_office  = "Fit for office",
  decent      = "Decent person",
  vote_prop   = "Vote propensity"
)

outcomes <- names(outcome_labels)

# Defining basic OLS formula
base_rhs <- "scandal_any + cand_reputation + congruence + cand_gender + cand_age"

# Calculating the mean of each outcome for the figure reference lines
outcome_means <- vignette_data %>%
  select(all_of(outcomes)) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "grand_mean") %>%
  mutate(outcome = factor(outcome, levels = outcomes, labels = outcome_labels))

# 04 Robustness Check ##########################################################

## 4a Distribution of outcome variables ########################################
# Calculating distributions
outcome_dist <- vignette_data %>%
  select(all_of(outcomes)) %>%
  pivot_longer(all_of(outcomes),
               names_to = "outcome",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(outcome = factor(outcome,
                          levels = outcomes,
                          labels = outcome_labels)) %>%
  count(outcome, value) %>%
  group_by(outcome) %>%
  mutate(share = n / sum(n) * 100) %>%
  ungroup()

# Plotting distributions
plot_outcome_dist <- outcome_dist %>%
  ggplot(aes(x = factor(value), y = share)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           color = "white") +
  facet_wrap(~outcome, ncol = 3, scales = "free_x") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Share of respondents (%)",
                     expand = c(0, 0)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(appendix_fig_path, "vignette_outcome_distributions.pdf"),
       plot = plot_outcome_dist,
       width = 6,
       height = 5)


## 4b Attribute frequency distribution #########################################
# Checking that candidate attributes are uniformly distributed across profiles

# Adding a dummy 'selected' variable so that cj_freqs can be used
vignette_freq_data <- vignette_data %>%
  mutate(selected = 1)

# Recoding candidate policy positions with descriptive labels
vignette_freq_data<- vignette_freq_data %>%
  mutate(
    cand_immig = factor(cand_immig,
                        levels = c("Less restrictive", "Stay as today", "More restrictive"),
                        labels = c("Less restrictive", "Status quo", "More restrictive")),
    cand_econ  = factor(cand_econ,
                        levels = c("More public welfare", "Same level", "Lower taxes"),
                        labels = c("More public welfare", "Staus Quo ", "Lower taxes"))
  )

# Adding correct labels
attr(vignette_freq_data$cand_immig, "label") <- "Immigration"
attr(vignette_freq_data$cand_econ, "label") <- "Economy"

# Checking and plotting the attribute distribution
freq_check_vignette <- cj_freqs(
  vignette_freq_data,
  selected ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age +  cand_immig + cand_econ ,
  id = ~ID
)

plot_attr_freq <- plot(freq_check_vignette, feature_headers = FALSE) +
  vignette_theme +
  theme(legend.position = "none") +
  ylab("Distribution of attribute levels") +
  vignette_facet +
  scale_fill_manual(values = rep("grey50", 20)) +
  scale_color_manual(values = rep("grey50", 20))

ggsave(file.path(appendix_fig_path, "vignette_attribute_frequencies.pdf"),
       plot = plot_attr_freq, width = 6, height = 7)


## 4c Carryover effects ########################################################
# Testing whether attribute effects differ by task order
# Labelling the tasks
vignette_data <- vignette_data %>%
  mutate(profil_f = factor(profil, levels = c(1, 2),
                           labels = c("Task 1", "Task 2")))


# Running formal F-tests for all outcomes to check for task order effects
map(outcomes, function(outcome) {
  cat("\nOutcome:", outcome_labels[outcome], "\n")
  temp_data          <- vignette_data
  temp_data$.outcome <- temp_data[[outcome]]
  print(cj_anova(temp_data,
                 .outcome ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
                 id = ~ID, by = ~profil_f))
})

# Testing which attributes drive the slight carryover effect on the competence outcome
fit_carryover <- lm_robust(
  competent ~ (cand_scandal + cand_reputation + congruence +
                 cand_gender + cand_age) * profil_f,
  data     = vignette_data,
  clusters = ID,
)

tidy(fit_carryover) %>%
  filter(grepl(":profil_f", term)) %>%
  select(term, estimate, std.error, p.value) %>%
  filter(p.value < 0.05)


# Estimating separate marginal means for each outcome by task order and plotting
profile_order_plots <- map(outcomes, function(outcome) {
  cj_result <- cregg::cj(
    vignette_data,
    as.formula(paste(outcome, "~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age")),
    id       = ~ID,
    estimate = "mm",
    by       = ~profil_f
  )

  grand_mean <- outcome_means$grand_mean[outcome_means$outcome == outcome_labels[outcome]]

  plot(cj_result, group = "profil_f", feature_headers = FALSE) +
    xlab(outcome_labels[outcome]) +
    geom_vline(xintercept = grand_mean, linetype = "dashed") +
    vignette_facet + vignette_theme +
    theme(
      legend.position = "bottom",
      legend.title    = element_blank(),
      plot.margin = margin(6, 6, 6, 0)
    ) +
    scale_color_discrete(labels = c("Task 1", "Task 2")) +
    scale_shape_manual(values  = c(16, 17))
})

# Extracting the legend from the first plot
legend <- get_legend(profile_order_plots[[1]])

# Removing the legend from all plots
plots_no_legend <- map(profile_order_plots, ~.x + theme(legend.position = "none"))

# Combining the five panels without legends
plot_no_legend <- grid.arrange(
  plots_no_legend[[1]],
  plots_no_legend[[2]] + theme(axis.text.y = element_blank(),
                               strip.text.y = element_blank()),
  plots_no_legend[[3]] + theme(axis.text.y = element_blank(),
                               strip.text.y = element_blank()),
  plots_no_legend[[4]] + theme(axis.text.y = element_blank(),
                               strip.text.y = element_blank()),
  plots_no_legend[[5]] + theme(axis.text.y = element_blank(),
                               strip.text.y = element_blank()),
  ncol   = 5,
  widths = c(2.2, 1, 1, 1, 1)
)

# Adding the shared legend below and saving the figure
plot_task_order <- grid.arrange(
  plot_no_legend,
  arrangeGrob(as_ggplot(legend),
              padding = unit(0, "cm"),
              vp = viewport(x = 0.6, y = 0.5)),
  ncol    = 1,
  heights = c(10, 0.5)
)

ggsave(file.path(appendix_fig_path, "vignette_carryover.pdf"),
       plot = plot_task_order, width = 8.6, height = 6.3)


## 4d Balance vignette #########################################################

# Checking that randomisation holds across respondent gender
mm_bal_gender_v <- mm(
  vignette_data %>% mutate(resp_gender_num = ifelse(resp_gender == "Woman", 1, 0)),
  resp_gender_num ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(ifelse(vignette_data$resp_gender == "Woman", 1, 0), na.rm = TRUE)
)

plot_bal_gender_v <- plot(mm_bal_gender_v, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (woman resp.)") + vignette_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(ifelse(vignette_data$resp_gender == "Woman", 1, 0), na.rm = TRUE),
             linetype = "dashed") + vignette_facet

# Checking that randomisation holds across mean respondent age
mm_bal_age_v <- mm(
  vignette_data %>% filter(!is.na(resp_age)),
  resp_age ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(vignette_data$resp_age, na.rm = TRUE)
)

plot_bal_age_v <- plot(mm_bal_age_v, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (resp. age)") + vignette_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(vignette_data$resp_age, na.rm = TRUE),
             linetype = "dashed") + vignette_facet

# Checking that dropping out is unrelated to candidate attributes
mm_dropout_v <- mm(
  vignette_data %>% mutate(completed_num = as.numeric(completed)),
  completed_num ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID,
  h0 = mean(vignette_data$completed, na.rm = TRUE)
)

plot_dropout_v <- plot(mm_dropout_v, vline_color = "white", feature_headers = FALSE) +
  xlab("Marginal mean (survey completion)") + vignette_theme +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = mean(vignette_data$completed, na.rm = TRUE),
             linetype = "dashed") +
  scale_x_continuous(limits = c(0.95, 1.03), breaks = c(0.95, 1.00, 1.05)) +
  vignette_facet

# Combining the panels into one figure
plot_balance_v <- grid.arrange(
  plot_bal_gender_v,
  plot_bal_age_v   + theme(axis.text.y = element_blank(),
                           strip.text.y = element_blank()),
  plot_dropout_v   + theme(axis.text.y = element_blank(),
                           strip.text.y = element_blank()),
  ncol   = 3,
  widths = c(1.7, 1, 1)
)

ggsave(file.path(appendix_fig_path, "vignette_balance_tests.pdf"),
       plot = plot_balance_v, width = 8.6, height = 6.3)

# Extracting p-values from the balance tests to check whether any are significant
as.data.frame(mm_bal_gender_v) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()

as.data.frame(mm_bal_age_v) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()

as.data.frame(mm_dropout_v) %>%
  select(feature, level, estimate, std.error, p) %>%
  filter(p < 0.05) %>%
  print()
# The only values below p = 0.05 are those with a single observation and therefore no variation

## 4e Estimating AMCE for all attributes using vote propensity #################

# Estimating AMCEs for all attributes using vote propensity, which mimics a rating-based outcome
amce_vote_prop <- cregg::amce(
  vignette_data,
  vote_prop ~ cand_scandal + cand_reputation + congruence + cand_gender + cand_age,
  id = ~ID
)

plot_amce_vote_prop <- plot(amce_vote_prop,
                            vline_color = "white", feature_headers = FALSE) +
  xlab("AMCE (vote propensity, 1-5 scale)") +
  scale_colour_manual(values = rep("black", 10)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  vignette_facet + vignette_theme

ggsave(file.path(appendix_fig_path, "amce_vote_prop.pdf"),
       plot = plot_amce_vote_prop, width = 6, height = 7)


# 05 Analysis ##################################################################

## 5a Overall effect of scandal on all outcomes ################################
# Estimating the effect of a scandal across outcomes

# Running OLS across outcomes
overall_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = base_rhs
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)))

# Plotting results (Figure 7 in main paper)
plot_overall <- overall_results %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome)) +
  geom_point(size = 3) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome),
               linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (scandal vs. no scandal)", limits = c(-1.8, 1.8)) +
  scale_y_discrete(name = "") +
  theme_bw() +
  vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(paper_fig_path, "vignette_overall.pdf"),
       plot = plot_overall, width = 6, height = 4)

# Saving the results table
overall_tidy <- overall_results %>%
  transmute(
    Outcome  = as.character(outcome),
    Coefficient = estimate,
    SE       = std.error,
    p        = p.value,
    Lower    = conf.low,
    Upper    = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Outcome = "N respondents",  Coefficient = sprintf("%d", n_distinct(vignette_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Outcome = "N observations", Coefficient = sprintf("%d", nrow(vignette_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(overall_tidy,
                 caption = "Effect of Scandal on Evaluation of Candidate (Figure 7 in Main Paper)",
                 label   = "tab:vignette_overall",
                 path    = file.path(appendix_tab_path, "vignette_overall.tex"))


## 5b Hypothesis 1 #############################################################
# Estimating the effect of an office-related relative to a personal scandal

# Setting personal scandal as reference level
vignette_data <- vignette_data %>%
  mutate(scandal_type = factor(scandal_type,
                               levels = c("Personal", "No scandal", "Office-related")))

# Running OLS across outcomes
h1_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = "scandal_type + cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_typeOffice-related") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)))

# Plotting results (Figure 8 in main paper)
plot_h1 <- h1_results %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome)) +
  geom_point(size = 3) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome),
               linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (office-related vs. personal)", limits = c(-0.55, 0.55)) +
  scale_y_discrete(name = "") +
  theme_bw() +
  vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(paper_fig_path, "vignette_h1.pdf"),
       plot = plot_h1, width = 6, height = 4)

# Saving the results table
h1_tidy <- h1_results %>%
  transmute(
    Outcome  = as.character(outcome),
    Coefficient = estimate,
    SE       = std.error,
    p        = p.value,
    Lower    = conf.low,
    Upper    = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Outcome = "N respondents",  Coefficient = sprintf("%d", n_distinct(vignette_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Outcome = "N observations", Coefficient = sprintf("%d", nrow(vignette_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h1_tidy,
                 caption = "Effect of Office-related Relative to Personal Scandal on Evaluation of Candidate (Figure 8 in Main Paper)",
                 label   = "tab:vignette_h1",
                 path    = file.path(appendix_tab_path, "vignette_h1.tex"))


# Estimating and plotting marginal means
outcome_mm_h1 <- map_dfr(outcomes, function(o) {
  mm(vignette_data, reformulate("scandal_type", response = o), id = ~ID) %>%
    as.data.frame() %>%
    transmute(
      scandal_type = level,
      outcome      = factor(o, levels = outcomes, labels = outcome_labels),
      mean  = estimate,
      se    = std.error,
      lower = lower,      # clustered CI from cregg
      upper = upper
    )
}) %>%
  mutate(scandal_type = factor(scandal_type,
                               levels = c("No scandal", "Personal", "Office-related")))

plot_h1_mm_top <- outcome_mm_h1 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[1:3]) %>%
  mutate(feature = "Scandal type") %>%
  ggplot(aes(x = mean, y = scandal_type, shape = scandal_type, color = scandal_type)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 16, 16)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

plot_h1_mm_bot <- outcome_mm_h1 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[4:5]) %>%
  mutate(feature = "Scandal type") %>%
  ggplot(aes(x = mean, y = scandal_type, shape = scandal_type, color = scandal_type)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Marginal mean (1-5 scale)") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 16, 16)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

g_top <- ggplotGrob(plot_h1_mm_top)
g_bot <- ggplotGrob(plot_h1_mm_bot)

plot_h1_mm_v <- grid.arrange(
  g_top,
  arrangeGrob(g_bot,
              vp = viewport(x     = 0.5, y = 0.5,
                            width = 0.7, height = 1,
                            just  = c("centre", "centre"))),
  ncol    = 1,
  heights = c(1, 1)
)

ggsave(file.path(appendix_fig_path, "vignette_h1_mm.pdf"),
       plot = plot_h1_mm_v, width = 6.5, height = 3.5)


## 5c Hypothesis 2 #############################################################
# Estimating the effect of an apology relative to a denial

# Setting denial as reference level
vignette_data <- vignette_data %>%
  mutate(scandal_reaction = factor(scandal_reaction,
                                   levels = c("Denial", "No scandal", "Apology")))

# Running OLS across outcomes
h2_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = "scandal_reaction + cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_reactionApology") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)))

# Plotting results (Figure 9 in main paper)
plot_h2 <- h2_results %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome)) +
  geom_point(size = 3) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome),
               linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (apology vs. denial)", limits = c(-0.3, 0.3)) +
  scale_y_discrete(name = "") +
  theme_bw() +
  vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(paper_fig_path, "vignette_h2.pdf"),
       plot = plot_h2, width = 6, height = 4)

# Saving the results table
h2_tidy <- h2_results %>%
  transmute(
    Outcome  = as.character(outcome),
    Coefficient = estimate,
    SE       = std.error,
    p        = p.value,
    Lower    = conf.low,
    Upper    = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Outcome = "N respondents",  Coefficient = sprintf("%d", n_distinct(vignette_data$ID)),
           SE = "", p = "", Lower = "", Upper = ""),
    tibble(Outcome = "N observations", Coefficient = sprintf("%d", nrow(vignette_data)),
           SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h2_tidy,
                 caption = "Effect of Apology relative to Denial on Evaluation of Candidate (Figure 9 in Main Paper)",
                 label   = "tab:vignette_h2",
                 path    = file.path(appendix_tab_path, "vignette_h2.tex"))


# Estimating and plotting marginal means
outcome_mm_h2 <- map_dfr(outcomes, function(o) {
  mm(vignette_data, reformulate("scandal_reaction", response = o), id = ~ID) %>%
    as.data.frame() %>%
    transmute(
      scandal_reaction = factor(level, levels = c("No scandal", "Denial", "Apology")),
      outcome          = factor(o, levels = outcomes, labels = outcome_labels),
      mean  = estimate,
      lower = lower,
      upper = upper
    )
})

plot_h2_mm_top <- outcome_mm_h2 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[1:3]) %>%
  mutate(feature = "Cand. reac.") %>%
  ggplot(aes(x = mean, y = scandal_reaction, shape = scandal_reaction, color = scandal_reaction)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 16, 16)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

plot_h2_mm_bot <- outcome_mm_h2 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[4:5]) %>%
  mutate(feature = "Cand. reac.") %>%
  ggplot(aes(x = mean, y = scandal_reaction, shape = scandal_reaction, color = scandal_reaction)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Marginal mean (1-5 scale)") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 16, 16)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

g_top_h2 <- ggplotGrob(plot_h2_mm_top)
g_bot_h2 <- ggplotGrob(plot_h2_mm_bot)

plot_h2_mm_v <- grid.arrange(
  g_top_h2,
  arrangeGrob(g_bot_h2,
              vp = viewport(x     = 0.5, y = 0.5,
                            width = 0.7, height = 1,
                            just  = c("centre", "centre"))),
  ncol    = 1,
  heights = c(1, 1)
)

ggsave(file.path(appendix_fig_path, "vignette_h2_mm.pdf"),
       plot = plot_h2_mm_v, width = 6.5, height = 3.5)


## 5d Hypothesis 3 #############################################################
# Estimating the effect of a scandal conditional on competence

# Running OLS across outcomes
h3_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = "scandal_any * cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(grepl("scandal_anyScandal:cand_reputation", term)) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcome_labels)),
    term    = case_when(
      grepl("Competent", term) ~ "Scandal × Competent",
      TRUE                     ~ term
    )
  )

# Estimating conditional effects by competence for the plot
h3_neutral <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(cand_reputation == "Neutral"),
  outcome = .x,
  rhs     = "scandal_any + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "Neutral")

h3_competent <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(cand_reputation == "Competent"),
  outcome = .x,
  rhs     = "scandal_any + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "Competent")

# Binding the conditional effects into one dataframe
h3_plot_data <- bind_rows(h3_neutral, h3_competent) %>%
  mutate(group = factor(group, levels = c("Neutral", "Competent")))

# Plotting interaction effects
plot_h3_int <- h3_results %>%
  ggplot(aes(x = estimate, y = outcome, shape = "Interaction effect")) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Outcome", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Interaction coefficient (scandal x competent)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )

# Plotting conditional effects
h3_labels <- h3_plot_data %>%
  filter(outcome == "Trustworthy") %>%
  mutate(nudge = case_when(
    group == "Neutral"   ~ -0.11,
    group == "Competent" ~  0.15
  ))

plot_h3_cond <- h3_plot_data %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome, shape = group, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (scandal vs. no scandal)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2),
                      labels = c("Neutral", "Competent")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("Neutral", "Competent")) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  ) +
  geom_label(data        = h3_labels,
            aes(x       = conf.high + 0.05,
                y       = outcome,
                label   = group),
            nudge_y     = h3_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Combining the panels and saving the figure
plot_h3_cond_titled <- arrangeGrob(plot_h3_cond,
                                   top = textGrob("A) Conditional Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))
plot_h3_int_titled  <- arrangeGrob(plot_h3_int,
                                   top = textGrob("B) Interaction Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))

plot_h3_full <- grid.arrange(
  plot_h3_cond_titled,
  plot_h3_int_titled,
  ncol    = 1,
  heights = c(2, 2)
)

ggsave(file.path(appendix_fig_path, "vignette_h3_full.pdf"),
       plot = plot_h3_full, width = 6, height = 7.5)

# Saving interaction term table
h3_tidy <- h3_results %>%
  transmute(
    Interaction = term,
    Outcome     = as.character(outcome),
    Coefficient    = estimate,
    SE          = std.error,
    p           = p.value,
    Lower       = conf.low,
    Upper       = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h3_tidy,
                 caption = "Interaction Between Scandal and Competence Across Candidate Evaluations",
                 label   = "tab:vignette_h3",
                 path    = file.path(appendix_tab_path, "vignette_h3.tex"))


# Saving conditional effects table
h3_cond_tidy <- h3_plot_data %>%
  transmute(
    Competence    = as.character(group),
    Outcome  = as.character(outcome),
    Coefficient = estimate,
    SE       = std.error,
    p        = p.value,
    Lower    = conf.low,
    Upper    = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Competence = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Competence = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h3_cond_tidy,
                 caption = "Effect of Scandal on Evaluation of Candidate Conditional on Competence",
                 label   = "tab:vignette_h3_cond",
                 path    = file.path(appendix_tab_path, "vignette_h3_cond.tex"))

# Estimating and plotting marginal means
outcome_mm_h3 <- map_dfr(outcomes, function(o) {
  cregg::cj(vignette_data, reformulate("scandal_any", response = o),
            id = ~ID, estimate = "mm", by = ~cand_reputation) %>%
    as.data.frame() %>%
    transmute(
      scandal_any     = factor(level, levels = c("No scandal", "Scandal")),
      cand_reputation = factor(cand_reputation, levels = c("Neutral", "Competent")),
      outcome         = factor(o, levels = outcomes, labels = outcome_labels),
      mean  = estimate,
      lower = lower,
      upper = upper
    )
})

plot_h3_mm_top <- outcome_mm_h3 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[1:3]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = cand_reputation, color = cand_reputation)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

plot_h3_mm_bot <- outcome_mm_h3 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[4:5]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = cand_reputation, color = cand_reputation)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Marginal mean (1-5 scale)") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

legend_h3 <- get_legend(plot_h3_mm_top + theme(legend.position = "bottom",
                                               legend.title    = element_blank()))
g_top_h3  <- ggplotGrob(plot_h3_mm_top)
g_bot_h3  <- ggplotGrob(plot_h3_mm_bot)

plot_h3_mm_v <- grid.arrange(
  g_top_h3,
  arrangeGrob(g_bot_h3,
              vp = viewport(x     = 0.5, y = 0.5,
                            width = 0.7, height = 1,
                            just  = c("centre", "centre"))),
  arrangeGrob(as_ggplot(legend_h3),
              padding = unit(0, "cm"),
              vp = viewport(x = 0.58, y = 0.5)),
  ncol    = 1,
  heights = c(1, 1, 0.15)
)

ggsave(file.path(appendix_fig_path, "vignette_h3_mm.pdf"),
       plot = plot_h3_mm_v, width = 6.5, height = 3.75)


## 5e Hypothesis 4 #############################################################
# Estimating the effect of a scandal conditional on congruence

# Recoding to set medium as the reference level
vignette_data <- vignette_data %>%
  mutate(congruence = relevel(factor(congruence), ref = "Medium"))

# Running OLS across outcomes
h4_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = "scandal_any * congruence + cand_reputation + cand_gender + cand_age"
)) %>%
  filter(grepl("scandal_anyScandal:congruence", term)) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcome_labels)),
    term    = case_when(
      grepl("Low",  term) ~ "Scandal × Low congruence",
      grepl("High", term) ~ "Scandal × High congruence",
      TRUE                ~ term
    )
  )

h4_results <- h4_results %>%
  mutate(term = factor(term, levels = c("Scandal × Low congruence", "Scandal × High congruence")))

# Estimating conditional effects by congruence for the plot
h4_low <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(congruence == "Low"),
  outcome = .x,
  rhs     = "scandal_any + cand_reputation + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "Low")

h4_medium <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(congruence == "Medium"),
  outcome = .x,
  rhs     = "scandal_any + cand_reputation + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "Medium")

h4_high <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(congruence == "High"),
  outcome = .x,
  rhs     = "scandal_any + cand_reputation + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "High")

# Binding the conditional effects into one dataframe
h4_plot_data <- bind_rows(h4_low, h4_medium, h4_high) %>%
  mutate(group = factor(group, levels = c("Low", "Medium", "High")))

h4_int_labels <- h4_results %>%
  filter(outcome == "Trustworthy") %>%
  group_by(outcome) %>%
  mutate(
    x_pos = max(conf.high) + 0.05,
    nudge  = case_when(
      grepl("High", term) ~  0.15,
      grepl("Low",  term) ~ -0.11
    )
  ) %>%
  ungroup()

# Plotting interaction effects
plot_h4_int <- h4_results %>%
  ggplot(aes(x = estimate, y = outcome, shape = term, color = term)) +
  geom_point(size = 3, stroke = 1, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 linewidth = 0.5,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Outcome", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Interaction coefficient (scandal x congruence)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(4, 8)) +
  theme_bw() + vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  ) +
  geom_label(data        = h4_int_labels,
            aes(x       = conf.high + 0.05,
                y       = outcome,
                label   = term),
            nudge_y     = h4_int_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Plotting conditional effects
h4_labels <- h4_plot_data %>%
  filter(outcome == "Trustworthy") %>%
  mutate(
    label = paste(group, "Congruence"),
    nudge = case_when(
      group == "Low"    ~ -0.22,
      group == "Medium" ~  0,
      group == "High"   ~  0.22
    )
  )

plot_h4_cond <- h4_plot_data %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome, shape = group, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (scandal vs. no scandal)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3),
                      labels = c("Low", "Medium", "High")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Low", "Medium", "High")) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  ) +
  geom_label(data        = h4_labels,
            aes(x       = conf.high + 0.05,
                y       = outcome,
                label   = label),
            nudge_y     = h4_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Combining the panels and saving the figure
plot_h4_cond_titled <- arrangeGrob(plot_h4_cond,
                                   top = textGrob("A) Conditional Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))
plot_h4_int_titled  <- arrangeGrob(plot_h4_int,
                                   top = textGrob("B) Interaction Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))

plot_h4_full <- grid.arrange(
  plot_h4_cond_titled,
  plot_h4_int_titled,
  ncol    = 1,
  heights = c(2, 2)
)

ggsave(file.path(appendix_fig_path, "vignette_h4_full.pdf"),
       plot = plot_h4_full, width = 6, height = 7.5)

# Saving interaction term table
h4_tidy <- h4_results %>%
  transmute(
    Interaction = term,
    Outcome     = as.character(outcome),
    Coefficient    = estimate,
    SE          = std.error,
    p           = p.value,
    Lower       = conf.low,
    Upper       = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h4_tidy,
                 caption = "Interaction Between Scandal and Congruence Across Candidate Evaluations",
                 label   = "tab:vignette_h4",
                 path    = file.path(appendix_tab_path, "vignette_h4.tex"))

# Saving conditional effects table
h4_cond_tidy <- h4_plot_data %>%
  transmute(
    Congruence = as.character(group),
    Outcome    = as.character(outcome),
    Coefficient   = estimate,
    SE         = std.error,
    p          = p.value,
    Lower      = conf.low,
    Upper      = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Congruence = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Congruence = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h4_cond_tidy,
                 caption = "Effect of Scandal on Evaluation of Candidate Conditional on Congruence",
                 label   = "tab:vignette_h4_cond",
                 path    = file.path(appendix_tab_path, "vignette_h4_cond.tex"))

# Estimating and plotting marginal means
outcome_mm_h4 <- map_dfr(outcomes, function(o) {
  cregg::cj(vignette_data, reformulate("scandal_any", response = o),
            id = ~ID, estimate = "mm", by = ~congruence) %>%
    as.data.frame() %>%
    transmute(
      scandal_any = factor(level, levels = c("No scandal", "Scandal")),
      congruence  = factor(congruence, levels = c("Low", "Medium", "High")),
      outcome     = factor(o, levels = outcomes, labels = outcome_labels),
      mean  = estimate,
      lower = lower,
      upper = upper
    )
})

plot_h4_mm_top <- outcome_mm_h4 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[1:3]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = congruence, color = congruence)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.6)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 17, 15)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

plot_h4_mm_bot <- outcome_mm_h4 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[4:5]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = congruence, color = congruence)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.6)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Marginal mean (1-5 scale)") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 3)) +
  scale_shape_manual(values = c(16, 17, 15)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

legend_h4 <- get_legend(plot_h4_mm_top + theme(legend.position = "bottom",
                                               legend.title    = element_blank()))
g_top_h4  <- ggplotGrob(plot_h4_mm_top)
g_bot_h4  <- ggplotGrob(plot_h4_mm_bot)

plot_h4_mm_v <- grid.arrange(
  g_top_h4,
  arrangeGrob(g_bot_h4,
              vp = viewport(x     = 0.5, y = 0.5,
                            width = 0.7, height = 1,
                            just  = c("centre", "centre"))),
  arrangeGrob(as_ggplot(legend_h4),
              padding = unit(0, "cm"),
              vp = viewport(x = 0.58, y = 0.5)),
  ncol    = 1,
  heights = c(1, 1, 0.15)
)

ggsave(file.path(appendix_fig_path, "vignette_h4_mm.pdf"),
       plot = plot_h4_mm_v, width = 6.5, height = 3.75)

## 5f Hypothesis 5 #############################################################
# Estimating the effect of a scandal conditional on trust in politicians

# Recoding to set low trust as reference level
vignette_data <- vignette_data %>%
  mutate(high_trust_median = factor(high_trust_median,
                                    levels = c("Low trust", "High trust")))

# Running OLS across outcomes
h5_results <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data,
  outcome = .x,
  rhs     = "scandal_any * high_trust_median + cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(grepl("scandal_anyScandal:high_trust_median", term)) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcome_labels)),
    term    = case_when(
      grepl("High trust", term) ~ "Scandal × High trust",
      TRUE                      ~ term
    )
  )

# Estimating conditional effects by trust for the plot
h5_low <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(high_trust_median == "Low trust"),
  outcome = .x,
  rhs     = "scandal_any + cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "Low trust")

h5_high <- map_dfr(outcomes, ~run_ols(
  data    = vignette_data %>% filter(high_trust_median == "High trust"),
  outcome = .x,
  rhs     = "scandal_any + cand_reputation + congruence + cand_gender + cand_age"
)) %>%
  filter(term == "scandal_anyScandal") %>%
  mutate(outcome = factor(outcome, levels = rev(outcome_labels)),
         group   = "High trust")

# Binding the conditional effects into one dataframe
h5_plot_data <- bind_rows(h5_low, h5_high) %>%
  mutate(group = factor(group, levels = c("Low trust", "High trust")))

# Plotting interaction effects
plot_h5_int <- h5_results %>%
  ggplot(aes(x = estimate, y = outcome, shape = "Interaction effect")) +
  geom_point(size = 3, stroke = 1) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome),
               linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~"Outcome", ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Interaction coefficient (scandal × high trust)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_shape_manual(values = c("Interaction effect" = 4)) +
  theme_bw() + vignette_theme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )


# Plotting conditional effects
h5_labels <- h5_plot_data %>%
  filter(outcome == "Trustworthy") %>%
  mutate(nudge = case_when(
    group == "Low trust"  ~ -0.11,
    group == "High trust" ~  0.15
  ))


plot_h5_cond <- h5_plot_data %>%
  mutate(feature = "Outcome") %>%
  ggplot(aes(x = estimate, y = outcome, shape = group, color = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~feature, ncol = 1L, scales = "free_y", strip.position = "left") +
  scale_x_continuous(name = "Coefficient (scandal vs. no scandal)", limits = c(-2, 2)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2),
                      labels = c("Low trust", "High trust")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("Low trust", "High trust")) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  ) +
  geom_label(data        = h5_labels,
            aes(x       = conf.high + 0.05,
                y       = outcome,
                label   = group),
            nudge_y     = h5_labels$nudge,
            size        = 3,
            hjust       = 0,
            label.size = 0,
            fill       = "white",
            inherit.aes = FALSE)

# Combining the panels and saving the figure
plot_h5_cond_titled <- arrangeGrob(plot_h5_cond,
                                   top = textGrob("A) Conditional Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))
plot_h5_int_titled  <- arrangeGrob(plot_h5_int,
                                   top = textGrob("B) Interaction Effect",
                                                  x  = 0.33,
                                                  gp = gpar(fontsize = 10,
                                                            fontface = "bold")))

plot_h5_full <- grid.arrange(
  plot_h5_cond_titled,
  plot_h5_int_titled,
  ncol    = 1,
  heights = c(2, 2)
)

ggsave(file.path(appendix_fig_path, "vignette_h5_full.pdf"),
       plot = plot_h5_full, width = 6, height = 7.5)

# Saving interaction term table
h5_tidy <- h5_results %>%
  transmute(
    Interaction = term,
    Outcome     = as.character(outcome),
    Coefficient    = estimate,
    SE          = std.error,
    p           = p.value,
    Lower       = conf.low,
    Upper       = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Interaction = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Interaction = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h5_tidy,
                 caption = "Interaction Between Scandal and Trust in Politicians Across Candidate Evaluations",
                 label   = "tab:vignette_h5",
                 path    = file.path(appendix_tab_path, "vignette_h5.tex"))

# Saving conditional effects table
h5_cond_tidy <- h5_plot_data %>%
  transmute(
    Trust    = as.character(group),
    Outcome  = as.character(outcome),
    Coefficient = estimate,
    SE       = std.error,
    p        = p.value,
    Lower    = conf.low,
    Upper    = conf.high
  ) %>%
  format_estimates() %>%
  bind_rows(
    tibble(Trust = "N respondents",  Outcome = sprintf("%d", n_distinct(vignette_data$ID)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = ""),
    tibble(Trust = "N observations", Outcome = sprintf("%d", nrow(vignette_data)),
           Coefficient = "", SE = "", p = "", Lower = "", Upper = "")
  )

save_results_tex(h5_cond_tidy,
                 caption = "Effect of Scandal on Evaluation of Candidate Conditional on Trust in Politicans",
                 label   = "tab:vignette_h5_cond",
                 path    = file.path(appendix_tab_path, "vignette_h5_cond.tex"))


# Estimating and plotting marginal means
outcome_mm_h5 <- map_dfr(outcomes, function(o) {
  cregg::cj(vignette_data, reformulate("scandal_any", response = o),
            id = ~ID, estimate = "mm", by = ~high_trust_median) %>%
    as.data.frame() %>%
    transmute(
      scandal_any       = factor(level, levels = c("No scandal", "Scandal")),
      high_trust_median = factor(high_trust_median, levels = c("Low trust", "High trust")),
      outcome           = factor(o, levels = outcomes, labels = outcome_labels),
      mean  = estimate,
      lower = lower,
      upper = upper
    )
})

plot_h5_mm_top <- outcome_mm_h5 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[1:3]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = high_trust_median, color = high_trust_median)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

plot_h5_mm_bot <- outcome_mm_h5 %>%
  left_join(outcome_means, by = "outcome") %>%
  filter(outcome %in% outcome_labels[4:5]) %>%
  mutate(feature = "Scandal") %>%
  ggplot(aes(x = mean, y = scandal_any, shape = high_trust_median, color = high_trust_median)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_vline(aes(xintercept = grand_mean), linetype = "dashed") +
  facet_grid(feature ~ outcome, scales = "free_x", switch = "y") +
  scale_x_continuous(name = "Marginal mean (1-5 scale)") +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = rep("black", 2)) +
  scale_shape_manual(values = c(16, 17)) +
  theme_bw() + vignette_theme +
  theme(
    legend.position    = "none",
    strip.text.y.left  = element_text(size = 10, angle = 90),
    strip.text.x       = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

legend_h5 <- get_legend(plot_h5_mm_top + theme(legend.position = "bottom",
                                               legend.title    = element_blank()))
g_top_h5  <- ggplotGrob(plot_h5_mm_top)
g_bot_h5  <- ggplotGrob(plot_h5_mm_bot)

plot_h5_mm_v <- grid.arrange(
  g_top_h5,
  arrangeGrob(g_bot_h5,
              vp = viewport(x     = 0.5, y = 0.5,
                            width = 0.7, height = 1,
                            just  = c("centre", "centre"))),
  arrangeGrob(as_ggplot(legend_h5),
              padding = unit(0, "cm"),
              vp = viewport(x = 0.58, y = 0.5)),
  ncol    = 1,
  heights = c(1, 1, 0.15)
)

ggsave(file.path(appendix_fig_path, "vignette_h5_mm.pdf"),
       plot = plot_h5_mm_v, width = 6.5, height = 3.75)

# Script end ###################################################################

