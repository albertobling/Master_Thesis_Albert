# Descriptive Data Analysis ####################################################

# 00 Dependencies ##############################################################

# Loading packages
library(tidyverse)
library(readxl)

# Clearing environment
rm(list = ls())

# Setting working directory to analysis folder. Must be replaced to reproduce the analysis
setwd('/Users/albertobling/Desktop/Statskundskab/Kandidat/Masters Thesis/Analysis/')

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

# Importing the cleaned descriptive data
D <- read_rds('02. Clean data/descriptive_data.rds')

# Importing the population data fetched from Statistics Denmark
pop_region     <- read_excel("01. Raw data/Population_data.xlsx", sheet = "Region")
pop_gender     <- read_excel("01. Raw data/Population_data.xlsx", sheet = "Gender")
pop_education  <- read_excel("01. Raw data/Population_data.xlsx", sheet = "Education")
pop_age        <- read_excel("01. Raw data/Population_data.xlsx", sheet = "Age")
pop_party      <- read_excel("01. Raw data/Population_data.xlsx", sheet = "Party")

# 02 Recoding the population data ##############################################

pop_age <- pop_age %>%
  mutate(
    age_group = factor(
      case_when(
        Age >= 80 ~ "80+",
        Age >= 70 ~ "70-79",
        Age >= 60 ~ "60-69",
        Age >= 50 ~ "50-59",
        Age >= 40 ~ "40-49",
        Age >= 30 ~ "30-39",
        Age >= 18 ~ "18-29"
      ),
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
    )
  ) %>%
  group_by(age_group) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(Share = Count / sum(Count))

pop_education <- pop_education %>%
  mutate(Education = case_when(
    str_detect(Education, "H10")  ~ "Primary school",
    str_detect(Education, "H20")  ~ "Upper secondary",
    str_detect(Education, "H30")  ~ "Vocational",
    str_detect(Education, "H40")  ~ "Short-cycle higher",
    str_detect(Education, "H50")  ~ "Medium-cycle higher",
    str_detect(Education, "H70")  ~ "Long-cycle higher",
    str_detect(Education, "H80")  ~ "PhD/Researcher",
    TRUE ~ NA_character_
  ))

# 03 Sample vs. Population demographics ########################################

# Building sample shares for each background variable
sample_gender <- D %>%
  filter(!is.na(resp_gender)) %>%
  count(resp_gender) %>%
  mutate(sample_share = n / sum(n)) %>%
  rename(Category = resp_gender)

sample_age <- D %>%
  filter(!is.na(resp_age_group)) %>%
  count(resp_age_group) %>%
  mutate(sample_share = n / sum(n)) %>%
  rename(Category = resp_age_group)

sample_education <- D %>%
  filter(!is.na(resp_education)) %>%
  count(resp_education) %>%
  mutate(sample_share = n / sum(n)) %>%
  rename(Category = resp_education)

sample_region <- D %>%
  filter(!is.na(resp_region)) %>%
  count(resp_region) %>%
  mutate(sample_share = n / sum(n)) %>%
  rename(Category = resp_region)

sample_party <- D %>%
  filter(!is.na(party_vote)) %>%
  count(party_vote) %>%
  mutate(sample_share = n / sum(n)) %>%
  rename(Category = party_vote)

compare_gender <- pop_gender %>%
  rename(Category = Gender, pop_share = Share) %>%
  full_join(sample_gender, by = "Category") %>%
  mutate(sample_share = ifelse(is.na(sample_share), 0, sample_share)) %>%
  select(Category, sample_share, pop_share)

compare_age <- pop_age %>%
  rename(Category = age_group, pop_share = Share) %>%
  full_join(sample_age, by = "Category") %>%
  mutate(sample_share = ifelse(is.na(sample_share), 0, sample_share)) %>%
  select(Category, sample_share, pop_share)

compare_education <- pop_education %>%
  rename(Category = Education, pop_share = Share) %>%
  full_join(sample_education, by = "Category") %>%
  mutate(sample_share = ifelse(is.na(sample_share), 0, sample_share)) %>%
  select(Category, sample_share, pop_share)

compare_region <- pop_region %>%
  rename(Category = Region, pop_share = Share) %>%
  full_join(sample_region, by = "Category") %>%
  mutate(sample_share = ifelse(is.na(sample_share), 0, sample_share)) %>%
  select(Category, sample_share, pop_share)

compare_party <- pop_party %>%
  rename(Category = Party, pop_share = Share) %>%
  full_join(sample_party, by = "Category") %>%
  mutate(sample_share = ifelse(is.na(sample_share), 0, sample_share)) %>%
  select(Category, sample_share, pop_share)

# Creating a helper function to format one row of the descriptive table
format_row <- function(category, sample, pop) {
  pop_str <- ifelse(is.na(pop), "--", sprintf("%.1f", pop * 100))
  sprintf("\\quad %s & %.1f & %s \\\\", category, sample * 100, pop_str)
}

# Building the sample and population comparison table
lines <- c(
  "\\begin{table}[htb]",
  "\\caption{Sample and Population Comparison}",
  "\\centering",
  "\\footnotesize",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\textbf{Sample \\%} & \\textbf{Population \\%} \\\\",
  "\\midrule",
  "\\textit{Gender} & & \\\\",
  mapply(format_row, compare_gender$Category, compare_gender$sample_share, compare_gender$pop_share),
  "\\textit{Age} & & \\\\",
  mapply(format_row, compare_age$Category, compare_age$sample_share, compare_age$pop_share),
  "\\textit{Education} & & \\\\",
  mapply(format_row, compare_education$Category, compare_education$sample_share, compare_education$pop_share),
  "\\textit{Region} & & \\\\",
  mapply(format_row, compare_region$Category, compare_region$sample_share, compare_region$pop_share),
  "\\textit{Party (2026 election)} & & \\\\",
  mapply(format_row, compare_party$Category, compare_party$sample_share, compare_party$pop_share),
  "\\midrule",
  sprintf("\\textit{N = %d} & & \\\\", nrow(D)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\label{tab:sample_comparison}",
  "\\end{table}"
)

writeLines(paste(lines, collapse = "\n"), "03. Output/Appendix/Tables/sample_comparison.tex")

# 04. Distribution of trust variable ###########################################

# Summarising the distribution of the trust variable
trust_summary <- D %>%
  filter(!is.na(trust)) %>%
  summarise(
    Mean   = mean(trust),
    Median = median(trust),
    SD     = sd(trust),
    Min    = min(trust),
    Max    = max(trust),
    N      = n()
  )

# Plotting the distribution of the trust variable
trust_plot <- D %>%
  filter(!is.na(trust)) %>%
  ggplot(aes(x = factor(trust))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = "grey50", color = "white") +
  scale_x_discrete(name = "Trust in politicians (0-10)") +
  scale_y_continuous(name = "Share of respondents (%)", expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(size = 10, color = "black"),
    axis.title         = element_text(size = 11, color = "black")
  )

ggsave("03. Output/Appendix/Figures/trust_distribution.pdf",
       trust_plot, width = 7, height = 4)

# 05 Distribution of attitudes #################################################

# Plotting immigration policy preferences
immig_plot <- D %>%
  filter(!is.na(immig_pref)) %>%
  count(immig_pref) %>%
  mutate(share = n / sum(n) * 100) %>%
  ggplot(aes(x = immig_pref, y = share)) +
  geom_bar(stat = "identity", fill = "grey50", color = "white") +
  scale_x_discrete(name = "Immigration policy preference") +
  scale_y_continuous(name = "Share of respondents (%)", expand = c(0, 0), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(size = 15, color = "black"),
    axis.title         = element_text(size = 15, color = "black")
  )

# Plotting economic policy preferences
econ_plot <- D %>%
  filter(!is.na(econ_pref)) %>%
  count(econ_pref) %>%
  mutate(share = n / sum(n) * 100) %>%
  ggplot(aes(x = econ_pref, y = share)) +
  geom_bar(stat = "identity", fill = "grey50", color = "white") +
  scale_x_discrete(name = "Economic policy preference") +
  scale_y_continuous(name = "Share of respondents (%)", expand = c(0, 0), limits = c(0, 50)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(size = 15, color = "black"),
    axis.title         = element_text(size = 15, color = "black")
  )

# Plotting the second preference of respondents who chose the status quo on immigration
middleim_plot <- D %>%
  dplyr::filter(!is.na(middleim)) %>%
  count(middleim) %>%
  mutate(
    share    = n / sum(n) * 100,
    middleim = factor(middleim,
                      levels = c(1, 3),
                      labels = c("Leaning more restrictive", "Leaning less restrictive"))
  ) %>%
  ggplot(aes(x = middleim, y = share)) +
  geom_bar(stat = "identity", fill = "grey50", color = "white") +
  scale_x_discrete(name = "Immigration policy preference (status quo voters)") +
  scale_y_continuous(name = "Share of respondents (%)", expand = c(0, 0), limits = c(0, 70)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(size = 15, color = "black"),
    axis.title         = element_text(size = 15, color = "black")
  )

# Plotting the second preference of respondents who chose the status quo on the economy
middleecon_plot <- D %>%
  dplyr::filter(!is.na(middleecon)) %>%
  count(middleecon) %>%
  mutate(
    share      = n / sum(n) * 100,
    middleecon = factor(middleecon,
                        levels = c(1, 3),
                        labels = c("Leaning lower taxes", "Leaning more public welfare"))
  ) %>%
  ggplot(aes(x = middleecon, y = share)) +
  geom_bar(stat = "identity", fill = "grey50", color = "white") +
  scale_x_discrete(name = "Economic policy preference (status quo voters)") +
  scale_y_continuous(name = "Share of respondents (%)", expand = c(0, 0), limits = c(0, 70)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(size = 15, color = "black"),
    axis.title         = element_text(size = 15, color = "black")
  )

ggsave("03. Output/Appendix/Figures/immig_preferences.pdf",
       immig_plot, width = 6, height = 4)
ggsave("03. Output/Appendix/Figures/econ_preferences.pdf",
       econ_plot, width = 6, height = 4)
ggsave("03. Output/Appendix/Figures/middleim_preferences.pdf",
       middleim_plot, width = 6, height = 4)
ggsave("03. Output/Appendix/Figures/middleecon_preferences.pdf",
       middleecon_plot, width = 6, height = 4)

# Script end ###################################################################

