# Power Analysis for Conjoint Experiment #######################################

# 00 Dependencies ##############################################################

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

# Loading packages
library(cjpowR)

# Clearing environment
rm(list = ls())

# 01 Main effects ##############################################################

# Calculating the minimum sample needed to detect an AMCE of 0.05 on the scandal attribute
eff_sample_main = cjpowr_amce(amce = 0.05, power = 0.8, levels = 4)
eff_sample_main$n
eff_sample_main$n/(2*5)
# Detecting an AMCE of 0.05 requires 6,263 observations, i.e. 626 complete responses


# 02 Interaction effects #######################################################

# Calculating the minimum sample needed to detect an ACIE of 0.08 on scandal x congruence
eff_sample_int1<-cjpowr_amcie(delta3 =  0.08, power = 0.8, levels1 = 2, levels2 = 3,
             p00 = 0.25, p10 = 0.25,
             p01 = 0.25, p11 = 0.25)

eff_sample_int1$n
eff_sample_int1$n/(2*5)
# Detecting an ACIE of 0.08 requires 7,241 observations, i.e. 724 complete responses

# Calculating the minimum sample needed to detect an ACIE of 0.07 on scandal x competence and scandal x trust
eff_sample_int2<-cjpowr_amcie(delta3 =  0.07, power = 0.8, levels1 = 2, levels2 = 2,
                             p00 = 0.25, p10 = 0.25,
                             p01 = 0.25, p11 = 0.25)

eff_sample_int2$n
eff_sample_int2$n/(2*5)
# Detecting an ACIE of 0.07 requires 6,329 observations, i.e. 633 complete responses

# Script end ###################################################################
