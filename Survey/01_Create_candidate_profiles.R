# 00 Dependencies ###############################################################
# This script creates a dataset with randomized candidate profiles which are used in both experiments.

# Loading relevant libraries
library(here)
library(cbcTools)
library(tidyverse)


# 01. Creating profiles ########################################################

# Setting seed for so the randomization can be reproduced
set.seed(123)

# Defining profiles with attributes and levels
profiles <- cbc_profiles(
  Køn         = c('Mand', 'Kvinde'),
  Alder       = c('35 år', '45 år', '55 år'),
  Kompetence  = c('Har hverken et negativt eller positivt omdømme',
                  'Anses bredt som kompetent og i stand til at levere resultater'),
  Indvandring = c('Udlændingepolitikken bør være strammere end i dag',
                  'Udlændingepolitikken bør fastholdes som i dag',
                  'Udlændingepolitikken bør være mere åben end i dag'),
  Okonomi     = c('Skatterne bør sænkes, selvom det betyder mindre offentlig velfærd',
                  'Skatterne og den offentlige velfærd bør fastholdes på det nuværende niveau',
                  'Den offentlige velfærd bør øges, selvom det betyder højere skatter'),
  Skandale    = c('Ingen omtale i medierne for nylig',
                  'Ingen omtale i medierne for nylig',
                  'Ingen omtale i medierne for nylig',
                  'Ingen omtale i medierne for nylig',
                  'Beskyldt for seksuelt upassende adfærd til en fest, men har benægtet',
                  'Beskyldt for seksuelt upassende adfærd til en fest og har undskyldt',
                  'Beskyldt for at misbruge offentlige midler til private formål, men har benægtet',
                  'Beskyldt for at misbruge offentlige midler til private formål og har undskyldt')
)

# Generating the design
design <- cbc_design(profiles, n_resp = 2000, n_alts = 2, n_q = 7)

# Previewing design
head(design)
table(design$Køn)
table(design$Alder)
table(design$Kompetence)
table(design$Indvandring)
table(design$Okonomi)
table(design$Skandale)

# Writing the dataset to CSV to be uploaded alongside the shiny app deployment.
write_csv(design, here("data", "choice_questions.csv"))
