# load packages
library(dplyr)
library(ggplot2)
library(skimr)
library(pander)
library(stargazer)
library(broom)
library(lme4)
library(lmerTest)
library(readr)
library(ggExtra)
library(kableExtra)
library(emmeans)

# load data
TPLO <- read_csv("TPLO.csv")

# fix gender variable, clean other variables
TPLO <-
  TPLO %>%
  mutate(
    GenderMF =
      case_when(
        Gender == "FI" ~ "F",
        Gender == "FS" ~ "F",
        Gender == "MI" ~ "M",
        Gender == "MN" ~ "M"
      ),
    Incinfection.num = case_when(Incinfection == "Y" ~ 1,
                                 Incinfection == "N" ~ 0),
  )
TPLO <- TPLO %>% mutate_if(is.character, as.factor)

TPLO <- TPLO[-111,]

TPLO_no_antibiotic <- TPLO %>%
  filter(Antibiotic == "N")

TPLO$Patientnumber <- as.factor(TPLO$Patientnumber)


## run mixed model to predict Hosp
TPLO %>%
  lmer(Hosp ~ GenderMF + Age + Weight + Meniscus + PRP + BCS + (1 |
                                                                  Patientnumber),
       data = .) %>%
  summary()

# asses whether patient number random effect matters for model fit
TPLO %>%
  lm(Hosp ~ GenderMF + Age + Weight + Meniscus + PRP + BCS, data = .) %>%
  summary()

# better model
TPLO %>%
  lm(Hosp ~ Age + PRP, data = .) %>%
  summary()

TPLO %>%
  lm(Hosp ~ PRP, data = .) %>%
  summary()

# use some control to try to improve the lmer model
TPLO %>%
  lmer(Hosp ~ PRP + (1 | Patientnumber),
       data = .,
       control = lmerControl(optCtrl = list(maxfun = 2e5))) %>%
  summary()

# summary table
TPLO %>%
  xtabs(data = ., ~ Hosp + PRP)

# summarize Hosp variable
TPLO %>%
  select(Hosp) %>%
  summary()

# postOA models
TPLO %>%
  lmer(PostOA ~ GenderMF + Age + Weight + Meniscus + PRP + PreOA + BCS +
         (1 | Patientnumber),
       data = .) %>%
  summary()

TPLO %>%
  lmer(PostOA ~ PRP + PreOA + (1 | Patientnumber), data = .) %>%
  summary()

# postG models
TPLO %>%
  lmer(PostG ~ Weight + PRP + PreG + (1 |
                                        Patientnumber), data = .) %>%
  summary()

# postOs model
TPLO %>%
  lmer(PostOs ~ Meniscus + PRP + PreOS + (1 |
                                            Patientnumber), data = .) %>%
  summary()

# postM model
TPLO %>%
  lmer(PostM ~ Meniscus + PRP + PreM - 1 + (1 |
                                              Patientnumber), data = .) %>%
  summary()

# infection.num model
TPLO_no_antibiotic %>%
  glmer(Incinfection.num ~ PRP + Age + (1 |
                                          Patientnumber),
        data = .,
        family = binomial) %>%
  summary

# incinfection.num models
TPLO_no_antibiotic %>%
  glmer(
    Incinfection.num ~ PRP + Age + Weight + Meniscus + Nocita +
      (1 | Patientnumber),
    data = .,
    family = binomial
  ) %>%
  summary()

TPLO_no_antibiotic %>%
  glmer(Incinfection.num ~ PRP +
          (1 | Patientnumber),
        data = .,
        family = binomial) %>%
  summary()
