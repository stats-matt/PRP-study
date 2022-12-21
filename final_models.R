# libraries ----
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
library(optimx)
library(glmmTMB)
library(RCurl)
library(details)
library(rstatix)


# fix data ----
#TPLO <- read.csv("TPLO.csv")
TPLO <- read_csv("TPLO.csv")
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

TPLO <- TPLO[-111, ]

TPLO_no_antibiotic <- TPLO %>%
  filter(Antibiotic == "N")

TPLO$Patientnumber <- as.factor(TPLO$Patientnumber)


# add improvement variables ----
TPLO <-
  TPLO %>%
  mutate(
    GWorse = case_when(PostG > PreG ~ 1,
                       PostG == PreG ~ 0,
                       PostG < PreG ~ 0),
    OAWorse = case_when(PostOA > PreOA ~ 1,
                        PostOA == PreOA ~ 0,
                        PostOA < PreOA ~ 0),
    MWorse = case_when(PostM > PreM ~ 1,
                       PostM == PreM ~ 0,
                       PostM < PreM ~ 0),
    OSWorse = case_when(PostOs > PreOS ~ 1,
                        PostOs == PreOS ~ 0,
                        PostOs < PreOS ~ 0),
    EffChange = case_when(PostE > PreE ~ "Worse",
                          PostE == PreE ~ "Same",
                          PostE < PreE ~ "Better")
  )

TPLO %>%
  xtabs(data = ., ~ PreE + PostE)

TPLO %>%
  select(PostE, PreE, EffChange) %>%
  group_by(PostE, PreE, EffChange) %>%
  count()

# differences in PRP between groups ----
# diff in gender
TPLO %>%
  xtabs(data = ., ~ PRP + GenderMF) %>%
  chisq.test()

# diff in age
TPLO %>%
  t.test(data = ., Age ~ PRP)

TPLO %>%
  t_test(Age ~ PRP)

# diff in meniscal
TPLO %>%
  xtabs( ~ Meniscus + PRP, .) %>%
  chisq.test()

# diff in weight
TPLO %>%
  t_test(Weight ~ PRP)

# diff in BCS
TPLO %>%
  xtabs( ~ BCS + PRP, .) %>%
  fisher.test()

# no differences

# GWorse Models ------------
TPLO %>%
  xtabs(data = ., ~ GWorse + PRP)

TPLO %>%
  glmmTMB(GWorse ~ PRP + (1 |
                            Patientnumber),
          data = .,
          family = "binomial") %>%
  summary()

gworse_model <-
  TPLO %>%
  glmer(GWorse ~
          #GenderMF +
          #Age +
          #Weight +
          #Meniscus +
          PRP +
          #BCS +
          (1 | Patientnumber),
        data = .,
        family = "binomial")

gfinal <- lmer_best(gworse_model)
summary(gfinal)

mod2 <- allFit(gworse_model)
summary(mod2)

gfinal <- mod2[2][[1]]
summary(gfinal)
emmip(gfinal, ~ PRP, type = "response")

# OAWorse models ----
TPLO %>%
  xtabs(data = ., ~ OAWorse + PRP)

TPLO %>%
  glmmTMB(OAWorse ~ PRP + (1 |
                             Patientnumber),
          data = .,
          family = "binomial") %>%
  summary()

OAworse_model <-
  TPLO %>%
  glmer(OAWorse ~
          #GenderMF +
          #Age +
          #Weight +
          Meniscus +
          PRP +
          #BCS +
          (1 | Patientnumber),
        data = .,
        family = "binomial")

all <- allFit(OAworse_model)
summary(all)

OAfinal <- all[1][[1]]

emmeans(OAfinal, ~ PRP | Meniscus, type = "response")

emmip(OAfinal, ~ PRP | Meniscus, type = "response")

# MWorse ----
TPLO %>%
  xtabs(data = ., ~ MWorse + PRP)

TPLO %>%
  glmmTMB(MWorse ~ PRP + (1 |
                            Patientnumber),
          data = .,
          family = "binomial") %>%
  summary()


MWorse_model <-
  TPLO %>%
  glmer(MWorse ~
          #GenderMF +
          #Age +
          #Weight +
          #Meniscus +
          PRP +
          #BCS +
          (1 | Patientnumber),
        data = .,
        family = "binomial")

summary(MWorse_model)

emmip(MWorse_model, ~ PRP, type="response")

# OSWorse ----
TPLO %>%
  xtabs(data = ., ~ OSWorse + PRP)

TPLO %>%
  glmmTMB(OSWorse ~ PRP + (1 |
                             Patientnumber),
          data = .,
          family = "binomial") %>%
  summary()

OSWorse_model <-
  TPLO %>%
  glmer(OSWorse ~
          #GenderMF +
          #Age +
          #Weight +
          #Meniscus +
          PRP +
          #BCS +
          (1 | Patientnumber),
        data = .,
        family = "binomial")

model1 <- lmer_best(OSWorse_model)
summary(model1)


# Echange ----
TPLO %>%
  xtabs(data = ., ~ EffChange + PRP) %>%
  chisq_test()

TPLO %>%
  ggplot(aes(x = PRP, y = EffChange)) +
  geom_jitter()

EffChange_model <-
  TPLO %>%
  lmer(EffChange ~
         #GenderMF +
         #Age +
         #Weight +
         #Meniscus +
         PRP +
         #BCS +
         (1 | Patientnumber),
       data = .)

model1 <- lmer_best(EffChange_model)
summary(model1)