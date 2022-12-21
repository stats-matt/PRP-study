# These are some summary tables for the PRP analysis

library(rstatix)
library(janitor)

sing_dogIds <-
  TPLO %>%
  count(Patientnumber) %>%
  filter(n == 1) %>%
  select(Patientnumber)

TPLO %>%
  group_by(PRP) %>%
  summarize(
    mean = mean(TPA, na.rm = T),
    min = min(TPA, na.rm = T),
    max = max(TPA, na.rm = T)
  )

TPLO %>%
  t_test(TPA ~ PRP)

sing_dogs <-
  TPLO %>%
  filter(Patientnumber %in% sing_dogIds$Patientnumber)

sing_dogs

sing_dogs %>%
  filter(Antibiotic == "N") %>%
  group_by(PRP) %>%
  summarize(
    mean = mean(TPA, na.rm = T),
    min = min(TPA, na.rm = T),
    max = max(TPA, na.rm = T),
    n = n()
  )

TPLO %>%
  group_by(PRP) %>%
  summarize(mean = mean(PreOA, na.rm = T))

TPLO %>%
  t_test(PreOA ~ PRP)

TPLO %>%
  t_test(PostOA ~ PRP)

TPLO %>%
  t_test(PreG ~ PRP)

TPLO %>%
  t_test(PostG ~ PRP)

TPLO %>%
  t_test(PreE ~ PRP)

TPLO %>%
  t_test(PostE ~ PRP)

TPLO %>%
  t_test(PreOS ~ PRP)

TPLO %>%
  t_test(PostOs ~ PRP)

TPLO %>%
  t_test(PreM ~ PRP)

TPLO %>%
  t_test(PostM ~ PRP)

TPLO %>%
  lm(data = ., TPA ~ PRP) %>%
  summary()



linmod <- lm(data = TPLO, TPA ~ PRP)

hist(resid(linmod))

plot(fitted(linmod), resid(linmod))

TPLO %>%
  t_test(TPA ~ PRP)

hist(TPLO$TPA)

mean(TPLO$TPA, na.rm = T)


TPLO %>%
  filter(Sxleg == "R") %>%
  tabyl(PRP) %>%
  select(-1,-3) %>%
  t() %>%
  chisq_test()

TPLO %>%
  filter(Sxleg == "L") %>%
  tabyl(PRP) %>%
  select(-1,-3) %>%
  t() %>%
  chisq_test()

TPLO %>%
  filter(Bilateral == "Y") %>%
  tabyl(PRP)
