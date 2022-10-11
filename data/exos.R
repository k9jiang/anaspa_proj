library(dplyr)
library(knitr)
library(questionr)

rpls2020 = readRDS('data/RPLS2020.RDS')

rp2018_logement = readRDS('data/menag2018.RDS')

rpls2020$DEPCOM <- as.character(rpls2020$DEPCOM)
rpls2020$DEP <- as.character(rpls2020$DEP)
rpls2020$LIBCOM <- as.factor(rpls2020$LIBCOM)
rpls2020$PLG_IRIS <-
  paste(rpls2020$DEPCOM, rpls2020$PLG_IRIS, sep = "_")
rpls2020$SURFHAB <- as.numeric(rpls2020$SURFHAB)
rpls2020$FINAN <- as.character(rpls2020$FINAN)

rpls2020_94 <- rpls2020 %>% filter(DEP == 94)

ex1a <- rpls2020 %>%
  filter(CONV == 1) %>%
  select(DEPCOM, LIBCOM, CONSTRUCT) %>%
  mutate(
    LOG19461970 = case_when(
      CONSTRUCT >= 1946 & CONSTRUCT < 1971 ~ 1,
      CONSTRUCT < 1946 |
        CONSTRUCT >= 1971 ~ 0
    ),
    LOG19711996 = case_when(
      CONSTRUCT >= 1971 & CONSTRUCT < 1997 ~ 1,
      CONSTRUCT < 1971 |
        CONSTRUCT >= 1997 ~ 0
    ),
    LOG19972019 = case_when(CONSTRUCT >= 1997 ~ 1,
                            CONSTRUCT < 1997 ~ 0)
  ) %>%
  select(-CONSTRUCT) %>%
  group_by(DEPCOM, LIBCOM) %>% summarise_all(sum)



ex1a1 <- rpls2020 %>%
  filter(CONV == 1 &
           (CODEPOSTAL == '75014' |
              DEPCOM %in% c('92046', '94003', '92049'))) %>%
  select(DEPCOM, LIBCOM, CONSTRUCT) %>%
  mutate(
    LOG19461970 = case_when(
      CONSTRUCT >= 1946 & CONSTRUCT < 1971 ~ 1,
      CONSTRUCT < 1946 |
        CONSTRUCT >= 1971 ~ 0
    ),
    LOG19711996 = case_when(
      CONSTRUCT >= 1971 & CONSTRUCT < 1997 ~ 1,
      CONSTRUCT < 1971 |
        CONSTRUCT >= 1997 ~ 0
    ),
    LOG19972019 = case_when(CONSTRUCT >= 1997 ~ 1,
                            CONSTRUCT < 1997 ~ 0)
  ) %>%
  select(-CONSTRUCT) %>%
  group_by(DEPCOM, LIBCOM) %>% summarise_all(sum)


rpls2020_ze <- rpls2020 %>%
  filter((CODEPOSTAL == '75014' |
            DEPCOM %in% c('92046', '94003', '92049')))


ex1b <- rpls2020_ze %>%
  select(DEPCOM, LIBCOM, SURFHAB) %>%
  group_by(DEPCOM, LIBCOM) %>% summarise(SURFHAB_MOY = mean(SURFHAB))

rpls2020_zeb <- rpls2020_ze %>%
  mutate(
    FINAN5 = case_when(
      FINAN %in% c("10", "11") ~ "PLAI",
      FINAN %in% c("12", "13", "50", "51", "52", "53", "54", "55") ~ "PLUS",
      FINAN %in% c("14", "15", "17") ~ "PLS",
      FINAN %in% c("16") ~ "PLI",
      FINAN %in% c("49", "99") ~ "Autres"
    )
  )

ex1c <- rpls2020_zeb %>%
  select (DEPCOM, LIBCOM, FINAN5) %>%
  mutate(
    PLAI = case_when(FINAN5 == "PLAI" ~ 1,
                     FINAN5 != "PLAI" ~ 0),
    PLUS = case_when(FINAN5 == "PLUS" ~ 1,
                     FINAN5 != "PLUS" ~ 0),
    PLS = case_when(FINAN5 == "PLS" ~ 1,
                    FINAN5 != "PLS" ~ 0),
    PLI = case_when(FINAN5 == "PLI" ~ 1,
                    FINAN5 != "PLI" ~ 0),
    AUTRES = case_when(FINAN5 == 'Autres' ~ 1,
                       FINAN5 != 'AUtres' ~ 0)
  ) %>%
  select(-FINAN5) %>% group_by (DEPCOM, LIBCOM) %>%
  summarise(
    TX_PLAI = sum(PLAI) * 100 / n(),
    TX_PLUS = sum(PLUS) * 100 / n(),
    TX_PLS = sum(PLS) * 100 / n(),
    TX_PLI = sum(PLI) * 100 / n(),
    TX_AUTRES = sum(AUTRES) * 100 / n()
  )

ex2a <- rpls2020_zeb %>%
  select(DEPCOM, LIBCOM, IMMEU, DEP) %>%
  group_by(DEPCOM, LIBCOM, IMMEU, DEP) %>%
  summarise(NB = n()) %>%
  filter(NB > 100 & IMMEU != '') %>%
  group_by(DEP) %>%
  summarise(TOT = n())

ex2b <- rpls2020_zeb %>%
  select(DEPCOM, LIBCOM, IMMEU, DEP) %>%
  group_by(DEPCOM, LIBCOM, IMMEU, DEP) %>%
  summarise(NB = n()) %>%
  filter(NB < 10 & IMMEU != '') %>%
  group_by(DEP) %>%
  summarise(TOT = n())

ex3a_a <- rpls2020_zeb %>%
  filter(CONV == 1) %>%
  select(DEPCOM, LIBCOM) %>%
  group_by(DEPCOM, LIBCOM) %>% summarise(nblogsoc_rpls = n())

ex3a_b <- rp2018_logement %>%
  filter(HLML == "1" & (COMMUNE %in%
                          c('92046', '94003', '92049') | ARM == '75114')) %>%
  select(COMMUNE, IPONDL) %>%
  group_by(COMMUNE) %>% summarise_all(sum) %>%
  mutate(nblogHLM_rp = round(IPONDL)) %>% select(-IPONDL)

ex3a_ab <-
  merge(
    x = ex3a_a,
    y = ex3a_b,
    by.x = "DEPCOM",
    by.y = "COMMUNE",
    all.x = T
  )
ex3a_ab <- ex3a_ab %>%
  mutate(
    RPLS_RP = case_when(
      nblogsoc_rpls > nblogHLM_rp ~ "RPLS > RP",
      nblogsoc_rpls < nblogHLM_rp ~ "RP > RPLS",
      nblogsoc_rpls == nblogHLM_rp ~ "RP = RPLS"
    )
  )

plot(ex3a_ab$nblogsoc_rpls, ex3a_ab$nblogHLM_rp)

rpls2020_zeb <-
  rpls2020_zeb %>% mutate(IRIS = case_when(
    CODEPOSTAL == "75014" ~ paste('75114', PLG_IRIS, sep = ''),
    CODEPOSTAL != "75014" ~ paste(DEPCOM, PLG_IRIS, sep = '')
  ))

rpls2020_zeb$IRIS


ex3a_aa <- rpls2020_zeb %>%
  filter(CONV == 1) %>%
  select(DEPCOM, LIBCOM, IRIS) %>%
  group_by(DEPCOM, LIBCOM, IRIS) %>% summarise(nblogsoc_rpls = n())

ex3a_bb <- rp2018_logement %>%
  filter(HLML == "1" & (COMMUNE %in%
                          c('92046', '94003', '92049') |
                          ARM == '75114')) %>%
  select(COMMUNE, IPONDL, IRIS) %>%
  group_by(COMMUNE, IRIS) %>% summarise_all(sum) %>%
  mutate(nblogHLM_rp = round(IPONDL)) %>% select(-IPONDL)

ex3a_aabb <-
  merge(
    x = ex3a_aa,
    y = ex3a_bb,
    by.x = "IRIS",
    by.y = "IRIS",
    all.x = T
  )
ex3a_aabb <- ex3a_aabb %>%
  mutate(
    RPLS_RP = case_when(
      nblogsoc_rpls > nblogHLM_rp ~ "RPLS > RP",
      nblogsoc_rpls < nblogHLM_rp ~ "RP > RPLS",
      nblogsoc_rpls == nblogHLM_rp ~ "RP = RPLS"
    )
  )

reg = lm(ex3a_aabb$nblogsoc_rpls ~ ex3a_aabb$nblogHLM_rp)

plot(ex3a_aabb$nblogsoc_rpls, ex3a_aabb$nblogHLM_rp)
abline(reg)


# RP (calcul du nombre de résidences principales)
ex3b_b <- rp2018_logement %>%
  filter(CATL %in% c("1") &
           (COMMUNE %in%
              c('92046', '94003', '92049') | ARM == '75114')) %>%
  select(COMMUNE, IPONDL) %>%
  group_by(COMMUNE) %>% summarise_all(sum) %>%
  mutate(nbresdprinc_rp = round(IPONDL)) %>% select(-IPONDL)

Ex3b_rpls_rp <-
  merge(
    x = ex3a_a,
    y = ex3b_b,
    by.x = "DEPCOM",
    by.y = "COMMUNE",
    all.x = T
  )
Ex3b_rpls_rp <- Ex3b_rpls_rp %>%
  mutate(pct_logsoc_SRU = round((nblogsoc_rpls * 100) / nbresdprinc_rp, 1))

tab_sel <- rp2018_logement %>%
  mutate(COMMUNE = substr(IRIS, 1,5)) %>%
  filter(AGEMEN8 == "25" & (COMMUNE %in% c('92049', '75114', '92046', '94003'))) %>%
  select(COMMUNE, DIPLM, HLML, IPONDL)

tab_sel$HLML <- as.factor(tab_sel$HLML)
levels(tab_sel$HLML) <- c("HLM-O", "HLM-N", NA)
tab_sel$DIPLM <- as.factor(tab_sel$DIPLM)
levels(tab_sel$DIPLM) <- c("< CEP", "< CEP", "< CEP", "< BAC","< BAC","< BAC",
                           "BAC","BAC",
                           "BAC+123","BAC+123","> BAC+3","> BAC+3",NA)

table(tab_sel$HLML, tab_sel$DIPLM)

tab_cond_wtd <- wtd.table(tab_sel$HLML, tab_sel$DIPLM, weights = tab_sel$IPONDL)
tab_cond_wtd

tab_pct_wtd = cprop(tab_cond_wtd)
tab_pct_wtd
