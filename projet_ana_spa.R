library(tidyverse)
library(vcd)
library(questionr)

rpls <- readRDS("data/RPLS2020.RDS")

rp <- readRDS("data/menag2018.RDS")

rp <- rp %>% filter(ARM =='75114' | COMMUNE %in% c('92049', '92046', '94003'))
rp <- rp %>% filter(HLML != 'Y' & HLML != 'Z')
rp2 <- rp %>% select(HLML, INP17M, IPONDL, IMMIM, INPAM)

rpls$DEPCOM <- as.character(rpls$DEPCOM)
rpls$DEP <- as.character(rpls$DEP)
rpls$LIBCOM <- as.factor(rpls$LIBCOM)
rpls$SURFHAB <- as.numeric(rpls$SURFHAB)
rpls$FINAN <- as.character(rpls$FINAN)
rpls$CODEPOSTAL <- as.character(rpls$CODEPOSTAL)

rpls <-
  rpls %>% filter(CODEPOSTAL == '75014' |
                    DEPCOM %in% c('92049', '92046', '94003'))

rpls <- rpls %>%
  mutate(
    FINAN5 = case_when(
      FINAN %in% c("10", "11") ~ "PLAI",
      FINAN %in% c("12", "13", "50", "51", "52", "53", "54", "55") ~ "PLUS",
      FINAN %in% c("14", "15", "17", "16") ~ "PLS_PLI",
      FINAN %in% c("49", "99") ~ "Autres"
    )
  )


rpls <- rpls %>%
  mutate(
    PLAI = case_when(FINAN5 == "PLAI" ~ 1,
                     FINAN5 != "PLAI" ~ 0),
    PLUS = case_when(FINAN5 == "PLUS" ~ 1,
                     FINAN5 != "PLUS" ~ 0),
    PLS_PLI = case_when(FINAN5 %in% c("PLS","PLI") ~ 1,
                    !(FINAN5 %in% c("PLS,'PLI")) ~ 0),
    AUTRES = case_when(FINAN5 == 'Autres' ~ 1,
                       FINAN5 != 'AUtres' ~ 0))


rplsDPE <- rpls %>% filter(DPESERRE != '')

row.names(contingence2) <- c(5, 1, 4, 3, 2)
contingence2

mg <- margin.table(table(rplsDPE$FINAN5, rplsDPE$DPESERRE), 1)
mg7 <- cbind(mg, mg, mg, mg, mg, mg, mg)
mg7
contingence / mg7

prop.table(contingence, margin = 1)

mg1 = margin.table(prop.table(contingence), 1)
mg2 = margin.table(prop.table(contingence), 2)
loi_ideale = mg1 %*% t(mg2)
loi_ideale * 100
prop.table(contingence) / loi_ideale

chisq.test(contingence, p = p)

mosaicplot(contingence, shade = T)

str(rpls)
write.csv2(rpls, file = 'rpls.csv')

rpls %>%
  distinct(X,Y)

rpls <- rpls %>%
  mutate(
    CLASSES_ENERGIE = case_when(
      DPESERRE %in% c("A", "B") ~ "AB",
      DPESERRE %in% c("C","D") ~ "CD",
      DPESERRE %in% c("E","F","G") ~ "EFG"
    )
  )

rpls <- rpls %>%
  mutate(
    AB = case_when(CLASSES_ENERGIE == "AB" ~ 1,
                     CLASSES_ENERGIE != "AB" ~ 0),
    CD = case_when(CLASSES_ENERGIE == "CD" ~ 1,
                     CLASSES_ENERGIE != "CD" ~ 0),
    EFG = case_when(CLASSES_ENERGIE == "EFG" ~ 1,
                    CLASSES_ENERGIE != "EFG" ~ 0))
rpls2 <- rpls %>%
  select(DEPCOM, LIBCOM, DEP, FINAN5, PLAI, PLUS, PLS_PLI, AUTRES, CLASSES_ENERGIE, AB, CD, EFG, X, Y)

contingence <- table(rpls2$FINAN5, rpls2$CLASSES_ENERGIE)
mosaicplot(contingence, shade=T)

write.csv2(rpls2, file = 'rpls2.csv')

fit <- aov(y ~  , data=mydataframe) # y est la variable numérique et A indique les groupes
summary(fit)


#IMMIM : 1 = immigré, 2 = non immigré
#INP17M : 0, 2 ou 3+ (personnes mineures du ménage)
#INPAM : 0, 1, 2+ (personnes actives du ménage)

rp3 <- rp2 %>%
  mutate(
    C_P17 = case_when(INP17M == 0 ~ '00',
                      (INP17M == 1 | INP17M == 2) ~ '12',
                      INP17M >2 ~ '3+'),
    C_PA = case_when(INPAM == 0 ~ '00',
                     INPAM == 1 ~ '01',
                     INPAM >1 ~ '2+'))
rp3$im_17_pa <- paste(rp3$IMMIM,rp3$C_P17,rp3$C_PA,sep="-")



c_rp3 <- rp3 %>% group_by(im_17_pa)%>% summarise(NB = n())

cont_type_hlm <- table(rp3$im_17_pa, rp3$HLML)
cont_type_hlm
