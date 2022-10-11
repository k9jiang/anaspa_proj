library(knitr)
library(xtable)
library(survey)
library(tidyverse)
library(questionr)
library(sf)
library(ggplot2)
library(mapsf)
library(mapview)
library(RColorBrewer)

don <- readRDS("data/don.RDS")

don$DEP <- as.character(don$DEP)
don$LIBCOM <- as.factor(don$LIBCOM)
don$SURFHAB <- as.numeric(don$SURFHAB)
don$FINAN <- as.character(don$FINAN)
don$PLG_IRIS <- paste(don$DEPCOM, don$PLG_IRIS, sep ='')

sel <- don %>% mutate(DEPCOM = substr(result_id,1,5)) %>%
  filter(DEPCOM %in% c("75114", "92049", "94003", "92046"))

iris <- readRDS('data/map_iris.RDS')

iris_2 <- iris %>% filter(INSEE_COM %in% c('75114','92049','94003','92046'))
par(mar=c(0,0,0,0))
plot(iris_2$geometry, col="lightyellow", border="gray70",
     lwd=0.2)
points(sel$X, sel$Y, cex = 0.2, col = 'red', pch ='16')

adr <- sel %>% select(result_id, X, Y) %>%
  filter(duplicated(result_id) == F)%>%
  filter(is.na(X)==F, is.na(Y)==F)

map_adr <- st_as_sf(adr, coords = c('X','Y'))
st_crs(map_adr) <- 2154
str(map_adr)

logt_by_adr <- sel %>% 
  group_by(result_id) %>%
  summarise(nblog = n(),
            datemoy = mean(CONSTRUCT))

kable(head(logt_by_adr, 10))

map_logt <- inner_join(logt_by_adr,map_adr) %>% st_as_sf()

mf_theme("agolalight")
mybreaks = c(1900, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)
library(RColorBrewer)
mypal=brewer.pal(n = 8,name = "Spectral")
mf_map(iris_2, type = "base",  
       col = "gray80",border="white", lwd=0.3)
mf_prop_choro( x = map_logt,  var = c("nblog", "datemoy"), 
               inches = 0.1, col_na = "grey", pal=mypal,
               breaks = mybreaks, nbreaks = 4, lwd = 0.1,
               leg_pos = c("right", "left"),leg_val_rnd = c(0,0),
               leg_title = c("nb. logements", "ancienneté"),
               add = TRUE)
mf_layout(title = "Les logements sociaux en 2020",
          frame = TRUE, credits = "Sources : IGN et RPLS")

