## prepare data for analyzing grand average N400s over all ROIs

library(tidyverse)
library(reshape2)
library(gtp) # download with: devtools::install_github("michael-franke/game_theoretic_pragmatics_ORE", subdir="gtp")
library(bootstrap)

theme_set(theme_bw() + theme(plot.background=element_blank()) )


d = read.csv('../data/data.csv')
d = d %>% filter(win %in% c("+0300..+0400"))

semantiker = c("s02", "s03", "s04", "s06", "s07", "s11", "s16", "s18", "s19", "s20", "s25", "s26")

d = rename(d, context = cont2, quantifier = quan2, shape = form2) %>%
  select(subj, cond, chan, win, mean, context, quantifier, shape, roi, position) %>%
  mutate(roi = factor(roi)) %>%
  mutate(position = factor(position, levels = c("quant", "adj", "form")),
         context = toupper(context),
         shape = toupper(shape),
         cond = toupper(cond),
         quantifier = toupper(quantifier)) %>%
  mutate(shape2 = ifelse(shape == "X", "Q", "K"),
         semPragType = ifelse(subj %in% semantiker, "S", "P"))


# quantifier
qd = filter(d, position == "quant") %>% mutate(context = ifelse(context %in% c("A", "D"), "A/D", "B/C")) %>% mutate(obs = mean)
# without SemPragType distinction
qsd = qd %>% group_by(context, quantifier) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))
qsp = ggplot(qsd, aes(x = quantifier, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes( ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  facet_grid(. ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(qsp)
# with SemPragType distinction
qsdSP = qd %>% group_by(context, quantifier, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))
qspSP = ggplot(qsdSP, aes(x = quantifier, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  facet_grid(semPragType ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(qspSP)


# adjective
ad = filter(d, position == "adj") %>% mutate(obs = mean)
# without SemPragType distinction
asd = ad %>% group_by(context, quantifier) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975)) %>%
  mutate(frag = ifelse(quantifier == "A", "AB", "EB"))
asp = ggplot(asd, aes(x = frag, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(. ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(asp)
# with SemPragType distinction
asdSP = ad %>% group_by(context, quantifier, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))
asdSP = asdSP %>% mutate(frag = ifelse(quantifier == "A", "AB", "EB"))
aspSP = ggplot(asdSP, aes(x = frag, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(semPragType ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(aspSP)

# shape noun

# without SemPragType distinction
nd = filter(d, position == "form") %>% mutate(obs = mean)
nsd = nd %>% group_by(context, quantifier, shape2) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975)) %>%
  mutate(frag = paste0(quantifier, "B")) 
nsp = ggplot(nsd, aes(x = shape2, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(frag ~ context) + xlab("shape noun") + ylab("grand average N400")
show(nsp)
# with SemPragType distinction
ndSP = filter(d, position == "form") %>% mutate(obs = mean)
nsdSP = ndSP %>% group_by(context, quantifier, shape2, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))%>%
  mutate(frag = paste0(quantifier, "B")) 
nspSP = ggplot(nsdSP, aes(x = shape2, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(frag + semPragType ~ context) + xlab("shape noun") + ylab("grand average N400")
show(nspSP)
