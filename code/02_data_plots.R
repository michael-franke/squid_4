######################################################
## plot data for different positions separatedly
######################################################

###############
## quantifier
###############

# without SemPragType distinction
qsp = ggplot(qsd, aes(x = quantifier, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes( ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  facet_grid(. ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(qsp)

# with SemPragType distinction
qspSP = ggplot(qsdSP, aes(x = quantifier, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  facet_grid(semPragType ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(qspSP)

###############
## adjective
###############

# without SemPragType distinction
asp = ggplot(asd, aes(x = frag, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(. ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(asp)

# with SemPragType distinction
aspSP = ggplot(asdSP, aes(x = frag, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(semPragType ~ context) + xlab("sentence fragment") + ylab("grand average N400")
show(aspSP)

###############
## shape noun
###############

# without SemPragType distinction
nsp = ggplot(nsd, aes(x = shape2, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(frag ~ context) + xlab("shape noun") + ylab("grand average N400")
show(nsp)

# with SemPragType distinction
nspSP = ggplot(nsdSP, aes(x = shape2, y = obs)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  facet_grid(frag + semPragType ~ context) + xlab("shape noun") + ylab("grand average N400")
show(nspSP)
