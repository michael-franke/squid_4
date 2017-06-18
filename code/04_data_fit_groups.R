## prepare data for analyzing grand average N400s over all ROIs

library(tidyverse)
library(reshape2)
library(gtp) # download with: devtools::install_github("michael-franke/game_theoretic_pragmatics_ORE", subdir="gtp")

theme_set(theme_bw() + theme(plot.background=element_blank()) )

########################################################
### set global parameters
########################################################

hetero_bias = 0.5
lambda = 1

###################################
## - penalty values between 0 and 1
## - higher value, lower penalty
###################################

states   = c("A", "B", "C", "D",     # original pictures
             "Ar", "Br", "Cr", "Dr") # pictures with shapes reversed
messages = c("ARQ", "ARK", "ABQ", "ABK",
             "ERQ", "ERK", "EBQ", "EBK") # sentences
semantics = t(matrix(c(0,0,1,0,0,1,1,1,  # picture A
                       0,0,1,1,0,0,1,1,  # picture B
                       1,1,0,0,1,1,0,0,  # picture C
                       1,0,0,0,1,1,0,1,  # picture D
                       0,0,0,1,1,0,1,1,  # picture Ar
                       0,0,1,1,0,0,1,1,  # picture Br
                       1,1,0,0,1,1,0,0,  # picture Cr
                       0,1,0,0,1,1,1,0   # picture Dr
                       ), 
                         nrow = 8, ncol = 8, byrow = TRUE))
rownames(semantics) = messages
colnames(semantics) = states
game = create_game(states = states, 
                   messages = messages, 
                   semantics = semantics)

get_preds = function(lambda = 1, depth = 1, hetero_bias = 0.5){
  
  weights = apply_IQR(game, depth = depth, lambda = lambda)$sen[1:4,]
  # weights = prop.table(weights + 0.001, 1)
  
  
  ## define sets of sentences in which a word (sequence) occurs for convenience:
  A = c(1,2,3,4)
  E = c(5,6,7,8)
  R = c(1,2,5,6)
  B = c(3,4,7,8)
  K = c(2,4,6,8)
  Q = c(1,3,5,7)
  AR = c(1,2)
  AB = c(3,4)
  ER = c(5,6)
  EB = c(7,8)
  
  expectations = weights
  # more complex speaker function
  expectations[,Q] = prop.table(expectations[,Q],1) * (1 - hetero_bias)
  expectations[,K] = prop.table(expectations[,K],1) * hetero_bias
  
  surprisal_position_1 = -log(matrix(c(rowSums(expectations[,A]),
                                       rowSums(expectations[,E])),
                                     ncol = 2, nrow = 4))
  rownames(surprisal_position_1) = rownames(weights)
  colnames(surprisal_position_1) = c("A", "E")
  
  surprisal_position_2 = -log(matrix(c(rowSums(expectations[,AR] / rowSums(expectations[,A])),
                                       rowSums(expectations[,AB] / rowSums(expectations[,A])),
                                       rowSums(expectations[,ER] / rowSums(expectations[,E])),
                                       rowSums(expectations[,EB] / rowSums(expectations[,E]))),
                                     ncol = 4, nrow = 4))
  rownames(surprisal_position_2) = rownames(weights)
  colnames(surprisal_position_2) = c("AR", "AB", "ER", "EB")
  
  surprisal_position_3 = -log(matrix(c(expectations[,"ARQ"] / rowSums(expectations[,AR]),
                                       expectations[,"ARK"] / rowSums(expectations[,AR]),
                                       expectations[,"ABQ"] / rowSums(expectations[,AB]),
                                       expectations[,"ABK"] / rowSums(expectations[,AB]),
                                       expectations[,"ERQ"] / rowSums(expectations[,ER]),
                                       expectations[,"ERK"] / rowSums(expectations[,ER]),
                                       expectations[,"EBQ"] / rowSums(expectations[,EB]),
                                       expectations[,"EBK"] / rowSums(expectations[,EB])
  ),
  ncol = 8, nrow = 4))
  rownames(surprisal_position_3) = rownames(weights)
  colnames(surprisal_position_3) = colnames(weights)
  
  surprisals = cbind(surprisal_position_1,
                     surprisal_position_2,
                     surprisal_position_3)
  
  # show(round(surprisals,3))
  
  predictions = melt(surprisals) %>%
    rename(context = Var1, segment = Var2, surprisal = value) %>%
    mutate(position = c(rep("quant", 8), rep("adj", 16), rep("form", 32) ))
  predictions = filter(predictions, ! grepl("R", x = predictions$segment))
  predictions$expectation = exp(-predictions$surprisal)
  predictions$truth = c(1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,2,2,0,1,2,2,1,1,2,0,1,1,2,1)
  # 0 - false; 1 - true; 2 - falsified before
  predictions$ui    = c(0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  predictions$nf    = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
return(predictions)
}


######################################################
## model fit
######################################################


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

predictions = rbind(get_preds(lambda = 1, depth = 1),  # semantics
                    get_preds(lambda = 1, depth = 1)) %>% 
  mutate(semPragType = rep(c("S", "P"), each = 32)) %>% mutate(context = as.character(context))
predictions[nchar(as.character(predictions$segment)) == 1,]$context = rep(c("A/D", "B/C", "B/C", "A/D"), times = 4)
predictions %>% mutate(observation = 0, bslo = 0, bshi = 0)
predictions = unique(predictions)


for (i in 1:nrow(predictions)) {
  cc = predictions$segment[i] # current condition
  ccont = predictions$context[i] # current context
  ct = predictions$semPragType[i] # current semPrag type
  if (nchar(as.character(cc)) == 1) {
    csub = filter(qsdSP, context == ccont, quantifier == substr(cc,1,1), semPragType == ct)
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }
  if (nchar(as.character(cc)) == 2) {
    csub = filter(asdSP, context == ccont, quantifier == substr(cc,1,1), semPragType == ct)
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }
  if (nchar(as.character(cc)) == 3) {
    csub = filter(nsdSP, context == ccont, quantifier == substr(cc,1,1), semPragType == ct, shape2 == substr(cc,3,3))
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }  
}

predictions_selected = filter(predictions, observation > -2)

with(filter(predictions, semPragType == "P"), cor.test(expectation, observation)) %>% show
with(filter(predictions, semPragType == "S"), cor.test(expectation, observation)) %>% show
# with(predictions_selected, cor.test(expectation, observation)) %>% show


# show(cor.test(predictions_selected$expectation, predictions_selected$observation))
myLMS = lm(observation ~ expectation, data = filter(predictions, semPragType == "S"))
myLMP = lm(observation ~ expectation, data = filter(predictions, semPragType == "P"))

p = ggplot(predictions, aes(x = expectation, y = observation, shape = context, color = segment)) + 
  geom_point() + # geom_abline(slope = myLM$coefficients[2], intercept = myLM$coefficients[1], color = "gray") +
  xlab("probability of incoming word") + facet_wrap( ~ semPragType, scales = "free") +
  ylab("grand average ERP signal") + ggtitle("correlation: predictions - observation")
# show(p)

# ggsave(filename = "predObs_N400.pdf", plot = p, height = 4.5, width = 8)


## detailed plots

qdata = filter(predictions, nchar(as.character(segment)) == 1) %>% mutate(quantifier = substr(segment,1,1)) %>%
  mutate(expectation = ifelse(semPragType == "S", 
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1],
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1]))
qsp2 = ggplot(qdata, aes(x = context, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x=context, ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  geom_point(aes(x = context, y = expectation, color = "firebrick")) +
  facet_grid(quantifier ~ semPragType)
show(qsp2)

adata = filter(predictions, nchar(as.character(segment)) == 2) %>% mutate(quantifier = substr(segment,1,1)) %>%
  mutate(expectation = ifelse(semPragType == "P", 
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1],
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1]))
asp2 = ggplot(adata, aes(x = context, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x=context, ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  geom_point(aes(x = context, y = expectation, color = "firebrick")) +
  facet_grid(quantifier ~ semPragType)
show(asp2)

ndata = filter(predictions, nchar(as.character(segment)) == 3) %>% mutate(quantifier = substr(segment,1,1),
                                                                          shape = substr(segment,3,3)) %>%
  mutate(expectation = ifelse(semPragType == "P", 
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1],
                              expectation * myLMS$coefficients[2] + myLMS$coefficients[1]))
nsp2 = ggplot(ndata, aes(x = context, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x=context, ymin=bslo, ymax=bshi), width=0.5, position = "dodge") + 
  geom_point(aes(x = context, y = expectation, color = "firebrick")) +
  facet_grid(quantifier + shape ~ semPragType)
show(nsp2)

