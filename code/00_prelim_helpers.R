########################################################
### includes and technical preliminaries
########################################################

library(tidyverse)
library(reshape2)
library(gtp) # download with: devtools::install_github("michael-franke/game_theoretic_pragmatics_ORE", subdir="gtp")
library(bootstrap)

theme_set(theme_bw() + theme(plot.background=element_blank()) )

########################################################
### set global parameters
########################################################

lambda = 6.5
save.folder = "../pics/"

########################################################
### setting up the game
########################################################

state.names = c("A", "B", "C", "D",
                "Ar", "Br", "Cr", "Dr")
states   = paste(rep(state.names, each = 2), rep(c("Q", "K"), times = 8), sep = "") 
messages = c("ARQ", "ARK", "ABQ", "ABK",
             "ERQ", "ERK", "EBQ", "EBK") # sentences
semantics = t(matrix(c(0,0,1,0,0,0,1,0,  # picture A, shape Q
                       0,0,0,0,0,1,0,1,  # picture A, shape K
                       0,0,1,0,0,0,1,0,  # picture B, shape Q
                       0,0,0,1,0,0,0,1,  # picture B, shape K
                       1,0,0,0,1,0,0,0,  # picture C, shape Q
                       0,1,0,0,0,1,0,0,  # picture C, shape K
                       1,0,0,0,1,0,0,0,  # picture D, shape Q
                       0,0,0,0,0,1,0,1,  # picture D, shape K
                       0,0,0,1,0,0,1,0,  # picture Ar, shape Q
                       0,0,0,0,1,0,0,1,  # picture Ar, shape K
                       0,0,1,0,0,0,1,0,  # picture Br, shape Q
                       0,0,0,1,0,0,0,1,  # picture Br, shape K
                       1,0,0,0,1,0,0,0,  # picture Cr, shape Q
                       0,1,0,0,0,1,0,0,  # picture Cr, shape K
                       0,0,0,0,1,0,1,0,  # picture Dr, shape Q
                       0,1,0,0,0,1,0,0   # picture Dr, shape K
), 
nrow = 16, ncol = 8, byrow = TRUE))
rownames(semantics) = messages
colnames(semantics) = states

game = create_game(states = states, 
                   messages = messages, 
                   semantics = semantics)

########################################################
### calculate listener's next-word expectations 
########################################################

get_predictions = function(sen) {

  # global expectations based on sender behavior
  senQ = sen[(1:8)*2 - 1,]
  senK = sen[(1:8)*2,]
  expectations = senQ[1:4,]
  for (i in 1:4) {
    expectations[i,] = 0.5 * senQ[i,] + 0.5 * senK[i,]
  }
  rownames(expectations) = c("A", "B", "C", "D")
  
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
  
  # derive next-word expectations
  surprisal_position_1 = -log(matrix(c(rowSums(expectations[,A]),
                                       rowSums(expectations[,E])),
                                     ncol = 2, nrow = 4))
  rownames(surprisal_position_1) = row.names(expectations)
  colnames(surprisal_position_1) = c("A", "E")
  
  surprisal_position_2 = -log(matrix(c(rowSums(expectations[,AR] / rowSums(expectations[,A])),
                                       rowSums(expectations[,AB] / rowSums(expectations[,A])),
                                       rowSums(expectations[,ER] / rowSums(expectations[,E])),
                                       rowSums(expectations[,EB] / rowSums(expectations[,E]))),
                                     ncol = 4, nrow = 4))
  rownames(surprisal_position_2) = rownames(expectations)
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
  rownames(surprisal_position_3) = rownames(expectations)
  colnames(surprisal_position_3) = colnames(expectations)
  
  surprisals = cbind(surprisal_position_1,
                     surprisal_position_2,
                     surprisal_position_3)
  
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


predictions_mapper = function(condition, position, column = "surprisal") {
  condition = toupper(condition)
  form_mapper = data.frame(context = c("A", "A", "D", "D"),
                           letter = c("X", "Y", "X", "Y"),
                           word = c("Q", "K", "K", "Q"))
  if (position == "quant") {
    index = which(predictions$context == substr(condition,1,1) & predictions$segment == substr(condition,2,2))
  }
  if (position == "adj") {
    index = which(predictions$context == substr(condition,1,1) & predictions$segment == paste0(substr(condition,2,2),"B"))
  }
  if (position == "form") {
    letter = as.character(form_mapper[which(form_mapper$context == substr(condition,1,1) & form_mapper$letter == substr(condition,3,3)), "word"])
    index = which(predictions$context == substr(condition,1,1) & predictions$segment == paste0(substr(condition,2,2),"B", letter))
  }
  return(predictions[index, column])
}


######################################################
## read and prepare data to match info in predictions
######################################################

semantiker = c("s02", "s03", "s04", "s06", "s07", "s11", "s16", "s18", "s19", "s20", "s25", "s26")

d = read.csv('../data/data.csv')
d = d %>% filter(win %in% c("+0300..+0400"))

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

get_observations = function(){
  sen = apply_IQR(game, depth = 1, lambda = 5)$sen
  predictions = get_predictions(sen)
  predictions$observation = 0
  for (i in 1:nrow(predictions)) {
    cc = predictions$segment[i] # current condition
    ccont = predictions$context[i] # current condition
    cp = predictions$position[i] # current position
    if (nchar(as.character(cc)) <= 2) {
      csub = filter(d, position == cp, context == ccont, quantifier == substr(cc,1,1))
      predictions$observation[i] = mean(csub$mean)
    }
    if (nchar(as.character(cc)) == 3) {
      csub = filter(d, position == cp, context == ccont, quantifier == substr(cc,1,1), shape2 == substr(cc,3,3))
      predictions$observation[i] = mean(csub$mean)
    }  
  }
  return(predictions$observation)
}

observations = get_observations()

######################################################
## get data for different positions separatedly
######################################################

###############
## quantifier
###############

qd = filter(d, position == "quant") %>% 
  mutate(context = ifelse(context %in% c("A", "D"), "A/D", "B/C")) %>% 
  mutate(obs = mean)

# without SemPragType distinction
qsd = qd %>% group_by(context, quantifier) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))

# with SemPragType distinction
qsdSP = qd %>% group_by(context, quantifier, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))

###############
## adjective
###############

ad = filter(d, position == "adj") %>% mutate(obs = mean)

# without SemPragType distinction
asd = ad %>% group_by(context, quantifier) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975)) %>%
  mutate(frag = ifelse(quantifier == "A", "AB", "EB"))

# with SemPragType distinction
asdSP = ad %>% group_by(context, quantifier, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))
asdSP = asdSP %>% mutate(frag = ifelse(quantifier == "A", "AB", "EB"))

###############
## shape noun
###############

# without SemPragType distinction
nd = filter(d, position == "form") %>% mutate(obs = mean)
nsd = nd %>% group_by(context, quantifier, shape2) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975)) %>%
  mutate(frag = paste0(quantifier, "B")) 

# with SemPragType distinction
ndSP = filter(d, position == "form") %>% mutate(obs = mean)
nsdSP = ndSP %>% group_by(context, quantifier, shape2, semPragType) %>% 
  summarize(obs = mean(obs),
            bslo = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.025),
            bshi = quantile(bootstrap::bootstrap(x = mean, nboot = 1000, theta = function(x){mean(x)})$thetastar, 0.975))%>%
  mutate(frag = paste0(quantifier, "B")) 

##############################
## helper functions
##############################

remove_redundant_rel_clauses = function(predictions) {
  # this is not the most transparent way, but it does remove the 
  # data points from the noun-condition where the relative clause is
  # redundant 
  sen = apply_IQR(game, depth = 1, lambda = bestLambda)$sen
  p = get_predictions(sen)
  predictions[which(p$expectation != 0.5),]
}

##############################
## predictions for sem & prag
##############################

apply_IQR(game, depth = 1, lambda = 5)$sen
predictions = rbind(get_predictions(senSemantics),  # semantics
                    get_predictions(senSemantics)) %>% 
  mutate(semPragType = rep(c("S", "P"), each = 32)) %>% 
  mutate(observation = rep(observations, times = 2)) %>% 
  mutate(context2 = as.character(context))
predictions[nchar(as.character(predictions$segment)) == 1,]$context2 = rep(c("A/D", "B/C", "B/C", "A/D"), times = 4)
predictions %>% mutate(observation = 0, bslo = 0, bshi = 0)
# predictions = unique(predictions)


for (i in 1:nrow(predictions)) {
  cc = predictions$segment[i] # current condition
  ccont = predictions$context2[i] # current context
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
observations_sem = filter(predictions, semPragType == "S")$observation
observations_prag = filter(predictions, semPragType == "P")$observation