
get_semantic_sender = function(lambda) {
  sen = exp(lambda * apply_IQR(game, depth = 0)$sen)
  prop.table(sen,1)
}

rSquared_groups = function(lambda, observations, semPragType = "S"){
  if (semPragType == "P") {
    sen = apply_IQR(game, depth = 1, lambda = lambda)$sen  
  } else {
    sen = get_semantic_sender(lambda)
  }
  predictions = get_predictions(sen) %>% 
    mutate(observation = observations)
  predictions_selected = remove_redundant_rel_clauses(predictions) # exclude redundant relative clauses
  myLM = lm(observation ~ expectation, data = predictions_selected)
  - summary(myLM)$r.squared
}

bestLambdaSen = optim(par = c(lambda = 5),
                   fn = function(lambda) rSquared_groups(lambda, observations_sem, semPragType = "S"), 
                   lower = 0, 
                   method = "L-BFGS-B")$par[1]

show(paste0("Best fitting lambda for semantic responders: ", round(bestLambdaSen,3)))

bestLambdaPrag = optim(par = c(lambda = 5),
                      fn = function(lambda) rSquared_groups(lambda, observations_prag, semPragType = "P"), 
                      lower = 0, 
                      method = "L-BFGS-B")$par[1]

show(paste0("Best fitting lambda for pragmatic responders: ", round(bestLambdaPrag,3)))


senSemantics  = get_semantic_sender(bestLambdaSen)
senPragmatics = apply_IQR(game, depth = 1, lambda = bestLambdaPrag)$sen

predictions = rbind(get_predictions(senSemantics),  # semantics
                    get_predictions(senPragmatics)) %>% 
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

predictions_selected_S = remove_redundant_rel_clauses(filter(predictions, semPragType == "S"))
predictions_selected_P = remove_redundant_rel_clauses(filter(predictions, semPragType == "P"))
predictions_selected = rbind(predictions_selected_S, predictions_selected_P)

with(filter(predictions_selected, semPragType == "P"), cor.test(expectation, observation)) %>% show
with(filter(predictions_selected, semPragType == "S"), cor.test(expectation, observation)) %>% show
# with(predictions_selected, cor.test(expectation, observation)) %>% show


# show(cor.test(predictions_selected$expectation, predictions_selected$observation))
myLMS = lm(observation ~ expectation, data = filter(predictions_selected, semPragType == "S"))
myLMP = lm(observation ~ expectation, data = filter(predictions_selected, semPragType == "P"))

regLines = tibble(sl = c(myLMS$coefficients[2],
                            myLMP$coefficients[2]),
                  int = c(myLMS$coefficients[1],
                                myLMP$coefficients[1]),
                  semPragType = c("S", "P"))

p = ggplot(predictions, aes(x = expectation, y = observation, shape = context, color = segment)) +
  geom_point() + geom_abline(data = regLines, mapping = aes(slope = sl, intercept = int), color = "gray") +
  xlab("probability of incoming word") + facet_wrap( ~ semPragType, scales = "free_x") +
  ylab("grand average ERP signal") + ggtitle("correlation: predictions - observation")
show(p)

ggsave(filename = paste0(save.folder,"predObs_N400_grouped.pdf"), plot = p, height = 4.5, width = 8)
ggsave(filename = "../slides/02_Bochum/pics/predObs_N400_grouped.jpg", plot = p, height = 4.5, width = 8)

stop()


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

