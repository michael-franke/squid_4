
## this is a total hack now -> improve

predictions_SC = predictions

predictions$context = as.character(predictions$context)
predictions[nchar(as.character(predictions$segment)) == 1,]$context = rep(c("A/D", "B/C", "B/C", "A/D"), times = 2)
predictions %>% mutate(observation = 0, bslo = 0, bshi = 0)
predictions = unique(predictions)


for (i in 1:nrow(predictions)) {
  cc = predictions$segment[i] # current condition
  ccont = predictions$context[i] # current context
  if (nchar(as.character(cc)) == 1) {
    csub = filter(qsd, context == ccont, quantifier == substr(cc,1,1))
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }
  if (nchar(as.character(cc)) == 2) {
    csub = filter(asd, context == ccont, quantifier == substr(cc,1,1))
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }
  if (nchar(as.character(cc)) == 3) {
    csub = filter(nsd, context == ccont, quantifier == substr(cc,1,1), shape2 == substr(cc,3,3))
    predictions$observation[i] = mean(csub$obs)
    predictions$bslo[i] = mean(csub$bslo)
    predictions$bshi[i] = mean(csub$bshi)
  }  
}

predictions_selected = filter(predictions, observation > -2)


# show(cor.test(predictions_selected$expectation, predictions_selected$observation))
myLM= lm(observation ~ expectation, data = predictions_selected)

# ggsave(filename = "predObs_N400.pdf", plot = p, height = 4.5, width = 8)


## detailed plots

qdata = filter(predictions, nchar(as.character(segment)) == 1) %>% mutate(quantifier = substr(segment,1,1)) %>%
  mutate(expectation = expectation * myLM$coefficients[2] + myLM$coefficients[1])
q2 = ggplot(qdata, aes(x = segment, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  geom_point(aes(y = expectation, color = "firebrick")) +
  facet_grid(. ~ context) + ylab("av. N400 & predictions") + guides(color=FALSE)
show(q2)

adata = filter(predictions, nchar(as.character(segment)) == 2) %>% mutate(quantifier = substr(segment,1,1)) %>%
  mutate(expectation = expectation * myLM$coefficients[2] + myLM$coefficients[1])
a2 = ggplot(adata, aes(x = segment, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  geom_point(aes(y = expectation, color = "firebrick")) +
  facet_grid(. ~ context) + ylab("av. N400 & predictions") + guides(color=FALSE)
show(a2)

ndata = filter(predictions, nchar(as.character(segment)) == 3) %>% mutate(quantifier = substr(segment,1,1),
                                                                          shape = substr(segment,3,3)) %>%
  mutate(expectation = expectation * myLM$coefficients[2] + myLM$coefficients[1])
n2 = ggplot(ndata, aes(x = shape, y = observation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=bslo, ymax=bshi), width=0.5, color = "black", position = "dodge") + 
  geom_point(aes(y = expectation, color = "firebrick")) +
  facet_grid(quantifier ~ context) + ylab("av. N400 & predictions") + guides(color=FALSE)
show(n2)

height = 4.5
width = 8
# ggsave(filename = paste0(save.folder, "predObs_N400_Quantifier.pdf"), plot = q2, height = height, width = width)
# ggsave(filename = paste0(save.folder, "predObs_N400_Adjective.pdf"), plot = a2, height = height, width = width)
# ggsave(filename = paste0(save.folder, "predObs_N400_Noun.pdf"), plot = n2, height = height, width = width)


# total dirty hack
predictions = predictions_SC

