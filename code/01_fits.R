######################################################
## find best fitting lambda for pragmatic model
######################################################

rSquared = function(lambda, observations){
  sen = apply_IQR(game, depth = 1, lambda = lambda)$sen
  predictions = get_predictions(sen) %>% 
    mutate(observation = observations)
  predictions_selected = remove_redundant_rel_clauses(predictions) # exclude redundant relative clauses
  myLM = lm(observation ~ expectation, data = predictions_selected)
  - summary(myLM)$r.squared
}

bestLambda = optim(par = c(lambda = 5),
                   fn = function(lambda) rSquared(lambda, observations), 
                   lower = 0, 
                   method = "L-BFGS-B")$par[1]

show(paste0("Best fitting lambda: ", round(bestLambda,3)))

######################################################
## get prediction & plot results for best-fit
######################################################

sen = apply_IQR(game, depth = 1, lambda = bestLambda)$sen
predictions = get_predictions(sen) %>% 
  mutate(observation = get_observations())

predictions_selected = filter(predictions, observation > -2)

# show(cor.test(predictions$expectation, predictions$observation))
# show(cor.test(predictions_selected$expectation, predictions_selected$observation))
myLM = lm(observation ~ expectation, data = predictions_selected)

p = ggplot(predictions, aes(x = expectation, y = observation, shape = context, color = segment)) + 
  geom_point() + geom_abline(slope = myLM$coefficients[2], intercept = myLM$coefficients[1], color = "gray") +
  xlab("probability of incoming word") + 
  ylab("grand average ERP signal") + ggtitle("correlation: predictions - observation")
show(p)

# ggsave(filename = "predObs_N400.pdf", plot = p, height = 4.5, width = 8)
