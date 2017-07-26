library(tidyverse)
theme_set(theme_bw() + theme(plot.background=element_blank()) )

#########################################
## helper function from Kruschke (2015)
#########################################

HDIofICDF = function( ICDFname , credMass=0.95 , tol=1e-8 , ... ) {
  # Arguments:
  #   ICDFname is R's name for the inverse cumulative density function
  #     of the distribution.
  #   credMass is the desired mass of the HDI region.
  #   tol is passed to R's optimize function.
  # Return value:
  #   Highest density iterval (HDI) limits in a vector.
  # Example of use: For determining HDI of a beta(30,12) distribution, type
  #   HDIofICDF( qbeta , shape1 = 30 , shape2 = 12 )
  #   Notice that the parameters of the ICDFname must be explicitly named;
  #   e.g., HDIofICDF( qbeta , 30 , 12 ) does not work.
  # Adapted and corrected from Greg Snow's TeachingDemos package.
  incredMass =  1.0 - credMass
  intervalWidth = function( lowTailPr , ICDFname , credMass , ... ) {
    ICDFname( credMass + lowTailPr , ... ) - ICDFname( lowTailPr , ... )
  }
  optInfo = optimize( intervalWidth , c( 0 , incredMass ) , ICDFname=ICDFname ,
                      credMass=credMass , tol=tol , ... )
  HDIlowTailPr = optInfo$minimum
  return( c( ICDFname( HDIlowTailPr , ... ) ,
             ICDFname( credMass + HDIlowTailPr , ... ) ) )
}

#########################################
## data on answer type classification
## k - number of participtants cateforized 
##     pragmatic responders
## N - number of participants classified
##     as either pragm. or semantic
#########################################

sp_data = tribble(
  ~study, ~k, ~N,
  "Noveck & Posada", 11, 19,
  "Hunt et al.", 10, 21,
  "Spychalska et al.", 26, 54,
  "Exp. 1", 1, 23,
  "Exp. 2", 13, 25
) %>% 
  mutate(alpha = 1+k, 
         beta = N-k+1, 
         mode = k/N
         )

sp_data$HDI_lo = map_dbl(1:nrow(sp_data), function(i) HDIofICDF(qbeta, 
                                               shape1 = sp_data$alpha[i], 
                                               shape2 = sp_data$beta[i])[1])
sp_data$HDI_hi = map_dbl(1:nrow(sp_data), function(i) HDIofICDF(qbeta, 
                                                        shape1 = sp_data$alpha[i], 
                                                        shape2 = sp_data$beta[i])[2])

sp_plot = ggplot(sp_data, aes(x = study, y = mode)) + 
  geom_point() + 
  coord_flip() +
  geom_linerange(aes(ymin = HDI_lo, ymax = HDI_hi)) +
  ylim(0,1) + xlab("") + ylab("probability of pragmatic type")

f = 0.25
ggsave("../../pics/answer_types.pdf", sp_plot, width = 16*f, height = 8*f)