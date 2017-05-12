#Consider ATT for specific routes to JFK, 
#e.g. departure = LAX or SFO, etc.
#then compute balance and weights via CBPS
#and finally ATT for total Airborne time
#rather than airborne delay which requires
#also estimating 'nominal' airborne time

#Improvement would be to consider continous treatment
#rather than dichtomizing on EDCTWoff (empty means no ground delay)
#note: EDCTETE=-1 implies no ground delay also

#Taken from indivFlightsPropScore.r
#

load("/Volumes/Xstore/code/nasa/src/causalInfFAA/data/aspmFlightsWithCBPSWeights.Rdata")
lax<-aspmFlights[aspmFlights$Dep=="LAX",]
library(CBPS)
ps.cbps.aspm<-CBPS(treat ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
                     +                        A_JFK + ETE, data=lax)

pretreatFeats<-c("treat","crosswind_ASPM", "vis_ASPM", "ceil_ASPM", "windspeed_ASPM", 
                   +                  "A_JFK","ETE")
bal.cbps.aspm<-dx.wts(x=ps.cbps.aspm$weights,data=lax[,pretreatFeats],
                        +                       vars=pretreatFeats[-1],treat.var="treat",estimand="ATT")

bal.table(bal.cbps.aspm)
lax$cbpsAlone<-ps.cbps.aspm$weights

library(survey)
design.aspm<-svydesign(ids=~1, weights=~cbpsAlone, data=lax)
glm.simple.aspm<-svyglm(ActAirborne ~ treat, design=design.aspm)
summary(glm.simple.aspm)

#Results show counterfactual (not applying EDCT) would increase total airborne time
#from LAX to JFK by avg 4.5 minutes (but not statistically significant)
# 
# Call:
#   svyglm(formula = ActAirborne ~ treat, design = design.aspm)
# 
# Survey design:
#   svydesign(ids = ~1, weights = ~cbpsAlone, data = lax)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  311.292      5.468  56.931   <2e-16 ***
#   treat         -4.523      5.611  -0.806     0.42    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 1844.289)
# Number of Fisher Scoring iterations: 2

#For SFO the results show the opposite direction, increase of 5.5 mins
#but the non-weighted outcome distributions differe more greatly for SFO

# > summary(sfo[sfo$treat==0,]$ActAirborne)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 273.0   295.0   303.0   305.4   310.0   780.0 
# > summary(sfo[sfo$treat==1,]$ActAirborne)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 284.0   302.0   313.0   318.4   327.0   734.0 
# > summary(lax[lax$treat==0,]$ActAirborne)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 269.0   289.0   296.0   300.8   303.0   694.0 
# > summary(lax[lax$treat==1,]$ActAirborne)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 267.0   293.0   301.0   306.8   312.5   545.0 
