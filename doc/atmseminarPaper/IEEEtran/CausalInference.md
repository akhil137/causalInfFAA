Causal Inference
========================================================
author: Akhil Shah
date: 

Counterfactual Estimation
========================================================
* Consider a situation that will likely require GDP
* Decision makers would like to quantify *what if* we don't apply a GDP
* Causal Inference methods can help estimate this counterfactual
* Use machine learning methods to generate these counterfactuals
* example: What happens to airborne delay if a GDP is not implemented?

Causal Inference in a nutshell
========================================================
* Unbiased counterfactual estimation requires eliminating confounders
* Eliminate counfounding through propensity scores - probability to recive treatment (GDP) based on covariates (weather,traffic)
* Estimation of propensity score (or log-odds of): Linear Logistic regression 

$$\frac{P(GDP=1|X)}{1-P(GDP=1|X)}=F(X) = \beta X$$

* GBM (using K trees) is often better (more flexible and robust) than linear logistic regression

$$F(X) = \sum^K\beta_k g_k(X)$$ 

Recall original motivation
========================================================
* Context: a situation (time window) where GDP is to be applied (e.g. inclimate weather)
* Decision making question: "What if we don't implement a GDP?" 
* Staticians call this estimation of "average treatment effect on the treated (ATT)": estimate the counterfactual of not applying a GDP
* What outcome do we want to analyze as a result of applying or not-applying GDP?
  * Airborn delay
  * Arrival Queue length
  
Data manipulation and alignment
=======================================================
* Analysis requires hourly TAF, ASPM (various modules), TFMI  
* ASPM has hourly values for average airborne delay
  * Queue length from TAER module of ASPM is 15-min based
  * Arrival Demand and Effective Arrivals (number of serviced aircraft)
* TAF is (nominally) generated for 0,6,12,18h 
* TFMI is event based 
  * GDP initations (root advisories) can be modified
  * Must follow the sub time-series to get actual start/stop times
  * End-result is 'status' of each hour (GDP or No-GDP)


GDP events: planned vs "backdated"
========================================================

- JFK 2010-2014: 496 GDP root advisories; 71 were 'backdated' 
- Large variance in time horizons for planned GDP, 
<img src="CausalInference-figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

* whereas backdated GDP events usually start within 10 minutes prior to sending
<img src="CausalInference-figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

Airborne Delay/Arrival Queue dynamics
========================================================
* What effect does a GDP have on these outcomes?
* Consider +/-5 hours centered on backdated GDP events





![plot of chunk unnamed-chunk-6](CausalInference-figure/unnamed-chunk-6-1.png)![plot of chunk unnamed-chunk-6](CausalInference-figure/unnamed-chunk-6-2.png)
* Both have delayed (offset) decrease after initiation of GDP 
* Consider outcomes at multiple offsets from treatment (GDP) initiation



Data manipulation and alignment
=======================================================
* Analysis requires hourly TAF, ASPM (various modules), TFMI  
* ASPM has hourly values for average airborne delay
  * Queue length from TAER module of ASPM is 15-min based
  * Arrival Demand and Effective Arrivals (number of serviced aircraft)
* TAF is (nominally) generated for 0,6,12,18h 
* TFMI is event based 
  * GDP initations (root advisories) can be modified
  * Must follow the sub time-series to get actual start/stop times
  * End-result is 'status' of each hour (GDP or No-GDP)


Feature Relevance
=======================================================

```
[0]	train-error:0.023655
[1]	train-error:0.022529
[2]	train-error:0.020698
[3]	train-error:0.019713
[4]	train-error:0.019150
[5]	train-error:0.018023
[6]	train-error:0.017319
[7]	train-error:0.016333
[8]	train-error:0.016615
[9]	train-error:0.015066
[10]	train-error:0.013940
[11]	train-error:0.012672
[12]	train-error:0.012672
[13]	train-error:0.010983
[14]	train-error:0.009716
[15]	train-error:0.009434
[16]	train-error:0.008730
[17]	train-error:0.008589
[18]	train-error:0.007603
[19]	train-error:0.007463
```


Feature Relevance 2
=======================================================
























```
processing file: CausalInference.Rpres
Quitting from lines 187-190 (CausalInference.Rpres) 
Error: Ckmeans.1d.dp package is required for plotting the importance
In addition: Warning messages:
1: Removed 1 rows containing non-finite values (stat_bin). 
2: Removed 426 rows containing non-finite values (stat_bin). 
Execution halted
```
