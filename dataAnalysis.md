#data description
Our analysis uses both individual flight data and quarter-hour based weather and traffic demand data from the FAA's ASPM database.  In the present analysis, we will focus on arrivals into JFK during the month of July in 2014.  Despite the focus on a single month, there are still 17,904 flights represented in this sample, consisting of various major carriers departing from various US airports (footnote: ASPM carriers and airports are limited to...)

The individual flight data records indicate the airborne duration experienced by a given flight and whether that flight recieved an EDCT (footonote: although not explicitly indicated, we make the assumption that the EDCT was due to a GDP in effect at the arrival airport, i.e. JFK, although it is possible for an AFP to be the cause of the EDCT.  To justify our assumption that a GDP was most likely the source of the EDCT, we analyzed NTML data and times that a GDP was in effect at JFK and noticed a large correlation between...).  Furthermore, the individual flight data also lists scheduled and actual wheels-off and wheels-on times, enabling us to identify weather and traffic demand features at the arrival airport that may be relevant for a given flight's airborne duration or ground delay if present.

#data preparation
To fit a propensity score model we need to extract features that can be predictive of a flight's treatment status, namely if the flight is likely to be part of a GDP.  As mentioned above, if there is a non-empty entry for the EDCTWoff variable for an individual flight, then we categorize that flight as being part of the treatment group and otherwise as part of the control group.  



#introduction

Improvements in Air Traffic Management (ATM) require accurate performance measurements or estimates of Traffic Flow Management Initiatives (TFMI).  Traffic managers and other decision-makers should be able to estimate potential impacts of performance metrics due to various courses of actions and inaction, such as specific TFMI (GDP, AFP, GS, Reroutes, etc.).  In addition to enabling tradeoffs between different courses of action, counterfactual estimates of perfomance due to a given TFMI,  also provide decision makers an estimate of the potential cost of inaction, for example average airborne delays if a GDP had not been imposed at a given arrival airport.  We propose to employ the statiticlaly rigorous methodolgy of causal inference to derive these counterfactual estimates, and to the authors knowledge, a first application of these well-know techniques to the context of ATM performance measurement.   

As noticed in \cite{bilimoria2016analysis}, flights with GDP ground-delays tend to have larger airborne delays than flights that did not recieve an EDCT under a GDP\footnote{Roughly four minutes of additional airborne delay was observed for flights into JFK during 2013-2015}.  However it is commonly understood that had those flights under GDP not recieved a ground-delay, they would experience an even larger airborne delay than actually observed, due to congestion at the arrival airport caused by weather and/or traffic.  In one sense, our present analysis attempts to quantify this potential savings in airborne delay.  However, since the counterfactual cannot actually be observed for the ground-delayed flights, we require a statistically sound methodolgy to estimate it from our dataset.  This motivation of performance measurement is of a very specific ATFMI, namely a GDO, but we believe that counterfactual estimation is an important requirement for various other situations relevant for improved ATM.





#how to compute airborne delay; using 10% quantile of nominal airborne times
One goal of a GDP is to limit airborne delay by imposing less costly ground delays on flights, and thus airborne delay is cleary one outcome of interest when estimating the counterfactual of not imposing a GDP.  Thus we need to decide how to measure airborne delay.  One option is to simply take the difference between the actual airborne duration and the operator reported expected time enroute (ETE), both of which are reported in our ASPM dataset.  However it is well known that airlines routinely pad schedules and thus the reported ETE usually includes a buffer \cite{skaltsas2011analysis},\cite{hao2013airlines}.  Previous research has offered an alternative method to compute airborne delay by first computing the tenth percentile of observed airborne durations for a fixed triplet of origin, destination and carrier \cite{skaltsas2011analysis}, and using this quantile as the 'nominal' airborne time to compute observed airborne delays.  We will use both of these approaches in our estimation of the average treatment effect on the treated (ATT), i.e the average difference between the observed and estimated counterfactual airbrone delay for flights under a GDP. 

#alternative metric
In addtion to computing ground delays for flights (e.g. EDCT) with various objectives such as equity and efficiency \cite{glover2013stochastic} , GDP planning also requires ensuring that arriving flights do not exceed the capacity at the arrival airport, defined by the Airport Acceptance Rate (AAR).

their concluision is: 
"indicating that some of these flights may have been subjected to other TMIs in addition to a ground hold at the departure airport"hou

#future work
We suggest several extensions of our intial application of causal inference framework and methodology to ATM.  One extension is to consider multiple discrete treatments at the flight level, such as whether a GDP is ongoing at any of the major New York metro airports, and thus capturing the influence of network flows of neighboring airports due to runway configuration choices imposed by GDP or reduced AAR.  Another extesnsion is to consider continuous valued treatments by explictly considering the length of the ground-delay as the treatement variable, and estimating what in statistics is called the "dose-response curve"\cite{fong2014covariate}.  

#conclusion


