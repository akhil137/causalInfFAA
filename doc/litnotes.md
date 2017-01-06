"Endogenous control of service rates in stochastic and dynamic queuing models of airport congestion"
Jacquillat, Transportation Research Part E: Logistics and Transportation Review
Volume 73, January 2015, Pages 133–151


abstract: 
integrated model of airport congestion that combines a tactical model of capacity utilization into a strategic queuing model. The model quantifies the relationships between flight schedules, airport capacity and flight delays, while accounting for the way arrival and departure service rates can be controlled over the day to maximize operating efficiency.

 * Airport capacity is defined as the expected number of movements that can be operated at the airport per unit of time under continuous demand

* Some of these factors are not determined exogenously in advance. Instead, air traffic managers exercise a control over the runway configuration in use and the balance of arrivals and departures to make the best use of available capacity over the course of the day.

*Models of airport congestion fall into three categories: microscopic, mesoscopic and macroscopic models.

* this is a macro model: stochastic queueing;  which aims to capture the dynamics of formation and propagation of delays over the day as well as the uncertainty and variability associated with airport operations. Previous research has shown that this model approximates queue dynamics at major US airports well

* The delay estimates obtained with any queuing model depend critically on the estimates of the rates at which arrivals and departures are serviced, which are constrained by the capacity of the airport. 
	* this work models this 'endogenous' dependence: they depend on the schedules of flights and on observed queue lengths. For instance, if a large number of landings are scheduled, then air traffic managers might decide to enhance the arrival throughput, at the expense of the departure throughput. As well, the arrival throughput might be enhanced if the observed arrival queue is longer than expected—or if the departure queue is shorter than expected. 


#model details
inputs: 
1. schedule arrv/deps
2. airport capacity estimates: represent the capacity of each runway configuration by means of an Operational Throughput Envelope (OTE) [OTE based on runway config and weather (VMC/IMC)]

control:
1. determining runway configuration to be used in each period and 
2. specific arrival and departure service rates to be applied to each period.

setup:
* Service is provided by the runway system
* Arriving aircraft are queuing in the terminal airspace, in the en-route airspace, or at the origin airport if a Ground Delay Program is implemented.
* the stochastic evolution of the arrival queue is assumed to be independent of that of the departure queue.
* The arrival and departure service rates are not independent since they are both subject to the same weather-related constraints and are negatively correlated: increasing the arrival throughput reduces the departure throughput
* This model is stochastic and dynamic: both demand and service are time-varying random processes.
* The dynamics of each M(t)/Ek(t)/1 queuing model can be described by a system of first-order differential equations. We solve it off-line and store the queue transition probabilities in a look-up table. 

