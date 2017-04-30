#How ab delay is computed in ASPM:

* On a per flight basis:
Uses difference between Estimated Time Enroute and Actual Airborne time (difference between actual wheels off and wheels on).

This flight planned ETE can be different (very diff?) from using the 10th percentile of actual airborne times (for a given triplet of orig; dest; carrier) as the stand-in for unimpeded airborne time.

Note, that ETE, is flight planned ETE; from aspm wiki:
Estimated Time Enroute. The flight plan filed ETE (Estimated Time Enroute) is the planned airborne time, in minutes.

ref: http://aspmhelp.faa.gov/index.php/ASPM:_Individual_Flights:_Definitions_of_Variables

* On an hourly basis:

Below we explain how the hourly averages are computed.


Airport analysis module (w/login) reports average ABdelay per carrier per hour.  Only positive airborne differences ()


Data from individual flights module for for JFK at 0th local hour on 7/2/14:

            ts Carrier ETE AirborneDiff ActAirborne
548 2014-07-02     JBU 306          -19         287
549 2014-07-02     DAL 119            0         119
550 2014-07-02     DAL 106            2         108
551 2014-07-02     DAL 324           -4         320
552 2014-07-02     UAL 285           -3         282
553 2014-07-02     JBU 148           -4         144
554 2014-07-02     DAL 293            6         299
555 2014-07-02     AAL 304            4         308
556 2014-07-02     CMP 282           -9         273
557 2014-07-02     BWA 203            0         203

*Above there are 10 carriers and mean for airborne diff with postive values is = 12mins/10=1.2mins
*that is what is reported in teh airport analysis module (w/o login) for the 0th hour, using Arrivals for metric computation (10 ASPM flights) and not the Schedule Arrivals (which is 11 for the 0th hour).
*w/login, the data is reported by carrier, so e.g. DAL has 4 flights with total of 8 mins positive delay, thus an average ab delay of 2mins.

*per ASPM wiki; ETE is flight plan based:
Estimated Time Enroute. The flight plan filed ETE (Estimated Time Enroute) is the planned airborne time, in minutes. 


#How efficiecny is computed:
ASPM module : Airport Efficiency : Daily Weather By Hour Report

Capacity AAR is airport arrival rate w/o TMI (reported by facility)
Efficiency AAR 