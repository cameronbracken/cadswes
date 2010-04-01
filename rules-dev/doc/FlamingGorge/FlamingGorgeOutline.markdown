# Flaming Gorge Operational Rules Outline for Probabilistic Midterm Model

#### Revisions:

- Rev. 1, _27 January 2010:_ Cameron Bracken
- Rev. 2, _3 February 2010:_ Cameron Bracken
- Rev. 3, _16 February 2010:_ Cameron Bracken
- Rev. 4, _15 March 2010:_ Cameron Bracken
- Rev. 5, _28 March 2010:_ Cameron Bracken

---
## Data

---
### Data Object
- 1963 - Present historic Data for definitions of hydrologic classification
- Look up tables for Hydrologic Classification Wet, Moderately Wet, Average, Moderately Dry, Dry (based on quantile of obs or fc)
	- Dry: > 90%
	- Moderately Dry: 90% - 70.1%
	- Average: 70% - 30%
	- Mod Wet: 30% - 10.1%
	- Wet: < 10%
- May 1 Target elevation table
- Look up table for Upper Limit Drawdown Elevation
- Base flow limits and ramp down rated for each classification
- Yampa Forecast
- Manual input Feb-May flows and usage flag indicating special case year
- Required days at Power Plant Capacity
	- Dry: 0 - 7
	- Moderately Dry: 7 - 14 
	- Average: 14 - 28 
	- Mod Wet: 28 - 42
	- Wet: 42 +
	

---
## Rules

---	
### Sanity Checks
_Execution Constraint_: None

_Description_: Various model sanity checks

- August - February rate of change of release does not exceed 50 cfs/day.
- Max Flow, Min Flow

### RIP Adjusted Release Request
_Execution Constraint_: None

_Description_: Manual entry in first year of model run. Highest priority.

### Shift Water for Power Generation **
_Execution Constraint_: January - April 

_Description_: In special cases of Dry-Mod Dry and releases are minimum, use manually input values for Feb-May flow. 

** Only used in special cases when flag is set in FG data object

- Take calculated base flow releases and lower them in January and February
- Possible to switch off shifting from January. 


### Spring Flow Operations
_Execution Constraint_: May - July (depending on classification)

_Description_:  Sets the spring release to meet power plant capacity and  control flooding. Higher priority than Base flow operations. August is not included because it will always be handled by the base flow operations rule. Any of the components of the monthy flow when splitting (ppc, downramping and baseflow) may be zero.  E.g. ppc is zero when only desidual downramping from the previous month is left, downramping and baseflow are zero when the whole month is at ppc. 

	IF month is May 
		Get Number of days at power plant capacity 
		Split release and begin ramp up 
		reduce number of days at ppc
	ELSE IF Month is June or July and days left at ppc 
		Set Release to split between ppc, downramping and baseflow
		Reduce days left at ppc
		
- Power plant capacity unless more release is necessary to prevent flooding. 
- Assume May 23 ramp up date, 2000 cfs/day (partition volume across months)
- Fix ramp down periods
- Crude checking if total volume in month is enough to meet peak flow requirement.
- May flow will be: previous base flow + ramp up period + days at ppc + possible downramp

###Base flow Operations
_Execution Constraint_: June - April

_Description_: Depending on the model start month, set the base flow operations based on the hydrologic classification in order to meet the May 1 ULDE.  Only set June, July if downramping has completely finished in the previous month. 

		IF no days left at ppc AND no leftover dowramping
			Set Release to GetBaseFlowMagnitude(SpringHClass)
    
- Overlap with spring flow rule since ramp down month is variable
- Automatic way to check if the base flow operations have already been set.  For example if it is a Dry year and we start the model in July, the down ramp period will have already occurred. This will be tricky since the down ramp period will get rolled into the previous months average flow (maybe check for changes in pool elevation?).

### Spring Flow Hydrologic Classification
_Execution Constraint_: January - July
	
_Description_: Use forecast of April-July Volume to determine the hydrologic classification for the given month

- In any particular month, classify based on following season forecasted April - July unregulated inflow. 
- When running from different start dates, the hydrologic classification may change from month to month but will not change during a single run
- Sets FGData.SpringHClass

_Consider implementing as function which returns classification as string_

### Base Flow Hydrologic Classification
_Execution Constraint_: August - February 
	
_Description_: Use observed April-July for classification. 

- Classification will not change from spring classification during a single run
- Between runs with start dates August-December, classification will not change
- May vary between runs with start dates Jan - July since incomplete or no observations are available
- Sets FGData.BaseFlowHClass

_Consider implementing as function which returns classification as string_


---
## Functions

---

### `DOUBLE GetNumberOfDaysAtPPC(STRING Class)`
_Arguments_: 

Class
: A string representing the current hydrologic classification

_Description_: Given the Hydrologic classification, interpolate required days at power plant capacity based off exceedance probability of forecast.  For example the exceedance range for Moderately Wet is 30% to 10.1% and the number of days at PPC is 28 - 42.  Say the forecast we have has an exceedance probability of 20%, then interpolating we would stay approximately 35 day at capacity.

_Value_: A number representing the number of days to stay at power plant capacity  

### `DOUBLE GetPPCSplitProportion(STRING Month, NUMERIC RemainingDays)`
_Arguments_: 

Month
: The name of the month to get the split proportion

RemainingDays
: The number of days required at power plant capacity.  

_Description_: Returns the proportion of the given month which should be at PPC. If RemainingDays > number of days in Month, then the returned proportion is 1. 

### `SplitMonthRelease( STRING Month, STRING Class )`
_Arguments_: 

Month
: The name of the month to split

Class
: A string representing the current hydrologic classification

_Description_: Returns an average monthly release which is composed of any previous operations before ramp up, the ramp up period, the period at ppc, the downramping period and the remaining time at base flow. It is possible in very dry years to have all of these terms be nonzero in the same month.  In June and July previous operations and ramp up will always be zero; In June or July is is possible to 

NOTE:  Beware of the case where not enough time is left in the current month to fully downramp. Must be handled specially. 

_Value_: An average monthly release in units of flow. Also sets end of month flow if downramping was not complete within the month. 

	PreviousProportion * PreviousOperations + 
	RampUpProportion * RampUpPeriod + 
    GetPPCSplitProportion() * PowerplantCapacity +
    DownrampProportion * DownrampPeriod +
    RemainingProportion * GetBaseFlowMagnitude()
	
### `DOUBLE GetBaseFlowMagnitude( STRING Class )`
_Arguments_: 

Class
: A string representing the current hydrologic classification

_Description_: Look up magnitude based on given Hydrologic Classification

_Value_: The base flow magnitude in units of flow
    
### `LIST Downramp( DOUBLE Flow, STRING Class )`
_Arguments_: 

Flow
: The current magnitude of flow to downramp

Class 
: The name of the current hydrologic classification

_Description_: Returns an average flow which includes downramping based on given Hydrologic Classification.  For example: if starting at 4300 cfs and downramp rate is 500 and baseflow is 800 the function would add 3800 + 3300 + 2800 + 2300 + 1800 + 1300 + 800 and divide by total days in the downramping period.  

_Value_: A list with 2 elements (1) the average downramped flow, (2) the ending flow after downramping.  If the ending flow after downramping equals the baseflow then the downramping completed all within the same month; Otherwise it is necessary to finish downramping during the next month. 

### `DOUBLE GetDownrampRate( STRING Class )`
_Arguments_: 

Class
: A string representing the current hydrologic classification

_Description_: Returns the downramping rate based on given hydrologic classification

_Value_: The Peak flow to base flow downramping rate in flow/day
