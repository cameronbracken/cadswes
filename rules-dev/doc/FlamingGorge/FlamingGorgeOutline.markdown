# Flaming Gorge Operational Rules Outline for Probabilistic Midterm Model

#### Revisions:

- Rev. 1, _27 January 2010:_ Cameron Bracken
- Rev. 2, _3 February 2010:_ Cameron Bracken
- Rev. 3, _16 February 2010:_ Cameron Bracken
- Rev. 4, _15 March 2010:_ Cameron Bracken

---
## Data

---
### Data Object
- 1963 - Present historic Data for definitions of hydrologic calssification
- Look up tables for Hydrologic Classification Wet, Moderately Wet, Average, Moderately Dry, Dry
- May 1 Target elevation table
- Look up table for Upper Limit Drawdown Elevation
- Base flow limits and ramp down rated for each classification
- Yampa Forecast

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

### Shift Water for Power Generation 
_Execution Constraint_: January - April 

_Description_: Based on forecast, set the release to power plant capacity in march at the expense of meeting the ULDE target.  Use available water from May > Apr > Feb > Jan.

- Take calculated base flow releases and lower them in January and February
- Possible to switch off shifting from January. 


### Spring Flow Operations
_Execution Constraint_: May - July (depending on classification)

_Description_:  Sets the spring release to meet power plant capacity and  control flooding. Higher priority than Base flow operations. August is not included because it will always be handled by the base flow operations rule. 

	IF month is May 
		Split release and begin ramp up 
	ELSE IF Month is {June, July} and NOT downramped
		Set Release to power plant capcity + bypass 
	ELSE
		Error
		
- Power plant capacity unless more release is necessary to prevent flooding. 
- Assume May 23 ramp up date, 2000 cfs/day (partition volume across months)
- Fix ramp down periods
- Crude checking if total volume in month is enough to meet peak flow requirement.

###Base flow Operations
_Execution Constraint_: August - April

_Description_: Depending on the model start month, set the base flow operations based on the hydrologic classification in order to meet the May 1 ULDE.  

		IF month is June and HClass is Wet and NOT downramped
			Set Release to SplitMonthRelease("June","Wet")
			Set BaseFlow to GetBaseFlowMagnitude("Dry")
			Set Downramp Flag
		ELSE IF month is July and HClass ModeratelyDry and NOT downramped
			Set Release to GetBaseFlowMagnitude("ModeratelyDry")
			Set Downramp Flag
		ELSE IF month is July and HClass Average and NOT downramped
			Set Release to SplitMonthRelease("July","Average")
			Set Downramp Flag
		ELSE IF month is August and HClass ModeratelyWet and NOT downramped
			Set Release to GetBaseFlowMagnitude("ModeratelyWet")
			Set Downramp Flag
		ELSE IF month is August and HClass ModeratelyWet and NOT downramped
			Set Release to GetBaseFlowMagnitude("Wet")
			Set Downramp Flag
		ELSE if downramped 
				# Handles all the cases of simply setting baseflow after downramping has occured
			Set Release to GetBaseFlowMagnitude(HClass)
		ELSE 
			Error
    

- Overlap with spring flow rule since ramp down month is variable
- Automatic way to check if the base flow operations have already been set.  For example if it is a Dry year and we start the model in July, the down ramp period will have already occurred. This will be tricky since the down ramp period will get rolled into the previous months average flow (maybe check for changes in pool elevation?).

### Spring Flow Hydrologic Classification
_Execution Constraint_: January - July
	
_Description_: Use forecast of April-July Volume to determine the hydrologic classification for the given month

- In any particular month, classify based on following season forecasted April - July unregulated inflow. 
- When running from different start dates, the hydrologic classification may change from month to month but will not change during a single run
- Sets FGData.HClass

_Consider implementing as function which returns classification as string_

### Base Flow Hydrologic Classification
_Execution Constraint_: August - February 
	
_Description_: Use observed April-July for classification. 

- Classification will not change from spring classification during a single run
- Between runs with start dates August-December, classification will not change
- May vary between runs with start dates Jan - July since incomplete or no observations are available
- Sets FGData.HClass

_Consider implementing as function which returns classification as string_


---
## Functions

---

### `SplitMonthRelease( STRING Month, STRING Class )`
_Description_: Returns a release which is composed of a peak flow for half of the given month and a downramped base flow for the second half (based on given Class). 

    .5*(PowerplantCapacity + NecessaryBypass) + downramp(.5*BaseFlowMagnitude( Class , Class)
	
### `GetBaseFlowMagnitude( STRING Class )`
_Description_: Look up magnitude based on given Hydrologic Classification
    
### `Downramp( DOUBLE Flow, STRING Class )`
_Description_: Returns an average flow which includes downramping based on given Hydrologic Classification.  For example: if starting at 4300 cfs and  downramp rate is 500 and baseflow is 800 the function would add 3800 + 3300 + 2800 + 2300 + 1800 + 1300 + 800 * (number of days left in month) and divide by total days 

_Is this even important or can we just show a sharp break?_
	
### `GetDownrampRate( STRING Class )`
_Description_: Returns the downramping rate based on given hydrologic classification
