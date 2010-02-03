# Flaming Gorge Operational Rules Outline for Probabilistic Midterm Model

___Revisions:___

_27 January 2010:_ First Draft, Cameron Bracken

_3 February 2010:_ Revised Draft, Cameron Bracken

## Data
### Data Object
- 1963 - Present historic Data
- Look up table for Hydrologic Classification Wet, Moderately Wet, Average, Moderately Dry, Dry
- Look up table for Upper Limit Drawdown Elevation
- Base flow limits for each classification

## DMI
### Retrieve Forecasts

## Rules

### Spring Flow Hydrologic Classification
_Execution Constraint_: Jan - July
	
_Description_: Use forecast of April-July Volume to determine the hydrologic classification for the given

- In any particular month, classify based on following season forecasted April - July unregulated inflow. 

	
### Set Safe Operating Elevation Limit
_Execution Constraint_: March or April

_Description_: Sets the May 1 target elevation
	
- Look at max probable forecast
	
### Check Max daily rate of change
_Execution Constraint_: August - April

_Description_: Ensures that rate of change of release does not exceed 50 cfs/day.


###Base flow Operations
_Execution Constraint_: June - February

_Description_: Depending on the model start month, set the base flow operations based on the hydrologic classification in order to meet the May 1 ULDE.  

- Sub time step calculations since downramp periods may occur within a month.  - Automatic way to check if the base flow operations have already been set.  For example if it is a Dry year and we start the model in July, the down ramp period will have already occurred. This will be tricky since the down ramp period will get rolled into the previous months average flow (maybe check for changes in pool elevation?).


### Set Spring Flow
_Execution Constraint_: March or April

_Description_:  Sets the spring release to meet power plant capacity and  control flooding. 

- Power plant capacity unless more release is necessary to prevent flooding. 
- Look at Yampa flows or assume May 23 ramp up date?
- Sub time step calculations to account for ramp up period.

### Transition period flows
_Execution Constraint_: March, April, May

_Description_:  Use base flow operation value but adjust if necessary to meet May 1 target. 

- If the base flow operations value is not right to meet the May 1 target, adjust based on 


