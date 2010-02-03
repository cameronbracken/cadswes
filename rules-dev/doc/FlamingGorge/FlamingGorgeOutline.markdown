# Flaming Gorge Operational Rules Outline for Probabilistic Midterm Model

####Revisions:

- V1, _27 January 2010:_ Cameron Bracken
- V2, _3 February 2010:_ Cameron Bracken

## Data

---
### Data Object
- 1963 - Present historic Data for definitions of hydrologic calssification
- Look up tables for Hydrologic Classification Wet, Moderately Wet, Average, Moderately Dry, Dry
- May 1 Target elevation table
- Look up table for Upper Limit Drawdown Elevation
- Base flow limits and ramp down rated for each classification

## Rules

---
### Spring Flow Hydrologic Classification
_Execution Constraint_: January - July
	
_Description_: Use forecast of April-July Volume to determine the hydrologic classification for the given month

- In any particular month, classify based on following season forecasted April - July unregulated inflow. 
- When running from different start dates, the hydrologic classification may change from month to month but will not change during a single run

### Base Flow Hydrologic Classification
_Execution Constraint_: August - December
	
_Description_: Use observed April-July for classification. 

- Classification will not change from spring classification during a single run
- Between runs with start dates August-December, classification will not change
- May vary between runs with start dates Jan - July since incomplete or no observations are available 
- _In forecast mode when no observational data is available should the forecast be used as the 'observed' once the model reached August?_
	
### Set Safe Operating Elevation Limit
_Execution Constraint_: March or April

_Description_: Sets the May 1 target elevation
	
- Based on the most probable forecast for May 1 flow, set the target elevation accorg
	
### Check Max daily rate of change
_Execution Constraint_: August - February

_Description_: Ensures that rate of change of release does not exceed 50 cfs/day.


###Base flow Operations
_Execution Constraint_: June - February

_Description_: Depending on the model start month, set the base flow operations based on the hydrologic classification in order to meet the May 1 ULDE.  

- Overlap with spring flow rule since ramp down month is variable
- Automatic way to check if the base flow operations have already been set.  For example if it is a Dry year and we start the model in July, the down ramp period will have already occurred. This will be tricky since the down ramp period will get rolled into the previous months average flow (maybe check for changes in pool elevation?).


### Spring Flow Operations
_Execution Constraint_: March - August (depending on classification)

_Description_:  Sets the spring release to meet power plant capacity and  control flooding. Higher priority than Base flow operations. 

- Power plant capacity unless more release is necessary to prevent flooding. 
- Assume May 23 ramp up date, 2000 cfs/day (partition volume across months)
- Fix ramp down periods
- Crude checking weather total volume in month is enough to meet peak flow requirement. 


