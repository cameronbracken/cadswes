# Fontenelle Operational Rules Outline for Probabilistic Midterm Model (Revision 2)

## Data	
### Data Object
- July 1 Target Elevation = 6500 ft
- August 1 Target Elevation = 6505.5 ft
- April 1 Target Elevation = 6468 ft
- Max Elevation = 6506 ft
- Min Flow = 300 cfs 
- Safe Channel Capacity = 11500 cfs

## Rules
In order of priority. 
### Min Flow 	
Prevents the total outflow from dropping below the "Min Flow"

- If release is below "Min Flow"
	- Set release to "Min Flow"

### Max Elevation
Prevents overtopping by releasing whatever necessary, even if "Safe Channel Capacity" is exceeded. 

- If Elevation > "Max Elevation" 
	- Set release so "Max Elevation" is not violated
	- Use GetMaxOutflowGivenHW

### Safe Channel Capacity
Prevents outflow from exceeding "Safe Chanel Capacity" unless there is a chance of overtopping, in which case, the max elevation rule will take precedence. 

- If Release is above "Safe Channel Capacity"
	- Set Release to "Safe Channel Capacity"

### Set September-November Baseflow
This rule is the first of two that set the steady baseflow at Fontenelle.  This rule looks ahead at the forecasted volumes and determines what the max steady flow for the months of September-March can be. 

_Execution Constraint_: Month is September

Set to September-March to outflow that will meet "April 1 Target Elevation"

- Compute change in volume given "April 1 Target Elevation" and current elevation
- Add volume gain from forecasts September-March (7 months)
- Compute max steady flow over September-March (7 months)

### Set Baseflow in off-month
Execution Constraint: Month is October, November, January, February, March

Set release in current month to release in previous month. Handles case when model is started in any of these months, in those cases the release will be set to past observed release. 


### Set December-March Baseflow
Execution Constraint: Month is December

Set December-March outflow to steady value that will meet "April 1 Target Elevation"

- Compute change in volume given "April 1 Target Elevation" and current elevation
- Add volume gain from forecasts December-March (4 months)
- Compute max steady flow over December-March (4 months)

### Set April-May Release
Execution Constraint: Month is April

Set April Release to "Power Plant Capacity"

- Use GetMaxReleaseGivenInflow
	- Incorporates PeakPowerCalc method
	- Incorporates monthlySpillCalc method
	
Set May Release to April Release

### Set June Release
Execution Constraint: Month is June

Set June release to meet "July 1 Target Elevation"

- Use GetMaxOutflowGivenHW

### Set July Release	
Execution Constraint: Month is July

Set July release to meet "August 1 Target Elevation"

- Use GetMaxOutflowGivenHW

### Set August Release
Execution Constraint: Month is August

Set August release to "Power Plant Capacity" 

- Use GetMaxReleaseGivenInflow
	- Incorporates PeakPowerCalc method
	- Incorporates monthlySpillCalc method
	
	
Last Edited: 8-28-09 Cameron Bracken