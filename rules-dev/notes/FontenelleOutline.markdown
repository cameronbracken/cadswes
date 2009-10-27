# Fontenelle Operational Rules Outline for Probabilistic Midterm Model
In rough order of priority. 

## Data	
### Data Object
- Power Plant Capacity Above Trigger = 1700 cfs
- Power Plant Capacity Below Trigger = 1200 cfs
- July 1 Target Elevation = 6500 ft
- August 1 Target Elevation = 6505.5 ft
- April 1 Target Elevation = 6468 ft
- April 1 Target Elevation Tolerance = ???? ft
- Min Elevation = 6468 ft (Flexible)
- Max Elevation = 6506 ft
- Power Plant Capacity Trigger = 6476 ft
- Steady Baseflow Release Range = 1000 -- 1200 cfs
- Min Flow = 300 cfs 

## Rules
### Min Flow 	
- If release is below "Min Flow"
	- Set to "Min Flow"

### Max Elevation
- If Elevation > "Max Elevation" 
	- Set release so "Max Elevation" is not violated

### Safe Channel Capacity
- If release is above "Safe Channel Capacity"
	- Set to "Safe Channel Capacity"

### Set September-November Baseflow
Execution Constraint: Month is September

Set to September-March to outflow that will meet "April 1 Target Elevation"

- Compute change in volume given "April 1 Target Elevation" and current elevation
- If model is started this year, Look at forecast  ??? criteria
- Compute max steady flow over September-March months

### Set December-March Baseflow
Execution Constraint: Month is December

Set December-March outflow to steady value that will meet "April 1 Target Elevation"

- compute change in volume given "April 1 Target Elevation" and current elevation
- If model is started this year, look at forecast  ??? criteria 
- Compute max steady flow over December-March months

### Set April-May Release
Execution Constraint: Month is September

Set April and May release to "Power Plant Capacity"

### Set June Release
Execution Constraint: Month is June

Set June release to meet "July 1 Target Elevation"

- Solve outflow given forecast and the change in storage necessary to meet the "July 1 Target Elevation"

### Set July Release	
Execution Constraint: Month is June

Set July release to meet "August 1 Target Elevation"

- Solve outflow given forecast and the change in storage necessary to meet the "August 1 Target Elevation"

### Set August Release
Execution Constraint: Month is August

Set August release to "Power Plant Capacity"


## Functions 
### Get "Power Plant Capacity"
- If Elevation > "Power Plant Capacity Trigger"
	- return "Power Plant Capacity Above Trigger"
- If Elevation < "Power Plant Capacity Trigger"
	- return Power Plant Capacity below Trigger
	
Last Edited: 8-27-09 Cameron Bracken