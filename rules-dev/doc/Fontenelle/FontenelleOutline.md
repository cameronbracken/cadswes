
# Fontenelle Operational Rules Outline for Probabilistic Midterm Model

_Last Edited: 10 November 2009 Cameron Bracken_
_Last Edited: 24 November 2010 Cameron Bracken_


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

---
__Rules for Extreme Conditions__

---

### Min Flow 
_Execution Constraint_: In 1,2,3 order, only execute if any of the Normal Operations rules have executed
	
_Description_: Prevents the total outflow from dropping below the "Min Flow"

- If release is below "Min Flow"
	- Set release to "Min Flow"

### Max Elevation
_Execution Constraint_: In 1,2,3 order, only execute if any of the Normal Operations rules have executed

_Description_: Prevents overtopping by releasing whatever necessary, even if "Safe Channel Capacity" is exceeded. 

- If Elevation > "Max Elevation" 
	- Set release so "Max Elevation" is not violated

### Min Elevation
_Execution Constraint_: In 1,2,3 order, only execute if any of the Normal Operations rules have executed

_Description_: Prevents pool elevation from dropping below minimum

- If Elevation < "Min Elevation" 
	- Solve for outflow Given storage available at min elelvation
	
### Safe Channel Capacity
_Execution Constraint_: In 1,2,3 order, only execute if any of the Normal Operations rules have executed

_Description_: Prevents outflow from exceeding "Safe Chanel Capacity" unless there is a chance of overtopping, in which case, the max elevation rule will take precedence. 

- If Release is above "Safe Channel Capacity"
	- Set Release to "Safe Channel Capacity"


---
__Rules for Normal Operations__: These are mutually exclusive so order does not matter except for the last rule which must be the lowest priority.

---

### Set September-November Baseflow
_Execution Constraint_: Month is September

_Description_: This rule is the first of two that set the steady baseflow at Fontenelle.  This rule looks ahead at the forecasted volumes and determines what the max steady flow for the months of September-March can be. 

Set to September-March to outflow that will meet "April 1 Target Elevation"

- Compute change in volume given "April 1 Target Elevation" and current elevation
- Add volume gain from forecasts September-March (7 months)
- Compute max steady flow over September-March (7 months)

### Set December-March Baseflow
_Execution Constraint_: Month is December

_Description_: Set December-March outflow to steady value that will meet "April 1 Target Elevation"

- Compute change in volume given "April 1 Target Elevation" and current elevation
- Add volume gain from forecasts December-March (4 months)
- Compute max steady flow over December-March (4 months)

### Set April, May or August Release
_Execution Constraint_: Month is April, May or August

_Description_: Set April Release to "Power Plant Capacity". Only set if Outflow at current timestep has a value, otherwise power and spill methods will fail. 

- Set total outflow with GetMaxReleaseGivenInflow, which will also manage low flow years when ppc might draw down the res too far. 
- Check if PPC will cause elevation to go below min, if so, drop release accordingly
	- Incorporates PeakPowerCalc method
	- Incorporates monthlySpillCalc method
	
Rule applies to both April, May and August. 

### Set August
_Execution Constraint_: Month is April, May or August

_Description_: Set April Release to "Power Plant Capacity". Only set if Outflow at current timestep has a value, otherwise power and spill methods will fail. 

- Set total outflow with GetMaxReleaseGivenInflow, which will also manage low flow years when ppc might draw down the res too far. 
	- Incorporates PeakPowerCalc method
	- Incorporates monthlySpillCalc method
	
Rule applies to both April, May and August. 

### Set Baseflow in off-month
_Execution Constraint_: Month is October, November, January, February, March or May

_Description_: Handles the case when the model is started in any month without an explicit decision to be made.  These months are "slaves" to other rules. 

Set release in current month to release in previous month.

### Set June Outflow
_Execution Constraint_: Month is June

_Description_: Set June release to meet "July 1 Target Elevation." If the pool elevation is already above the July 1 Target then shoot for the August 1 Target instead.

- Use SolveOutflow
	- Inflow from forecast
	- Beginning storage from last time step
	- Ending storage from "July 1 Target Elevation"

### Set July Outflow	
_Execution Constraint_: Month is July

_Description_: Set July release to meet "August 1 Target Elevation"

- Use SolveOutflow
	- Inflow from forecast
	- Beginning storage from last time step
	- Ending storage from "July 1 Target Elevation"

### Set Unset Outflow
_Description_: Set Fontenelle outflow to the inflow.  This is the lowest priority rule.  Sets the outflow to a dummy value for the spill and power methods.  Dependencies are then registered for higher priority rules. 

## Functions

### `NUMERIC FontenelleTargetFlow(Numeric Elevation)`
_Description_: Returns an outflow for Fontenelle which is the minimum of the Maximum possible outflow and the outflow to meet an end of month target given by `Elevation`.

- Use GetMaxOutflowGivenInflow or SolveOutflow

### `SLOT MyGetSlot`