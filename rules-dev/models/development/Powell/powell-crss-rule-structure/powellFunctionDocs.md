Title:  Powell Rule Documentation  
Author: Cameron Bracken
Date:   July 25, 2005
Format: complete  
Base Header Level: 2

# Powell Rule Documentation

This document is a reference for Lake Powell rules in CRSS.

## The Rules

### Check Bypass Capacity

If the reservoir is below the power pool, check if the set outflow is greater than the capacity of the bypass tubes.If it is, reset it to the bypass capacity.If the reservoir is above the power pool, then the power plant capacity is in play. This is roughly 25,000 cfs, or the maximum ROD release anyway, and the limitations of the bypass tubes do not matter.

### Compute 602a Storage

This rule fires once a year and sets the 602a storage. Although this solves for each year in the run duration, the value is only used while the Guidelines are in effect, through 2026.

### Compute 70R Assurance Level Surplus Volume

This rule fires at the beginning of the year and sets the 70R surplus volume. This is the estimated end-of-year volume of water in excess of the system space requirement assuming a 70th percentile runoff, a 7.5 maf Lower Basin delivery, the Upper Basin scheduled use and the current Powell and Mead volumes. If this volume is greater than 0, a 70R or Quantified Surplus is declared in the Surplus Rules: 70R Assurance Level Surplus and 7 State Plan Level 1. 

### EOWYStorageForecasts

### Equalization Post Guidelines

### Equalization Tier

This rule fires monthly January through September and computes the Equalization (EQ) release from Powell, given that the conditions for EQ are met. Note that Powell's previous EOCY storage is compared to the EQ Line. This has the effect of not allowing Powell to bounce in and out of EQ. The new EQ conditions in the Guidelines (release to keep Mead above 1105 so as Powell doesn't go more than 20' below the EQ Line) are performed in the function CheckEqualizationRelease_Mead1105. 

### Estimate Upper Basin Storage

### Lower Elevation Balancing Tier

### Meet Powell Min Objective Release

### Mid Elevation Release Tier

### Powell Forecast Error

Brad Vickers, Wave Engineering Inc.January, 1999This rule checks to see if the PowellForecastError slot on the MeadFloodControlData object has been set, if it has not and the month is not June (because June has some special limits) thenthe forecast error for Powell is computed with the ComputeForecastError function.  The first part of the IF statement in the rule limits June's computed forecast error to one half of May's forecast error. This check was performed in the originall CRSS.  Ideally I believe there should be limits on every month to ensure that the forecast error is within the range of historical forecast error.  This becomes a problem with the random component of how the forecast error is derived.  See the comments on the ComputeForecastError function for further information onhow the forecast error is computed. NOTE the print statement in this rule.  this calls the resetRanDev function which rewinds the file of random numbers to the hydrology start date that is inputby the user on the MeadFloodControlData.HydrologyStartYear table slot.  This function is only called at the first of the model run.  It is used with a print statement because it has to be in a rule rather than a function and there are no slots set with it.

### Powell Limit Outflow Rule

### Powell Operations Rule

### Powell Runoff Forecast

This rule was added to compute and store the forecasts for inflows into Lake Powell.  These computationsare done for Mead's inflow forecast, so we should clean that up eventually.It assumes that the forecast error has been computed previously by the Powell Forecast Error rule and stored on the data/slot  MeadFloodControlData.PowellForecastError

### Powell Smooth July Operation Rule 

### Powell Spike Flow Rule

### Sum Powell Inflow Fall

### Sum Powell Inflow Spring

### Upper Elevation Balancing Tier April thru Sept

This rule fires in April through September and makes any required adjustments to Powell's release if Powell began the year in the Upper Elevation Balancing Tier. The decision to switch to the EQ Tier or to increase the release for balancing is based on the April EOWY forecasts. Meaning, the decision made in April stays for the rest of the water year.

### Upper Elevation Balancing Tier Jan thru March

This rule fires in January through March and determines if Powell is in the Upper Elevation Balancing Tier (based on previous EOCY storage). If Powell is in this tier and Mead (again, previous EOCY storage) is below the minimum balancing elevation of 1075, Powell makes balancing releases within the min and max of 700 and 900 kaf. If Mead is above 1075, Powell releases 823 maf set in the rule Meet Powell Min Objective Release, which is lower priority and required to fire before this rule.

## Functions

`NUMERIC 602aStorage()`


`NUMERIC 602aStorageValue()`


`NUMERIC AnnualEvaporation( OBJECT reservoir, LIST dates )`


`NUMERIC AnnualMinObjectiveRelease()`


`NUMERIC AnnualReducedRelease( STRING releaseIndex )`


`NUMERIC AnnualVolume( SLOT slot )`


`LIST ArizonaPriority2and3UsersWaterUser()`


`NUMERIC AvailableSpace( OBJECT reservoir)`


`NUMERIC BypassCapacity( NUMERIC elevation )`

>The Bypass Tube Capacity table is per tube, and there are 4 tubes at Powell. This assumes all tubes are always available.


`NUMERIC CheckEqualizationRelease( LIST results )`


`LIST ComputeEqualizationReleaseList( NUMERIC EOWYSPowell, NUMERIC EOWYSMead )`

>This function returns a five element list, the order of the list is Equalization Release, Powell Release, EOWY storage for Powell, EOWY storage for Mead, and a count. The list result is initialized with the function InitializedEqualizationReleaseList. Which of course is a five element list in the same order as above.  PER TJF: FEB, 2000:  I think this is a 4 element list ... the equalization release is not a part of it.

 
`NUMERIC CheckEqualizationRelease_Mead1105( NUMERIC equalizationRelease )`

>This function checks the EQ release to ensure that it doesn't cause Powell to go below the EQ Line, unless Mead is less than 1105. If Mead is less than 1105, Powell releases more until the first of the following 3 occurs: 1) the reservoirs are equal 2) Mead is at 1105 3) Powell is 20' below the EQ Line. Then a final check is made that the release is at least what would bring Powell to the EQ Line. If Mead is above 1105, then Powell is constrainted to the EQ Line.


`NUMERIC CheckEqualizationRelease602a( NUMERIC equalizationRelease )`

>After the function ComputeEqualizationReleaseList has completed its while loop, which varies Powell's release until a balance between Powell's and Mead EOWY storages are within equalization tolerance.  Equalization tolerance is a user input variable and is currently set at 2,000 acre-feet. This value is input on the equalizationTolerance slot in the EqualizationData data object. This function does a final check on the release from Lake Powell to insure that making this release will not violate the upper basin 602a storage.  This check is computed by subtracting the value of Powell equalization release (which is represented by the function GetListElement(0,equalizationReleaseList) and the value of ForecastPowellRelease from the amount of storage in the Upper Basin which is computed by the function SumUpperBasinStorage. which is set with a rule and written out to a dataobject slot.  this value is then compared to the value of 602a storage that has already been set with another rule.  Also note that the results of this check are also constrained by a minimum value of zero. 


`NUMERIC CheckERMeadExclusiveFCS( NUMERIC EOWYSMead, NUMERIC equalizationRelease )`

>This function is used in conjuction with ComputeEqualizationRelease.  It is called to check if the computed equalization release will cause Mead toviolate its exclusive flood control space.  If the release will cause this violation, a new equalization release is computed as the live capacity of meadminus the minimum flood control space minus the estimated end of water year storage for Mead.  Otherwise no changes to the equalization release are made.  Also note that this function is contrained to have a minumum value of zero.


`BOOLEAN CheckSpikeFlowCriteria()`


`NUMERIC ComputeEqualizationEvaporation( OBJECT reservoir, LIST dates )`


`NUMERIC ComputeInflowAtProbability()`

>Replaced Tcl function: ComputeInflowAtProbability (3/1/04)-Returns the inflow at a specified assurance probability


`NUMERIC ComputeInitialPowellForecastError()`

>Computes the forecast error based on the previous forecast error, regressioncoefficients, Powell's runoff and the random deviation.  The forecast error is 0Aug-December and is a quarter of the June's error during July.  The value of thisfunction is returned to ComputePowellForecastError() which, during June, limitsthe error to half of May's error. 


`NUMERIC ComputeMinObjReleaseRemaining()`


`NUMERIC ComputeNewPowellRelease( LIST results )`


`NUMERIC ComputePowellForecastError()`

>Replaced Tcl function: ComputePowellForecastError (3/1/04)-this function is called directly from the rule: Powell Forecast Error and limits June's error to half of May's error.  Otherwise, the value of ComputeInitialPowellForecastError() is returned. 


`NUMERIC ComputePowellRelease( NUMERIC equalizationRelease ) 


`NUMERIC ComputePowellReleaseLowerBalancing( NUMERIC equalizationRelease, STRING minRelease, STRING maxRelease )`


`NUMERIC ComputePowellReleaseUpperBalancing( NUMERIC equalizationRelease, STRING minRelease, STRING maxRelease )`

>Computes the monthly balancing release from Powell if in either the Upper or Lower Elevation Balancing Tiers. NOTE: the release is constrained by the specified min and max release for that tier. This specified release is the STRING argument minRelease and maxRelease.


`NUMERIC ComputeReducedReleaseRemaining( STRING releaseIndex )`


`NUMERIC ComputeScheduledMinObjRelRemainingThruSept()`


`NUMERIC ConvertPowellRelease( LIST equalizationReleaseList )`


`NUMERIC ConvertPowellReleaseBalancing( LIST equalizationReleaseList )`


`NUMERIC CriticalPeriodUBDepletion()`

>Replaced Tcl function: CriticalPeriodUBDepletion (3/1/04)From the current year to as many years into the future as the critical period, theUB Depletions are summed. 


`NUMERIC EOWYStorage( OBJECT reservoir, NUMERIC powellRelease, NUMERIC meadRelease )`

>This function estimates the End of Water Year (end of September) storage for Powell and Mead, with the given release from Powell and Mead. by subtractingthe estimated evaporation and bankstorage from the given release.  This function is used in conjunction with equalization.


`BOOLEAN EqualizationConditionsMet()`


`NUMERIC EqualizationRuleCurveStorage( OBJECT reservoir )`


`NUMERIC Estimate Bank Storage without Evap( OBJECT reservoir, NUMERIC startStorage, NUMERIC endStorage )`

>Function to estimate the change in bank storage, given a starting and ending storage.This is different from the function Estimate Bank Storage. In that function, the evaporationis also estimated and subtracted from the ending storage.  why I don't know.

`NUMERIC EstimateBankStorage( OBJECT reservoir, NUMERIC startStorage, NUMERIC endStorage )`


`NUMERIC EstimateEqualizationRelease( NUMERIC EOWYSPowell, NUMERIC EOWYSMead )`


`NUMERIC EstimateEvaporation( OBJECT reservoir, NUMERIC startStorage, NUMERIC endStorage, DATETIME startDate, DATETIME endDate )`


`NUMERIC ForecastMeadRelease()`


`NUMERIC ForecastPowellFallRangeOutflowVolume( NUMERIC initialStorage, DATETIME startDate, DATETIME endDate )`

>Forecasts the Powell outflow volume for a range of months in the fall drawdown period. The Min Constraint has been altered from just the 823 pattern to actually consider how much water is leftin the Minimum Objective release, and also considers the minimum flow constraint.


`NUMERIC ForecastPowellInflow()`


`NUMERIC ForecastPowellJulyStorage()`

>forecasts the Powell july storage at the current timestep by using the spring operation, execpt that the forecasted outflow volumeis contrainied by the min release volume, min objective release volume, and the max release volume.


`NUMERIC ForecastPowellRelease()`


`NUMERIC GetDenominator()`


`NUMERIC GetListElement( NUMERIC index, LIST list )`


`NUMERIC GetNumerator()`


`NUMERIC GetPreviousForecastError()`

>Called by ComputeInitialPowellForecastError() and returns the previous month'sforecast error unless the current month is January.  In that case, 0 is returned.


`NUMERIC GetRandSeed()`

>Returns a seed for use by the RandomNormal function. The seed is incremented by the trace number, as stored in the first timestep of theHydrology Increment slot. The trace number could be available as a RPL function, but would not be enough to fulfill our requirements.In the current setup, all MRM runs will have the same set of random numbers at the same trace, but all traces will have different sets, as controlled by the Hydrology Increment slot, which is rotated with the MRM runs.This also allows single trace runs to have identical random numbers generated as well, which is not possible with a RPL function.Probably should rename the Hydrology Increment slot to Trace Number.With Random Seed = 1, is equivalent to just using the Trace Number as the seed for each trace.


`NUMERIC GetT()`


`NUMERIC InactiveCapacity( OBJECT reservoir )`


`NUMERIC InitialEOWYStorageMead( NUMERIC powellRelease, NUMERIC meadRelease )`


`NUMERIC InitialEOWYStoragePowell( NUMERIC powellRelease )`


`LIST InitialEqualizationReleaseList( NUMERIC EOWYSPowell, NUMERIC EOWYSMead )`


`BOOLEAN InLowerElevationBalancingTier()`

>Determines if Powell (based on previous EOCY storage) is in the Lower Elevation Balancing Tier. Returns TRUE if so.


`BOOLEAN InMidElevationReleaseTier()`

>Determines if Powell (based on previous EOCY storage) is in the Mid Elevation ReleaseTier. Returns TRUE if so.


`BOOLEAN InUpperElevationBalancingTier()`

>Determines if Powell (based on previous EOCY storage) is in the Upper Elevation Balancing Tier. Returns TRUE if so.


`NUMERIC IterateBypassCapacity( NUMERIC elevation )`


`NUMERIC LiveCapacity( OBJECT reservoir )`


`STRING MakeSlotNameAggDiv( OBJECT diversion )`


`STRING MakeSlotNameWaterUser( OBJECT diversion )`


`STRING MakeSlotNameWaterUserPriority2and3( OBJECT diversion )`


`NUMERIC MaxRelease( OBJECT reservoir )`


`NUMERIC MaxReleaseVolume( OBJECT reservoir, DATETIME startDate, DATETIME endDate )`


`NUMERIC MinRelease( OBJECT reservoir )`


`NUMERIC MinReleaseVolume( OBJECT reservoir, DATETIME startDate, DATETIME endDate )`


`NUMERIC NumberMonthsRemaining()`


`LIST OtherArizonaPriority4UsersAggDivSite()`


`LIST OtherArizonaPriority4UsersWaterUser()`


`NUMERIC PowellComputeFallSeasonRelease()`

>This function computes the release for the month, from the fall outflow volume and the release weights. It is constrained by the miniumu and maximum releases.


`NUMERIC PowellComputeRunoffSeasonRelease()`

>This function computes the release for the month, from the spring outflow volume and the releaseweights. It is constrained by the minimum and maximum release.On March 4, 2000, TJF added the check to see if the outflow volume average (equally distributed over the remainingmonths) exceeds a check (currently 1 maf).  If so, we want to just equally weight the outflow for the remaining months.This was added for high forecast years, where we really would want to start dumping the water quicker than what thecurrent weights would have us do.


`NUMERIC PowellComputeStorageAtGivenOutflow( NUMERIC outflow )`

> Returns Powell's storage for a given outflow. First checks that there is enough storage in Powell for the given outflow. If there isn't enough, set Powell's storage to 0. This check is required for hydrologic sequences that take Powell this low. RiverWare's SolveStorage function will return a NaN if a negative storage is computed. NOTE: the final storage is constrained by Powell's minimum content, 0 and live capacity.


`NUMERIC PowellFallOutflowVolume( NUMERIC initialStorage, DATETIME startDate, DATETIME endDate )`

>This function computes the outflow volume from Powell from the current month through December necessary to meet the December target.It assums a perfect forecast.


`NUMERIC PowellFallRelease()`


`NUMERIC PowellFallSeasonStorage()`

>This function takes the computed fall season release and computes the resulting storage giventhe inflow for the current month. The ComputeStorage function also checks to be sure that theresulting storage is between live and inactive capacity.


`NUMERIC PowellMinObjRelforCurrentMonth()`


`NUMERIC PowellMinObjRelVolRemaining()`


`NUMERIC PowellMinObjRelVolumeFall( DATETIME startDate, DATETIME endDate )`


`NUMERIC PowellMinObjRelVolumeSpring( DATETIME endDate )`

>Estimates the volume of minimum objective release remaining from the current timestep through the endDateand scales it by the ratio of the amount remaining through EWOY to the annual value.


`NUMERIC PowellMinSpikeFlowOutflow()`


`NUMERIC PowellNaturalInflowVolume( DATETIME startDate, DATETIME endDate )`


`NUMERIC PowellReducedRelforCurrentMonth( STRING releaseIndex )`


`NUMERIC PowellReducedRelVolRemaining( STRING releaseIndex )`


`NUMERIC PowellRegulatedInflowVolume( DATETIME startDate, DATETIME endDate )`

>This function computes the regulated inflow into Powell, from the startDate through the endDate.No forecast error is used.


`NUMERIC PowellReleaseFraction( DATETIME startDate, DATETIME endDate )`

>This function computes the release fraction for the current month. Since the tables are indexed from zero,we have to subtract 1 from the numeric month.


`NUMERIC PowellRunoffSeasonStorage()`

>This function takes the computed runoff season release and computes the resulting storage given the inflowat the current timestep. The ComputeStorage function also checks it to be sure that the resulting storage is betweeninactive and live capacity.


`NUMERIC PowellSpringOutflowAverage()`


`NUMERIC PowellSpringOutflowVolConstrained()`


`NUMERIC PowellSpringOutflowVolume()`


`NUMERIC PowellSumReleaseWeights( DATETIME startDate, DATETIME endDate )`


`NUMERIC PreviousStorage( OBJECT reservoir )`


`NUMERIC PreviousYearBankBalance( STRING player )`


`NUMERIC RegressionCoefficient( OBJECT reservoir, STRING col )`


`NUMERIC ReleaseMade()`


`NUMERIC RuleCurveStorage( OBJECT reservoir, DATETIME date )`

>Returns the rule curve storage for a reservoir based on the month provided.


`NUMERIC SetRandomDeviation()`

>This function calls the random number generator and returns the random number if it is Jan - June.  Otherwise, there is no random deviation. The valuereturned by this function is stored in PowellForecastData.random. The 0.000 passed to RandomNormal() only specifies the units of the returned number, in this case,NONE.  To get a different set of random numbers, change the seed inPowellForecastData.Random Seed.The min and max constraints on this function are from the old generator, which had these limits. We have a slightly different result here, because the old generator would create a new number if the value exceeded these limits, we just return a number at the limit instead. Things will probably break if we get a random number even close to this large.Since the generator has less than a 1 in 1e-21 chance of generating something outside these bands, it probably isn't useful to have those constraints anyway. :)`


`NUMERIC ShortedUBDepletion()`


`BOOLEAN SpikeAlreadyMade()`


`NUMERIC SumDemandsDownstreamofMead()`

>I changed this function 2/26/2000 to add in Coachella and IID as they are now not  included in the Below Mead sub-basin since they may be surplused.Also, CRSS did some funky thing and only used the current timesteps MWD and CAP diversion schedule, and looked in the past year for the rest of the schedule.It probably was because it didn't know whether they were surplused or not.  Since we do, I changed this function to just sum what's on the slots.


`NUMERIC SumDemandsDownstreamOfPowell()`

>The original CRSS did not include CAP and MWD in this calculation, SumDepletionsBelowVolume with Mead includes every diversion below Mead with the exception of SNWP, CAP, MWD, and Mexico. therefore the annual volume of SNWP and Mexico depletion schedules are added to match CRSS results.  This seems to be a very bad calculation and I would recommend thatit be examined closely in conjuction with all of the functions that sum demands into the future such as SumDemandsDownstreamOfMead, and SumDemandsBelowMead and SumCurrentDemandsBelowMead, to see if a common function could replace all of these.  It is not possible now in order to match CRSS results.The above comments was due to Brad Vickers, Wave Eng.  On Feb 26, 2000, Terry Fulp changed this function to include MWD, CAP, and Coachella and IID, as they are nowbeiing surplused.


`NUMERIC SumDepletionsBelowVolume( OBJECT reservoir, DATETIME startDate, DATETIME endDate )`


`NUMERIC SumEvapCoeff( OBJECT reservoir, NUMERIC startRow, NUMERIC endRow )`


`NUMERIC SumInflowBelowMead()`


`NUMERIC SumLBDemands()`


`NUMERIC SumPreviousYearICSCredits()`

>This function returns the beginning of the year ICS balance for Arizona, California, Nevada and Unassigned. There is no participation assumed by Unassgined for the current Guidelines ruleset.  If the current timestep is after 2036, when the ICS program expires, this function returns 0. 


`NUMERIC SumUBDemands()`


`NUMERIC SumUpperBasinStorage()`


`NUMERIC SurplusMaxStorage()`


`NUMERIC TotalOtherPriority4ActualUse( DATETIME startDate, DATETIME endDate )`


`NUMERIC TotalOtherPriority4ScheduledUse( DATETIME startDate, DATETIME endDate )`


`NUMERIC TotalPowellRelease( LIST results )`


`NUMERIC TotalPriority2and3ActualUse( DATETIME startDate, DATETIME endDate )`


`NUMERIC TotalPriority2and3ScheduledUse( DATETIME startDate, DATETIME endDate )`


`NUMERIC UBDepletionsRange( DATETIME startDate, DATETIME endDate )`


`NUMERIC UBDrawDown( OBJECT reservoir, DATETIME startDate, DATETIME endDate )`


`NUMERIC UBEffectiveStorage()`


`NUMERIC UBInitialStorage( OBJECT reservoir, DATETIME date )`


`NUMERIC UBRegulation( DATETIME startDate, DATETIME endDate )`


`NUMERIC UBTargetStorage( OBJECT reservoir, DATETIME date )`

>This function computes the target storage from the live capacity and target spacefor the Upper Basin reservoirs.


`NUMERIC WaterAvailableInUpperBasin()`


`STRING GetSlotNameForScheduleWaterUser( STRING diversion )`


## Function Call Structure

### 1 - Compute 602a Storage
	ASSIGN TO $ "EqualizationData.value602a" [@"24:00:00 December 31, Current Year"]
		602aStorage
			AnnualMinObjectiveRelease
			ShortedUBDepletion
				CriticalPeriodUBDepletion
				
### 2 - Compute 70R Assurance Level Surplus Volume
	ASSIGN TO $ "Surplus.SurplusRelease" []
		ComputeInflowAtProbability
			GetDenominator
				GetT
			GetNumerator
				GetT
			GetT
		SumLBDemands
			AnnualEvaporation
				EstimateEvaporation
					SumEvapCoeff
				RuleCurveStorage
			SumDemandsDownstreamOfPowell
				AnnualVolume
				SumDepletionsBelowVolume
				TotalOtherPriority4ScheduledUse
					MakeSlotNameAggDiv
					MakeSlotNameWaterUser
						GetSlotNameForScheduleWaterUser
					OtherArizonaPriority4UsersAggDivSite
					OtherArizonaPriority4UsersWaterUser
				TotalPriority2and3ScheduledUse
					ArizonaPriority2and3UsersWaterUser
					MakeSlotNameWaterUserPriority2and3
						GetSlotNameForScheduleWaterUser
		SumPreviousYearICSCredits
			PreviousYearBankBalance
		SumUBDemands
		SurplusMaxStorage
			LiveCapacity
			
### 3 - Powell Forecast Error
	ASSIGN TO $ "MeadFloodControlData.PowellForecastError" []
		ComputePowellForecastError
			ComputeInitialPowellForecastError
				GetPreviousForecastError
				RegressionCoefficient
				SetRandomDeviation
					GetRandSeed
	ASSIGN TO $ "PowellForecastData.forecastError" []
		ComputePowellForecastError
			ComputeInitialPowellForecastError
				GetPreviousForecastError
				RegressionCoefficient
				SetRandomDeviation
					GetRandSeed
	ASSIGN TO $ "PowellForecastData.random" []
		SetRandomDeviation
			GetRandSeed
			
### 4 - Powell Runoff Forecast
	ASSIGN TO $ "PowellForecastData.Natural Inflow with Error" []
	ASSIGN TO $ "PowellForecastData.Reg Inflow with Error" []
		UBDepletionsRange
		UBEffectiveStorage
			AvailableSpace
				LiveCapacity
	ASSIGN TO $ "PowellForecastData.Unreg Inflow with Error" []
		UBDepletionsRange
		
### 5 - EOWYStorageForecasts
	ASSIGN TO $ "EqualizationData.ForecastEOWYSMead" []
		EOWYStorage
			EstimateBankStorage
				EstimateEvaporation
					SumEvapCoeff
				LiveCapacity
			EstimateEvaporation
				SumEvapCoeff
			InactiveCapacity
			InitialEOWYStorageMead
				PreviousStorage
			InitialEOWYStoragePowell
				ForecastPowellInflow
					PowellRegulatedInflowVolume
						PowellNaturalInflowVolume
						UBDepletionsRange
						UBRegulation
							UBDrawDown
								UBInitialStorage
									UBTargetStorage
								UBTargetStorage
				PreviousStorage
			LiveCapacity
			PreviousStorage
		ForecastMeadRelease
			ComputeEqualizationEvaporation
				EstimateEvaporation
					SumEvapCoeff
				RuleCurveStorage
			EqualizationRuleCurveStorage
				PreviousStorage
				RuleCurveStorage
			SumDemandsDownstreamofMead
				SumDepletionsBelowVolume
				TotalOtherPriority4ActualUse
				TotalPriority2and3ActualUse
			SumInflowBelowMead
		ForecastPowellRelease
			ForecastPowellFallRangeOutflowVolume
				MaxReleaseVolume
					MaxRelease
				MinReleaseVolume
					MinRelease
				PowellFallOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					PowellRegulatedInflowVolume
						PowellNaturalInflowVolume
						UBDepletionsRange
						UBRegulation
							UBDrawDown
								UBInitialStorage
									UBTargetStorage
								UBTargetStorage
					UBTargetStorage
				PowellMinObjRelVolumeFall
					ComputeScheduledMinObjRelRemainingThruSept
					PowellMinObjRelVolRemaining
						AnnualMinObjectiveRelease
						ReleaseMade
							PowellFallRelease
				PowellSumReleaseWeights
			ForecastPowellJulyStorage
				Estimate Bank Storage without Evap
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				LiveCapacity
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
				UBTargetStorage
			PowellSpringOutflowVolConstrained
				MaxReleaseVolume
					MaxRelease
				MinReleaseVolume
					MinRelease
				PowellMinObjRelVolumeSpring
					ComputeScheduledMinObjRelRemainingThruSept
					PowellMinObjRelVolRemaining
						AnnualMinObjectiveRelease
						ReleaseMade
							PowellFallRelease
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
	ASSIGN TO $ "EqualizationData.ForecastEOWYSPowell" []
		EOWYStorage
			EstimateBankStorage
				EstimateEvaporation
					SumEvapCoeff
				LiveCapacity
			EstimateEvaporation
				SumEvapCoeff
			InactiveCapacity
			InitialEOWYStorageMead
				PreviousStorage
			InitialEOWYStoragePowell
				ForecastPowellInflow
					PowellRegulatedInflowVolume
						PowellNaturalInflowVolume
						UBDepletionsRange
						UBRegulation
							UBDrawDown
								UBInitialStorage
									UBTargetStorage
								UBTargetStorage
				PreviousStorage
			LiveCapacity
			PreviousStorage
		ForecastMeadRelease
			ComputeEqualizationEvaporation
				EstimateEvaporation
					SumEvapCoeff
				RuleCurveStorage
			EqualizationRuleCurveStorage
				PreviousStorage
				RuleCurveStorage
			SumDemandsDownstreamofMead
				SumDepletionsBelowVolume
				TotalOtherPriority4ActualUse
				TotalPriority2and3ActualUse
			SumInflowBelowMead
		ForecastPowellRelease
			ForecastPowellFallRangeOutflowVolume
				MaxReleaseVolume
					MaxRelease
				MinReleaseVolume
					MinRelease
				PowellFallOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					PowellRegulatedInflowVolume
						PowellNaturalInflowVolume
						UBDepletionsRange
						UBRegulation
							UBDrawDown
								UBInitialStorage
									UBTargetStorage
								UBTargetStorage
					UBTargetStorage
				PowellMinObjRelVolumeFall
					ComputeScheduledMinObjRelRemainingThruSept
					PowellMinObjRelVolRemaining
						AnnualMinObjectiveRelease
						ReleaseMade
							PowellFallRelease
				PowellSumReleaseWeights
			ForecastPowellJulyStorage
				Estimate Bank Storage without Evap
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				LiveCapacity
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
				UBTargetStorage
			PowellSpringOutflowVolConstrained
				MaxReleaseVolume
					MaxRelease
				MinReleaseVolume
					MinRelease
				PowellMinObjRelVolumeSpring
					ComputeScheduledMinObjRelRemainingThruSept
					PowellMinObjRelVolRemaining
						AnnualMinObjectiveRelease
						ReleaseMade
							PowellFallRelease
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
					
### 6 - Estimate Upper Basin Storage
	ASSIGN TO $ "EqualizationData.AvailableWater" []
		SumUpperBasinStorage
			PreviousStorage
			
### 7 - Sum Powell Inflow Spring
	FOREACH date IN @"24:00:00 January 31, Current Year" TO @"24:00:00 July 31, Current Year"
		ASSIGN TO $ "PowellForecastData.Powell Inflow Volume" [date]
		
### 8 - Sum Powell Inflow Fall
	FOREACH date IN @"24:00:00 August 31, Current Year" TO @"24:00:00 December 31, Current Year"
		ASSIGN TO $ "PowellForecastData.Powell Inflow Volume" [date]
		
### 9 - Powell Spike Flow Rule
	ASSIGN TO $ "Powell Spike Flow Data.Spike Flow Flag" []
		CheckSpikeFlowCriteria
			PowellSpringOutflowAverage
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
	ASSIGN TO $ "Powell.Bypass" []
		CheckSpikeFlowCriteria
			PowellSpringOutflowAverage
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
	ASSIGN TO $ "Powell.Storage" []
		CheckSpikeFlowCriteria
			PowellSpringOutflowAverage
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
		PowellMinSpikeFlowOutflow
	SpikeAlreadyMade
	
### 10 - Equalization Post Guidelines
	ASSIGN TO "Powell.Storage" []
		602aStorageValue
		CheckEqualizationRelease602a
			602aStorageValue
		ComputeEqualizationReleaseList
			ComputeNewPowellRelease
				CheckEqualizationRelease
					CheckERMeadExclusiveFCS
						LiveCapacity
					EstimateEqualizationRelease
					GetListElement
				TotalPowellRelease
					EstimateEqualizationRelease
					GetListElement
			EOWYStorage
				EstimateBankStorage
					EstimateEvaporation
						SumEvapCoeff
					LiveCapacity
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				InitialEOWYStorageMead
					PreviousStorage
				InitialEOWYStoragePowell
					ForecastPowellInflow
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
					PreviousStorage
				LiveCapacity
				PreviousStorage
			ForecastMeadRelease
				ComputeEqualizationEvaporation
					EstimateEvaporation
						SumEvapCoeff
					RuleCurveStorage
				EqualizationRuleCurveStorage
					PreviousStorage
					RuleCurveStorage
				SumDemandsDownstreamofMead
					SumDepletionsBelowVolume
					TotalOtherPriority4ActualUse
					TotalPriority2and3ActualUse
				SumInflowBelowMead
			GetListElement
			InitialEqualizationReleaseList
				ForecastPowellRelease
					ForecastPowellFallRangeOutflowVolume
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellFallOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							PowellRegulatedInflowVolume
								PowellNaturalInflowVolume
								UBDepletionsRange
								UBRegulation
									UBDrawDown
										UBInitialStorage
											UBTargetStorage
										UBTargetStorage
							UBTargetStorage
						PowellMinObjRelVolumeFall
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSumReleaseWeights
					ForecastPowellJulyStorage
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						InactiveCapacity
						LiveCapacity
						PowellSpringOutflowVolConstrained
							MaxReleaseVolume
								MaxRelease
							MinReleaseVolume
								MinRelease
							PowellMinObjRelVolumeSpring
								ComputeScheduledMinObjRelRemainingThruSept
								PowellMinObjRelVolRemaining
									AnnualMinObjectiveRelease
									ReleaseMade
										PowellFallRelease
							PowellSpringOutflowVolume
								Estimate Bank Storage without Evap
								EstimateEvaporation
									SumEvapCoeff
								UBTargetStorage
						UBTargetStorage
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
			TotalPowellRelease
				EstimateEqualizationRelease
				GetListElement
		ComputePowellRelease
			MinRelease
			NumberMonthsRemaining
		ConvertPowellRelease
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
		WaterAvailableInUpperBasin
	ASSIGN TO $ "EqualizationData.EqualFlag" []
		602aStorageValue
		WaterAvailableInUpperBasin
		
### 11 - Equalization Tier
	ASSIGN TO "Powell.Storage" []
		CheckEqualizationRelease_Mead1105
		ComputeEqualizationReleaseList
			ComputeNewPowellRelease
				CheckEqualizationRelease
					CheckERMeadExclusiveFCS
						LiveCapacity
					EstimateEqualizationRelease
					GetListElement
				TotalPowellRelease
					EstimateEqualizationRelease
					GetListElement
			EOWYStorage
				EstimateBankStorage
					EstimateEvaporation
						SumEvapCoeff
					LiveCapacity
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				InitialEOWYStorageMead
					PreviousStorage
				InitialEOWYStoragePowell
					ForecastPowellInflow
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
					PreviousStorage
				LiveCapacity
				PreviousStorage
			ForecastMeadRelease
				ComputeEqualizationEvaporation
					EstimateEvaporation
						SumEvapCoeff
					RuleCurveStorage
				EqualizationRuleCurveStorage
					PreviousStorage
					RuleCurveStorage
				SumDemandsDownstreamofMead
					SumDepletionsBelowVolume
					TotalOtherPriority4ActualUse
					TotalPriority2and3ActualUse
				SumInflowBelowMead
			GetListElement
			InitialEqualizationReleaseList
				ForecastPowellRelease
					ForecastPowellFallRangeOutflowVolume
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellFallOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							PowellRegulatedInflowVolume
								PowellNaturalInflowVolume
								UBDepletionsRange
								UBRegulation
									UBDrawDown
										UBInitialStorage
											UBTargetStorage
										UBTargetStorage
							UBTargetStorage
						PowellMinObjRelVolumeFall
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSumReleaseWeights
					ForecastPowellJulyStorage
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						InactiveCapacity
						LiveCapacity
						PowellSpringOutflowVolConstrained
							MaxReleaseVolume
								MaxRelease
							MinReleaseVolume
								MinRelease
							PowellMinObjRelVolumeSpring
								ComputeScheduledMinObjRelRemainingThruSept
								PowellMinObjRelVolRemaining
									AnnualMinObjectiveRelease
									ReleaseMade
										PowellFallRelease
							PowellSpringOutflowVolume
								Estimate Bank Storage without Evap
								EstimateEvaporation
									SumEvapCoeff
								UBTargetStorage
						UBTargetStorage
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
			TotalPowellRelease
				EstimateEqualizationRelease
				GetListElement
		ComputePowellRelease
			MinRelease
			NumberMonthsRemaining
		ConvertPowellRelease
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
	ASSIGN TO $ "EqualizationData.EqualBasinStatesFlag" []
	
### 12 - Upper Elevation Balancing Tier Jan thru March
	ASSIGN TO $ "Coordinated Operation.UpperLevelBalancingFlag" []
		InUpperElevationBalancingTier
	ASSIGN TO $ "Powell.Storage" []
		ComputeEqualizationReleaseList
			ComputeNewPowellRelease
				CheckEqualizationRelease
					CheckERMeadExclusiveFCS
						LiveCapacity
					EstimateEqualizationRelease
					GetListElement
				TotalPowellRelease
					EstimateEqualizationRelease
					GetListElement
			EOWYStorage
				EstimateBankStorage
					EstimateEvaporation
						SumEvapCoeff
					LiveCapacity
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				InitialEOWYStorageMead
					PreviousStorage
				InitialEOWYStoragePowell
					ForecastPowellInflow
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
					PreviousStorage
				LiveCapacity
				PreviousStorage
			ForecastMeadRelease
				ComputeEqualizationEvaporation
					EstimateEvaporation
						SumEvapCoeff
					RuleCurveStorage
				EqualizationRuleCurveStorage
					PreviousStorage
					RuleCurveStorage
				SumDemandsDownstreamofMead
					SumDepletionsBelowVolume
					TotalOtherPriority4ActualUse
					TotalPriority2and3ActualUse
				SumInflowBelowMead
			GetListElement
			InitialEqualizationReleaseList
				ForecastPowellRelease
					ForecastPowellFallRangeOutflowVolume
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellFallOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							PowellRegulatedInflowVolume
								PowellNaturalInflowVolume
								UBDepletionsRange
								UBRegulation
									UBDrawDown
										UBInitialStorage
											UBTargetStorage
										UBTargetStorage
							UBTargetStorage
						PowellMinObjRelVolumeFall
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSumReleaseWeights
					ForecastPowellJulyStorage
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						InactiveCapacity
						LiveCapacity
						PowellSpringOutflowVolConstrained
							MaxReleaseVolume
								MaxRelease
							MinReleaseVolume
								MinRelease
							PowellMinObjRelVolumeSpring
								ComputeScheduledMinObjRelRemainingThruSept
								PowellMinObjRelVolRemaining
									AnnualMinObjectiveRelease
									ReleaseMade
										PowellFallRelease
							PowellSpringOutflowVolume
								Estimate Bank Storage without Evap
								EstimateEvaporation
									SumEvapCoeff
								UBTargetStorage
						UBTargetStorage
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
			TotalPowellRelease
				EstimateEqualizationRelease
				GetListElement
		ComputePowellReleaseUpperBalancing
			NumberMonthsRemaining
			PowellReducedRelforCurrentMonth
				ComputeReducedReleaseRemaining
					AnnualReducedRelease
				MinRelease
				PowellReducedRelVolRemaining
					AnnualReducedRelease
					ReleaseMade
						PowellFallRelease
		ConvertPowellReleaseBalancing
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		InUpperElevationBalancingTier
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
			
### 13 - Upper Elevation Balancing Tier April thru Sept
	ASSIGN TO $ "Coordinated Operation.EQTrumpUpperLevelBalancing1105GovFlag" []
		EqualizationConditionsMet
		InUpperElevationBalancingTier
	ASSIGN TO $ "Coordinated Operation.EQTrumpUpperLevelBalancingFlag" []
		EqualizationConditionsMet
		InUpperElevationBalancingTier
	ASSIGN TO $ "Coordinated Operation.UpperLevelBalancingFlag" []
		EqualizationConditionsMet
		InUpperElevationBalancingTier
	ASSIGN TO $ "Powell.Storage" []
		CheckEqualizationRelease_Mead1105
		ComputeEqualizationReleaseList
			ComputeNewPowellRelease
				CheckEqualizationRelease
					CheckERMeadExclusiveFCS
						LiveCapacity
					EstimateEqualizationRelease
					GetListElement
				TotalPowellRelease
					EstimateEqualizationRelease
					GetListElement
			EOWYStorage
				EstimateBankStorage
					EstimateEvaporation
						SumEvapCoeff
					LiveCapacity
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				InitialEOWYStorageMead
					PreviousStorage
				InitialEOWYStoragePowell
					ForecastPowellInflow
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
					PreviousStorage
				LiveCapacity
				PreviousStorage
			ForecastMeadRelease
				ComputeEqualizationEvaporation
					EstimateEvaporation
						SumEvapCoeff
					RuleCurveStorage
				EqualizationRuleCurveStorage
					PreviousStorage
					RuleCurveStorage
				SumDemandsDownstreamofMead
					SumDepletionsBelowVolume
					TotalOtherPriority4ActualUse
					TotalPriority2and3ActualUse
				SumInflowBelowMead
			GetListElement
			InitialEqualizationReleaseList
				ForecastPowellRelease
					ForecastPowellFallRangeOutflowVolume
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellFallOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							PowellRegulatedInflowVolume
								PowellNaturalInflowVolume
								UBDepletionsRange
								UBRegulation
									UBDrawDown
										UBInitialStorage
											UBTargetStorage
										UBTargetStorage
							UBTargetStorage
						PowellMinObjRelVolumeFall
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSumReleaseWeights
					ForecastPowellJulyStorage
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						InactiveCapacity
						LiveCapacity
						PowellSpringOutflowVolConstrained
							MaxReleaseVolume
								MaxRelease
							MinReleaseVolume
								MinRelease
							PowellMinObjRelVolumeSpring
								ComputeScheduledMinObjRelRemainingThruSept
								PowellMinObjRelVolRemaining
									AnnualMinObjectiveRelease
									ReleaseMade
										PowellFallRelease
							PowellSpringOutflowVolume
								Estimate Bank Storage without Evap
								EstimateEvaporation
									SumEvapCoeff
								UBTargetStorage
						UBTargetStorage
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
			TotalPowellRelease
				EstimateEqualizationRelease
				GetListElement
		ComputePowellRelease
			MinRelease
			NumberMonthsRemaining
		ComputePowellReleaseUpperBalancing
			NumberMonthsRemaining
			PowellReducedRelforCurrentMonth
				ComputeReducedReleaseRemaining
					AnnualReducedRelease
				MinRelease
				PowellReducedRelVolRemaining
					AnnualReducedRelease
					ReleaseMade
						PowellFallRelease
		ConvertPowellRelease
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		ConvertPowellReleaseBalancing
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		EqualizationConditionsMet
		InUpperElevationBalancingTier
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
### 14 - Mid Elevation Release Tier
	ASSIGN TO $ "Coordinated Operation.ReducedReleaseFlag" []
		InMidElevationReleaseTier
	ASSIGN TO $ "Powell.Storage" []
		InMidElevationReleaseTier
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
		PowellReducedRelforCurrentMonth
			ComputeReducedReleaseRemaining
				AnnualReducedRelease
			MinRelease
			PowellReducedRelVolRemaining
				AnnualReducedRelease
				ReleaseMade
					PowellFallRelease
					
### 15 - Lower Elevation Balancing Tier
	ASSIGN TO $ "Coordinated Operation.LowerLevelBalancingFlag" []
		InLowerElevationBalancingTier
	ASSIGN TO $ "Powell.Storage" []
		ComputeEqualizationReleaseList
			ComputeNewPowellRelease
				CheckEqualizationRelease
					CheckERMeadExclusiveFCS
						LiveCapacity
					EstimateEqualizationRelease
					GetListElement
				TotalPowellRelease
					EstimateEqualizationRelease
					GetListElement
			EOWYStorage
				EstimateBankStorage
					EstimateEvaporation
						SumEvapCoeff
					LiveCapacity
				EstimateEvaporation
					SumEvapCoeff
				InactiveCapacity
				InitialEOWYStorageMead
					PreviousStorage
				InitialEOWYStoragePowell
					ForecastPowellInflow
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
					PreviousStorage
				LiveCapacity
				PreviousStorage
			ForecastMeadRelease
				ComputeEqualizationEvaporation
					EstimateEvaporation
						SumEvapCoeff
					RuleCurveStorage
				EqualizationRuleCurveStorage
					PreviousStorage
					RuleCurveStorage
				SumDemandsDownstreamofMead
					SumDepletionsBelowVolume
					TotalOtherPriority4ActualUse
					TotalPriority2and3ActualUse
				SumInflowBelowMead
			GetListElement
			InitialEqualizationReleaseList
				ForecastPowellRelease
					ForecastPowellFallRangeOutflowVolume
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellFallOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							PowellRegulatedInflowVolume
								PowellNaturalInflowVolume
								UBDepletionsRange
								UBRegulation
									UBDrawDown
										UBInitialStorage
											UBTargetStorage
										UBTargetStorage
							UBTargetStorage
						PowellMinObjRelVolumeFall
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSumReleaseWeights
					ForecastPowellJulyStorage
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						InactiveCapacity
						LiveCapacity
						PowellSpringOutflowVolConstrained
							MaxReleaseVolume
								MaxRelease
							MinReleaseVolume
								MinRelease
							PowellMinObjRelVolumeSpring
								ComputeScheduledMinObjRelRemainingThruSept
								PowellMinObjRelVolRemaining
									AnnualMinObjectiveRelease
									ReleaseMade
										PowellFallRelease
							PowellSpringOutflowVolume
								Estimate Bank Storage without Evap
								EstimateEvaporation
									SumEvapCoeff
								UBTargetStorage
						UBTargetStorage
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
			TotalPowellRelease
				EstimateEqualizationRelease
				GetListElement
		ComputePowellReleaseLowerBalancing
			NumberMonthsRemaining
			PowellReducedRelforCurrentMonth
				ComputeReducedReleaseRemaining
					AnnualReducedRelease
				MinRelease
				PowellReducedRelVolRemaining
					AnnualReducedRelease
					ReleaseMade
						PowellFallRelease
		ConvertPowellReleaseBalancing
			ForecastPowellRelease
				ForecastPowellFallRangeOutflowVolume
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellFallOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						PowellRegulatedInflowVolume
							PowellNaturalInflowVolume
							UBDepletionsRange
							UBRegulation
								UBDrawDown
									UBInitialStorage
										UBTargetStorage
									UBTargetStorage
						UBTargetStorage
					PowellMinObjRelVolumeFall
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSumReleaseWeights
				ForecastPowellJulyStorage
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					InactiveCapacity
					LiveCapacity
					PowellSpringOutflowVolConstrained
						MaxReleaseVolume
							MaxRelease
						MinReleaseVolume
							MinRelease
						PowellMinObjRelVolumeSpring
							ComputeScheduledMinObjRelRemainingThruSept
							PowellMinObjRelVolRemaining
								AnnualMinObjectiveRelease
								ReleaseMade
									PowellFallRelease
						PowellSpringOutflowVolume
							Estimate Bank Storage without Evap
							EstimateEvaporation
								SumEvapCoeff
							UBTargetStorage
					UBTargetStorage
				PowellSpringOutflowVolConstrained
					MaxReleaseVolume
						MaxRelease
					MinReleaseVolume
						MinRelease
					PowellMinObjRelVolumeSpring
						ComputeScheduledMinObjRelRemainingThruSept
						PowellMinObjRelVolRemaining
							AnnualMinObjectiveRelease
							ReleaseMade
								PowellFallRelease
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
			GetListElement
		InLowerElevationBalancingTier
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
			
### 16 - Check Bypass Capacity
	ASSIGN TO $ "EqualizationData.BypassCapFlag" []
		BypassCapacity
		InactiveCapacity
	ASSIGN TO $ "Powell.Storage" []
		BypassCapacity
		InactiveCapacity
		IterateBypassCapacity
			BypassCapacity
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage

### 17 - Powell Limit Outflow Rule
	ASSIGN TO $ "Powell.Storage" []
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
			
### 18 - Powell Smooth July Operation Rule
	ASSIGN TO $ "Powell.Storage" []
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
			
### 19 - Meet Powell Min Objective Release
	ASSIGN TO "Powell.Storage" []
		PowellComputeStorageAtGivenOutflow
			LiveCapacity
			PreviousStorage
		PowellMinObjRelforCurrentMonth
			ComputeMinObjReleaseRemaining
				AnnualMinObjectiveRelease
			MinRelease
			PowellMinObjRelVolRemaining
				AnnualMinObjectiveRelease
				ReleaseMade
					PowellFallRelease
	ASSIGN TO $ "EqualizationData.MinObjRelFlag" []
		PowellMinObjRelforCurrentMonth
			ComputeMinObjReleaseRemaining
				AnnualMinObjectiveRelease
			MinRelease
			PowellMinObjRelVolRemaining
				AnnualMinObjectiveRelease
				ReleaseMade
					PowellFallRelease
					
### 20 - Powell Operations Rule
	ASSIGN TO $ "Powell.Storage" []
		PowellFallSeasonStorage
			InactiveCapacity
			LiveCapacity
			PowellComputeFallSeasonRelease
				MaxRelease
				MinRelease
				PowellFallOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					PowellRegulatedInflowVolume
						PowellNaturalInflowVolume
						UBDepletionsRange
						UBRegulation
							UBDrawDown
								UBInitialStorage
									UBTargetStorage
								UBTargetStorage
					UBTargetStorage
				PowellReleaseFraction
			PowellComputeStorageAtGivenOutflow
				LiveCapacity
				PreviousStorage
		PowellRunoffSeasonStorage
			InactiveCapacity
			LiveCapacity
			PowellComputeRunoffSeasonRelease
				MaxRelease
				MinRelease
				PowellReleaseFraction
				PowellSpringOutflowAverage
					PowellSpringOutflowVolume
						Estimate Bank Storage without Evap
						EstimateEvaporation
							SumEvapCoeff
						UBTargetStorage
				PowellSpringOutflowVolume
					Estimate Bank Storage without Evap
					EstimateEvaporation
						SumEvapCoeff
					UBTargetStorage
			PowellComputeStorageAtGivenOutflow
				LiveCapacity
				PreviousStorage

