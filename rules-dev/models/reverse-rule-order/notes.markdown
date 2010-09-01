#Reversing rule order in Expanded 24 Month Study: Documentation

- Change order of all utility groups 

__FROM__:

- _Data Setup_
- __Shortage__
- __LC WaterUse Data Setup__
- __FloodControl__
- __Set Future Uses - SNWP, CAP, MWD__
- __Reset SNWP Forecast Use__
- __Reset CAP Forecast Use__
- __Reset MWD Forecast Use__
- __24 Month__
- __Calculate Final State Use and Overruns__

__TO:__

- __Calculate Final State Use and Overruns__
- __FloodControl__
- __Set Future Uses - SNWP, CAP, MWD__
- __24 Month__
- __Reset MWD Forecast Use__
- __Reset CAP Forecast Use__
- __Reset SNWP Forecast Use__
- __LC WaterUse Data Setup__
- __Shortage__
- __Data Setup__

__Steps to Reproduce__

- Reverse rules in __LC WaterUse Data Setup__
- Reverse rules in __Calculate Final State Use and Overruns__
- Reverse rules in __Shortage__
- Reverse rules in __Data Setup__
- reverse rules in __24 Month__
- reverse order of second 5 rules in __Set Future Uses - SNWP, CAP, MWD__
- Moved `Compute Hoover Capacity` from _24 Month_ utility set to _FloodControl_ utility group before `Mojave Rule Curve`  Before this was changed, the Mojave rule curve had a higher priority and Mead.Power was set by this rule and not by `Compute Hoover Capacity` (as was intended). 
- Moved `Set Normal Schedules MWD & SNWP` and `Set Normal Schedules CAP` from the `Set Future Uses - SNWP, CAP, MWD` utility group to the highest priority in `LC WaterUse Data Setup`.

At this point all slots of both models match.  Differences exist in which rules set diversions, but this does not affect the output. For example, in the reverse (...,3,2,1) order, `CAPDiversion.Total Depletion` is set by `Havasu Rule Curve`; In the original order (1,2,3,...), the rule `Set Below Imp Outflow` sets this slot (and nearly every other divesion slot). This is simply a difference in the order that the objects solve. 