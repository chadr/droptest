# droptest

Simulates LOX/GOX drop testing.

Background information:

Drop testing -- performed by the military and NASA -- yields results
that are difficult to analyze. Numerous tech briefs and standards have
attempted to address the problem. While fundamentally a binomial 
process, drop testing almost always produces truncated data. Testing
stops as soon as a failure condition is reached. If the failure
condition occurs on drop one or two -- depending on the failure 
criteria -- then the test returns only one or two result values.
Alternatively, if the failure condition occurs on the last observation,
then the test returns as many result values as observations.

Drop testing is used to evaluate if a material will interact with liquid
(LOX) or gaseous oxygen (GOX). The material is exposed to the LOX or GOX
and an impactor is dropped onto the sample. Each drop is a bernoulli
trial where a reaction is a failure and a non-reaction is a success. The
specified number of trials -- until failure -- produces one test run.

Simulation can be used to examine the behavior of this truncated test
procedure.




For more information on drop testing: 