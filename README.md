# droptest

An R package for simulating LOX/GOX drop testing.

### Installation:

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("chadr/droptest")
```

### Getting Started:

### Examples and documentation

### Background information:

Drop testing -- sometimes called impact testing -- is used to evaluate if a
material will interact with liquid (LOX) or gaseous oxygen (GOX). The material
is exposed to the LOX/GOX and an impactor is dropped onto the sample. Each drop
is a bernoulli trial where a reaction is a failure and a non-reaction is a
success. The specified number of trials -- until failure -- produces one test
run.

Drop testing -- performed by the military and NASA -- yields results that are
difficult to analyze. Numerous tech briefs and standards have attempted to
address the problem (see below for more information). While fundamentally a
binomial process, drop testing almost always produces truncated data. Testing
stops as soon as a failure condition is reached. If the failure condition occurs
on drop one or two -- depending on the failure criteria -- then the test returns
only one or two result values. Alternatively, if a material passes, or if the
failure condition occurs on the last observation, then the test returns as many
result values as observations.

Simulation can be used to examine the behavior of this truncated test procedure.

Inspired by NASA Technical Note NASA-TN-D-7663 (1974):
https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19750004618.pdf

### Applicable Standards

Pass/Fail criteria and number of observations required have been defined in the
following standards:

* NASA-STD-6001B
(https://spaceflightsystems.grc.nasa.gov/SpaceDOC_II/Standards/documents/NASA-STD-6001B-1.pdf)
* ASTM D2512 (https://www.astm.org/Standards/D2512.htm)
* ASTM G86-17 (https://www.astm.org/Standards/G86.htm)

**Note:** This package is not constrained by any standard. Arbitrary test 
criteria and observations can be specified for maximum flexibility.

### For more information on drop testing: 
* "An Instrument for Determination of Impact Sensitivity of Materials in Contact with 
Liquid Oxygen" (AB6002-EB). 1960. 
https://www.astm.org/DIGITAL_LIBRARY/STP/MMR/PAGES/AB6002-EB.htm

* "Lox/Gox Mechanical Impact Tester Assessment" (TM-74106). 1980.
https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19800006920.pdf