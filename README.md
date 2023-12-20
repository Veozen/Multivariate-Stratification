# Multivariate-Stratification

Implements multivariate stratification in SAS.
The method divides the domain in squares/cubes.. sections and assign a strata to each. All elements within a section gets assigned the same strata.
For a given strata assignement, sample allocation is performed. The allocation measures the fitness of a given strata assignement.

The method then attempt to change the strata assignement of each section in turn until it finds one that yields an improved allocation. This new assignement is kept and the process is repeated until no improvement can be found. 

This stratification may be coarse so the method then subdivides each section into smaller subsection which inherit the strata assignement of the section from which they derive. The method is then reapplied on the set of smaller subsections to find refine the stratification.
The subdivision process is repeated until no significant changes are found.

In essence, the strata assignement applies a local search method where each decision variable is a section of domain that can take a finite number of discrete values(strata). The iterative subdivision process is used to provide a good starting point to the search applied on the larger number of decision variable at the next subdivision step.

```SAS
%macro MultiVarStrat(Pop= , Nstrat= , stratVar= ,  Objective= Size, Exp=1, SampleSize=, tol=0.01, DataOut=_stratOut );
Pop
Nstrat
stratVar
Objective
Exp
SampleSize
tol
DataOut

```
