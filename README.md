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
Pop : File
	Must variables names in StratVar parameter
Nstrat
	Number of strata
stratVar  
	List of variable names found in Pop file that are to by used for stratification
Objective  
	Objective function must be either Variance or CV;
Exp
SampleSize
	total sample size
tol
DataOut
	&stratVar List of stratification variables
	StratID Stratum Identifier for that combination of stratification variable's value

```
# Usage  

```SAS
%macro Pop(npop,nvar,Nstrat);
	%local i j;

	data Population;
		do i = 1 to &npop;
			%do j = 1 %to &nvar;
				*var&j = rand('uniform')*10;
				var&j = rand('lognormal')*10;
				*var&j = i + rand("normal");
			%end;
			output;
		end;
	run;
%mend;


%Pop(1000,2,3);

/*Bivariate Stratification*/
%MultiVarStrat(	Pop=population , 
		Nstrat=3,
		sampleSize=300, 
		stratVar=var1 var2,
		Objective=variance,  
		tol=0.1, 
		DataOut=Stratified);
ods html;
proc sort data=Stratified;
	by stratId;
run;
goptions colors=(blue red green);
goptions colors=(blue red cyan);
symbol1 value= dot;
proc gplot data=Stratified;
	plot var1*var2=stratId;
run;
quit;
```
