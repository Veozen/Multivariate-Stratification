/*Partition d une population en morceau de taille(distance) identiques */

%macro Nobs(dataIn);
/*Returns the number of observations in a dataset*/
	%local dataid nobs rc;
	%let dataid=%sysfunc(open(&dataIn));
	%let nobs=%sysfunc(attrn(&dataid,nobs));
	%let rc=%sysfunc(close(&dataid));
	&nobs 
%mend Nobs;

%macro NVar(dataIn);
/*Returns the number of variables in a dataset*/
	%local dataId nobs rc;
	%let dataid=%sysfunc(open(&dataIn));
	%let nobs=%sysfunc(attrn(&dataid,nvar));
	%let rc=%sysfunc(close(&dataid));
	&nobs 
%mend NVar;

%macro Sum(data,Var);
/* yields the sum of a dataset s variable*/
	%local i dataid varnum nobs sum rc;
	%let dataid=%sysfunc(open(&data));
	%let varnum=%sysfunc(varnum(&dataId,&var));
	%let nobs = %sysfunc(attrn(&dataId,nobs));
	%let sum=0;
	%do i = 1 %to &nobs;
		%let rc= %sysfunc(fetch(&dataId));
		%let var = %sysfunc(getvarN(&dataId,&varnum));
		%let sum= %sysevalf(&sum+&var);
	%end;
	%let rc = %sysfunc(close(&dataId));
	&sum
%mend Sum;

%macro saveOptions();
	/*save some common options*/
	%local notes mprint symbolgen source options;
	%let notes = %sysfunc(getoption(Notes));
	%let mprint = %sysfunc(getoption(mprint));
	%let symbolgen = %sysfunc(getoption(Symbolgen));
	%let source = %sysfunc(getoption(source));

	%let options = &notes &mprint &symbolgen &source;
	&options;
%mend saveOptions;

%macro Time(from);
/*returns the current time  or if input provided: 
returns the elaspsed time from the input time */
	%local dataTime now time;
	%let datetime = %sysfunc( datetime() );
	%let now=%sysfunc( timepart(&datetime) );

	%if (&from ne ) %then %do;
		%let timefrom = %sysfunc(inputn(&from,time9.));
		%if %sysevalf(&now<&timefrom) %then %do;
			%let time =  %sysevalf(86400-&timefrom,ceil);
			%let time = %sysevalf(&time + %sysevalf(&now,ceil));
		%end;
		%else %do;
			%let time = %sysevalf(&now-&timefrom,ceil);
		%end;
		%let time = %sysfunc(putn(&time,time9.));
	%end;
	%else %do;
		%let time = &now;
		%let time = %sysfunc(putn(&time,time9.));
	%end;
	&time
%mend Time;

%macro ListLen(list,sep=%str( ));
/*returns the length of a list*/
	%local count;
	%let count= %sysfunc(countw(&list,&sep));
	&count
%mend ListLen;

%macro VarNames(Data);
	/*Generates the complete list of column names in a SAS dataset */
	%local VarList CurrentVar DSID NumVars;
	%let DSID = %sysfunc(open(&Data));
	%let NumVars = %sysfunc(attrn(&DSID, nvars));

	/* loop through all variables and get their names */
	%let CurrentVar = 1;
	%do %while(&CurrentVar <= &NumVars);
		%let VarList = &VarList %sysfunc(varname(&DSID, &CurrentVar));
		/* append current variable's name to output list */
		%let CurrentVar = %eval(&CurrentVar + 1);
	%end;

	%let DSID = %sysfunc(close(&DSID));
	&VarList 
%mend VarNames;

%macro BuildPart(Pop=, var=, div=, PopOut=, partOut=);
	/*option nonotes nomprint nosource;*/
	%local i j nvar PartOutMissing curvar;
	%let nvar = %listLen(&var);
	%let PartOutMissing=No;
	%do i=1 %to &nvar; 
		%local min%scan(&var,&i)  max%scan(&var,&i); 
	%end; 
	%if (&partOut eq ) %then %do;
		%let partOut = _part;
		%let PartOutMissing=Yes;
	%end;

	/*get max */
	proc means data = &Pop noprint;
		var &var;
		output out=_outmean max= min= / autoname;
	run;

	data _null_;
		set _outmean;
		%do i = 1 %to &nvar;
			call symputx("min%scan(&var,&i)",%scan(&var,&i)_min);
			call symputx("max%scan(&var,&i)",%scan(&var,&i)_max);
		%end;
	run;
	proc delete data=_outmean;run;

	/*compute step*/
	%do i = 1 %to &nvar;
		%let curvar= %scan(&var,&i);
		%local step&curvar;
		%let step&curvar=%sysevalf((&&max&curvar - &&min&curvar) / &div);
	%end;

	/*liste des points diagonaux*/
	data _diag;
		%do j= 1 %to &div;
			%do i = 1 %to &nvar;
				%let curvar = %scan(&var,&i);
				%if &j = &div %then %do;
					&curvar = &&max&curvar + 1e-10;
				%end;
				%else %do;
					&curvar = &&min&curvar + &&step&curvar * &j;
				%end;
			%end;
			g=&j;
			output;
		%end;	
	run;

	/*produit cartésien pour obtenir toutes les zones*/
	proc sql;
		create table &partOut as
		select 	%do i = 1 %to %eval(&nvar-1);
					%scan(&var,&i).%scan(&var,&i),
					/*%scan(&var,&i).g as %scan(&var,&i)_Coord,*/
				%end;
					%scan(&var,&nvar).%scan(&var,&nvar)/*,
					%scan(&var,&nvar).g as %scan(&var,&nvar)_Coord*/
		from 	%do i = 1 %to %eval(&nvar-1);
					_diag as %scan(&var,&i),
				%end;
				_diag as %scan(&var,&nvar);
	quit;

	/*calcul des min et max par zones*/
	data &partOut ;
		set &partOut;	
		zoneId=_N_;
		%do i=1 %to &nvar;
			%let curvar= %scan(&var,&i);
			&curvar._low= &curvar- &&step&curvar;
			if &curvar._low < &&min&curvar + 0.0000001 then do;
				&curvar._low = &&min&curvar - 0.0000000001;
			end;
			&curvar._up= &curvar;
		%end;
		drop %do i=1%to &nvar;
			%scan(&var,&i)
		%end;
		;
	run;


	/*Partition de la population*/
	/*%if (&popOut ne ) %then %do;
		proc sql;
		 create table &PopOut as
		 select p.*, t.*
		 from &pop as p , &partOut as t 
		 where 
			%do i = 1 %to %eval(&nvar-1);
				%let curvar = %scan(&var,&i);
				(p.&curvar >= t.&curvar._low) and (p.&curvar < t.&curvar._up) and
			%end;
			%let curvar = %scan(&var,&nvar);
			(p.&curvar >= t.&curvar._low) and (p.&curvar < t.&curvar._up)
		 ;
		quit;

	%end;*/

	proc delete data=_diag ;run;
	%if (&partOutMissing=Yes) %then %do;
		proc delete data= &partOutMissing ;run;	
	%end;

%mend BuildPart;

%macro PopPart(Pop=, Partition=, var= , PopOut=);
	%local i nvar curVar;
	%let nvar = %listLen(&var);

	/*Partition de la population*/
	proc sql;
	 create table &PopOut as
	 select p.*, t.*
	 from &pop as p , &partition as t 
	 where 
		%do i = 1 %to %eval(&nvar-1);
			%let curvar = %scan(&var,&i);
			(p.&curvar >= t.&curvar._low) and (p.&curvar < t.&curvar._up) and
		%end;
		%let curvar = %scan(&var,&nvar);
		(p.&curvar >= t.&curvar._low) and (p.&curvar < t.&curvar._up)
	 ;
	quit;

%mend PopPart;

%macro Stat(Pop= , by= , var= , StatOut=);
/*
Pop : file
	(by=)	:
	(var=)	: numeric

StatOut : file
	(&by)		:
	(&var._Sum)	:
	(&var._Var)	:
	n			:
*/
		/*for each zones, for each variable, calculate the sum of the variable and the sum*/
		%local i nvar curvar;
		%let nvar = %listLen(&var);

		proc sort data=&Pop; by &by;run;
		proc means data=&Pop noprint; by &by; output out=_PopZoneSize(drop= _type_ _freq_) n=n ;run;
		proc means data=&Pop noprint vardef=n; by &by; var &var ;output out=&StatOut.M(drop= _type_ _freq_)  sum= var= / autoname ;run;
		data &StatOut;
			merge &StatOut.M(in=inA) _PopZoneSize;
			by &by;
			%do i = 1 %to &nvar;
				%let curvar = %scan(&var,&i);
				/*calculate the sum of the squared variable, 
				use the variance calculated above to derive the sum of squared values
				*/
				&curvar._sum2 = (&curvar._var + (&curvar._sum/n)**2)*n;*(n-1)/n;
			%end;
			if inA;
		run;
		proc delete data=_PopZoneSize &StatOut.M;run;

%mend Stat;

%macro MergeSubPart(Part= , subPart= , keep= , var= ,  partOut= );
	%local i nvar curvar;
	%let nvar = %listLen(&var);

	proc sql;
		create table &partOut as
		select d1.&keep as &keep , d2.*
		from &Part as d1 right join &subPart as d2
		on
			%do i =1 %to %eval(&nvar-1);
				%let curVar = %scan(&var,&i);
				(d1.&curVar._low-d2.&curVar._low) <= 0.000000001  and (d1.&curVar._up - d2.&curVar._up) >= -0.000000001  and
			%end;
			%let curVar = %scan(&var,&nvar);
			(d1.&curVar._low-d2.&curVar._low) <= 0.000000001 and (d1.&curVar._up-d2.&curVar._up) >= -0.000000001
		;
	quit;

%mend MergeSubPart;

%macro Sum(data,Var);
/* yields the sum of a dataset s variable*/
	%local i dataid varnum nobs sum rc var;
	%let dataid=%sysfunc(open(&data));
	%let varnum=%sysfunc(varnum(&dataId,&var));
	%let nobs = %sysfunc(attrn(&dataId,nobs));
	%let sum=0;
	%do i = 1 %to &nobs;
		%let rc= %sysfunc(fetch(&dataId));
		%let var = %sysfunc(getvarN(&dataId,&varnum));
		%let sum= %sysevalf(&sum+&var);
	%end;
	%let rc = %sysfunc(close(&dataId));
	&sum
%mend Sum;

%macro varType(data,var);
	/*return a list of types for the variables of a Data set*/
	%local id nvar types rc N i varnum n;
	%let id= %sysfunc(open(&data));

	%if (&var eq) %then %do;
		%let nvar=%sysfunc(attrn(&id,nvar));
		%let types=;
		%do i = 1 %to &nvar;
			%let types= &types %sysfunc(varType(&id,&i));
		%end;
	%end;
	%else %do;
		%let n=  %sysfunc(countw(&var,%str( )));
		%let types=;
		%do i = 1 %to &n;
			%let varnum = %sysfunc(varnum(&id,%scan(&var,&i)));
			%let types= &types %sysfunc(varType(&id,&varnum));
		%end;
	%end;

	%let rc= %sysfunc(close(&id));
	&types
%mend varType;

%macro varExist_(Data,var);
	/*Check if a set of variables exists in a data set */
	%local count DSID varexist N varnum;
	%let DSID = %sysfunc(open(&Data));
	%let n=  %sysfunc(countw(&var,%str( )));

	%let count = 1;
	%let varexist=1;
	%do %while(&count <= &N);
		%let varnum = %sysfunc(varnum(&DSID, %scan(&var,&count)));
		%if &varnum eq 0 %then %do;
			%let varexist=0;
		%end;
		%let count = %eval(&count + 1);
	%end;
	
	%let DSID = %sysfunc(close(&DSID));
	&varexist 
%mend varExist_;
%macro Alloc_NeyOptimal();
	/*Census Bureau Optimal Allocation*/
	%local lowerSize Ndup;
	%let lowerSize= %Sum(&InfoOut,lb);
/*
	data _factor;
		set &InfoOut;
		_factors = (Count**2)*obj;

		
		do i = 1 to (&SampleSize-&lowerSize-1);
			if (i ge lb) and (i lt ub) then do;
				_Priorityfactors=_factors/(i*(i+1));
				output;
			end;;
		end;
		
	run;*/
	
	data _factor;
		set &InfoOut;
		_factors = (Count**2)*obj;

		Lim = min((ub-1),(&SampleSize-&lowerSize-1));
		
		do i = lb to Lim;
				_Priorityfactors=  _factors /(i*(i+1)) ;
				output;
		end;
		
		drop lim;
	run;

	proc sort data= _factor; by descending _Priorityfactors; run;

	data _factors _factors_low;
		set _factor;

		if _N_ le (&SampleSize-&lowerSize) then output _factors;
		else output _factors_low;
	run;

	proc sql noprint;
		select count(*) into : NDup
		from _factors as a , _factors_low as b
		where a._Priorityfactors=b._Priorityfactors
		;
	quit;

	%if &NDup gt 0 %then %do;	
		%put     WARNING: Optimal Allocation has multiple solutions;
	%end;

	proc sort data= _factors; by StratId ; run;
	proc means data= _factors noprint; by StratId; output out=_sizeOut (keep = StratId size) n=size;run;


	proc sql noprint;
		create table &InfoOut._ as
		select a.*, b.Size
		from &InfoOut as a left join _sizeOut as b
		on a.StratId=b.StratId
		;
	quit;

	data &InfoOut;
		set &InfoOut._ ;
		
		if missing(size) then size=0;
		size=size+lb;

	run;

	proc delete data= _sizeOut _factor _factors _factors_low; run;

%mend Alloc_NeyOptimal;

%macro Allocation(
					Selection=SRS,
					SampleSize=,
					MinSize=,
					Subdiv=1,
					LogPrint= yes,
				
					StratCons=,

					StratInfo=,
					VarInfo=,

					AllocOut= _allocOut,
					InfoOut = 
);
/*
Input 

	Selection (SRS Bern)
	SampleSize (numeric>0)
	MinSize (numeric >0)
	Subdiv (integer >= 1)

	StratCons : StratID LB UB 

	StratInfo : StratID Count 
	VarInfo : StratID VarID Total Variance Aux


Output

	AllocOut : StratId Size
	AllocOutInfo : StratId Count LB UB Size Obj Variance
	_ALLOCATIONSTATUS (OK ERROR)
	_ALLOCATIONObjective (numeric >0) 
*/

	%global  _ALLOCATIONSTATUS _ALLOCATIONObjective;
	%local Nstrat StratconsError options startTime objective InfoOutdelete SampSize;
	%let options = %saveOptions();
	%let _ALLOCATIONSTATUS = OK;
	%let _ALLOCATIONObjective= ;
	
	options nonotes nomprint nosource nosymbolgen;

	%let StartTime= %Time();

	%if (%upcase(&logPrint) eq YES) %then %do;
		%put ;
		%put ----------;
		%put Allocation;
		%put ----------;
		%put;
	%end;

	/*Verifications des paramètres*/
	%if (%upcase(&selection) ne BERN) and (%upcase(&selection) ne SRS) %then %do;
		%put ERROR: Selection method must be either SRS or BERN; 
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (&InfoOut eq ) %then %do;
		%let InfoOutDelete =1;
		%let InfoOut = _allocOutInfo;
	%end;
	%if (&InfoOut eq &allocOut) %then %do;
		%let InfoOutDelete =1;
		%let InfoOut = &allocOut._;
	%end;

	%if (&subdiv eq ) or (&subDiv lt 1) %then %do;
		%put WARNING: SubDiv must be at least 1;
		%let subDiv=1;
	%end;
	%if (&minSize ne ) and ( &minSize lt %sysevalf(1/&subdiv) ) %then %do;
		%put WARNING: MinSize must be at least %sysevalf(1/&subdiv);
		%let minsize = %sysevalf(1/&subdiv);
	%end;

	%if (&sampleSize eq ) %then %do;
		%put ERROR:  SampleSize must be be provided; 
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;
	%if (&sampleSize ne ) and %sysevalf(&sampleSize lt 1) %then %do;
		%put ERROR:  SampleSize must be greater than 0; 
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (&stratInfo eq ) %then %do;
		%put ERROR: StratInfo must be provided;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;
	%if (&varInfo eq ) %then %do;
		%put ERROR: VarInfo must be provided;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;



	/*Vérification des fichiers d entré*/
	%if (not %sysfunc(exist(&stratInfo))) %then %do;
		%put ERROR: StratInfo does not exist;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;
	%if (not %sysfunc(exist(&varInfo))) %then %do;
		%put ERROR: VarInfo does not exist;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (&stratCons ne ) and (not %sysfunc(exist(&stratCons))) %then %do;
		%put ERROR: StratCons does not exist;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (%varType(&stratInfo,stratId) ne %varType(&varInfo,stratId)) %then %do;
		%put ERROR: stratId types do not match in files stratInfo and VarInfo;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (&stratCons ne )  %then %do;
			%if (%varType(&stratCons,stratId) ne %varType(&varInfo,stratId)) %then %do;
				%put ERROR: stratId types do not match in files stratCons and VarInfo;
				%let _ALLOCATIONSTATUS = ERROR;
				%goto exit;
			%end;
	%end;


	%if (%varExist_(&stratInfo,STRATID COUNT) eq 0)  %then %do;
		%put ERROR: StratInfo must contain variables StratId and Count;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;
	%if (%upcase(&selection) eq SRS) %then %do;
		%if (%varExist_(&varInfo,STRATID VARID VARIANCE aux) eq 0)  %then %do;
			%put ERROR: VarInfo must contain variables StratId, VarId, Variance and Aux;
			%let _ALLOCATIONSTATUS = ERROR;
			%goto exit;
		%end;
	%end;
	%if (%upcase(&selection) eq BERN) %then %do;
		%if (%varExist_(&varInfo,STRATID VARID total VARIANCE aux) eq 0)  %then %do;
			%put ERROR: VarInfo must contain variables StratId, VarId, Total, Variance and Aux;
			%let _ALLOCATIONSTATUS = ERROR;
			%goto exit;
		%end;
	%end;




	/*Vérification du contenu de StratInfo VarInfo et StratCons*/
	data _StratInfo;
		set &stratInfo ;
		if not missing(Count);

		keep stratId Count ;
	run;
	proc sort data=_StratInfo nodupkey; by StratId; run;
	proc sql;
		create table _stratList as
		select distinct stratId 
		from &stratInfo 
		order by stratId;
	quit;
	%let NStrat	= %Nobs(_stratList);
	%if %Nobs(&StratInfo) gt &Nstrat %then %do;
		proc delete data = _stratList _StratInfo; run;
		%put ERROR: StratInfo contains missing Count or duplicate StratId;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	proc sql;
		create table _VarInfo as
		select s.*
		from _stratList as v left join &varInfo as s 
		on v.stratId=s.stratId;
	quit;
	data _varInfo;
		set _varInfo;
		if not missing(StratId);

		if missing(aux) then aux=1;
		if missing(variance) then variance=0;
		if missing(total) then total =0;
		keep StratId VarId total variance aux ;
	run;
	proc sort data=_varInfo nodupkey; by StratId VarId; run;
	%if %Nobs(_VarInfo) lt &Nstrat %then %do;
		proc delete data = _varInfo; run;
		%put ERROR: VarInfo doesnt cover all StratId in StratInfo or VarInfo contains duplicate pairs StratId VarId ;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	%if (&stratCons ne ) %then %do;
		proc sql;
			create table _stratCons as
			select s.*
			from &stratCons(keep = StratId lb ub) as s, _stratList as v
			where s.stratId=v.stratId
			order by s.stratId;
		quit;
		data _stratCons;
			merge _stratCons _stratInfo ;
			by stratId;

			%if (&minSize ne ) %then %do;
				if missing(lb) then lb=&minSize;
				if lb lt 0 then lb=&minSize;
			%end;
			if missing(lb) then lb=1/&subdiv;
			if lb lt 1 then lb=1/&subdiv;
			
			if missing(ub) then ub=count;
			if ub gt count then ub = count;
		run;
	%end;
	%else %do;
		data _stratCons;
			set _stratInfo(keep=stratId Count);
			lb=1/&subdiv;
			%if (&minSize ne ) %then %do;
				lb=&minSize;
			%end;
			ub=count;
		run;
	%end;
	%let StratconsError=0;
	data _stratCons;
		set _stratCons;

		if lb gt ub then do;
			call symputx("StratConsError",1);
		end;
	run;
	%if (&StratConsError = 1) %then %do;
		proc delete data = _stratList _stratInfo _VarInfo _stratCons;run;
		%put  ERROR: lower bound greater than upper bound ;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;

	

	/*Vérification des contraintes de taille*/
	%if %sysevalf(%Sum(_stratCons,lb) gt &sampleSize) %then %do;
		%put %Sum(_stratCons,lb) &sampleSize;
		%put ERROR: Sample size is too small to satisfy constraints;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;
	%if %sysevalf(%Sum(_stratCons,ub) lt &sampleSize) %then %do;
		%put %Sum(_stratCons,ub) &sampleSize;
		%put ERROR: Sample size is too large to be satisfied with the provided constraints;
		%let _ALLOCATIONSTATUS = ERROR;
		%goto exit;
	%end;



	/*Préparation */
	proc sql noprint;
		create table &InfoOut as
		select a.*, b.Count
		from _varInfo as a left join _stratInfo as b
		on a.StratId=b.StratId;
	quit;
	data &InfoOut;
		set &InfoOut;
		%if (%upcase(&Selection) eq SRS) %then %do;
			_varExp= aux*variance;
		%end;
		%if (%upcase(&Selection) eq BERN) %then %do;
			_varExp= aux* ((variance*(Count-1)/Count) + ((Total/Count)**2));
		%end;
	run;
	proc sort data=&InfoOut; by stratId ;run;
	proc means data=&InfoOut noprint; by stratId;var _varExp; output out=_LinVar(drop=_type_ _freq_) sum=obj;run;
	proc sql noprint;
		create table &InfoOut._ as
		select a.*, b.obj
		from _stratInfo as a left join _LinVar as b
		on a.StratId=b.StratId;
	quit;
	proc sql noprint;
		create table &InfoOut as
		select a.* , b.lb, b.ub
		from  &InfoOut._ as a left join _StratCons as b
		on a.StratId=b.StratId
		;
	quit;
	data &infoOut;
		set &infoOut;

		lb=lb*&subDiv;
		ub=Ub*&SubDiv;
	run;
	%let SampSize=&SampleSize;
	%let SampleSize=%sysevalf(&sampleSize * &subdiv);


	/*Calcul de la répartition*/
	%Alloc_NeyOptimal();

	%let SampleSize= &SampSize;

	/*Output*/
	data &InfoOut;
		set &InfoOut;
		
		size=size/&subdiv;
		lb=lb/&subDiv;
		ub=ub/&subDiv;

		variance= (count**2)*(1/size - 1/count)*obj;
	run;
	data &InfoOut;
		retain StratId Count LB UB Size Obj Variance;
		set &infoOut;

		keep StratId Count LB UB Size Obj Variance;
	run;
	data &allocOut;
		set &InfoOut;
		%if %upcase(&Selection) eq BERN %then %do;
			size=size/Count;
		%end;
		keep StratId size;
	run;
	

	/*Affichage et sortie*/
	%if (%upcase(&logPrint) eq YES) %then %do;
		%put %str(   ) Selection Method  : %upcase(&selection);
		%put;
		%put %str(   ) Number of Strata : %Nobs(_stratList);
		%put %str(   ) SampleSize       : %Sum(&InfoOut,size);
		%put;
		%put %str(   ) Objective Function : %Sum(&InfoOut,variance);
	%end;
	

	%let _ALLOCATIONObjective =  %Sum(&InfoOut,variance);

	%if (&InfoOutDelete eq 1) %then %do;
		proc delete data = &InfoOut ;run;
	%end;

	proc delete data = _stratList _stratInfo _VarInfo _stratCons &InfoOut._  _LinVar ;run;

	%exit:
	%if (%upcase(&logPrint) eq YES) %then %do;
		%put;
		%put Start 	at &StartTime;
		%put End    at %Time();
	%end;

	options &options;

%mend Allocation;
%macro Alloc(bound);
/*
ZoneStrat
_ZoneVar
*/

	proc sql noprint;
		create table StratInfo as 
		select StratId , sum(n) as Count
		from ZoneStrat
		group by StratId;
		;
	quit;
	
	proc sql noprint;
		create table varInfo as
		select a.varId , b.StratId, sum(sum) as T, sum(sum2) as T2 , sum(n) as count
		from _zoneVar as a left join ZoneStrat as b
		on a.ZoneId = b.ZoneId
		group by varId, stratID
		;
	quit;

	data VarInfo;
		set VarInfo;

		Total = T;
		Aux=1;
		Variance = T2/Count - ((T/Count)**2);

		keep StratId varId Total Variance Aux;
	run;

	%if (&bound eq yes) %then %do;
		data StratInfo;
			set StratInfo;
			if stratId eq 0 then delete;
		run;
		data VarInfo;
			set VarInfo;
			if StratId eq 0 then delete;
		run;
	%end;

	%if ( %sum(StratInfo,count) lt &sampleSize ) %then %do;
		%let _allocationObjective = 0;
		%let _allocationStatus = OK;
	%end;
	%else %do;
		%Allocation(
			Selection=SRS,
			SampleSize= &sampleSize,
			Subdiv=1,
			LogPrint=no,

			StratInfo= StratInfo,
			VarInfo= VarInfo,

			AllocOut= _allocOut,
			InfoOut = _infoOut
		);
	%end;
%mend Alloc;

%macro AdjustStrat(ZoneStrat=, stratVar=, StratStat=);
	/*Local search*/
	%local i k Nzones PrevObj BestObj FirstLoop skip;


	/*Keep the zones where there are some observation in*/
	data _ZoneCount(keep= zoneId count);
		set &zoneStrat(where=(n ne 0));
		count=n;
	run;

	%let NZones= %Nobs(_ZoneCount);

	data _zoneVar(keep= zoneId varId sum sum2);
		set &zoneStrat(where=(n ne 0) );
		retain 	%do i = 1 %to &nvar; 
					%let curvar = %scan(&stratVar,&i);
					&CurVar._Sum &CurVar._Sum2
				%end; ;
		%do i = 1 %to &nvar;
			%let curvar = %scan(&stratVar,&i);
			varId=&i;
			sum = &CurVar._Sum; 
			sum2 = &CurVar._Sum2; 
			output;
		%end;	
	run;
	
	data ZoneStrat;
		set &ZoneStrat;
	run;

	/*perform allocation on the initial assignement*/
	%Alloc();
	%put &_AllocationObjective;
	%let BestObj=&_AllocationObjective;
	%let PrevObj= %sysevalf(2*&bestObj);

	data BestZoneStrat;
		set ZoneStrat;
	run;

	%do %while( %sysevalf((&PrevObj - &BestObj)/&PrevObj)  gt &tol );
		%let PrevObj = &bestObj;

		/*For each zone, change the strata assingment and see of the resulting allocation is better	*/
		%do i= 1 %to &NZones;
			%do k= 1 %to &NStrat;
				%let skip=0;
				Data ZoneStrat;
					set BestZoneStrat;

					if _N_ eq &i then do;
						if stratId eq &k then call symputx("skip",1);
						StratID= &k;
					end;
				run;

				%if (&skip ne 1) %then %do;
					%Alloc();

					%if (%sysevalf(&_AllocationObjective lt &BestObj)) %then %do;
						/*put "New Best!" TotalDistance;*/
						%let BestObj = &_AllocationObjective ;
						data BestZoneStrat;
							set ZoneStrat;
						run;
						data BestStratInfo;
							set StratInfo;
						run;
						data BestAlloc;
							set _allocOut;
						run;
						%let improvement = 1;
						%put &BestObj;
					%end;

				%end;
				
			%end;
		%end;
	%end;
				
				
	%let Obj=&BestObj;

	data _stratSize;
		set BestAlloc;
	run;
	data _stratCount;
		set BeststratInfo;
	run;
	data &ZoneStrat(keep=zoneId stratId n);
		set BestZoneStrat;
	run;
	
	proc delete data= _zoneVar _zoneCount StratInfo VarInfo _allocOut _infoOut ZoneStrat BestAlloc BeststratInfo; run;
%mend AdjustStrat;



%macro InitStrat(ZoneIn=, nStrat=, ZoneStratOut= );
/*
zoneIn : File
	ZoneId

ZoneStratOut : File
	zoneId
	StratId


*/
/*produces an initial assignement of strata to each zones*/

	proc sql;
		create table _zones as
		select distinct zoneId
		from &zoneIn;
	quit;

	%local offset;
	%let offset =%sysfunc(rand(uniform));
	/*%let offset=0;*/
	data &ZoneStratOut;
		set _zones;
		stratId = mod(zoneId + floor(&offset*&Nstrat), &Nstrat) + 1 ;
	run;
	proc sort data=&ZoneStratOut;by zoneId;run;
	proc sort data=&zoneIn;by zoneId;run;
	data &ZoneStratOut;
		merge &ZoneStratOut &zoneIn;
		by zoneId;
	run;
	proc delete data=_zones;run;

%mend InitStrat;

%macro MultiVarStrat(Pop= , Nstrat= , stratVar= ,  Objective= Size, Exp=1, SampleSize=, tol=0.01, DataOut=_stratOut );

	/*Main program to be called*/
	%local i lvl divisions nvar options Nunits oldSize improvement startTime obj FirstLoop;

	%let options = %saveOptions();
	%let startTime = %time();
	%let lvl=1;
	%let improvement=1;

	option nonotes nomprint ;

	%put;
	%put --------------------------;
	%put Stratification Multivariée;
	%put --------------------------;
	%put;

	/*Verif*/
	%if (&pop eq ) 		%then %do;
		%put ERROR: Population File must be specified;
		%goto exit;
	%end;
	%if (&stratVar eq ) %then %do;
		%put ERROR: stratification variables must be specified;
		%goto exit;
	%end;
	%if (&nStrat eq ) 	%then %do;
		%put ERROR: Number of strata must be specified;
		%goto exit;
	%end;
	%if (&tol < 0 ) %then %do;
		%put ERROR: tolerance must be higher than 0;
		%goto exit;
	%end;
	%if (&sampleSize <= 0 ) %then %do;
		%put ERROR: Sample Size must be higher than 0;
		%goto exit;
	%end;
	%if (&sampleSize eq ) %then %do;
		%put ERROR: Sample Size must be provided;
		%goto exit;
	%end;
	%if (%upcase(&Objective) ne CV) and (%upcase(&Objective) ne VARIANCE) %then %do;
		%put ERROR: Objective function must be either Variance or CV;
		%goto exit;
	%end;


	%let nvar = %listlen(&stratVar);
	%let divisions = %sysfunc( floor(%sysevalf(&nStrat*(2**(&lvl))*1 /&nvar)));
	%let Nunits = %Nobs(&pop);

	%put Number of variables : &Nvar;
	%put Number of strata	 : &Nstrat;



	%BuildPart(	Pop=&pop 				,	var=&StratVar 			, 	div=&divisions		, 	PartOut=_Partition&lvl );
	%PopPart(	Pop=&pop 				,	Partition=_Partition&lvl , 	var=&stratVar 		,	PopOut=_PopZone );
	%InitStrat(	ZoneIn= _popZone		, 	nStrat=&nStrat 			,	ZoneStratOut= _PopZoneStrat&lvl);
	%Stat(		Pop=_PopZoneStrat&lvl 	, 	by=StratId 				, 	var= &stratVar 		, 	StatOut=_StratStat );


	/*set defaults*/
	data _stratSize(keep=stratId size);
		set _stratStat (keep= stratId N);
		Size=N; 
	run;

	proc means data= _stratStat noprint; var %do i = 1 %to &Nvar; %scan(&stratVar,&i)_Sum %scan(&stratVar,&i)_Sum2 N %end;; output out= _varTotal(drop= _type_ _freq_) sum= /autoname;run;
	data _varTotal(keep=varId sum sum2);
		set _varTotal;
		retain 	%do i = 1 %to &nvar; 
					%let curvar = %scan(&stratVar,&i);
					&CurVar._Sum_sum &CurVar._Sum2_sum
				%end; ;
		%do i = 1 %to &nvar;
			%let curvar = %scan(&stratVar,&i);
			varId=&i;
			sum = &CurVar._Sum_sum; 
			sum2 = &CurVar._Sum2_sum; 
			output;
		%end;
	run;


	%let FirstLoop=1;
	%let OldSize= 1;
	%let obj=%sysevalf(  &oldSize -&oldSize*&tol  - 1 )  ;

	%do %while( %sysevalf(%sysevalf( (&oldSize-&obj)/&oldSize ) > &tol ) and (&improvement eq 1)  or (&FirstLoop eq 1));
		/*chaque iteration subdivise la région*/
		%let FirstLoop=0;
		%let oldSize=&obj;
		%let improvement=0;
		%put ;
		%put Resolution: %sysfunc(strip(%sysfunc(roundz(%sysevalf((1/&divisions)**&nvar),0.0001),percent7.3)))   ;
		%put ;
	
		%Stat(Pop=_PopZoneStrat&lvl 	, by=zoneId 	, var=&stratVar 	, StatOut=_ZoneStat&lvl );
		proc sort data= _PopZoneStrat&lvl; by zoneId;run;
		proc sort data= _ZoneStat&lvl; by zoneId;run;
		proc sql;
			create table _zonestrat&lvl as
			select distinct zoneId, stratId
			from _popzonestrat&lvl;
		quit;
		data _ZoneStat&lvl;
			merge _ZoneStat&lvl(in=inA) _zonestrat&lvl(keep=zoneId stratId);
			by zoneId;
			if inA;
		run;
		
		%AdjustStrat(ZoneStrat=_ZoneStat&lvl, stratVar=&StratVar, StratStat=_StratStat );  

		%let lvl= %eval(&lvl+1);
		%let divisions = %sysfunc( floor(%sysevalf(&nStrat*(2**(&lvl))*1 /&nvar)));

		%BuildPart(Pop=&pop , var=&StratVar 	, div=&divisions , PartOut= _Partition&lvl);
		proc sort data=_Partition%eval(&lvl-1);by zoneId; run;
		proc sort data=_ZoneStat%eval(&lvl-1) ;by zoneId; run;
		data _Partition%eval(&lvl-1);
			merge _Partition%eval(&lvl-1) _ZoneStat%eval(&lvl-1)(in=inB keep= zoneId stratId);
			by zoneId;
			if inB;
		run;
		%MergeSubPart(	Part=_Partition%eval(&lvl-1) 	, subPart=_Partition&lvl 		, 	keep=StratId 		, 	var=&stratVar 	,  partOut= _PartZoneStrat&lvl);
		%PopPart(		Pop= &pop 						, Partition=_PartZoneStrat&lvl 	, 	var=&stratVar 		, 	PopOut=_PopZoneStrat&lvl );
		%Stat(			Pop=_PopZoneStrat&lvl 			, by=stratId 					,   var=&stratVar 		, 	StatOut=_stratStat );

	%end;




	%put;
	%put Solution details;
	%put;
	data &dataOut._Info;
		merge _stratSize _stratCount;
		by stratId;
		put @4 "StratId " stratId @ 16 "Size " Size @32 "Count " Count;
	run;
	%put;
	%put %str(   )Objective function: &Obj;
	%put %str(   )Total Sample Size : %sum(_stratSize,size);

	data &dataOut;
		set _PopZoneStrat&lvl;
		keep &stratVar StratID;
	run;

	%exit:

	%put;
	%put Start at &startTime;
	%put end   at %time();

	proc delete data= %do i = 2 %to &lvl; _PartZoneStrat&i  %end;; run ;
	proc delete data= %do i = 1 %to &lvl; _PopzoneStrat&i _Partition&i  %end;; run ;
	proc delete data= %do i = 1 %to %eval(&lvl-1); _ZoneStrat&i _ZoneStat&i  %end;; run ;
	proc delete data=_PopZone _varTotal BestZoneStrat _StratStat _stratSize _stratCount; run;
	option &options;
%mend MultiVarStrat;
