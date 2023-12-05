

/*--------------------------------*/
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

	data StratCons;
		%do i = 1 %to &Nstrat;
			stratId =&i;
			%do j = 1 %to &nvar;
				var&j= 10/(2**(&i-1)) ;
			%end;
			output;
		%end;
	run;
	data VarCons;
		%do j = 1 %to &nvar;
			var&j= 0.1 ;
		%end;
		output;
	run;
%mend;


%Pop(1000,2,3);


%MultiVarStrat(	Pop=population , 
				Nstrat=3,
				sampleSize=300, 
				stratVar=var1 var2,
				Objective=variance,  
				tol=0.1, 
				DataOut=Stratified);
ods html;
proc sort data=Stratified; by stratId;run;
goptions colors=(blue red green);
goptions colors=(blue red cyan);
symbol1 value= dot;
proc gplot data=Stratified;
	plot var1*var2=stratId;
run;
quit;
