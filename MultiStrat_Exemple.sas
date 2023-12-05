

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


/*

data possible;
	do rank =1 to 6;
		do counter= 1 to 3;
			output;
		end;
	end;
run;


data test;
	x = 3**6 + 3**5 + 3**4 + 3*3 + 3**2 + 3**1 + 3**0;
	put x=;
run;

proc sort data= tracker out=track nodupkey; by rank counter;run;

data tracker;	
	set tracker;
	retain n 0;

	
	group = (n-mod(n,6))/6;
	n=n+1;

	drop n;
run;

proc transpose data= tracker out=tracker_; by group;run;

data track_counter;
	set tracker_ (where=(_name_="counter"));
run;

data track_strat;
	set tracker_ (where=(_name_="stratId"));

run;

options notes;
proc sort data= track_counter nodupkey; by col1 col2 col3 col4 col5 col6 ;run;

proc sort data= track_strat nodupkey; by col1 col2 col3 col4 col5 col6 ;run;
*/
