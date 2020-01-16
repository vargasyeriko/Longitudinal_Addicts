PROC IMPORT DATAFILE="/home/ygvargas1/sasuser.v94/SASdocs/BSTHW/FinalProject/addicts.xlsx"
		    OUT=addicts
		    DBMS=XLSX
		    REPLACE;
RUN;

proc lifetest data= addicts method=km plots= (s,lls);
	Time SurvTime*Status(0);
	strata Clinic;
run;

proc lifetest data= addicts method= act plots= (s,h) notable;
	Time SurvTime*Status(0);
	strata Clinic;
run;

proc lifetest data= addicts method= act plots= (s,h) notable;
	Time SurvTime*Status(0);
run;
*********Models***********;

*Logistic Regression Model;
PROC LIFEREG DATA = ADDICTs;
MODEL SurvTime*Status(0) = Clinic Prison Metha / D= LOGISTIC NOINT ;
RUN;

*Logistic Regression Model with Quadratic Term;
data addicts1;
set addicts;
methaSQRT = metha*metha;
PROC LIFEREG DATA = ADDICTS1;
MODEL SurvTime*Status(0) = Clinic Prison metha methaSQRT / D= LOGISTIC NOINT ;
RUN;

*prepare the data for plots;
proc means data = addicts noprint;
var Prison Metha ;
output out =risk mean = Prison Metha;
run;

data inrisk;
set risk;
do i=1 to 2;
Clinic =i;
output;
end ;
run;

proc print data = inrisk;
var Clinic Prison Metha;
run;

*Cox PH Model ;
proc phreg data = addicts;
model SurvTime*Status(0) = Clinic Prison Metha /RL;
id id;
baseline covariates = inrisk out = model1 survival = s1 loglogs=logj /nomean;
assess var = (Clinic) ph /  resample;
run;

*Check for PH assumption;
proc phreg data = addicts noprint;
model SurvTime*Status(0) =/rl;
strata clinic ;
baseline out =clinic  loglogs=logh;
run;

proc plot data = clinic;
plot logh*SurvTime = clinic / ;
title 'Log Cumulative Hazard by Clinic';
run;
 
*Plots for Cox model;

proc plot data = model1;
title2 'PLOT SURVIVAL FUNCTION VS. TIME';
title3 'ADJUSTED FOR CLINIC, METHA(DOSE), AND PRISON';
PLOT s1*SurvTime =Clinic;
run;

*Cox Stratified model;
Proc phreg data = addicts;
model SurvTime*Status(0) = Clinic Prison Metha /RL;
strata Clinic;
id id;
baseline covariates = inrisk out = model2 survival = s2 loglogs=lls / nomean;
run;

*Plot-Adjusted Survival and log-log survival curves for Cox PH model Stratified by Clinic;
data model2;
set model2;
log_t=log(SurvTime);
label log_t='log of SurvTime (Days)';
run;

proc plot data = model2;
title2 'PLOTS OF SURVIVAL FUNCTION AND LOG (-LOG(S)) VS. TIME';
title3 'ADJUSTED FOR METHA(DOSE) AND PRISON';
title4 'STRATIFIED BY CLINIC';
PLOT S2*SurvTime = Clinic LLS*LOG_T=Clinic;
run;

*Extended Cox model with two 
time-dependent variables ;
proc phreg data = addicts ;
title2 ' ';
title3 ' ';
title4 ' ';
model SurvTime*Status(0)= Prison Metha ClinicT1 ClinicT2 / rl;
ClinicT1 =0;
if SurvTime < 365 then ClinicT1 = Clinic;
ClinicT2 =0;
if SurvTime >= 365 then ClinicT2 =Clinic;
run;

*Extended Cox model with one 
time-dependent variable;
Proc phreg data = addicts ;
model SurvTime*Status(0)= Clinic Prison Metha Clinic_T / rl covb;
	if 0 <= SurvTime <= 183 then T=1;
	if 183 < SurvTime <= 365 then T=3;
	if 365 < SurvTime <= 548 then T=5;
	if 548 < SurvTime <= 730 then T=7;
	if SurvTime > 730 then T=9;
	clinic_T = Clinic*T;
Run;
