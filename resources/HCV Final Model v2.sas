/**********************************************************************************************
Project Name:  HCV / HIV / IDU Vulnerability Analysis
Program Name: HCV Final Model https://urldefense.com/v3/__http://v2.sas__;!!GaaboA!_wjgv0kIoVv6KjOjndQIuRZ42gKPJorEeSi-RTKQHuPUul2-9ptWHupYtRGLaub_$   
Program author : Charles Rose
Date Created:  01/11/2017
Program Version #: 1
Program Description: This program uses the regression parameters for the final multivariable models
                     to compute the scores for ranking. 
 
PEER REVIEWER: Michelle Van Handel
Date Peer Reviewed: 08/21/2015
Pass/Fail: Pass
Peer Reviewer's verification procedure: 
* Review the annotated code for adequacy of program structure 
* Review for clarity of documentation
* Run program and review the output

Datasets or Programs Used: 
* Input data 1 = vulfinaldata.

Expected Output:  Final model information and parameter estimates.
                  
Storage location for SAS output: "\\cdc.gov\private\L107\cvr7\HIV\Indiana\Results"
Statistical vs. Data Management: Statistical
 
************************************************************************************************/

options ls=88 pagesize=100;
libname indiana "\\cdc.gov\private\L107\cvr7\HIV\Indiana\Data";

data pokey1;
set indiana.vulfinaldata;
run;


proc sort data=pokey1;
by state fips year;
run;
 

/************************************************************************************************
Final Model:

1) Data pokey1: Has 2 observations per county, 3143 * 2 = 6286 but there is missing data that 
                will be dropped when modeling as we do a complete case analysis

2) Variables: log base 10 income, percent unemployed, percent white non-Hispanic, capacity rate,
              drug deaths per 100 k, and drug distribution per 10 k

3) Poisson model with population defined as the offset

4) Offset is defined as the log base e of county population within a year

5) Random effects: state, county, for the county I used the newfips variable that numbers fips
                   from 1 to n within a state so that when using newfips(state) SAS knows that
                   number 1 in state AL is not the same as number 1 in sate AR (I could have
                   just used the original fips)

6) Output predictions (out=glimmixout) for post model processing

7) Output model parameter estimates (multi_IRR): Model output was originally sent to a RTF
                                                 using ODS but is not presented in this program

 
************************************************************************************************/

ods trace on;
proc glimmix data=pokey1 method=rspl noclprint;

   class newfips state ;
   model hcv = logincome pct_unemploy pct_whnonhisp capacityrate 
               dd_per100k drugdistper10k   
        /dist=poisson offset=off solution ddfm=kr2;
   random intercept / sub=state;
   random intercept / sub=newfips(state) ;
   output out=glimmixout predicted(ILINK)=predicted pred=pred pearson=pearson residual=residual;
   ods output parameterestimates = multi_IRR;
run;
ods trace off;



/************************************************************************************************
Final Model:

1) Data pokey2: This dataset is created by dropping those records with missing observations
                and running the exact same model as above

2) Creating a dataset without missing observations increases the speed for fitting the model in
   SAS and was the only purpose in dropping the observations as it's exactly the same model as aboe

************************************************************************************************/

data pokey2;
set pokey1;
if logincome=. or pct_unemploy=. or pct_whnonhisp=. or capacityrate=. or dd_per100k=. or drugdistper10k=.
   then delete;
if hcv = . then delete;
run;

ods trace on;
proc glimmix data=pokey2 method=rspl noclprint;

   class newfips state ;
  
   model hcv = logincome pct_unemploy pct_whnonhisp capacityrate 
               dd_per100k drugdistper10k   
        /dist=poisson offset=off solution ddfm=kr2;
   random intercept / sub=state;
   random intercept / sub=newfips(state) ;
   output out=glimmixout predicted(ILINK)=predicted pred=pred pearson=pearson residual=residual;
   ods output parameterestimates = multi_IRR;
run;
ods trace off;

