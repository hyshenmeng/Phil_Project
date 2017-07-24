* QUESTION3;
/* INPUT DATA */
DATA LUNG;
  INPUT Lung_Cancer $ Smoker $ CNT;
  DATALINES;
Control NO 59
Control YES 650
Case NO 21
Case YES 688
;
RUN;
/* CREATE CROSS-TABULATION TABLE */
PROC FREQ DATA=LUNG ORDER=FREQ;
  TABLE Smoker*Lung_Cancer / CHISQ RISKDIFF MEASURES;;
  WEIGHT CNT;
  TITLE;
RUN;

* QUESTION4a;
/* INPUT DATA */
DATA Berkeley1;
  INPUT  Admitted $ Gender $ CNT;
  DATALINES;
Yes Male 1198
No Male 1493
Yes Female 557
No Female 1278
;
RUN;
/* CREATE CROSS-TABULATION TABLE */
PROC FREQ DATA=Berkeley1 ORDER=DATA;
  TABLE Gender*Admitted / CHISQ RISKDIFF MEASURES;;
  WEIGHT CNT;
  TITLE;
RUN;

* QUESTION4b;
/* INPUT DATA */
data Berkeley2;
   input D $ S $ A $ count;
   cards;
DeptA  Male    Reject  313
DeptA  Male    Accept  512
DeptA  Female  Reject   19
DeptA  Female  Accept   89
DeptB  Male    Reject  207
DeptB  Male    Accept  353
DeptB  Female  Reject    8
DeptB  Female  Accept   17
DeptC  Male    Reject  205
DeptC  Male    Accept  120
DeptC  Female  Reject  391
DeptC  Female  Accept  202
DeptD  Male    Reject  278
DeptD  Male    Accept  139
DeptD  Female  Reject  244
DeptD  Female  Accept  131
DeptE  Male    Reject  138
DeptE  Male    Accept   53
DeptE  Female  Reject  299
DeptE  Female  Accept   94
DeptF  Male    Reject  351
DeptF  Male    Accept   22
DeptF  Female  Reject  317
DeptF  Female  Accept   24
;
/* CREATE CROSS-TABULATION TABLE */
PROC FREQ DATA=Berkeley2 ORDER=data;
WEIGHT count;
TABLES D*S*A/ CMH CHISQ RELRISK;
TABLES S*A/CHISQ RELRISK;
RUN;

/* LOGISTIC REGRESSION ANALYSIS WITHOUT INTERACTIONS */
PROC LOGISTIC DATA=Berkeley2 DESCENDING;
  CLASS D S / PARAM=REF REF=FIRST;
  WEIGHT count;
  MODEL A=D S / EXPB;
  TITLE;
RUN;
