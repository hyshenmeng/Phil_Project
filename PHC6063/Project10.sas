* TITLE: PROJECT #10;
* NAME: MENG "PHIL" SHEN;
* DATE: 03/26/2015;

/* INPUT DATASET */
PROC IMPORT DATAFILE= "F:\positive.csv"
  OUT=positive 
  DBMS=CSV REPLACE;
  GETNAMES=YES;    
RUN;
PROC IMPORT DATAFILE= "F:\negative.csv"
  OUT=negative 
  DBMS=CSV REPLACE;
  GETNAMES=YES;    
RUN;

/* MANIPULATE VARIABLES */
DATA positive2;
  SET positive;
  IF percentage_of_blasts<=10 THEN percentage_of_blasts1="low";
  IF (percentage_of_blasts>10) AND (percentage_of_blasts<=30) THEN percentage_of_blasts1="medium";
  IF percentage_of_blasts>30 THEN percentage_of_blasts1="high";
  IF AML_risk~="high" THEN AML_risk1="no high risk";
  IF AML_risk="high" THEN AML_risk1="high risk";
  status=1;
RUN;
DATA negative2;
  SET negative;
  IF percentage_of_blasts<=10 THEN percentage_of_blasts1="low";
  IF (percentage_of_blasts>10) AND (percentage_of_blasts<=30) THEN percentage_of_blasts1="medium";
  IF percentage_of_blasts>30 THEN percentage_of_blasts1="high";
  IF AML_risk~="high" THEN AML_risk1="no high risk";
  IF AML_risk="high" THEN AML_risk1="high risk";
  status=0;
RUN;

/* SET DATASET */
DATA PJ10;
  SET positive2 negative2;
RUN;
PROC CONTENTS DATA=PJ10;
RUN;

/* SUMMARIZE CONTINUOUS VARIABLES*/
PROC MEANS DATA=PJ10 N MEAN MEDIAN MIN MAX RANGE STD;
  CLASS status;
  VAR BM_cellularity WBC age percentage_of_blasts;
RUN;

/* MACRO FOR KRUSKAL-WALLIS RANK SUM TEST */
%MACRO RANKSUMTEST(x,y);
PROC NPAR1WAY DATA=PJ10;
  CLASS &x;
  VAR &y;
RUN;
%MEND;

/* KRUSKAL-WALLIS RANK SUM TEST FOR status WRT BM_cellularity */
%RANKSUMTEST(status,BM_cellularity);
/* KRUSKAL-WALLIS RANK SUM TEST FOR status WRT WBC */
%RANKSUMTEST(status,WBC);
/* KRUSKAL-WALLIS RANK SUM TEST FOR status WRT age */
%RANKSUMTEST(status,age);

/* SUMMARIZE CATEGORICAL VARIABLES*/
PROC FREQ DATA=negative2;
  TABLE AML_risk AML_risk1 blast_phenotype cytoreduction disease_status percentage_of_blasts1 previous_persistent_d14_marrows reason_no_treatment;
RUN;
PROC FREQ DATA=positive2;
  TABLE AML_risk AML_risk1 blast_phenotype cytoreduction disease_status percentage_of_blasts1 previous_persistent_d14_marrows reason_no_treatment;
RUN;

/* MACRO FOR FISHER'S EXACT TEST */
%MACRO FISHERTEST(x,y);
PROC FREQ DATA=PJ10;
  TABLES &x*&y;
  EXACT FISHER;
RUN;
%MEND;

/* FISHER'S EXACT TEST FOR status & AML_risk1 */
%FISHERTEST(status,AML_risk1);
/* FISHER'S EXACT TEST FOR status & blast_phenotype */
%FISHERTEST(status,blast_phenotype);
/* FISHER'S EXACT TEST FOR status & cytoreduction */
%FISHERTEST(status,cytoreduction);
/* FISHER'S EXACT TEST FOR status & disease_status */
%FISHERTEST(status,disease_status);
/* FISHER'S EXACT TEST FOR status & percentage_of_blasts1 */
%FISHERTEST(status,percentage_of_blasts1);
/* FISHER'S EXACT TEST FOR status & previous_persistent_d14_marrows */
%FISHERTEST(status,previous_persistent_d14_marrows);
/* FISHER'S EXACT TEST FOR status & reason_no_treatment */
%FISHERTEST(status,reason_no_treatment);

/* LOGISTIC REGRESSION */
PROC LOGISTIC DATA=PJ10 DESCENDING;
  CLASS AML_risk1 blast_phenotype disease_status previous_persistent_d14_marrows reason_no_treatment / param=ref;
  MODEL status=AML_risk1 blast_phenotype disease_status percentage_of_blasts previous_persistent_d14_marrows BM_cellularity WBC age / selection=BACKWARD;
RUN;
PROC LOGISTIC DATA=PJ10 DESCENDING;
  CLASS AML_risk1 blast_phenotype disease_status percentage_of_blasts1 previous_persistent_d14_marrows reason_no_treatment / param=ref;
  MODEL status=AML_risk1 blast_phenotype disease_status percentage_of_blasts1 previous_persistent_d14_marrows BM_cellularity WBC age / selection=BACKWARD;
RUN;

/* FINAL MODEL */
PROC LOGISTIC DATA=PJ10 DESCENDING PLOTS=ROC;
  CLASS AML_risk1 disease_status percentage_of_blasts1 / param=ref ref=first;
  MODEL status=AML_risk1 disease_status percentage_of_blasts1 / ctable;
  OUTPUT OUT=pred PREDPROB=individual;
RUN;




