* TITLE: PROJECT #1;
* NAME: MENG "PHIL" SHEN;
* DATE: 01/15/2015;

/* INPUT DATASET */
PROC IMPORT OUT= WORK.Project1 
            DATAFILE= "F:\Adjuvant Endocrine Data MODIFIED.xlsx" 
            DBMS=EXCELCS REPLACE;
     RANGE="Sheet1$"; 
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

/* DESCRIBE DATASET */
PROC CONTENTS DATA=Project1;
  TITLE;
RUN;

/* SUMMARIZE DATASET */
PROC MEANS DATA=Project1;
  TITLE;
RUN;

ODS RTF FILE="F:\Dropbox\PJ1.rtf";
/* SUMMARIZE CATEGORICAL VARIABLES */
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES REASONS_FOR_MISSING TYPE_OF_REMINDER SIDE_EFFECTS / PLOTS=FREQPLOT;
  TITLE;
RUN;

/* SUBSET DATASET */
PROC PRINT DATA=Project1;
  WHERE MISSED_DOSES=1;
  VAR MISSED_DOSES YEAR_1 YEAR_2 YEAR_3 YEAR_4 YEAR_5;
  TITLE;
RUN;
PROC PRINT DATA=Project1;
  WHERE MISSED_DOSES=2;
  VAR MISSED_DOSES YEAR_1 YEAR_2 YEAR_3 YEAR_4 YEAR_5;
  TITLE;
RUN;
PROC PRINT DATA=Project1;
  WHERE MISSED_DOSES=3;
  VAR MISSED_DOSES YEAR_1 YEAR_2 YEAR_3 YEAR_4 YEAR_5;
  TITLE;
RUN;
PROC PRINT DATA=Project1;
  WHERE MISSED_DOSES=4;
  VAR MISSED_DOSES YEAR_1 YEAR_2 YEAR_3 YEAR_4 YEAR_5;
  TITLE;
RUN;

/*DATA Project1a;
  SET Project1;
  IF MISSED_DOSES=1;
  KEEP PATIENT MISSED_DOSES YEAR_1 YEAR_2 YEAR_3 YEAR_4 YEAR_5;
RUN;
PROC PRINT DATA=Project1a;
  TITLE;
RUN;/*

/* CREATE CROSS-TABULATION TABLE */
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*RACE / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*MARITAL_STATUS / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*EDUCATION / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*EMPLOYMENT / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*HOUSEHOLD_INCOME / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*SMOKING_HISTORY / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*FAMILY_HISTORY / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*ACTIVITY_LEVEL / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;
PROC FREQ DATA=Project1;
  TABLE MISSED_DOSES*ZIP_CODE / PLCORR EXACT NOPERCENT NOROW NOCOL;
  TITLE;
RUN;

/* 1-Way ANOVA */
PROC GLM DATA=Project1;
  CLASS MISSED_DOSES;
  MODEL AGE=MISSED_DOSES;
  TITLE;
RUN;
ODS RTF CLOSE;