* TITLE: PROJECT #5;
* NAME: MENG "PHIL" SHEN;
* DATE: 02/12/2015;

/* INPUT DATASET 1*/
PROC IMPORT OUT=Project5 
            DATAFILE= "F:\OSA Feb25.xlsx" 
            DBMS=EXCELCS REPLACE;
     RANGE="Final sheet$"; 
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/* SELECT OBSERVATIONS FOR DATASET 1*/
DATA Project5a;
  SET Project5;
  IF MR_=. THEN DELETE;
RUN;
/* INPUT DATASET 2*/
PROC IMPORT OUT=Project5c 
            DATAFILE= "F:\OSA original dataset.xlsx" 
            DBMS=EXCELCS REPLACE;
     RANGE="fracture Y N$"; 
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/* SELECT OBSERVATIONS FOR DATASET 2*/
DATA Project5d;
  SET Project5c;
  IF MR_=. THEN DELETE;
RUN;
/* SELECT VARIABLES FOR DATASET 2*/
DATA Project5e;
  SET Project5d (KEEP=MR_ Age Sex);
RUN;
/* MERGE DATASET 1 & 2*/
PROC SORT DATA=Project5a;
  BY MR_;
RUN;
PROC SORT DATA=Project5e;
  BY MR_;
RUN;
DATA Project5f;
  MERGE Project5a Project5e;
    BY MR_;
RUN;
/* RENAME VARIABLES*/
DATA Project5g;
  SET Project5f;
  RENAME Region_of_bone_affected__D_M_A_=Region_of_bone_affected
                   __of_bone_length_affected_by_tum=Percentage_of_bone_length
                   volume_using_area_via_freehand_R=Volume_using_area
                   Degrees_of_Cortex_affected_by_ly=Degrees_of_Cortex
                   Proliferative_vs_lytic__1_all_pr= Proliferative_vs_lytic
                   Severity_of_lysis__0_none__5_sev=Severity_of_lysis
                   Time_to_fracture__days__or_date_=Time_to_fracture
                   Survival_time__days___if_LTFU__d= Survival_time
                   Patient_Status_as_of_1_7_14=Patient_Status;
RUN;
/* MANIPULATE VARIABLES*/
DATA Project5h;
  SET Project5g;
  IF Region_of_bone_affected="D,M,A" THEN Region_of_bone_affected1=1;
  IF Region_of_bone_affected="D, M, A" THEN Region_of_bone_affected1=1;
  IF Region_of_bone_affected="M, A" THEN Region_of_bone_affected1=1;
  IF Region_of_bone_affected1=. THEN Region_of_bone_affected1=0;
  IF Patient_Status=1 THEN Patient_Status1=1;
  IF Patient_Status1=. THEN Patient_Status1=0;
  IF Sex="F" THEN Sex1=0;
  IF Sex="FS" THEN Sex1=0;
  IF Sex1=. THEN Sex1=1;
RUN;
/* DESCRIBE FINAL DATASET */
PROC CONTENTS DATA=Project5h;
RUN;

/* SUMMARIZE CONTINUOUS VARIABLES*/
PROC MEANS DATA=Project5h MEAN MEDIAN MIN MAX RANGE STD;
  VAR Percentage_of_bone_length Percentage_of_bone_affected Volume_using_area Degrees_of_Cortex Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br 
      Proliferative_vs_lytic Severity_of_lysis Time_to_fracture Survival_time Age;
RUN;
PROC UNIVARIATE DATA=Project5h;
  VAR Percentage_of_bone_length Percentage_of_bone_affected Volume_using_area Degrees_of_Cortex Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br 
      Proliferative_vs_lytic Severity_of_lysis Time_to_fracture Survival_time Age;
  HISTOGRAM Percentage_of_bone_length Percentage_of_bone_affected Volume_using_area Degrees_of_Cortex Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br 
      Proliferative_vs_lytic Severity_of_lysis Time_to_fracture Survival_time Age / KERNEL;
RUN;
/* SUMMARIZE CATEGORICAL VARIABLES*/
PROC FREQ DATA=Project5h;
  TABLE Body_Part Region_of_bone_affected1 Fracture_status Patient_Status1 Sex1;
RUN;
/* KAPLAN-MEIER ESTIMATOR*/
PROC LIFETEST DATA=Project5h TIMELIST=90,180,270 PLOTS=SURVIVAL(CL);
  TIME Time_to_fracture*Fracture_status(0);
  TITLE "Time_to_fracture";
RUN;
PROC LIFETEST DATA=Project5h TIMELIST=90,180,270 PLOTS=SURVIVAL(CL);
  TIME Survival_time*Patient_Status1(0);
  TITLE "Survival_time";
RUN;

/* COX REGRESSION*/
/* MACRO FOR COX REGRESSION WITH A CONTINUOUS VARIABLE*/
%MACRO COX1(variable);
PROC PHREG DATA=Project5h;
   MODEL Time_to_fracture*Fracture_status(0)=&variable;
   TITLE "COX model with &variable";
  RUN;
%MEND;
%COX1(Percentage_of_bone_length);
%COX1(Volume_using_area );
%COX1(Degrees_of_Cortex );
%COX1(Degrees_of_complete_lysis );
%COX1(Decrees_of_complete_lysis_not_br );
%COX1(Proliferative_vs_lytic );
%COX1(Severity_of_lysis );
%COX1(Age );
/* MACRO FOR COX REGRESSION WITH A CATEGORICAL VARIABLE*/
%MACRO COX2(variable);
PROC PHREG DATA=Project5h;
   CLASS &&variable/ PARAM=REF REF=FIRST;
   MODEL Time_to_fracture*Fracture_status(0)=&variable;
   TITLE "COX model with &variable";
  RUN;
%MEND;
%COX1(Body_Part);
%COX1(Region_of_bone_affected1);
%COX1(Sex1);

PROC PHREG DATA=Project5h;
  CLASS Body_Part Region_of_bone_affected1 Sex1 / PARAM=REF REF=FIRST;
  MODEL Time_to_fracture*Fracture_status(0)=Percentage_of_bone_length Volume_using_area Degrees_of_Cortex  Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br Proliferative_vs_lytic Severity_of_lysis Age / SELECTION=FORWARD SLENTRY=0.2;
  TITLE;
RUN;
PROC PHREG DATA=Project5h;
  CLASS Body_Part Region_of_bone_affected1 Sex1 / PARAM=REF REF=FIRST;
  MODEL Time_to_fracture*Fracture_status(0)=Percentage_of_bone_length Volume_using_area Degrees_of_Cortex  Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br Proliferative_vs_lytic Severity_of_lysis Age / SELECTION=BACKWARD SLSTAY=0.2;
  TITLE;
RUN;
PROC PHREG DATA=Project5h;
  CLASS Body_Part Region_of_bone_affected1 Sex1 / PARAM=REF REF=FIRST;
  MODEL Time_to_fracture*Fracture_status(0)=Percentage_of_bone_length Volume_using_area Degrees_of_Cortex  Degrees_of_complete_lysis Decrees_of_complete_lysis_not_br Proliferative_vs_lytic Severity_of_lysis Age / SELECTION=STEPWISE SLENTRY=0.2 SLSTAY=0.2;
  TITLE;
RUN;
PROC PHREG DATA=Project5h;
  HAZARDRATIO Volume_using_area / CL=WALD;
  HAZARDRATIO Severity_of_lysis / CL=WALD;
  HAZARDRATIO Age / CL=WALD;
  MODEL Time_to_fracture*Fracture_status(0)=Volume_using_area Severity_of_lysis Age;
  TITLE;
RUN;

/* LOGRANK TEST*/
/* CEATE CATEGORICAL VARIABLES*/
DATA Project5i;
  SET Project5h;
  IF Severity_of_lysis>3 THEN Severity_of_lysis1=1;
  IF Severity_of_lysis1=. THEN Severity_of_lysis1=0;
  TITLE;
RUN;
PROC LIFETEST DATA=Project5i PLOTS=SURVIVAL;
  STRATA Severity_of_lysis1 / TEST=LOGRANK;
  TIME Time_to_fracture*Fracture_status(0);
RUN;
