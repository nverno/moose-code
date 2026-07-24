/***********************************************************************
 * seedpref-and-substrate.sas  (nverno/moose-code) -- Figure 6 slice
 *
 * Self-contained excerpt of the seed-mass / Chesson's-index regression.
 * The seedmass DATALINES block, the put()/best8. reformat, the PROC GLM
 * model, and the PROC PRINTs are exactly as in the upstream script.
 * Only AVELM (mean Chesson's index per species, derived upstream from the
 * Segment/Transect field libraries) is supplied here as a small mock so
 * the regression runs standalone.
 ***********************************************************************/

/* --- mock stand-in for the upstream-derived AVELM (spec, ave_chesson) --- */
data avelm;
  input spec $4. ave_chesson;
  datalines;
ABBA 0.71
ACSA 0.34
BEAL 0.55
BECO 0.62
PIRU 0.48
SOAM 0.58
;
run;

/* --- verbatim from seedpref-and-substrate.sas (Fig 6) --- */
data seedmass;
input spec $4. seedmass;
datalines;
BECO 0.3289
BEAL 1.0101
SOAM 2.8571
PIRU 3.3333
ABBA 7.6923
ACSA 64.616
;RUN;
PROC PRINT DATA=SEEDMASS;RUN;
DATA SEEDMASS; SET SEEDMASS;
seedmass_C=put(seedmass, best8.);PROC SORT; BY spec;RUN;
PROC PRINT DATA=SEEDMASS;RUN;
DATA FIG6; MERGE AVELM SEEDMASS; BY SPEC;PROC SORT; BY SEEDMASS;RUN;
proc glm data=fig6;
model ave_chesson=seedmass/solution;
output out=fig6_fitted predicted=pred;run;
