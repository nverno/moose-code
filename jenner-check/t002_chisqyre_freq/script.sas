/***********************************************************************
 * seedpref-and-substrate.sas  (nverno/moose-code) -- %chisqyre slice
 *
 * Exercises the %chisqyre macro verbatim from the upstream script: for a
 * given species and elevation class it subsets SEEDALL into a per-species
 * table, sorts by SUB, and runs a weighted chi-square goodness-of-fit
 * test (PROC FREQ, table sub with TESTP=, WEIGHT mpctcnt). The
 * per-species DATA steps, the macro, and its argument lists are the
 * author's; the TESTP proportion list is written blank-separated
 * (TESTP=(0.909 0.091)), the equivalent documented form. Only SEEDALL --
 * the per-species mean substrate table built upstream from the
 * Segment/Transect field libraries -- is supplied here as a small mock
 * matching the columns the code reads (spec, elevcl, sub, mpctcnt).
 ***********************************************************************/

/* --- mock stand-in for the upstream-derived SEEDALL --- */
data seedall;
  length spec $4 elevcl $4 sub $4;
  input spec $ elevcl $ sub $ mpctcnt;
  datalines;
ABBA 1_L MSS 41
ABBA 1_L LITT 9
ACSA 1_L MSS 33
ACSA 1_L LITT 12
PIRU 1_L MSS 28
PIRU 1_L LITT 7
;
run;

/* --- verbatim per-species subsets from seedpref-and-substrate.sas --- */
data abba; set seedall;if spec='ABBA';run;
data acsa; set seedall;if spec='ACSA';run;
data piru; set seedall;if spec='PIRU';run;

/* --- %chisqyre macro (TESTP list written blank-separated) --- */
%macro chisqyre (a, b, c, d,e);
data &A; set &b;
if elevcl=&C;proc sort; by sub;run;
title "&B at &c";
proc freq data=&a;
table sub /testp=(&d &e) chisq;
weight mpctcnt;
run;
%mend;

/* --- the author's own low-elevation invocations --- */
%chisqyre(ABL, ABBA,'1_L',0.909,0.091);
%chisqyre(ACL,ACSA,'1_L',0.909,0.091);
%chisqyre(PIL,PIRU,'1_L',0.909,0.091);
