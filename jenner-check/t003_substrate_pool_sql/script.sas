/***********************************************************************
 * seedpref-and-substrate.sas  (nverno/moose-code) -- substrate-pool slice
 *
 * Exercises the two-level PROC SQL substrate-coverage aggregation verbatim
 * from the upstream script: SUBELEV averages each cover type (as a
 * fraction of 100) by census and elevation class, SUBELEVPOOL then pools
 * those means by elevation class, and two DATA steps normalise the moss
 * vs. litter and LITD vs. LITC shares. The PROC SQL SELECTs and the
 * normalisation DATA steps are exactly the author's. Only QUADSUB -- the
 * quadrat substrate table built upstream from the Segment/Transect field
 * libraries -- is supplied here as a small mock matching the columns the
 * SQL reads.
 ***********************************************************************/

/* --- mock stand-in for the upstream-derived QUADSUB --- */
data quadsub;
  input cens elevcl $ mssg litt litd litc bla5 bld5 wdg bsoil rck water tipa stpa;
  datalines;
1989 1_L 42 38 20 18 5 3 2 4 6 1 2 1
1989 1_L 55 25 12 13 4 2 1 3 5 0 1 1
1998 2_M 30 50 28 22 6 4 3 5 4 2 1 2
1998 2_M 38 44 24 20 5 3 2 4 3 1 2 1
1999 3_H 20 62 35 27 8 5 4 6 3 3 2 2
1999 3_H 26 58 32 26 7 4 3 5 2 2 1 1
;
run;

/* --- verbatim PROC SQL substrate aggregation from seedpref-and-substrate.sas --- */
proc sql; create table subelev as select
cens,elevcl,mean(mssg)/100 as avemss,mean(litt)/100 as avelitt, MEAN(LITD)/100
AS AVELITD, MEAN(LITC)/100 AS AVELITC,mean(bla5)/100 as avebla5,
mean(bld5)/100 as avebld5,mean(wdg)/100 as avewdg,
mean(bsoil)/100 as avebsoil, mean(rck)/100 as averck, mean(water)/100 as
avewater, mean(tipa)/100 as avetip, mean(stpa)/100 as avestp
from quadsub group by cens, elevcl;quit;
proc sql; create table subelevpool as select
elevcl,mean(avemss) as MSS, mean(avelitt) as LITT, MEAN(AVELITD) AS LITD,
MEAN(AVELITC) AS LITC, mean(avebla5) as BLA5, MEAN(AVEBLD5) AS BLD5,
MEAN(AVEWDG) AS WDG,
MEAN(AVEBSOIL) AS BSOIL, MEAN(AVERCK) AS RCK, MEAN(AVEWATER) AS WATER,
MEAN(AVETIP) AS TIP, MEAN(AVESTP) AS STP
from subelev group by elevcl;quit;
data subelevpool; set subelevpool;
nmss=mss/(mss+litt);
nlitt=litt/(mss+litt);run;
data subelevpool_N; set subelevpool;
nlitd=litd/(litd+litc);
nlitc=litc/(litd+litc);run;

proc print data=subelevpool_N;run;
