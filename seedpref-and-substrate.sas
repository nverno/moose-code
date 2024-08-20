/***************This is the final code analyzing seedling preference and
substrate availability****************/
/***************We decided to focus only on "seedlings" that were less or
equal to 20cm tall for
/***********Lixi Kong 9/30/2023*******************/
libname seg11 'Segment2011';
libname tr11 'Transect2011';
libname soil 'SegTrPPSoil';
libname gps 'GPS_ELEV';
libname subp 'SubstratePaper';
ods graphics on;
/**************************Subset live seedling which we collected substrate
it grow on and plots where we collected substrate
coverage********************/
/**************************Substrate seeding data grow on collected in 1988,
1989, 1998, 1999*******************/
data seesapmas11; set seg11.seesapmas11;
if yrtag=1998 and ht98 ne . and sub98='' and sub99 ne '' then sub98=sub99;
if yrtag=1998 and ht98 ne . and sub99 ne '' and ht99=. then ht99=ht98;run;
%macro seedsub (a);
data segseed&a (keep=contnam stpace tag yrtag spec year year&a ht&A pht sub&a
sub sub2 elevcl aspcl); set seesapmas11;
if stat&a='ALIVE' and sub&a ne '' and spec ne '' ;
if ht&a gt 0 and ht&a le 20;
pht=ht&a;
sub=sub&a;
if sub&a='LITC' or sub&a='LITM' or sub&a='LITD' then sub='LITT';
sub2=sub&a;
year=1900+&a;
year&a=1;
proc sort; by contnam stpace tag;
run;
proc sql; create table sum&a as select
spec,elevcl, aspcl, count(distinct tag) as cnt&a from segseed&a group by spec,
elevcl, aspcl;quit;
proc sql; create table sum_ml_&a as select
spec,elevcl, aspcl, count(distinct tag) as cnt&a from segseed&a where sub in
('MSS', 'LITT') group by spec, elevcl, aspcl;quit;
%mend;
%seedsub(88); %seedsub(89);%seedsub(98); %seedsub(99);
data samplesize; merge sum88 sum89 sum98 sum99; by spec elevcl aspcl;run;
data samplesize_ML; merge sum_ml_88 sum_ml_89 sum_ml_98 sum_ml_99; by spec
elevcl aspcl;run;
/**************Rename 1989 tag number becasue it has duplicated tag numbers
with 1999, which will cause problem*************/
data segseed89; set segseed89;
tagn=input(cats('999999',tag), best8.);drop tag;run;
data segseed89; set segseed89; tag=tagn; drop tagn;run;
data segseedup(keep=contnam stpace tag spec segseedup year98 year99); merge
segseed98 segseed99; by contnam stpace tag;
if year98=1 and year99=1 then segseedup='98_99';run;
data segseed9899; set segseed98 segseed99;
plotid=cats(contnam,'S',stpace);
PROC SORT;by contnam stpace tag;run;
data segseed9899; merge segseed9899 segseedup;
by contnam stpace tag;
if spec ne 'ACSP' AND SPEC NE 'AMSPP' AND SPEC NE 'FAGR' AND SPEC NE 'PRPE';
IF SPEC='SOAM' AND YEAR=1998 THEN DELETE;proc sort; by contnam stpace tag;run;
/*************If the same seedling had data in both 98 and 99, we should take
the substrate collected in 99***********************/
DATA SEGSEED9899; SET SEGSEED9899;
IF segseedup='98_99' and sub98 ne '' then delete;run;
data segseed; set segseed88 segseed89 segseed9899;run;
/
******************************************************************************
************************/
/***************substrat where HH transect seedings grew on collected in 1999
and 2011 *********************/
data seed99(keep=tran tplot tag yrtag spec stat99 sub99 subon99 sub sub2 ht99
pht aspcl year year99);set tr11.trseedmas11;
if stat99='ALIVE' and sub99 ne '' AND ASPCL NE 'S' ;
IF SUB99='1.5' THEN DELETE;
sub=sub99;year=1999;
if sub99='LITC' OR SUB99='LITM' then sub='LITT';
sub2=sub99;
YEAR99=1;
pht=ht99;if ht99 gt 0 and ht99 le 20;
proc sort; by tran tplot tag;
run;
data seed11(keep=tran tplot tag yrtag spec stat11 sub11 subon11 sub sub2 ht11
pht aspcl year year11);set tr11.trseedmas11;
if stat11='ALIVE' and sub11 ne '';
sub=sub11; year=2011;
if sub11='LITC' OR SUB11='LITD' OR SUB11='LITM' THEN SUB='LITT';
sub2=sub11;
year11=1;
pht=ht11;if ht11 gt 0 and ht11 le 20;
proc sort; by tran tplot tag;run;
data trseedup(keep=tran tplot tag trdup); merge seed99 seed11; by tran tplot
tag;
if year99=1 and year11=1 then trdup='99_11';run;
data trseed;set seed99 seed11;
plotid=cats(tran,'p',tplot);
ELEVCL='4_HH';
proc sort; by tran tplot tag;run;
data trseed; merge trseed trseedup; by tran tplot tag;
if spec='ABBA' OR SPEC='BECO';run;
data trseed; set trseed;
if trdup='99_11' and sub11 ne '' then delete;run;
/
******************************************************************************
***********************************************/
/************************Combine all seeding
data***********************************************/
data segtrseed1; set segseed trseed;
IF ASPCL NE 'S';
IF ELEVCL='H' THEN ELEVCL='3_H';
IF ELEVCL='M' THEN ELEVCL='2_M';
IF ELEVCL='L' THEN ELEVCL='1_L';
if sub='WDG5' OR SUB='WDG1' OR SUB='WDG2' THEN SUB='WDG';
IF SPEC='BESPP' AND ELEVCL in ('4_HH' '3_H') THEN DO;
BEFLAG=1;SPEC='BECO';end; /****add in 2023: any BESPP at H/HH should be
BECO****************/
/*if spec='BEAL' OR SPEC='BECO' THEN SPEC='BESPP'; */ /**************We might
need to change this***********/
proc sort; by plotid spec sub;run;
data checkbeco; set segtrseed1; where beflag=1;run;
/
******************************************************************************
*****************/
/**************This part below is not finished
yet**************************************/
/**********New in 2023, species avaliable in each segments**************/
data spec1; set segtrseed1; PROC SORT; BY CENS CONTNAME STPACE ELEVCL SPEC;
RUN;
DATA SPEC2; SET SPEC1; BY CENS CONTNAME STPACE ELEVCL SPEC; IF FIRST.SPEC;RUN;
proc sql; create table checkabba as select sub, count(distinct tag) as cnt
from segtrseed1 WHERE SPEC='ABBA' AND ELEVCL='1_L' group by sub;quit;
proc sql; create table tagsum as select
spec, /*elevcl,*/ yrtag, count(distinct tag) as cnt from segtrseed1 group by
spec, /*elevcl,*/ yrtag;quit;
/
******************************************************************************
**************************/
/***************Substrate data collected in 1989, 1998, and
1999***************************/
data squadsub; set seg11.submas03c;
plotid=cats(contnam,'S',stpace);proc sort; by contnam stpace;run;
%macro quad(a);
data squad&a(keep=contnam stpace sgdsp&a year&A);set squadsub;
if cens=&a;year&a=1;sgdsp&a=sgdsp;
proc sort; by contnam stpace sgdsp&a;run;
data Squad&a; set Squad&a; by contnam stpace sgdsp&a;if first.sgdsp&a;run;
%mend;
%quad(1989);%quad(1998);%quad(1999);
data segdup; merge squad1989 squad1998 squad1999;by contnam stpace;
if year1989=1 and year1998=1 and year1999=1 then segdup='89_98_99';
if year1989=. and year1998=1 and year1999=1 then segdup='98_99';
if year1989=1 and year1998=. and year1999=1 then segdup='89_99';run;
data segsub; merge squadsub segdup; by contnam stpace;run;
/
******************************************************************************
***************/
/*********************HH elevation substrate data collected in
2011***********************/
data trquadsub; set tr11.trsubmas11;
plotid=cats(tran,'p',tplot);
aspcl=substr(tran,1,1);drop qpos;
ELEVCL='4_HH';
proc sort; by cens aspcl;run;
/
******************************************************************************
******************/
/******************Combine all Substrate
data*****************************************************/
data quadsub_0; set squadsub trquadsub;
IF ELEVCL='H' THEN ELEVCL='3_H';
IF ELEVCL='M' THEN ELEVCL='2_M';
IF ELEVCL='L' THEN ELEVCL='1_L';
IF WDG5 NE . THEN WDG=WDG5;
IF WDG2 NE . THEN WDG=WDG2;
IF WDG1 NE . THEN WDG=WDG1;proc sort; by plotid elevcl aspcl;run;
/
******************************************************************************
*****************************/
/***********************Add elevation data from GPS master file to seeding
data and substrate data*********************************/
PROC SQL; CREATE TABLE ELEV AS SELECT
TRAN, TPLOT, CONTNAM, STPACE, MEAN(ELEV) AS ELEVA FROM GPS.ELEVMAS12 where
PPLOT =. GROUP BY TRAN,TPLOT, CONTNAM,STPACE ORDER BY
TRAN,TPLOT,CONTNAM,STPACE;QUIT;
DATA ELEV; SET ELEV;
IF TRAN NE '' THEN plotid=cats(tran,'p',tplot);
IF CONTNAM NE '' THEN plotid=cats(contnam,'S',stpace);
;PROC SORT; BY PLOTID;RUN;
data segtrseed; merge segtrseed1 elev; by plotid;if tag ne .;
IF INDEX(PLOTID, 'L') NE 0 THEN ELEVA=840;
IF INDEX(PLOTID, 'M') NE 0 THEN ELEVA=990;
IF INDEX(PLOTID, 'H') NE 0 THEN ELEVA=1140;proc sort; by year plotid spec
sub;run;
/*data subp.segtr_seed;set segtrseed;run;*/ /** Do I need this?****/
data quadsub; merge quadsub_0 elev;by plotid; if cens ne .;
IF INDEX(PLOTID, 'L') NE 0 THEN ELEVA=840;
IF INDEX(PLOTID, 'M') NE 0 THEN ELEVA=990;
IF INDEX(PLOTID, 'H') NE 0 THEN ELEVA=1140;
IF LITM NE . AND LITM NE 0 AND LITC NE . AND LITC NE 0 THEN LITM_C=1;
IF LITC NE . AND LITC NE 0 AND LITD NE . AND LITD NE 0 THEN LITC_D=1;
IF LITD NE . AND LITD NE 0 AND LITM NE . AND LITM NE 0 THEN LITM_D=1;
IF LITC NE 0 AND LITC NE . AND LITD=0 AND LITM=0 THEN LITC_ONLY=1;
IF LITD NE 0 AND LITD NE . AND LITC=0 AND LITM=0 THEN LITD_ONLY=1;
IF LITM NE 0 AND LITM NE . AND LITC=0 AND LITD=0 THEN LITM_ONLY=1;proc sort;
by cens plotid elevcl aspcl;run;
PROC SQL; CREATE TABLE QUADSUB_DIST AS SELECT
CENS, ELEVCL, SUM(LITM_C) AS LITM_C_CNT, SUM(LITC_D) AS LITC_D_CNT,
SUM(LITM_D) AS LITM_D_CNT,
SUM(LITC_ONLY) AS LITC_O_CNT, SUM(LITD_ONLY) AS LITD_O_CNT, SUM(LITM_ONLY) AS
LITM_O_CNT FROM QUADSUB GROUP BY CENS, ELEVCL;QUIT;
PROC SQL; CREATE TABLE QUADCNT AS SELECT
CENS, ELEVCL, COUNT(*) AS CNT FROM QUADSUB GROUP BY CENS, ELEVCL;QUIT;
/*data subp.segtr_sub; set quadsub;run;*/ /*Do I need this?****************/
/*proc sql; create table cntpl as select
count(distinct plotid) as count from quadsub;quit;*/
/
******************************************************************************
**********************************************************/
/**********************************************Chi-square test and Chesson's
index at ELEVCL
level*************************************************************/
/****For each year/elevation/spec, % of seedings that grow on different
substrates************************************/
proc sql ;create table seedelev1 as select
segtrseed.year, segtrseed.elevcl, segtrseed.spec,sub,n(tag) as count from
segtrseed
where (sub='MSS' or sub='LITT')
group by year, elevcl, spec,sub;quit;
proc sql; create table seedelev as select *, sum(count) as subtotal from
seedelev1 group by year, elevcl, spec;quit;
data seedelev; set seedelev;
seedpct=count/subtotal;
proc sort; by sub year;run;
data sublist(keep=sub seedpct); set seedelev; by sub;if first.sub;
seedpct=0;run;
proc sort data=seedelev; by year elevcl spec;run;
data spec(keep=year elevcl spec); set seedelev;by year elevcl spec; if
first.spec;run;
proc sql; create table list as select
* from spec, sublist ORDER by year, elevcl,spec, sub;quit;
proc sort data=seedelev; by year elevcl spec sub;run;
data seedall1; update list seedelev;by year elevcl spec sub;
if count=. then count=0;run;
/****New analysis in 2023 distinguishing LITD and LITC*******************/
proc sql ;create table seedelev2 as select
segtrseed.year, segtrseed.elevcl, segtrseed.spec,sub2,n(tag) as count from
segtrseed
where (sub2='LITC' or sub2='LITD')
group by year, elevcl, spec,sub2;quit;
proc sql; create table seedelev_n as select *, sum(count) as subtotal from
seedelev2 group by year, elevcl, spec;quit;
data seedelev_n; set seedelev_n;
seedpct=count/subtotal;
proc sort; by sub2 year;run;
data sublist2(keep=sub2 seedpct); set seedelev_n; by sub2;if first.sub2;
seedpct=0;run;
proc sort data=seedelev_n; by year elevcl spec;run;
data spec(keep=year elevcl spec); set seedelev_n;by year elevcl spec; if
first.spec;run;
proc sql; create table list_n as select
* from spec, sublist2 ORDER by year, elevcl,spec, sub2;quit;
proc sort data=seedelev_n; by year elevcl spec sub2;run;
data seedall_2; update list_n seedelev_n;by year elevcl spec sub2;
if count=. then count=0;run;
/*proc sql; create table seedelev
as select segtrseed.year, segtrseed.elevcl, segtrseed.spec,sub,n(tag) as
count,
calculated count/subtotal as SEEDPCT
from segtrseed,
(select year,elevcl,spec,n(tag) as subtotal from segtrseed group by
year,elevcl,spec ) as segtrseed2
where segtrseed.year=segtrseed2.year and segtrseed.elevcl=segtrseed2.elevcl
and segtrseed.spec=segtrseed2.spec
group by segtrseed.year,segtrseed.elevcl, segtrseed.spec,segtrseed.sub
order by segtrseed.sub,segtrseed.year;
quit;*/
/******************for each elevation/spec, average of % and count of seeding
growing on different substrate over different census years
Only looking at MOSS and LITTER***************************/
/****shall we recalculate the previous % only looking at moss and litter?Yes,
updated this**/
proc sql; create table seedall2 as select
elevcl, spec, sub,mean(seedpct) as mseedpct, mean(count) as mcount
from seedall1 where (sub='MSS' or sub='LITT') group by elevcl, sub,spec order
by elevcl,spec;quit;
proc sql; create table seedall as select
*,sum(mcount) as sumcount from seedall2 group by elevcl,spec;quit;
data seedall; set seedall;
mpctcnt=int(mseedpct*sumcount);run;
/***new in 2023 which distinguishes LITD and LITC*********/
proc sql; create table seedall_n2 as select
elevcl, spec, sub2,mean(seedpct) as mseedpct, mean(count) as mcount
from seedall_2 where (SUB2='LITD' OR SUB2='LITC') group by elevcl, sub2,spec
order by elevcl,spec;quit;
proc sql; create table seedall_N as select
*,sum(mcount) as sumcount from seedall_N2 group by elevcl,spec;quit;
data seedall_N; set seedall_N;
mpctcnt=int(mseedpct*sumcount);
proc sort; by elevcl spec;run;
/**********all substrate not excluding MOSS and LITTER combining
ELEVCL********************/
/*proc sql; create table seedall2_a as select
spec, sub,mean(seedpct) as mseedpct, mean(count) as mcount from seedall1 group
by sub,spec order by spec;quit;
proc sql; create table seedall_a as select
*,sum(mcount) as sumcount from seedall2_a group by spec;quit;
data seedall_a; set seedall_a;
mpctcnt=int(mseedpct*sumcount);run;*/
/**************************************************************************/
/**************************Average MSS or LITT coverage at each ELEVCL******/
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
/*****new in 2023 comparing LITD and LITC*********/
data subelevpool_N; set subelevpool;
nlitd=litd/(litd+litc);
nlitc=litc/(litd+litc);run;
/
******************************************************************************
***/
/***********All substrate type covarege combining
ELEVCL****************************/
/*data quadsub; set quadsub;
if wdg5 ne . and wdg1=. then wdg=wdg5;
if wdg5=. and wdg1 ne . then wdg=wdg1;
if wdg5=. and wdg1=. then wdg=0;
proc sql; create table subelev_a as select
cens,mean(mssg)/100 as avemss,mean(litt)/100 as avelitt, mean(bla5)/100 as
avebla5, mean(bld5)/100 as avebld5,mean(wdg)/100 as avewdg,
mean(bsoil)/100 as avebsoil, mean(rck)/100 as averck, mean(water)/100 as
avewater, mean(tipa)/100 as avetip, mean(stpa)/100 as avestp
from quadsub group by cens;quit;
proc sql; create table subelevpool_a as select
mean(avemss) as MSS, mean(avelitt) as LITT, mean(avebla5) as BLA5,
MEAN(AVEBLD5) AS BLD5, MEAN(AVEWDG) AS WDG,
MEAN(AVEBSOIL) AS BSOIL, MEAN(AVERCK) AS RCK, MEAN(AVEWATER) AS WATER,
MEAN(AVETIP) AS TIP, MEAN(AVESTP) AS STP
from subelev_a;quit;*/
/********A Pre-test we did: we tested whether BEAL and BECO has different
distribution on LITT and MSS,
basing on average % (not average count). The result is not significant, so
we’ll keep combing BEAL and BECO.***********************************/
/*data becobeal; set seedall_a;
if (spec='BECO' OR SPEC='BEAL') AND (SUB='LITT' OR SUB='MSS');
ADJCNT=MSEEDPCT*SUMCOUNT;PROC SORT; BY SUB;RUN;
PROC FREQ DATA=BECOBEAL;
TABLE SPEC*SUB/CHISQ;
WEIGHT ADJCNT;
RUN;*/
/**********Chi-square tests******************************/
data abba; set seedall;if spec='ABBA';run;
data acsa; set seedall;if spec='ACSA';run;
data bespp; set seedall;if spec='BESPP';run;
data piru; set seedall;if spec='PIRU';run;
data soam; set seedall;if spec='SOAM';run;
DATA BEAL; SET SEEDALL; IF SPEC='BEAL';RUN;
DATA BECO; SET SEEDALL; IF SPEC='BECO';RUN;
%macro chisqyre (a, b, c, d,e);
data &A; set &b;
if elevcl=&C;proc sort; by sub;run;
title "&B at &c";
proc freq data=&a;
table sub /testp=(&d,&e) chisq;
weight mpctcnt;
run;
%mend;
%chisqyre(ABL, ABBA,'1_L',0.909,0.091);%chisqyre(ABM, ABBA,'2_M',0.807,0.193);
%chisqyre(ABH, ABBA,'3_H',0.751,0.249);%chisqyre(ABHH,
ABBA,'4_HH',0.464,0.536);
%chisqyre(ACL,ACSA,'1_L',0.909,0.091);
%chisqyre(PIL,PIRU,'1_L',0.909,0.091);%chisqyre(PIM, PIRU,'2_M',0.807,0.193);
%chisqyre(SOL,SOAM,'1_L',0.909,0.091);%chisqyre(SOM, SOAM,'2_M',0.807,0.193);
%chisqyre(SOH, SOAM,'3_H',0.751,0.249);
%chisqyre(BETUL,bespp,'1_L',0.909,0.091);%chisqyre(BETUM,
bespp,'2_M',0.807,0.193);/*%chisqyre(BEH, bespp,'3_H',0.751,0.249);
%CHISQYRE(BEHH, bespp, '4_HH', 0.464,0.536);*/
%chisqyre(BEALl,BEAL,'1_L',0.909,0.091);%chisqyre(BEALM,
BEAL,'2_M',0.807,0.193);
%chisqyre(BECOL,BECO,'1_L',0.909,0.091);%chisqyre(BECOM,
BECO,'2_M',0.807,0.193);
%chisqyre(BECOH, BECO,'3_H',0.751,0.249);%CHISQYRE(BECOHH, beCO, '4_HH',
0.464,0.536);
/**********************************************************Chesson's
index*************************************************************************
**/
/*********************************Fig
5********************************************************/
proc transpose data=subelevpool out=subelevpoolt(rename=(col1=quadpct));
var nmss nlitt;
by elevcl;run;
data subelevpoolt; set subelevpoolt;
if _name_='nlitt' then sub='LITT';
if _name_='nmss' then sub='MSS';proc sort; by elevcl sub;run;
proc sort data=seedall; by elevcl sub;run;
data chesson; merge seedall subelevpoolt; by elevcl sub;
rn=mseedpct/quadpct;
if spec='PIRU' and elevcl='3_H' then delete;proc sort; by spec elevcl sub;run;
proc sql; create table chessona as select
*, sum(rn) as srn from chesson group by spec,elevcl;run;
data chessona; set chessona;
if sub='LITT' THEN SUB='2_LITT';
if sub='MSS' then sub='1_MSS';
chesson=rn/srn;
IF SPEC='ABBA' THEN ABBA=CHESSON;
IF SPEC='ACSA' THEN ACSA=CHESSON;
IF SPEC='BECO' THEN BECO=CHESSON;
IF SPEC='BEAL' THEN BEAL=CHESSON;
IF SPEC='BESPP' THEN BESPP=CHESSON;
IF SPEC='PIRU' THEN PIRU=CHESSON;
IF SPEC='SOAM' THEN SOAM=CHESSON;
if elevcl='4_HH' then elev_r='1224-1385';
if elevcl='1_L' then elev_r='840';
if elevcl='2_M' then elev_r='990';
if elevcl='3_H' then elev_r='1140';
PROC SORT; BY SUB SPEC ELEVCL ;run;
/****new in 2023 which looks at LITC and LITD***********/
proc transpose data=subelevpool_n out=subelevpool_nt(rename=(col1=quadpct));
var nlitd nlitc;
by elevcl;run;
data subelevpool_nt; set subelevpool_nt;
if _name_='nlitc' then sub2='LITC';
if _name_='nlitd' then sub2='LITD';proc sort; by elevcl sub2;run;
proc sort data=seedall_n; by elevcl sub2;run;
data chesson_N; merge seedall_N subelevpool_Nt; by elevcl sub2;
rn=mseedpct/quadpct;
if spec='PIRU' and elevcl='3_H' then delete;proc sort; by spec elevcl
sub2;run;
proc sql; create table chesson_na as select
*, sum(rn) as srn from chesson_n group by spec,elevcl;run;
data chesson_na; set chesson_na;
/*if sub='LITC' THEN SUB='2_LITT';
if sub='MSS' then sub='1_MSS';*/
chesson=rn/srn;
IF SPEC='ABBA' THEN ABBA=CHESSON;
IF SPEC='ACSA' THEN ACSA=CHESSON;
IF SPEC='BECO' THEN BECO=CHESSON;
IF SPEC='BEAL' THEN BEAL=CHESSON;
IF SPEC='BESPP' THEN BESPP=CHESSON;
IF SPEC='PIRU' THEN PIRU=CHESSON;
IF SPEC='SOAM' THEN SOAM=CHESSON;
if elevcl='4_HH' then elev_r='1224-1385';
if elevcl='1_L' then elev_r='840';
if elevcl='2_M' then elev_r='990';
if elevcl='3_H' then elev_r='1140';
PROC SORT; BY SUB2 SPEC ELEVCL ;run;
/*****************************************Figure
5***********************************************/
data anno;
length function style color $8;
retain xsys '5' ysys '2' when 'a' style 'solid';
function='move'; x=1; y=0.1; output;
function='bar'; x=1; y=0.5; color='white'; output;
color='black'; size=1;
function='move'; xsys='1'; x=0; ysys='2'; y=0.1; output;
function='draw'; xsys='B'; ysys='B'; x=+2; y=+0.5; output;
function='draw'; x=-4; y=+1; output;
function='draw'; xsys='1'; x=0; ysys='2'; y=0.5; output;
run;
/*data
anno;
length function style color $ 8;
retain xsys '5' ysys '2' when 'a' style 'solid';
function='move'; x=0; y=0.1; output;
function='bar'; x=0; y=0.4; color='white'; output;
color='black'; size=3;
function='move'; xsys='1'; x=0; ysys='2'; y=0.1; output;
function='draw'; xsys='B'; ysys='B'; x=-2; y=-2; output;
function='move'; xsys='1'; x=0; ysys='2'; y=0.4; output;
function='draw'; xsys='B'; ysys='B'; x=-2; y=-2; output;
run;*/
ods graphics/border=off width=800px height=500px ;
LEGEND1 FRAME;
goptions reset=all;
SYMBOL1 INTERPOL=JOIN VALUE=CIRCLE COLOR=BLACK HEIGHT=1.5;
SYMBOL2 INTERPOL=JOIN VALUE=CIRCLEFILLED COLOR=GREY HEIGHT=1.5;
SYMBOL3 INTERPOL=JOIN VALUE=DIAMONDFILLED COLOR=RED HEIGHT=1.5;
SYMBOL4 INTERPOL=JOIN VALUE=SQUAREFILLED COLOR=BLUE HEIGHT=1.5;
SYMBOL5 INTERPOL=JOIN VALUE=DIAMOND COLOR=BLACK HEIGHT=1.5;
SYMBOL6 INTERPOL=JOIN VALUE=TRIANGLEDOWN COLOR=BLACK HEIGHT=1.5;
SYMBOL7 INTERPOL=JOIN VALUE=SQUARE COLOR=BLACK HEIGHT=1.5;
SYMBOL8 INTERPOL=JOIN VALUE=CIRCLEFILLED COLOR=GREY HEIGHT=1.5;
AXIS1 LABEL=("ELEVCL");
AXIS2 LABEL=("Chesson's index") ORDER=(0 TO 0.1 BY 0.1,0.4 TO 1 BY 0.1);
title "Chesson's index by ELEVCL";
proc sgplot data=chessona(where=(sub='1_MSS')) SGANNO=ANNO;
scatter x=elev_R y=ABBA/MARKERATTRS=(SYMBOL=CIRCLE COLOR=BLACK SIZE=14)
NAME='ABBA' legendlabel='Abies balsamea';
series x=elev_R y=ABBA/LINEATTRS=(COLOR=BLACK);
scatter x=elev_R y=ACSA/MARKERATTRS=(SYMBOL=CIRCLEFILLED COLOR=BLACK SIZE=14)
NAME='ACSA' legendlabel='Acer saccharum';
series x=elev_R y=ACSA/LINEATTRS=(COLOR=GREY);
scatter x=elev_R y=BECO/MARKERATTRS=(SYMBOL=DIAMOND COLOR=BLACK SIZE=14)
NAME='BECO' legendlabel='Betula cordifolia';
series x=elev_R y=BECO/LINEATTRS=(COLOR=GREY);
scatter x=elev_R y=BEAL/MARKERATTRS=(SYMBOL=DIAMONDFILLED COLOR=GREY SIZE=14)
NAME='BEAL' legendlabel='Betula alleghaniensis';
series x=elev_R y=BEAL/LINEATTRS=(COLOR=GREY);
/*scatter x=elevcl y=BESPP/MARKERATTRS=(SYMBOL=DIAMOND COLOR=BLACK
SIZE=9)NAME='BESPP';
series x=elevcl y=BESPP/LINEATTRS=(COLOR=BLACK);*/
scatter x=elev_R y=PIRU/MARKERATTRS=(SYMBOL=TRIANGLEDOWN COLOR=BLACK
SIZE=14)NAME='PIRU' legendlabel='Picea rubens';
series x=elev_R y=PIRU/LINEATTRS=(COLOR=BLACK);
scatter x=elev_R y=SOAM/MARKERATTRS=(SYMBOL=SQUARE COLOR=BLACK
SIZE=14)NAME='SOAM' legendlabel='Sorbus americana';
series x=elev_R y=SOAM/LINEATTRS=(COLOR=BLACK);
refline 0.5/axis=y lineattrs=(pattern=mediumdash);
KEYLEGEND 'BEAL' 'BECO' 'PIRU' 'ABBA' 'SOAM' 'ACSA' /noborder position=right
valueattrs=(size=12 style=italic) AUTOITEMSIZE;
xaxis label="Elevation (m)" labelattrs=(size=14 ) valueattrs=(size=14) ;
yaxis label="Preference for Moss (Chesson index)" labelattrs=(size=14)
valueattrs=(size=14);run;
/
******************************************************************************
******************************************************************************
*************/
/***************************************************Fig
6*****************************************************/
data chesson_lm; set chessona;
where sub='1_MSS' /*and elevcl in ('1_L' '2_M')*/;run;
proc sql; create table avelm as select
spec, mean(chesson) as ave_chesson from chesson_lm group by SPEC ORDER BY
SPEC;quit;
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
ods graphics/width=700px height=600px;
PROC SGPLOT DATA=FIG6_fitted;
SERIES X=SEEDMASS Y=AVE_CHESSON/MARKERS lineattrs=(color=black)
markerattrs=(color=black size=11);
series x=seedmass y=pred/lineattrs=(pattern=mediumdash color=bgr);
YAXIS LABEL="Seeding preference for moss (Chesson's index)" VALUES=(0 TO 1 BY
0.2) labelattrs=(size=12.5) valueattrs=(size=14);
XAXIS LABEL="Seeding mass (mg)" values=(0 to 65 by 5) labelattrs=(size=14)
valueattrs=(size=14);RUN;
PROC SGPLOT DATA=FIG6;
SERIES X=SEEDMASS_C Y=AVE_CHESSON/MARKERS lineattrs=(color=black)
markerattrs=(color=black size=11);
YAXIS LABEL="Seeding preference for moss (Chesson's index)" VALUES=(0 TO 1 BY
0.2) labelattrs=(size=12.5) valueattrs=(size=14);
XAXIS LABEL="Seeding mass (mg)" labelattrs=(size=14) valueattrs=(size=14);RUN;
/***New in 2023 compares LITC and LITD*****************/
data chesson_lm_n; set chesson_na;
where sub2='LITC' /*and elevcl in ('1_L' '2_M')*/;run;
proc sql; create table avelm_N as select
spec, mean(chesson) as ave_chesson from chesson_lm_n group by SPEC ORDER BY
SPEC;quit;
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
DATA FIG6_N; MERGE AVELM_N SEEDMASS; BY SPEC;PROC SORT; BY SEEDMASS;RUN;
proc glm data=fig6_n;
model ave_chesson=seedmass/solution;
output out=fig6_fitted predicted=pred;;run;
ods graphics/width=700px height=600px;
PROC SGPLOT DATA=FIG6_N;
SERIES X=SEEDMASS Y=AVE_CHESSON/ MARKERS lineattrs=(color=black)
markerattrs=(color=black size=11);
YAXIS LABEL="Seeding preference for LITC (Chesson's index)" VALUES=(0 TO 1 BY
0.2) labelattrs=(size=12.5) valueattrs=(size=14);
XAXIS LABEL="Seeding mass (mg)" values=(0 to 65 by 5) labelattrs=(size=14)
valueattrs=(size=14);RUN;
/
******************************************************************************
*****************************************/
/**********************************************Logistic Regression at
individual
level*************************************************************************
***********/
/*data ndupseed; set segtrseed;
if segseedup='98_99' and year=1999 then delete;
if trdup='99_11' and year=2011 then delete;proc sort; by plotid;run;
proc sql; create table subpl as select
plotid, mean(mssg) as avemss,mean(litt) as avelit from quadsub group by
plotid;quit;
data subpl; set subpl;
nmss=100*avemss/(avemss+avelit);
nlit=100*avelit/(avemss+avelit);
run;
data logit; merge ndupseed subpl; by plotid;
if spec='PIRU' and elevcl='3_H' then delete;
if nmss ne . and tag ne . and (sub='MSS' or sub='LITT');
IF NMSS=0 THEN NMSS=0.1;
LNMSS=LOG(NMSS);
if pht gt 0;
NONPROB=NMSS;
nonpodds=nmss/(100-nmss);
if sub='MSS' then subb=0;
if sub='LITT' then subb=1;
LNONPODDS=LOG(NONPODDS);proc sort; by spec elevcl;run;
proc sql; create table logit1 as select
*,mean(nmss) as avemsselev from logit group by spec,eleva order by spec,eleva,
avemsselev;quit;
proc sql; create table logitn as select
*, mean(avemsselev) as avemsselevcl from logit1 group by spec, elevcl order by
spec,eleva;quit;*/
/*data logitn; set logitn;
if elevcl='4_HH' then avemsselev=nmss;run;*/
/*PROC LOGISTIC DATA=LOGIT;
MODEL SUB(EVENT='MSS')=LNMSS;
BY SPEC;
OUTPUT OUT=POWER PREDICTED=PRED;RUN;
DATA POWER; SET POWER;
PPROB=PRED*100;RUN;
PROC SGPANEL DATA=POWER;
PANELBY SPEC;
SCATTER X=NMSS Y=PPROB;
SERIES X=NMSS Y=NMSS;RUN;*/
/*proc logistic data=logitn(where=(spec ne 'ACSA'));
model sub(event='MSS')=eleva;
by spec;
output out=allsp1 pred=pred lower=low upper=up;run;
data allsp1;set bespp1 nbespp1;
low1=low*100;up1=up*100;
if sub='LITT' then litt1=PRED*100;
if sub='MSS' then mss1=PRED*100;
drop pred low up;
proc sort; by plotid tag;run;
proc logistic data=logitn(where=(spec ne 'ACSA' and spec ne 'BESPP'));
model sub(event='MSS')=eleva pht /eleva*pht/;
by spec;
output out=nbespp2 pred=pred lower=low upper=up;run;
proc logistic data=logitn(where=(spec = 'BESPP'));
model sub(event='MSS')=eleva pht eleva*pht;
output out=bespp2 pred=pred lower=low upper=up;run;
data allsp2; set nbespp bespp;
low2=low*100;up2=up*100;
if sub='LITT' then litt2=PRED*100;
if sub='MSS' then mss2=PRED*100;
drop pred low up;proc sort; by plotid tag;run;
data allsp; merge allsp1 allsp2; by plotid tag; proc sort; by spec eleva;run;
proc sql; create table allspn as select
*, max(up1) as maxup1, min(low1) as minlow1, max(up2) as maxup2, min(low2) as
minlow2 from allsp group by spec, eleva order by spec,eleva;quit;
ods graphics/width=700px height=1000px;
proc sgpanel data=allspn;
panelby spec/rows=4;
scatter x=eleva y=litt2/markerattrs=(color=black symbol=circle size=10);
scatter x=eleva y=mss2/markerattrs=(color=black symbol=circlefilled size=9)
transparency=0.4;
series x=eleva y=litt1/lineattrs=(color=black thickness=1);
series x=eleva y=mss1/lineattrs=(color=black thickness=1) transparency=0.4;
series x=eleva y=avemsselevcl/lineattrs=(color=black thickness=2) name='nopre'
curvelabel='No-Preference Line';
series x=eleva y=minlow2/lineattrs=(pattern=longdash color=dargr);
series x=eleva y=maxup2/lineattrs=(pattern=longdash color=dargr);
series x=eleva y=minlow1/lineattrs=(pattern=mediumdash color=grey);
series x=eleva y=maxup1/lineattrs=(pattern=mediumdash color=grey);
rowaxis label="Predicted probability(%)" values=(0 to 100 by 20);
keylegend;
run;*/
/*proc sgpanel data=nbespp;
panelby spec;
scatter x=pht y=pprob/group=sub;
run;
proc sgplot data=bespp;
scatter x=pht y=pprob/group=sub;
run;*/
/*PROC CORR DATA=logit PLOTS=ALL;
VAR PHT NMSS;
by spec;RUN;
PROC GLM DATA=logit PLOTS=ALL;
model PHT=NMSS;
by spec;RUN;
*/
/*proc glm data=logit(where=(spec ne 'ACSA')) plots=all;
model nmss=eleva;
by spec;
output out=msselev residual=mssresi;run;
proc glm data=msselev;
model pht=mssresi eleva mssresi*eleva;
BY SPEC;run;
PROC GLM DATA=MSSELEV;
MODEL mssresi=pht eleva pht*eleva;
by spec;run;
proc glm data=msselev;
model eleva=pht mssresi pht*mssresi;
by spec;run;*/
/*proc logistic data=LOGIT;
model sub(event='MSS')=NMSS;
by spec;
OUTPUT OUT=LOGITSIMP PRED=PPROB;run;
data logitsimp; set logitsimp;
IF SUB='LITT' THEN LITT=PPROB;
IF SUB='MSS' THEN MSS=PPROB;run;
ods graphics/width=800px height=900px;
PROC SGPANEL DATA=LOGITSIMP;
PANELBY SPEC;
SCATTER X=NMSS Y=LITT/MARKERATTRS=(SYMBOL=CIRCLE COLOR=BLACK SIZE=9)
name='LITT' LEGENDLABEL='LITT';
SCATTER X=NMSS Y=MSS/MARKERATTRS=(SYMBOL=CIRCLEFILLED COLOR=BLACK SIZE=9)
TRANSPARENCY=0.4 name='MSS' legendlabel='MSS';
SERIES X=NMSS Y=NONPROB;RUN;
proc sql; create table AVEPROB as select
spec,elevcl, mean(pprob) as aveprob from logitsimp group by
spec,elevcl;quit;*/
/*proc logistic data=LOGIT;
model sub(event='MSS')=PHT NMSS/rsq lackfit;
BY SPEC;
output out=logitspec xbeta=xbeta pred=PRED;run;*/
/*proc logistic data=msselev(where=(spec ne 'ACSA'));
model sub(event='MSS')= MSSRESI ELEVA PHT ELEVA*PHT MSSRESI*PHT MSSRESI*ELEVA
ELEVA*PHT*MSSRESI/SELECTION=stepwise;
by spec;
OUTPUT OUT=LOGITBESPP1 PRED=pred;RUN;
run;*/
/*proc probit data=msselev;
class subb;
model subb=nmss;
by spec;run;
proc logistic data=msselev;
model sub(event='MSS')=nmss/link=probit;
by spec;run;*/
/*proc logistic data=msselev(where=(spec ='BESPP'));
model sub(event='MSS')= MSSRESI ELEVA;
OUTPUT OUT=LOGITBESPP2 PRED=PPROB2;run;*/
/*proc sql; create table tjur as select
spec,sub, mean(pred) as aveprob from &a group by spec,sub;quit;
proc transpose data=tjur out=tjurt;
var aveprob;
by spec;
id sub;run;
data tjur&A; set tjurt;
tjur=mss-litt;run;*/
/*DATA BESPPOUT; MERGE LOGITBESPP1 LOGITBESPP2;BY PLOTID;
IF SPEC='BESPP';
IF SUB='LITT' THEN LITT=PPROB1;
IF SUB='MSS' THEN MSS=PPROB1; proc sort; by mssresi;RUN;
PROC SGPLOT DATA=BESPPOUT;
SCATTER X=MSSRESI Y=LITT/MARKERATTRS=(SYMBOL=CIRCLE COLOR=BLACK SIZE=9)
name='LITT' LEGENDLABEL='LITT';
SCATTER X=MSSRESI Y=MSS/MARKERATTRS=(SYMBOL=CIRCLEFILLED COLOR=BLACK SIZE=9)
TRANSPARENCY=0.4 name='MSS' legendlabel='MSS';
SERIES X=MSSRESI Y=PPROB2/GROUP=ELEVCL LINEATTRS=(PATTERN=MEDIUMDASH
COLOR=BLACK THICKNESS=2) name='simple' legendlabel='Fit from model with only
ResiMSS and PHT';
YAXIS LABEL="Predicted probability";
keylegend 'LITT' 'MSS' ;
RUN;*/
/*************Chesson's index*********************/
/*proc transpose data=subelevpool out=subelevpoolt(rename=(col1=quadpct));
var nmss nlitt;
by elevcl;run;
data subelevpoolt; set subelevpoolt;
if _name_='NLITT' then sub='LITT';
if _name_='NMSS' then sub='MSS';proc sort; by elevcl sub;run;
proc sort data=seedall; by elevcl sub;run;
data chesson; merge seedall subelevpoolt; by elevcl sub;
rn=mseedpct/quadpct;
if spec='PIRU' and elevcl='3_H' then delete;proc sort; by spec elevcl sub;run;
proc sql; create table chessona as select
*, sum(rn) as srn from chesson group by spec,elevcl;run;
data chessona; set chessona;
if sub='LITT' THEN SUB='2_LITT';
if sub='MSS' then sub='1_MSS';
chesson=rn/srn;
IF SPEC='ABBA' THEN ABBA=CHESSON;
IF SPEC='ACSA' THEN ACSA=CHESSON;
IF SPEC='BESPP' THEN BESPP=CHESSON;
IF SPEC='PIRU' THEN PIRU=CHESSON;
IF SPEC='SOAM' THEN SOAM=CHESSON;PROC SORT; BY SUB SPEC ELEVCL;run;
/*proc gplot data=chessona(where=(sub='1_MSS'));
plot (ABBA ACSA BECO BEAL BESPP PIRU SOAM)*ELEVCL/OVERLAY HAXIS=AXIS1
VAXIS=AXIS2 ANNO=ANNO;
LEGEND;RUN;*/*/
goptions reset=all;
%macro sp(a, b);
DATA &a; SET &a;
ODDS=EXP(XBETA);
PPROB=PRED*100;
IF SUB='MSS' THEN MSS=pprob;
IF SUB='LITT' THEN LITT=pprob;RUN;
proc sql; create table tjur as select
spec,sub, mean(pred) as aveprob from &a group by spec,sub;quit;
proc transpose data=tjur out=tjurt;
var aveprob;
by spec;
id sub;run;
data tjur&A; set tjurt;
tjur=mss-litt;run;
proc sgpanel data=&A(where=(spec=&b));
panelby elevcl;
scatter x=NMSS Y=MSS/markerattrs=(symbol=circlefilled color=grey size=9)
transparency=0.2;
scatter x=NMSS Y=LITT/markerattrs=(symbol=circle color=grey size=9);
lineparm x=0 y=0 slope=1/curvelabel="no preference line";
rowaxis label="Predicted probability" VALUES=(0 TO 100 BY 20);RUN;
data plot; set &a;
if spec=&b and plotid='LE1860S1840';RUN;
proc sgpanel data=PLOT;
panelby elevcl;
scatter x=PHT Y=MSS/markerattrs=(symbol=circlefilled color=grey size=9);
scatter x=PHT Y=LITT/markerattrs=(symbol=circle color=black size=9);
colaxis label="Initial height(cm)";
rowaxis label="Predicted probability" values=(0 to 80 by 20);RUN;
%mend;
/*%sp(logitbespp1,'ABBA');%sp(logitbespp1,'BESPP');%SP(logitbespp1,'PIRU');
%SP(logitbespp1,'SOAM');*/
ods graphics off;
