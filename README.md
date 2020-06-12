# DNR-Reversal

libname dnr 'C:\Users\mehtaan\Documents\Research\DNR\2016-2018';
libname oshpd 'C:\Users\mehtaan\Documents\Research\OSHPD\Files';
libname icd10 'C:\Users\mehtaan\Documents\Research\icd10';
data merged;
set oshpd.pdd2016 oshpd.pdd2017 oshpd.pdd2018;
if rln = '---------' then delete; /**deleting invalid RLN**/
if rln = ' ' then delete; /**deleting missing RLN**/
run;
data merged; set merged; 
if surg1=1 then delete; run; /**delete surgical patients and those presenting from ambulatory surgical centers 8462379->5887761**/
data merged; set merged;
if age<40 then delete; run; /**delete age<40 5887761->4530728**/
data merged; set merged;
if dnr1=. then delete; run; /**deleting missing DNR information 4530728->4529635**/
proc freq data=merged;
table dnr1; run; /**10.88% of hospitalizations with DNR order**/
proc freq data=merged;
table oshpd_id*year; run;
/**calculate hospital DNR counts**/
proc freq data=merged noprint;
table oshpd_id/nocum out=totalhosp; run;
data totalhosp; set totalhosp;
drop percent; run;
proc freq data=merged noprint;
where dnr1=1;
table oshpd_id/nocum out=dnrhosp; run;
data dnrhosp; set dnrhosp;
drop percent;
rename count=dnrcount; run;
proc univariate data=dnrhosp; var dnrcount; run;
proc sort data=totalhosp; by oshpd_id; run;
proc sort data=dnrhosp; by oshpd_id; run;
data dnr.crudehospital; 
merge totalhosp dnrhosp; by oshpd_id;
if dnrcount=. then dnrcount=0;
dnrrate=(dnrcount/count)*100; run;
proc univariate data=dnr.crudehospital;
var dnrcount dnrrate; run;
proc sort data=merged; by oshpd_id; run;
data merged;
merge merged dnr.crudehospital; by oshpd_id; 
run;
data merged2; set merged; 
if dnrcount<25 then delete; run; /**excluding patients admitted to hospitals with <25 DNR orders 4529635->4386008**/
data dnr.crudehospital; set dnr.crudehospital; if dnrcount<25 then delete; run; /**439 -> 353 hospitals**/
proc univariate data=dnr.crudehospital; var dnrcount; run;
proc univariate data=dnr.crudehospital; var dnrrate; run;
data merged2; set merged2;
if dnrrate=. then dnrq=.;
else if dnrrate<4.676203 then dnrq=1;
else if 4.676203<=dnrrate<10.719958 then dnrq=2;
else if 10.719958<=dnrrate<19.585350 then dnrq=3;
else if dnrrate>=19.585350 then dnrq=4;
run;
data dnr.crudehospital;
set dnr.crudehospital;
if dnrrate=. then dnrq=.;
else if dnrrate<4.676203 then dnrq=1;
else if 4.676203<=dnrrate<10.719958 then dnrq=2;
else if 10.719958<=dnrrate<19.585350 then dnrq=3;
else if dnrrate>=19.585350 then dnrq=4;
run;
data merged2; set merged2;
if patcnty = '00' then delete;
run; /**after excluding non-CA residents 4240214 eligible hospitalizations**/

proc glimmix data=merged2; 
title 'Risk-Adjusted DNR rate';
class oshpd_id ethnic (ref=first) payer (ref=first);
model dnr1 (event=last) = age gender ethnic payer
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
shock neurofail hemefail acid hepafail renalfail arf
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id s;
covtest glm/cl;
run;
proc means data=dnr.crudehospital mean median q1 q3; var dnrrate; run; /**mean hospital DNR rate 13.9, median 10.7**/
/**IDENTIFY INCIDENT DNR ADMISSION FOR EACH PATIENT WITH ONE**/
data dnr; set merged2; 
if dnr1=0 then delete;
run; /**485264 hospitalizations with early DNR order**/
proc sort data=dnr; by rln admtdate dschdate; run;
data dnr; set dnr;
by rln;
if first.rln then first=1; else first=0;
run;
proc print data=dnr (obs=100); var rln admtdate dschdate first; run;
data dnr; set dnr (keep=rln admtdate dschdate first);
if first=0 then delete; 
if dschdate>21520 then delete; 
anydnr=1; 
rename admtdate=dnradmtdate;
rename dschdate=dnrdschdate;
drop first; run; /** identifying index DNR admissions between 1/1/16 - 12/1/18:   347292**/
proc sort data=dnr; by rln; run;
proc sort data=merged2; by rln; run;
data dnr2; 
merge merged2 dnr; by rln; run;
data dnr2; set dnr2; 
if admtdate=dnradmtdate and dschdate=dnrdschdate then index=1; else index=0;
if anydnr=. then anydnr=0; 
run;
proc sort data=dnr2; by rln admtdate dschdate; run;
proc print data=dnr2 (obs=200); var rln admtdate dnradmtdate index; run;
data dnr3; set dnr2;
if anydnr=0 then delete;
if admtdate<dnradmtdate then before=1; else before=0;
if before=1 then delete; 
run;
proc sort data=dnr3; by rln admtdate dschdate; run;
proc print data=dnr3 (obs=200); var rln dnradmtdate admtdate dschdate dnr1 anydnr; run;
data dnr4; set dnr3;
by rln;
if first.rln then hospnumber=0;
hospnumber+1;
if hospnumber>2 then delete; 
run;
proc print data=dnr4 (obs=50); var rln hospnumber dnradmtdate admtdate dschdate dnr1 anydnr; run;
proc sort data=dnr4; by rln descending admtdate; run;
proc freq data=dnr4; table mv trach niv cpr hf sepsis shock hemefail copd pna; run;
data dnr5; set dnr4;
by rln;
lagadmtdate=lag(admtdate);
if first.rln then lagadmtdate=.;
format lagadmtdate mmddyy10.;
lagdschdate=lag(dschdate);
if first.rln then lagdschdate=.;
format lagdschdate mmddyy10.;
lagdisphospice=lag(disphospice);
if first.rln then lagdisphospice=.;
lagdisphome=lag(disphome);
if first.rln then lagdisphome=.;
laglos=lag(los);
if first.rln then laglos=.;
lagdnr1=lag(dnr1);
if first.rln then lagdnr1=.;
lagdied=lag(died);
if first.rln then lagdied=.;
lagdnrq=lag(dnrq);
if first.rln then lagdnrq=.;
lagmv=lag(mv);
if first.rln then lagmv=.;
lagniv=lag(niv);
if first.rln then lagniv=.;
laghd=lag(hd);
if first.rln then laghd=.;
lagcpr=lag(cpr);
if first.rln then lagcpr2=.;
lagchf=lag(chf);
if first.rln then lagchf=.;
lagvalve=lag(valve);
if first.rln then lagvalve=.;
lagpulmcirc=lag(pulmcirc);
if first.rln then lagpulmcirc=.;
lagperivasc=lag(perivasc);
if first.rln then lagperviasc=.;
lagPARA=lag(para);
if first.rln then lagpara=.;
lagNEURO=lag(neuro);
if first.rln then lagneuro=.;
lagCHRNLUNG=lag(chrnlung);
if first.rln then lagchrnlung=.;
lagDM=lag(dm); 
if first.rln then lagdm=.;
lagDMCX=lag(dmcx);
if first.rln then lagdmcx=.;
lagHYPOTHY=lag(hypothy);
if first.rln then laghypothy=.;
lagRENLFAIL=lag(renlfail);
if first.rln then lagrenlfail=.;
lagLIVER=lag(liver);
if first.rln then lagliver=.;
lagULCER=lag(ulcer);
if first.rln then lagulcer=.;
lagAIDS=lag(aids);
if first.rln then lagaids=.;
lagLYMPH=lag(lymph);
if first.rln then laglymph=.;
lagMETS=lag(mets);
if first.rln then lagmets=.;
lagTUMOR=lag(tumor);
if first.rln then lagtumor=.;
lagARTH=lag(arth);
if first.rln then lagarth=.;
lagCOAG=lag(coag);
if first.rln then lagcoag=.;
lagOBESE=lag(obese);
if first.rln then lagobese=.;
lagWGHTLOSS=lag(wghtloss);
if first.rln then lagwghtloss=.;
lagLYTES=lag(lytes);
if first.rln then laglytes=.;
lagBLDLOSS=lag(bldloss);
if first.rln then lagbldloss=.;
lagANEMDEF=lag(anemdef);
if first.rln then laganemdef=.;
lagALCOHOL=lag(alcohol);
if first.rln then lagalcohol=.;
lagDRUG=lag(drug);
if first.rln then lagdrug=.;
lagPSYCH=lag(psych);
if first.rln then lagpsych=.;
lagDEPRESS=lag(depress);
if first.rln then lagdepress=.;
lagHTN_C=lag(htn_c);
if first.rln then laghtn_c=.;
lagshock=lag(shock);
if first.rln then lagshock=.;
lagarf=lag(arf);
if first.rln then lagarf=.;
lagneurofail=lag(neurofail);
if first.rln then lagneurofail=.;
lagrenalfail=lag(renalfail);
if first.rln then lagrenalfail=.;
laghemefail=lag(hemefail);
if first.rln then laghemefail=.;
laghepafail=lag(hepafail);
if first.rln then laghepafail=.;
lagacid=lag(acid);
if first.rln then lagacid=.;
lagarf=lag(arf);
if first.rln then lagarf=.;
lagpna=lag(pna);
if first.rln then lagpna=.;
lagcopd=lag(copd);
if first.rln then lagcopd=.;
lagasthma=lag(asthma);
if first.rln then lagasthma=.;
laghf=lag(hf);
if first.rln then laghf=.;
lagsepsis=lag(sepsis);
if first.rln then lagsepsis=.;
lagoshpd=lag(oshpd_id);
if first.rln then lagoshpd=' ';
laghplzip=lag(hplzip);
if first.rln then laghplzip=' ';
lagpatzip=lag(patzip);
if first.rln then lagpatzip=' ';
run;
proc print data=dnr5 (obs=50);
where dschdate>=21520; var rln; run;
proc print data=dnr5 (obs=200);
var rln dnradmtdate admtdate lagadmtdate dschdate lagdschdate hplzip laghplzip patzip lagpatzip dnr1 lagdnr1 shock lagshock chrnlung lagchrnlung;
run;
data dnr.backup; set dnr5; run;
data dnr.dnr; set dnr.backup;
if index=0 then delete;
if dschdate>21520 then delete; run; /**342231 index DNR**/
proc freq data=dnr.dnr;
table index disphospice; run;
data dnr.dnr; set dnr.dnr;
if disphospice=1 then delete; run; /**delete individuals discharged to hospice after 342231->302151**/


data dnr.dnr; set dnr.dnr; /**creating readmit30d variable**/
if lagadmtdate=. then timetoreadmit=.;
else timetoreadmit=lagadmtdate - dschdate; 
if timetoreadmit=. then readmit30d=0;
else if 0<=timetoreadmit<=30 then readmit30d=1;
else readmit30d=0;
if lagoshpd=' ' then same=.;
else if lagoshpd=oshpd_id then same=1;
else same=0;
run;
proc freq data=dnr.dnr;
where died=0;
table readmit30d; run; /**49336 (19.9%) readmitted within 30 days**/
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table lagdnr1; run; /**27085 (54.9%) DNR on readmission**/
proc ttest data=dnr.dnr;
where died=0 and readmit30d=1;
class lagdnr1;
var age; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table (gender payer ethnic)*lagdnr1/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table disphome*lagdnr1/chisq; run;
proc means data=dnr.dnr n median q1 q3;
where died=0 and readmit30d=1;
class lagdnr1;
var elix; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table (lagshock lagneurofail lagrenalfail laghemefail laghepafail lagacid)*lagdnr1/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table (mv hd)*lagdnr1/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table (lagpna lagsepsis laghf lagcopd)*lagdnr1/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table lagdnr1*same/relrisk; run; /**61.5% DNR vs 40.4% at different hospital, OR=2.35 (2.27-2.45)**/
proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table lagdnr1*lagdnrq; run;

proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table lagmv*same laghd*same lagdied*same/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table lagdisphospice*same/chisq; run;
proc means data=dnr.dnr n mean std median q1 q3;
where died=0 and readmit30d=1 and lagdnr1=0 and lagdied=0;
class same;
var laglos; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table lagmv*lagdnrq laghd*lagdnrq lagdied*lagdnrq/chisq; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table laghospice*lagdnrq/chisq; run;
proc means data=dnr.dnr n mean std median q1 q3;
where died=0 and readmit30d=1 and lagdnr1=0 and lagdied=0;
class lagdnrq;
var laglos; run;
proc logistic data=dnr.dnr descending;
where died=0 and readmit30d=1;
model lagmv = same lagdnr1 same*lagdnr1; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table lagmv*same laghd*same lagdisphospice*same lagdied*same/chisq; run;
proc means data=dnr.dnr n mean std median q1 q3;
where died=0 and readmit30d=1 and lagdnr1=1 and lagdied=0;
class same;
var laglos; run;
libname dnr 'C:\Users\mehtaan\Documents\Research\DNR\2016-2018';

proc glimmix data=dnr.dnr ; 
title 'Random Effects for Risk Adjusted DNR Hospital Instability rate';
where died=0 and readmit30d=1;
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first) ndpublic (ref=first) dhcs_rural (ref=first);
model lagdnr1 = same age gender ethnic payer disphome 
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
mv hd
ndpublic dhcs_rural
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id s;
covtest glm/cl;
run;

proc freq data=dnr.dnr;
where died=0 and readmit30d=1;
table lagoshpd/nocum out=hosp; run;
data hosp; set hosp;
drop percent; run;
proc freq data=dnr.dnr;
where died=0 and readmit30d=1 and lagdnr1=0;
table lagoshpd/nocum out=unstable; run;
data unstable; set unstable;
drop percent; rename count=unstable; run;
proc sort data=hosp; by lagoshpd; run; 
proc sort data=unstable; by lagoshpd; run;
data dnr.random; 
merge hosp unstable; by lagoshpd; 
if unstable=0 then unstablert=0;
else unstablert=(unstable/count)*100; run;
proc univariate data=dnr.random;
var unstablert; run;
proc glimmix data=dnr.dnr ; 
title 'Glimmix: Association of Different Hospital on DNR Status on Readmission';
where died=0 and readmit30d=1;
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first);
model lagdnr1 = same age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
mv hd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id s;
covtest glm/cl;
run;

proc glimmix data=dnr.dnr ; 
title 'Glimmix: Association of Different Hospital Quartile on DNR Status on Readmission';
where died=0 and readmit30d=1;
class oshpd_id lagdnrq (ref=last) ethnic (ref=first) payer (ref=first);
model lagdnr1 = lagdnrq age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
mv hd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id;
covtest glm/cl;
run;

proc sort data=rsdnr; by subject; run;
proc sort data=rsunstable; by subject; run;
data dnr.rsDNR; 
merge rsdnr rsunstable; by subject; run;

proc corr data=dnr.rsDNR spearman;
var ra_dnr rs_unstable; run;


proc glimmix data=dnr.dnr ; 
where died=0 and readmit30d=1 and lagdnr1=0;
title 'Odds of MV at different vs same hospital for people who have DNR instability';
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first);
model lagmv (event=last) = same age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id;
covtest glm/cl;
run;
proc glimmix data=dnr.dnr ; 
where died=0 and readmit30d=1 and lagdnr1=0;
title 'Odds of RRT at different vs same hospital for people who have DNR instability';
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first);
model laghd (event=last) = same age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id;
covtest glm/cl;
run;
proc glimmix data=dnr.dnr ; 
where died=0 and readmit30d=1 and lagdnr1=0;
title 'Odds of Discharge to Hospice at different vs same hospital for people who have DNR instability';
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first);
model lagdisphospice (event=last) = same age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id;
covtest glm/cl;
run;
proc glimmix data=dnr.dnr ; 
where died=0 and readmit30d=1 and lagdnr1=0;
title 'Odds of Hospital Death at different vs same hospital for people who have DNR instability';
class oshpd_id same (ref=last) ethnic (ref=first) payer (ref=first);
model lagdied (event=last) = same age gender ethnic payer disphome
CHF VALVE PULMCIRC PERIVASC PARA NEURO CHRNLUNG DM DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS 
LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS HTN_C 
lagshock lagarf lagneurofail lagrenalfail laghemefail laghepafail lagacid
lagpna lagsepsis laghf lagcopd
/ ddfm=bw link=logit dist = binary solution;
random intercept / subject=oshpd_id;
covtest glm/cl;
run;

