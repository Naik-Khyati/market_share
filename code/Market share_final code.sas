ODS PDF FILE='market share.pdf';

OPTIONS LINESIZE=78 NODATE NONUMBER;

DATA original;
INPUT y x1 x2 x3 x4;

LABEL y  = Mkt share (per cent)
      x1 = Avg monthly prices (per cent)
      x2 = GNRP index
      x3 = Discount
      x4 = Promotion;
ysq_inv=1/(y*y);

DATALINES;
3.15      2.198      498      1      1
2.52      2.186      510      0      0
2.64      2.293      422      1      1
2.55      2.420      858      0      1
2.69      2.179      566      1      0
2.38      2.207      536      0      0
3.02      2.127      585      1      1
2.52      2.206      310      1      0
2.45      2.305      211      0      0
2.42      2.260      504      0      1
3.16      2.205      234      1      1
2.60      2.340      347      0      0
2.98      2.171      430      1      0
2.50      2.201      518      0      1
2.45      2.248      465      0      0
3.06      2.184      684      1      1
2.34      2.373      152      0      0
2.88      2.157      453      1      1
2.94      2.129      485      1      1
2.72      2.557      78      1      0
2.27      2.587      72      0      1
2.33      2.255      391      0      0
2.64      2.124      322      1      0
2.76      2.683      317      1      1
3.05      2.336      252      1      1
2.48      2.266      446      0      1
2.23      2.443      521      0      0
2.65      2.478      435      1      1
2.56      2.394      402      1      0
2.66      2.414      468      1      1
2.99      2.233      262      1      0
2.30      2.302      182      0      1
2.88      2.421      145      1      1
2.80      2.518      270      1      0
2.48      2.497      322      0      1
2.85      2.781      317      1      1
;

/****************************/
/* linear relationship exists between dependent and independent variables */
/****************************/
proc gplot data=original;
 plot y*(x1 x2 x3 x4);
 symbol v=dot color=red;
 run  ;

/****************************/
/* untransformed model regression */
/****************************/
PROC REG DATA=original;
MODEL y=x1 x2 x3 x4 / P dw ;
RUN;

/****************************/
/* untransformed heteroskedestacity test - Modified levenes */
/****************************/

proc reg data= original;
 model y= x1 x2 x3 x4;
 output      out=temp      r=r;
 output out=temp2   p=yhat;
run;
data temp3 (drop=y x1 x2 x3 x4);
merge temp temp2;
run;
proc sort data=temp3 out=sorted;
key yhat/ascending;
run;
proc print data=sorted;
run;
data      sorted2;
            set      sorted;
            id      =      _n_;
            group      =      .;
            if      id      <=      18      then      group      =      1;
            if      id      >      18      then      group      =      2;
run;
proc sort data= sorted2;
by group;
run;
proc print data= sorted2;
run;
proc      means      data      =      sorted2      noprint;
            by      group;
            var      r;
            output      out=mout      median=mr;
run;
proc      print      data      =      mout;
var      group      mr;
run;
data      mtemp;
            merge      sorted2      mout;
            by      group;
            d      =      abs(r      - mr);
run;
proc      sort      data      =      mtemp;
            by      group;
run;
proc      means      data      =      mtemp      noprint;
            by      group;
            var      d;
            output      out=mout1      mean=md;
run;
proc      print      data      =      mout1;
            var      group      md;
run;
data      mtemp1;
            merge      mtemp      mout1;
            by      group;
            ddif      =      (d      - md)**2;
run;
proc      sort      data      =      mtemp1;
by      group      id;
run;
proc      ttest      data      =      mtemp1;
            class      group;
            var      d;
run;
proc      print      data      =      mtemp1;
by      group;
var      id      id      r      d      ddif;

run;

/*******************************************************/
/* Check for exponent for transformation using box cox*/
/*******************************************************/
ODS GRAPHICS ON;
PROC TRANSREG DATA = original TEST;
MODEL BOXCOX(y) = IDENTITY(x1 x2 x3 x4);
RUN;

/****************************/
/* transformed heteroskedestacity test - Modified levenes */
/****************************/

proc reg data= original;
 model ysq_inv= x1 x2 x3 x4;
 output      out=temp      r=r;
 output out=temp2   p=yhat;
run;
data temp3 (drop=y x1 x2 x3 x4);
merge temp temp2;
run;
proc sort data=temp3 out=sorted;
key yhat/ascending;
run;
proc print data=sorted;
run;
data      sorted2;
            set      sorted;
            id      =      _n_;
            group      =      .;
            if      id      <=      18      then      group      =      1;
            if      id      >      18      then      group      =      2;
run;
proc sort data= sorted2;
by group;
run;
proc print data= sorted2;
run;
proc      means      data      =      sorted2      noprint;
            by      group;
            var      r;
            output      out=mout      median=mr;
run;
proc      print      data      =      mout;
var      group      mr;
run;
data      mtemp;
            merge      sorted2      mout;
            by      group;
            d      =      abs(r      - mr);
run;
proc      sort      data      =      mtemp;
            by      group;
run;
proc      means      data      =      mtemp      noprint;
            by      group;
            var      d;
            output      out=mout1      mean=md;
run;
proc      print      data      =      mout1;
            var      group      md;
run;
data      mtemp1;
            merge      mtemp      mout1;
            by      group;
            ddif      =      (d      - md)**2;
run;
proc      sort      data      =      mtemp1;
by      group      id;
run;
proc      ttest      data      =      mtemp1;
            class      group;
            var      d;
run;
proc      print      data      =      mtemp1;
by      group;
var      id      id      r      d      ddif;

run;

/****************************/
/* transformed - linear relationship exists between dependent and independent variables */
/****************************/
proc gplot data=original;
 plot ysq_inv*(x1 x2 x3 x4);
 symbol v=dot color=red;
 run  ;

/****************************/
/* transformed full model regression */
/****************************/
PROC REG DATA=original;
MODEL ysq_inv=x1 x2 x3 x4 / P dw;
RUN;

/****************************/
/* transformed model AutoCorrelation - DW test */
/* transformed model validate Multicollinearity - correl matrix, VIF, condition index  */
/****************************/

PROC REG DATA=original OUTEST=set3 COVOUT SIMPLE CORR LINEPRINTER;

MODEL ysq_inv=x1 x2 x3 x4 / SS1 SS2 STB COVB CORRB SEQB VIF TOL COLLIN
                         P R DW INFLUENCE PARTIAL;
OUTPUT OUT=set2
         P=yhat            R=resid
      STDP=stdp         STDR=stdr
   STUDENT=student  RSTUDENT=rstudent
     COOKD=cookd           H=leverage
     PRESS=press      DFFITS=dffits
  COVRATIO=covratio;
PLOT R.*(OBS. P. x1 x2 x3 x4) / HPLOTS=2 VPLOTS=2;
RUN;

PROC PRINT DATA=set2;
RUN;

/****************************/
/* transformed model normality test - Shapiro wilk test*/
/****************************/
PROC UNIVARIATE DATA=set2 normal PLOT;
VAR resid;
HISTOGRAM / NORMAL;
PROBPLOT;
RUN;

/****************************/
/* Select Best Model */
/****************************/
PROC REG DATA=original;
MODEL ysq_inv=x1 x2 x3 x4 / SELECTION=RSQUARE ADJRSQ MSE CP AIC BIC B BEST=6 START=1 STOP=5;
MODEL ysq_inv=x1 x2 x3 x4 / SELECTION=STEPWISE SLE=.25 SLS=.15 DETAILS;
MODEL ysq_inv=x1 x2 x3 x4 / SELECTION=BACKWARD         SLS=.05 DETAILS;
RUN;

/****************************/
/* best model regression */
/****************************/
PROC REG DATA=original;
MODEL ysq_inv=x1 x3 x4 / P dw;
RUN;

/****************************/
/* validate AutoCorrelation - DW test */
/* best model - Multicollinearity - correlation matrix, VIF*/
/****************************/

PROC REG DATA=original OUTEST=set3 COVOUT SIMPLE CORR LINEPRINTER;

MODEL ysq_inv=x1 x3 x4 / SS1 SS2 STB COVB CORRB SEQB VIF TOL COLLIN
                         P R DW INFLUENCE PARTIAL;
OUTPUT OUT=set2
         P=yhat            R=resid
      STDP=stdp         STDR=stdr
   STUDENT=student  RSTUDENT=rstudent
     COOKD=cookd           H=leverage
     PRESS=press      DFFITS=dffits
  COVRATIO=covratio;
PLOT R.*(OBS. P. x1 x3 x4) / HPLOTS=2 VPLOTS=2;
RUN;

PROC PRINT DATA=set2;
RUN;

/****************************/
/* best model - Shapiro wilk test*/
/****************************/
PROC UNIVARIATE DATA=set2 normal PLOT;
VAR resid;
HISTOGRAM / NORMAL;
PROBPLOT;
RUN;

/****************************/
/* best model heteroskedestacity test - Modified levenes */
/****************************/

proc reg data= original;
 model ysq_inv= x1 x3 x4;
 output      out=temp      r=r;
 output out=temp2   p=yhat;
run;
data temp3 (drop=y x1 x3 x4);
merge temp temp2;
run;
proc sort data=temp3 out=sorted;
key yhat/ascending;
run;
proc print data=sorted;
run;
data      sorted2;
            set      sorted;
            id      =      _n_;
            group      =      .;
            if      id      <=      18      then      group      =      1;
            if      id      >      18      then      group      =      2;
run;
proc sort data= sorted2;
by group;
run;
proc print data= sorted2;
run;
proc      means      data      =      sorted2      noprint;
            by      group;
            var      r;
            output      out=mout      median=mr;
run;
proc      print      data      =      mout;
var      group      mr;
run;
data      mtemp;
            merge      sorted2      mout;
            by      group;
            d      =      abs(r      - mr);
run;
proc      sort      data      =      mtemp;
            by      group;
run;
proc      means      data      =      mtemp      noprint;
            by      group;
            var      d;
            output      out=mout1      mean=md;
run;
proc      print      data      =      mout1;
            var      group      md;
run;
data      mtemp1;
            merge      mtemp      mout1;
            by      group;
            ddif      =      (d      - md)**2;
run;
proc      sort      data      =      mtemp1;
by      group      id;
run;
proc      ttest      data      =      mtemp1;
            class      group;
            var      d;
run;
proc      print      data      =      mtemp1;
by      group;
var      id      id      r      d      ddif;

run;

ods pdf close;
