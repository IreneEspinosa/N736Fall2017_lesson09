* Encoding: UTF-8.
* working with helpmkh.sav SPSS dataset.

* draw a histogram with normal curve.
GRAPH
  /HISTOGRAM(NORMAL)=i1.

* can also get the stats and the histogram with curve
* using the FREQUENCIES procedure.

FREQUENCIES VARIABLES=i1
  /FORMAT=NOTABLE
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

* get bootstrapped estimate BCA type
* for the mean.

BOOTSTRAP
  /SAMPLING METHOD=SIMPLE
  /VARIABLES INPUT=i1 
  /CRITERIA CILEVEL=95 CITYPE=BCA  NSAMPLES=1000
  /MISSING USERMISSING=EXCLUDE.
DESCRIPTIVES VARIABLES=i1
  /STATISTICS=MEAN STDDEV MIN MAX.




