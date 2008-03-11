/*
prototype C header file for access to generic DLL
onera_desp_lib_windows.dll
created for ONERA/DESP library version 3.0
Paul O'Brien paul.obrien@aero.org
 */

/* functions supported

all functions derived from or supporting the onera 
desp library are given names of the form 
onera_desp_lib_*, e.g., onera_desp_lib_get_field
   
MAKE_LSTAR wrapper built _make_lstar - need to test maginput
MAKE_LSTAR_SHELL_SPLITTING wrapper built _make_lstar_shell_splitting - need to test
GET_FIELD wrapper built _get_field - need to test maginput
GET_MLT wrapper built _get_mlt, tested
FIND_MIRROR_POINT wrapper built _find_mirror_point, tested
FIND_MAGEQUATOR wrapper built _find_magequator, tested
TRACE_FIELD_LINE  wrapper built _trace_field_line - need to test maginput
DRIFT_SHELL wrapper built _drift_shell - need to test maginput
FLY_IN_NASA_AEAP wrapper built _fly_in_nasa_aeap
FLY_IN_AFRL_CRRES wrapper built _fly_in_afrl_crres

coordinate rotations are handled exclusively by _rotate or
_coord_trans, which calls the coord_trans subroutine.

SGP4_TLE
SGP4_ELE

The following functions are not available to IDL and
are not implemented in matlab (but are easy to build
uisng matlab's native datenum and datevec functions.) 
These really shouldn't be necessary since all the
wrappers do the date conversion from matlab date 
numbers
JULDAY
DECY2DATE_AND_TIME
DATE_AND_TIME2DECY
CALDAT
GET_DOY

 */

void make_lstar1_(long int *ntime, long int *kext,
		  long int *options,long int *sysaxes,
		  long int *iyear,long int *idoy,
		  double *UT,double *x1,
		  double *x2,double *x3,
		  double *maginput, double *Lm,
		  double *Lstar, double *Blocal,
		  double *Bmin, double *J,
		  double *MLT);

void make_lstar_shell_splitting1_(long int *ntime, 
				  long int *Nipa,
				  long int *kext,
				  long int *options,
				  long int *sysaxes,
				  long int *iyear,
				  long int *idoy,
				  double *UT,
				  double *x1,
				  double *x2,
				  double *x3,
				  double *alpha,
				  double *maginput, 
				  double *Lm,
				  double *Lstar, 
				  double *Blocal,
				  double *Bmin, 
				  double *J,
				  double *MLT);

void drift_shell1_(long int *kext, long int *options,
		   long int *sysaxes, long int *iyear,
		   long int *idoy, double * UT,
		   double *x1, double *x2, double *x3,
		   double *maginput,
		   double *Lm,
		   double *Lstar, 
		   double *Blocal,
		   double *Bmin, 
		   double *J,
		   double *posit,
		   long int *ind);

void trace_field_line1_(long int *kext, long int *options,
		   long int *sysaxes, long int *iyear,
		   long int *idoy, double * UT,
		   double *x1, double *x2, double *x3,
		   double *maginput,
		   double *Lm,
		   double *Blocal,
		   double *Bmin, 
		   double *J,
		   double *posit,
		   long int *ind);

void get_field1_(long int *kext, long int *options,
		 long int *sysaxes,
		 long int *iyear,long int *idoy,
		 double *UT,double *x1,
		 double *x2,double *x3,
		 double *maginput,
		 double *Bgeo,
		 double *B);

void find_mirror_point1_(long int *kext, 
			 long int *options,
			 long int *sysaxes,
			 long int *iyear,long int *idoy,
			 double *UT,double *x1,
			 double *x2,double *x3,
			 double *alpha,
			 double *maginput,
			 double *Blocal,
			 double *Bmirror,
			 double *xGEO);

void find_magequator1_(long int *kext, 
			 long int *options,
			 long int *sysaxes,
			 long int *iyear,long int *idoy,
			 double *UT,double *x1,
			 double *x2,double *x3,
			 double *maginput,
			 double *Bmin,
			 double *xGEO);

void get_mlt1_(long int *iyr, long int *idoy,
	       double *UT, double *xGEO, double *MLT);

void fly_in_nasa_aeap1_(long int *ntime, long int *sysaxes,
			long int *whichm, long int *whatf,
			long int *Nene, double *energy, 
			long int *iyear, long int *idoy, double *UT,
			double *x1,double *x2, double *x3,
			double *flux);

void fly_in_afrl_crres1_(long int *ntime, long int *sysaxes,
			 long int *whichm, long int *whatf,
			 long int *nene, double *energy, 
			 long int *iyear, long int *idoy, double *UT,
			 double *x1, double *x2, double *x3,
			 double *Ap15,
			 double *flux,
			 char *ascii_path,
			 long int *strlen);

void sgp4_tle1_(long int *runtype,double *startsfe,double *stopsfe,double *deltasec,
		char *InFileByte,long int *strlenIn,
		char *OutFileByte,long int *strlenOut);

void sgp4_ele1_(long int *sysaxes,
		long int *Yr,long int *Mon,long int *Day,long int *Hr,long int *Minute,double *Sec,
		double *e1, double *e2,	double *e3, double *e4,	double *e5, double *e6,
		long int *ele_opts,
		double *startsfe,double *stopsfe,double *deltasec,
		long int *outYr,long int *outDoy, double *outSec,
		double *x1, double *x2,	double *x3);

void coord_trans_vec1_(long int *ntime, long int *sysaxesIN,long int *sysaxesOUT,
		   long int *iyr,long int *idoy,double *secs,
		   double *xINV,double *xOUTV);

void rv2coe_(double *R, double *V, 
	     double *P, double *A, double *Ecc, double *Incl, double *Omega, 
	     double *Argp, double *Nu, double *M, double *ArgLat,
	     double *TrueLon, double *LonPer);
