/*
%***************************************************************************************************
% Copyright 2006, T.P. O'Brien
%
% This file is part of ONERA_DESP_LIB.
%
%    ONERA_DESP_LIB is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Lesser General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    ONERA_DESP_LIB is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Lesser General Public License for more details.
%
%    You should have received a copy of the GNU Lesser General Public License
%    along with ONERA_DESP_LIB.  If not, see <http://www.gnu.org/licenses/>.
%
prototype C header file for access to generic .dll or .so file
e.g., onera_desp_lib.dll
Paul O'Brien paul.obrien@aero.org
 */
#ifdef __linux__
#include <stdint.h>
#else
#include <sys/int_types.h>
#endif

void make_lstar1_(int32_t *ntime, int32_t *kext,
		  int32_t *options,int32_t *sysaxes,
		  int32_t *iyear,int32_t *idoy,
		  double *UT,double *x1,
		  double *x2,double *x3,
		  double *maginput, double *Lm,
		  double *Lstar, double *Blocal,
		  double *Bmin, double *J,
		  double *MLT);

void make_lstar_shell_splitting1_(int32_t *ntime,
				  int32_t *Nipa,
				  int32_t *kext,
				  int32_t *options,
				  int32_t *sysaxes,
				  int32_t *iyear,
				  int32_t *idoy,
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

void drift_shell1_(int32_t *kext, int32_t *options,
		   int32_t *sysaxes, int32_t *iyear,
		   int32_t *idoy, double * UT,
		   double *x1, double *x2, double *x3,
		   double *maginput,
		   double *Lm,
		   double *Lstar,
		   double *Blocal,
		   double *Bmin,
		   double *J,
		   double *posit,
		   int32_t *ind);

void trace_field_line1_(int32_t *kext, int32_t *options,
		   int32_t *sysaxes, int32_t *iyear,
		   int32_t *idoy, double * UT,
		   double *x1, double *x2, double *x3,
		   double *maginput,
		   double *Lm,
		   double *Blocal,
		   double *Bmin,
		   double *J,
		   double *posit,
		   int32_t *ind);

void get_field1_(int32_t *kext, int32_t *options,
		 int32_t *sysaxes,
		 int32_t *iyear,int32_t *idoy,
		 double *UT,double *x1,
		 double *x2,double *x3,
		 double *maginput,
		 double *Bgeo,
		 double *B);

void find_mirror_point1_(int32_t *kext,
			 int32_t *options,
			 int32_t *sysaxes,
			 int32_t *iyear,int32_t *idoy,
			 double *UT,double *x1,
			 double *x2,double *x3,
			 double *alpha,
			 double *maginput,
			 double *Blocal,
			 double *Bmirror,
			 double *xGEO);

void find_magequator1_(int32_t *kext,
			 int32_t *options,
			 int32_t *sysaxes,
			 int32_t *iyear,int32_t *idoy,
			 double *UT,double *x1,
			 double *x2,double *x3,
			 double *maginput,
			 double *Bmin,
			 double *xGEO);

void get_mlt1_(int32_t *iyr, int32_t *idoy,
	       double *UT, double *xGEO, double *MLT);

void fly_in_nasa_aeap1_(int32_t *ntime, int32_t *sysaxes,
			int32_t *whichm, int32_t *whatf,
			int32_t *Nene, double *energy,
			int32_t *iyear, int32_t *idoy, double *UT,
			double *x1,double *x2, double *x3,
			double *flux);

void get_ae8_ap8_flux_(int32_t *ntime, int32_t *whichm, int32_t *whatf,
		       int32_t *Nene, double *energy,
		       double *BBo, double *L, double *flux);

void fly_in_afrl_crres1_(int32_t *ntime, int32_t *sysaxes,
			 int32_t *whichm, int32_t *whatf,
			 int32_t *nene, double *energy,
			 int32_t *iyear, int32_t *idoy, double *UT,
			 double *x1, double *x2, double *x3,
			 double *Ap15,
			 double *flux,
			 char *ascii_path,
			 int32_t *strlen);

void get_crres_flux_(int32_t *ntime, int32_t *whichm, int32_t *whatf,
		     int32_t *nene, double *energy,
		     double *BBo,double *L, double *Ap15,
		     double *flux,
		     char *ascii_path,
		     int32_t *strlen);

void sgp4_tle1_(int32_t *runtype,double *startsfe,double *stopsfe,double *deltasec,
		char *InFileByte,int32_t *strlenIn,
		char *OutFileByte,int32_t *strlenOut);

void sgp4_ele1_(int32_t *sysaxes,
		int32_t *Yr,int32_t *Mon,int32_t *Day,int32_t *Hr,int32_t *Minute,double *Sec,
		double *e1, double *e2,	double *e3, double *e4,	double *e5, double *e6,
		int32_t *ele_opts,
		double *startsfe,double *stopsfe,double *deltasec,
		int32_t *outYr,int32_t *outDoy, double *outSec,
		double *x1, double *x2,	double *x3);

void coord_trans_vec1_(int32_t *ntime, int32_t *sysaxesIN,int32_t *sysaxesOUT,
		   int32_t *iyr,int32_t *idoy,double *secs,
		   double *xINV,double *xOUTV);

void rv2coe_(double *R, double *V,
	     double *P, double *A, double *Ecc, double *Incl, double *Omega,
	     double *Argp, double *Nu, double *M, double *ArgLat,
	     double *TrueLon, double *LonPer);

void fly_in_ige1_(int32_t *launch_year, int32_t *duration,
		  int32_t *whichm, int32_t *whatf,
		  int32_t *nene, double *energy,
		  double *Lower_flux, double *Mean_flux, double *Upper_flux);

void nrlmsise00_(int32_t *ntime,int32_t *whichAp,
		int32_t *DOY,double *UT,double *ALT,double *LAT,double *LON,
		double *F107A,double *F107,double *AP,double *Dens,double *Temp);

void msise90_(int32_t *ntime,int32_t *whichAp,
		int32_t *DOY,double *UT,double *ALT,double *LAT,double *LON,
		double *F107A,double *F107,double *AP,double *Dens,double *Temp);

void msis86_(int32_t *ntime,int32_t *whichAp,
		int32_t *DOY,double *UT,double *ALT,double *LAT,double *LON,
		double *F107A,double *F107,double *AP,double *Dens,double *Temp);

