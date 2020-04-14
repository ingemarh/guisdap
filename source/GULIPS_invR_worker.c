/*

  Rinvmex.c   .MEX file for updating QR decomposition

  The calling syntax is:

		cov = Rinvmex(R)
			
		R = the right triangular matrix (npar * npar)
  		cov = npar*1 vector for the diagonal values of the covariance

*/

#include <math.h>
#include "mex.h"
#include "GULIPS.h"

extern 	void GULIPS_invRCalc(DFLOAT *R,unsigned long npar);

#ifdef ANSI_C
	void GULIPS_invRCalc(DFLOAT *R,unsigned long npar)
#else
	void GULIPS_invRCalc(R,npar)
	DFLOAT *R;
	unsigned long npar;
#endif
	{
#ifdef MAC	
	register extended sum;
#else
	register double sum;
#endif
	register long i,j,jj;
	register unsigned long N;

	N = npar;
	for (j=N-1; j>=0; j--){
		  R[IND(j,j,N)]=1.0/R[IND(j,j,N)];
		  for (i=j-1; i>=0; i--){
				  sum=0.0; for (jj=i+1; jj<=j; jj++) { sum+=R[IND(i,jj,N)]*R[IND(jj,j,N)];}
				  R[IND(i,j,N)]=-sum/R[IND(i,i,N)];
				}
		}
	}
