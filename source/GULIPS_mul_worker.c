/*

  Rmulmex.c   .MEX file for multiplying matrices used in QR-decomposition.

  The calling syntax is:

		sol =	Rmulmex(R,y)
			
		R = the right triangular matrix (npar * npar)
		y = the right hand side so far (npar * nmeas)

*/

/*#include <math.h>*/
#include "GULIPS.h"

extern 	void GULIPS_mulCalc(DFLOAT *R,double *y,double *sol,unsigned long yn,unsigned long ym);

#ifdef ANSI_C
	void GULIPS_mulCalc(DFLOAT *R,double *y,double *sol,unsigned long yn,unsigned long ym)
#else
	void GULIPS_mulCalc(R,y,sol,yn,ym)
	DFLOAT *R;
	double *y,*sol;
	unsigned long yn,ym;
#endif
{
#ifdef MAC
    register extended sum;
#else
    register double sum;
#endif
    unsigned long i,npar,nmeas,imeas,irow,N;
		
    npar=ym; nmeas=yn;
    N = npar;
    for (imeas=0; imeas<yn; imeas++){
	    for (irow=0; irow<npar; irow++){
		    sum=0.0;
			for (i=irow; i<npar; i++) {
			    sum += R[IND(irow,i,N)] * y[IND2(i,imeas,N)];
			}
			sol[IND2(irow,imeas,N)]=sum;
		}
	}
}
