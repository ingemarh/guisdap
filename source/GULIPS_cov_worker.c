/*

  Rinvmex.c   .MEX file for updating QR decomposition

  The calling syntax is:

		cov = Rinvmex(R)
			
		R = the right triangular matrix (npar * npar)
  		cov = npar*1 vector for the diagonal values of the covariance
*/

#include <stdlib.h>
#include <math.h>
#include "GULIPS.h"

extern void GULIPS_covCalc(DFLOAT *R,long *indices,unsigned long indiceslength,unsigned long code,unsigned long npar,double *cov);

#ifdef ANSI_C
void GULIPS_covCalc(DFLOAT *R,long *indices,unsigned long indiceslength,unsigned long code,unsigned long npar,double *cov)
#else
void GULIPS_covCalc(R,indices,indiceslength,code,npar,cov)
DFLOAT *R;
double *cov;
long *indices;
unsigned long indiceslength,code,npar;
#endif
	{
#ifdef MAC
	register extended sum;
#else
	register double sum;
#endif
	register long i,j,jj,itemp,jtemp;
	register unsigned long N;

	N = npar;
/*
	code  = defines the format of the output covariance matrix
		  if code = 1 then output is normal npar*npar matrix
		  if code = 2 then output is left triangular matrix
		  if code = 3 then output is npar*1 vector for the diagonal values of the covariance
*/

	switch(code)
		{
		
		case 1 :
			{
			for (itemp=0; itemp<indiceslength; itemp++)
				{
				 i = indices[itemp];
				 for (jtemp=0; jtemp<=itemp; jtemp++)
				 	{
					j = indices[jtemp];
					sum=0.0;
					for (jj=i; jj<N; jj++)
						sum+=R[IND(i,jj,N)]*R[IND(j,jj,N)];
					cov[IND2(itemp,jtemp,indiceslength)]=sum;
					cov[IND2(jtemp,itemp,indiceslength)]=sum;
					}
				}
			}	break;
		case 2 :
			{
			for (itemp=0; itemp<indiceslength; itemp++)
			 	{
				i = indices[itemp];
				for (jtemp=0; jtemp<=itemp; jtemp++)
				 	{
					j = indices[jtemp];
					sum=0.0;
					for (jj=i; jj<N; jj++)
					 	sum+=R[IND(i,jj,N)]*R[IND(j,jj,N)];
					cov[IND3(i,j)]=sum;
					}
				}
			}break;
		case 3 :
			{
			for (itemp=0; itemp<indiceslength; itemp++)
			 	{
				i = indices[itemp];
			 	sum=0.0;
			 	for (jj=i; jj<N; jj++)
					sum+=R[IND(i,jj,N)]*R[IND(i,jj,N)];
			 	cov[i]=sum;
				}
			}
			break;

		}
	}
