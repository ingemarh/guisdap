/*

  GULIPS_addm_worker.c   
 
  The calling syntax is:



			GULIPS_addm(R,y,A,m,KhiSqr,storage,errorBar)
		
		
			
		R 			the right triangular matrix (npar * npar)
		
		y 			the right hand side so far (npar * nmeas)
		
		A 			coefficient matrix of the new measurements (nnew * npar)
		
		m 			vectors of the new measurements (nnew * nmeas)
  		
  		KhiSqr 		vector that contains residual square sums
		
		storage 	Angle storage. Can be
		
						1) Empty matrix: the angles are not saved in this case.
						2) Text matrix that contains the name of the 
						   file to be created and where the angles are written (appended)
						3) Normal matrix in which the angles are stored.
						   The size of of the storage is then (nnew * npar)/2.
		
		errorBar 	matrix that contains the errorBars. 
					Can be left out or be empty if they are not needed.
					Size of errorBar is that of the measurement vector (nnew * nmeas).

  
*/

#include <stdio.h>
#include <math.h>
#include "mex.h"
#include "GULIPS.h"

extern void GULIPS_addmCalc(DFLOAT *R, double *y, double *A, double *m,double *KhiSqr,
				            unsigned long nnew,unsigned long npar,unsigned long nmeas, 
				            mxArray* storage, double *errorBarPr);

#ifdef ANSI_C
	void GULIPS_addmCalc(DFLOAT *R, double *y, double *A, double *m,double *KhiSqr,
				   unsigned long nnew,unsigned long npar,unsigned long nmeas, 
				   mxArray* storage, double *errorBarPr)
#else
	void GULIPS_addmCalc(R,y,A,m,KhiSqr,nnew,npar,nmeas,storage,errorBarPr)
	DFLOAT *R;
	double *y,*A,*m,*KhiSqr,*errorBarPr;
	unsigned long nnew,npar,nmeas;
	mxArray *storage;
#endif
{
#ifdef MAC
	extended r;
	register extended c,s,e1,e2;
#else
	double r;
	register double c,s,e1,e2;
#endif
    long inew,irow,icol,storageAvailable = 0,err,i;
    long *angle=NULL;
    register unsigned long N;
    DFLOAT *v0;
    double *v1,*v2,*v3,*v4,*v5;
    double *ARowStorage, *mRowStorage;					/* Temporary work array that contains one row of the matrices */
    char fileName[50];
    FILE *angleFile=NULL;	

    N = npar;

    fileName[0] = 0;									/* Make sure the fileName is initially empty */

    if ((mxGetM(storage)*mxGetN(storage))!=0)
	    storageAvailable = 1;
	
    if (mxIsChar(storage))
	    err = mxGetString(storage,fileName,(mxGetN(storage)+1));
    else
	    angle = (long *)mxGetPr(storage);				/* Take the storage from storage marix */	

		
if(fileName[0] != 0)								/* Open file storage if requested by the user */
	{
	angleFile = fopen(fileName,"a");
	angle = (long *)mxCalloc(npar,sizeof(long));
	}

ARowStorage = (double *)mxCalloc(npar,sizeof(double));		/* Allocate the temporary work arrays */
mRowStorage = (double *)mxCalloc(nmeas,sizeof(double));

for (inew=0; inew<nnew; inew++)						/* Loop that goes through number of new measurements (rows of the A&m matrix)*/
	{
	v3 = &(A[IND2(inew,0,nnew)]);					/* Take the inewth row of the matrix A */
	v4 = &(m[IND2(inew,0,nnew)]);					/* Take the inewth row of the matrix m */
	v5 = &(errorBarPr[IND2(inew,0,nnew)]);			/* Take the first element of the inewth row of the matrix errorBar */
	for(i=0;i<npar;i++)								/* Copy the rows of the matrices A & m that are under rotation to temp array */
		{
		ARowStorage[i] = (*v3)/(*v5);				/* A[i] divided by the corresponding errorBar */
		v3 += nnew;
		}
	for(i=0;i<nmeas;i++)
		{
		mRowStorage[i] = (*v4)/(*v5);				/* measurement[i] divided by the corresponding errorBar */
		v4 += nnew; 
		}

	for (irow=0; irow<N; irow++)					/* Loop that goes though number of colums in matrix A */
		{
	  	v0 = &(R[IND(irow,irow,N)]);				/* Index from the right triangular matrix R */
		v2 = &(ARowStorage[irow]);					/* Index from the ArowStorage to be handled */
		if (ddabs(*v2)>1e-99) 
			{
			r=sqrt(*v0 * *v0 + *v2 * *v2);
			c=*v0/r; s=*v2/r;								

/* Angle is stored as 32 bit integer where 2¹ is 2^32.
 * Atan returns the angle as radians and it is then conveted to be between 0..2.
 * The last multiplication converts the angle to 0..2^32 range.
 */
			if(storageAvailable)						
				{
				if(fileName[0] != 0)			/* the storage is file */
					{
#ifdef MAC
  					angle[irow] = ffloor((atan2(s,c)/3.1415926535897932385)*2147483648+0.5);
#else
					angle[irow] = (unsigned long)((atan2(s,c)/3.1415926535897932385)*2147483648.+0.5);
#endif				
					}
				else							/* the storage is matrix */			
					{
#ifdef MAC
  					angle[inew * N + irow] = ffloor((atan2(s,c)/3.1415926535897932385)*2147483648+0.5);
#else
					angle[inew * N + irow] = (unsigned long)((atan2(s,c)/3.1415926535897932385)*2147483648.+0.5);
#endif				
					}				
				}

			for (icol=irow; icol<npar; icol++) 
				{
				e1 = *v0;
				e2 = *v2;
 				*v0 = (DFLOAT)(c*e1+s*e2);
				*v2 = c*e2-s*e1;
				v0++; v2++;
				}
			v1 = y+nmeas*irow;
			v2 = &(mRowStorage[0]);
			for (icol=0; icol<nmeas; icol++) 
				{
				e1 = *v1;
				e2 = *v2;
				*v1 = c*e1+s*e2;
				*v2 = c*e2-s*e1;
				v1+=npar; v2++;
				}
			}
		}

		for (icol=0; icol<nmeas; icol++) 
			KhiSqr[icol] += mRowStorage[icol] * mRowStorage[icol];
	
		if(fileName[0] != 0)
				fwrite((void*)angle,sizeof(long),npar,angleFile);
	}
if(fileName[0] != 0)
	fclose(angleFile);
}
