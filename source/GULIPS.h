/*
   GULIPS.h contains all the definitions that are shared between the GULIPS parts 
*/
#ifndef _GULIPS_H
#define ddabs(A) ((A)<(0) ? (-A) : (A))
#define	max(A, B)	((A) > (B) ? (A) : (B))
#define	min(A, B)	((A) < (B) ? (A) : (B))

#define IND(row,col,N) (row*N - ((row+2)*(row-1))/2L + col-1)		/* Index of right triangular matrix, stored row-wise */
#define IND2(row,col,N) ((row+N*col))								/* Index of regular matrix, stored columnwise*/
#define IND3(row,col) (((row*(row+1))>>1)+col)						/* Index of left triangular matrix, stored row-wise*/

#ifdef THIRTYTWOBIT
#define DFLOAT float 
#else
#define DFLOAT double
#endif

#ifdef ANSI_C
extern void GULIPS_addmCalc(double *R,double *y,double *A,double *m,double *KhiSqr,unsigned long nnew,unsigned long npar,unsigned long nmeas,mxArray* storage,double *errorBarPr,double *RowStorage);
extern void GULIPS_covCalc(double *R,long *indices,unsigned long indiceslength,unsigned long code,unsigned long npar, double *cov);
extern void GULIPS_mulCalc(double *R,double *y,double *sol,unsigned long yn,unsigned long ym);
extern void GULIPS_invRCalc(double *R,unsigned long npar);
#else
extern void GULIPS_addmCalc();
extern void GULIPS_covCalc();
extern void GULIPS_mulCalc();
extern void GULIPS_invRCalc();
#endif

#define _GULIPS_H
#endif
