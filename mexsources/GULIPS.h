/*
   GULIPS.h contains all the definitions that are shared between the GULIPS parts 
*/

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
