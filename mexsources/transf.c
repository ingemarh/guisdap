#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mex.h"
#include "guisdap.h"

/* Output Arguments */
#define	NIN0_OUT	plhs[0]
#define	TIT0_OUT	plhs[1]
#define	MIM0_OUT	plhs[2]
#define	PSI_OUT	plhs[3]
#define	VI_OUT	plhs[4]

/* Input Arguments */

#define	P_IN	prhs[0]
#define	M_IN	prhs[1]

#ifdef MAC
extern long ffloor(double x);
#endif

#ifdef ANSI_C
	void mexFunction(int nlhs,mxArray *plhs[],int nrhs,const mxArray *prhs[])
#else
	mexFunction(nlhs, plhs, nrhs, prhs)
	int nlhs, nrhs;
	mxArray *plhs[];
	const mxArray *prhs[];
#endif
{
	unsigned long ns,nion;
	double *pPr,*nin0Pr,*tit0Pr,*mim0Pr,*psiPr,*viPr,*p_m0;

/* First check that we get the proper number of input & output arguments */

	if(nrhs!=2)
		mexErrMsgTxt("Transf requires two input arguments.");
	if(nlhs!=5)
		mexErrMsgTxt("Transf requires five output arguments.");
	
/* Then get the pointers to the data part of the matrices */
	ns  =mxGetM(P_IN);	/* Number of spectra */
	pPr =mxGetPr(P_IN);
	p_m0=mxGetPr(M_IN);
	nion=mxGetM(M_IN)*mxGetN(M_IN);

	NIN0_OUT=mxCreateDoubleMatrix(ns, nion+1, mxREAL);
	TIT0_OUT=mxCreateDoubleMatrix(ns, nion+1, mxREAL);
	MIM0_OUT=mxCreateDoubleMatrix(ns, nion+1, mxREAL);
	PSI_OUT =mxCreateDoubleMatrix(ns, nion+1, mxREAL);
	VI_OUT  =mxCreateDoubleMatrix(ns, nion+1, mxREAL);

	nin0Pr=mxGetPr(NIN0_OUT);
	tit0Pr=mxGetPr(TIT0_OUT);
	mim0Pr=mxGetPr(MIM0_OUT);
	psiPr =mxGetPr(PSI_OUT);
	viPr  =mxGetPr(VI_OUT);

	Transf(pPr,nin0Pr,tit0Pr,mim0Pr,psiPr,viPr,p_m0,nion);	
}
