#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mex.h"
#include "guisdap.h"

#define TEXT   1	/* mat.type indicating text */
#define MATRIX 0	/* mat.type indicating matrix */


/* Output Arguments */
#define	AA_OUT		plhs[0]
#define	CHI2_OUT	plhs[1]
#define	ITS_OUT		plhs[2]
#define	ALPHA_OUT	plhs[3]

/* Input Arguments */

#define	AA_IN		prhs[0]
#define	YM_IN		prhs[1]
#define	VARIANCE_IN	prhs[2]
#define	FTOL_IN		prhs[3]
#define ITMAX_IN	prhs[4]
#define KD2_IN		prhs[5]
#define P_COEFFG_IN	prhs[6]
#define WOM_IN		prhs[7]
#define OM_IN		prhs[8]
#define PLDFVV_IN	prhs[9]
#define M_IN		prhs[10]
#define PHYSLIM_IN	prhs[11]
/*
 * Beginning of the program !!!!!!!!
 */

#ifdef ANSI_C
void mexFunction(int nlhs,mxArray *plhs[],int nrhs,const mxArray *prhs[])
#else
mexFunction(nlhs,plhs,nrhs,prhs)
int nlhs, nrhs;
mxArray *plhs[];
const mxArray *prhs[];
#endif
{
	long nom,womM,aaN,ns,varianceM,varianceN,coefM,coefN,nion;
	double *aaPr,*coefPr,*womPr,*kd2Pr,*omPr, *ymPr, *variancePr,*ftolPr,*itMaxPr;
	double *aaOutPr,*itsPr,*chi2Pr,*alphaPr,*pldfvPr,*pldfvPi,*physlimPr,*p_m0;


/* Deal with the input arguments */

/* First check that we get the proper number of input & output arguments */

	if (nrhs != 12)
		mexErrMsgTxt("Mrqmndiag requires twelve input arguments.");
	if (nlhs != 4)
		mexErrMsgTxt("Mrqmndiag requires four output arguments.");

/* Then get the pointers to the data part of the matrices */

	ns = mxGetM(AA_IN);
	aaN = mxGetN(AA_IN);
	aaPr = mxGetPr(AA_IN);

	ymPr = mxGetPr(YM_IN);

	variancePr = mxGetPr(VARIANCE_IN);
	varianceM = mxGetM(VARIANCE_IN);
	varianceN = mxGetN(VARIANCE_IN);

	ftolPr = mxGetPr(FTOL_IN);

	itMaxPr = mxGetPr(ITMAX_IN);

	coefPr = mxGetPr(P_COEFFG_IN);
	coefM = mxGetM(P_COEFFG_IN);
	coefN = mxGetN(P_COEFFG_IN);

	womM = mxGetM(WOM_IN);
	womPr = mxGetPr(WOM_IN);

	nom = mxGetM(OM_IN)*mxGetN(OM_IN);
	omPr = mxGetPr(OM_IN);

	kd2Pr = mxGetPr(KD2_IN);

	pldfvPr = mxGetPr(PLDFVV_IN);
	pldfvPi = mxGetPi(PLDFVV_IN);

	p_m0 = mxGetPr(M_IN);
	nion=mxGetM(M_IN)*mxGetN(M_IN);

	physlimPr = mxGetPr(PHYSLIM_IN);

/* Create output arguments */

	AA_OUT = mxCreateDoubleMatrix(ns, aaN, mxREAL);	/* Fitted parameters */
	CHI2_OUT = mxCreateDoubleMatrix(1,1, mxREAL);
	ITS_OUT = mxCreateDoubleMatrix(1,1, mxREAL);
	ALPHA_OUT = mxCreateDoubleMatrix(aaN, aaN, mxREAL);	/* Covariance matrix of the result */

/* Deal with the output arguments */
		
	aaOutPr = mxGetPr(AA_OUT);
	chi2Pr = mxGetPr(CHI2_OUT);
	itsPr = mxGetPr(ITS_OUT);
	alphaPr = mxGetPr(ALPHA_OUT);

/* Do the actual calculation */
	MrqmndiagCalc(ns,aaN,aaPr,ymPr,variancePr,varianceM,varianceN,ftolPr,itMaxPr,
		coefPr,coefM,coefN,womM,womPr,kd2Pr,nom,omPr,aaOutPr,chi2Pr,itsPr,
		alphaPr,pldfvPr,pldfvPi,physlimPr,p_m0,nion);

}
