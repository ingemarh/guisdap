# include <stdlib.h>
# include <stdio.h>
# include <math.h>
# include "mex.h"

#define ddabs(A) ((A)<(0) ? (-A) : (A))


/* Output Arguments */
#define	ACF_OUT	plhs[0]

/* Input Arguments */

#define	AA_IN	prhs[0]
#define	COEF_IN	prhs[1]
#define	WOM_IN	prhs[2]
#define	KD2_IN	prhs[3]
#define OM_IN	prhs[4]
#define PLDFVV_IN prhs[5]
#define M_IN	prhs[6]

#ifdef ANSI_C
	extern void DirtheCalc(long ns,long aaN,double *aaPr,double *coefPr,long womM,double *womPr,double *kd2Pr,long nom,double *omPr,double *pldfvPr,double *pldfvPi,double *acfPr,long use_reference,double *scr,double *scr1);
#else
	extern void DirtheCalc();
#endif

extern double *p_m0;
extern unsigned long nion;

#ifdef ANSI_C
	void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
#else
	void mexFunction(nlhs, plhs, nrhs, prhs)
	int nlhs, nrhs;
	mxArray *plhs[];
	const mxArray *prhs[];
#endif
{
	long nom,womM,aaN,ns;
	double *aaPr,*coefPr,*womPr,*kd2Pr,*omPr,*acfPr,*pldfvPr,*pldfvPi,*scr,*scr1;

/* First check that we get the proper number of input & output arguments */

	if (nrhs != 7)
		mexErrMsgTxt("Dirthe requires seven input arguments.");
	if (nlhs != 1)
		mexErrMsgTxt("Dirthe requires one output argument.");
	
/* Then get the pointers to the data part of the matrices */
	
	nom = mxGetM(OM_IN)*mxGetN(OM_IN);
	womM = mxGetM(WOM_IN);
	ns = mxGetM(AA_IN);
	aaN = mxGetN(AA_IN);
	aaPr = mxGetPr(AA_IN);
	coefPr = mxGetPr(COEF_IN);
	womPr = mxGetPr(WOM_IN);
	kd2Pr = mxGetPr(KD2_IN);
	omPr = mxGetPr(OM_IN);
	pldfvPr = mxGetPr(PLDFVV_IN);
	pldfvPi = mxGetPi(PLDFVV_IN);
	p_m0=mxGetPr(M_IN);
	nion=mxGetM(M_IN)*mxGetN(M_IN);
	
/*	ACF_OUT = mxCreateDoubleMatrix((mxGetM(COEF_IN) + 5), mxGetN(COEF_IN), mxREAL);*/
	ACF_OUT = mxCreateDoubleMatrix((mxGetM(COEF_IN) + aaN), mxGetN(COEF_IN), mxREAL);
	acfPr = mxGetPr(ACF_OUT);
	scr = (double *)mxCalloc((nion+2)*(3+4*nom),sizeof(double));
	scr1= (double *)mxCalloc(((nion+1)*5+nom+aaN)*ns,sizeof(double));

	DirtheCalc(ns,aaN,aaPr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,acfPr,0,scr,scr1);
}
