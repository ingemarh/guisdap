# include <stdlib.h>
# include <stdio.h>
# include <math.h>
# include "mex.h"

/* Input Arguments */
#define	NIN0_IN	prhs[0]
#define	TIT0_IN	prhs[1]
#define	MIM0_IN	prhs[2]
#define	PSI_IN	prhs[3]
#define	VI_IN	prhs[4]
#define	KD2_IN	prhs[5]
#define	OM_IN	prhs[6]
#define PLDFVV_IN prhs[7]

/* Output Arguments */

#define	RES_OUT	plhs[0]

# include "guisdap.h"

#ifdef ANSI_C
	void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
#else
	mexFunction(nlhs, plhs, nrhs, prhs)
	int nlhs, nrhs;
	mxArray *plhs[];
	const mxArray *prhs[];
#endif
{
    unsigned long ns,nom,nion;
    double *pldfvPr,*pldfvPi,*nin0Pr,*tit0Pr,*mim0Pr,*psiPr,*viPr,*kd2Pr,*omPr,*resPr;
    double *scr;

/* First check that we get the proper number of input & output arguments */
	if (nrhs != 8)
		mexErrMsgTxt("Spec requires eight input arguments.");
	if (nlhs != 1)
		mexErrMsgTxt("Spec requires one output argument.");
	
/* Then get the pointers to the data part of the matrices */
    ns = mxGetM(MIM0_IN);								/* Number of spectra */
    nion = mxGetN(MIM0_IN)-1;
    nom = mxGetM(OM_IN)*mxGetN(OM_IN);
    RES_OUT = mxCreateDoubleMatrix(nom,ns,mxREAL);

    pldfvPr = mxGetPr(PLDFVV_IN); 
    pldfvPi = mxGetPi(PLDFVV_IN);
    nin0Pr = mxGetPr(NIN0_IN);
    tit0Pr = mxGetPr(TIT0_IN);
    mim0Pr = mxGetPr(MIM0_IN);
    psiPr = mxGetPr(PSI_IN);
    viPr = mxGetPr(VI_IN);
    kd2Pr = mxGetPr(KD2_IN);
    omPr = mxGetPr(OM_IN);
    resPr = mxGetPr(RES_OUT);
/*  scr = (double *)mxCalloc((12+16*nom),sizeof(double));*/
    scr = (double *)mxCalloc((nion+2)*(3+4*nom),sizeof(double));

    specCalc(pldfvPr,pldfvPi,nin0Pr,tit0Pr,nion,mim0Pr,psiPr,viPr,kd2Pr[0],scr,nom,omPr,resPr,0);
}

