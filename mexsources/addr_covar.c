#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mex.h"
#include "guisdap.h"

/* Input Arguments */
#define ADDR1_IN 	prhs[0]
#define ADDR2_IN 	prhs[1]
#define VC_SIGNAL_IN 	prhs[2]
#define LP_VC_IN	prhs[3]
#define LP_DT_IN	prhs[4]
#define LP_RA_IN	prhs[5]
#define LP_RI_IN	prhs[6]
#define LP_NT_IN	prhs[7]
#define LP_T1_IN 	prhs[8]
#define LP_T2_IN 	prhs[9]
#define LP_DEC_IN	prhs[10]
#define LP_NFIR_IN 	prhs[11]
#define LP_FIR_IN	prhs[12]

/* Output Arguments */

#define	COVAR_RE_OUT	plhs[0]
#define	COVAR_IM_OUT	plhs[1]

#ifdef ANSI_C
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
#else
mexFunction(nlhs, plhs, nrhs, prhs)
	int nlhs, nrhs;
	mxArray *plhs[];
	const mxArray *prhs[];
#endif
{
		double *vc_signalPr, 
			*lp_vcPr, *lp_dtPr, *lp_raPr, *lp_riPr,
			*lp_ntPr, *lp_t1Pr, *lp_t2Pr, *lp_decPr,
			*lp_nfirPr, *lp_firPr,
			*covarRe, *covarIm;

		long signallength, signalvcs, nlp, maxfir, i;
		
		long addr1, addr2,
			*lp_vc, *lp_dt, *lp_ra, *lp_ri,
			*lp_nt, *lp_t1, *lp_t2, *lp_dec,
			*lp_nfir;
			
		/* Check for proper number of arguments */
	
		if (nrhs != 13) {
			mexErrMsgTxt("Covar33 requires 13 input arguments.");
		} else if (nlhs > 2) {
			mexErrMsgTxt("Covar33 requires two output arguments.");
		}

		
		/* Assign pointers to the various parameters */
	
		addr1=(int)(*mxGetPr(ADDR1_IN)+0.5);
		addr2=(int)(*mxGetPr(ADDR2_IN)+0.5);
		vc_signalPr=mxGetPr(VC_SIGNAL_IN);
		lp_vcPr=mxGetPr(LP_VC_IN);
		lp_dtPr=mxGetPr(LP_DT_IN);
		lp_raPr=mxGetPr(LP_RA_IN);
		lp_riPr=mxGetPr(LP_RI_IN);
		lp_ntPr=mxGetPr(LP_NT_IN);
		lp_t1Pr=mxGetPr(LP_T1_IN);
		lp_t2Pr=mxGetPr(LP_T2_IN);
		lp_decPr=mxGetPr(LP_DEC_IN);
		lp_nfirPr=mxGetPr(LP_NFIR_IN);
		lp_firPr=mxGetPr(LP_FIR_IN);

		signallength = mxGetM(VC_SIGNAL_IN);
		signalvcs = mxGetN(VC_SIGNAL_IN);
		nlp = mxGetM(LP_VC_IN)*mxGetN(LP_VC_IN);
		maxfir = mxGetM(LP_FIR_IN);

		lp_vc=(long *)mxCalloc(nlp,sizeof(long));  
		lp_dt=(long *)mxCalloc(nlp,sizeof(long));   
		lp_ra=(long *)mxCalloc(nlp,sizeof(long));   
		lp_ri=(long *)mxCalloc(nlp,sizeof(long));  
		lp_nt=(long *)mxCalloc(nlp,sizeof(long));   
		lp_t1=(long *)mxCalloc(nlp,sizeof(long));   
		lp_t2=(long *)mxCalloc(nlp,sizeof(long));   
		lp_dec=(long *)mxCalloc(nlp,sizeof(long));  
		lp_nfir=(long *)mxCalloc(nlp,sizeof(long));		
		
		for(i=0;i<nlp;i++)
			{
		lp_vc[i]=(int)(0.5+lp_vcPr[i]);
		lp_dt[i]=(int)(0.5+lp_dtPr[i]);
		lp_ra[i]=(int)(0.5+lp_raPr[i]); 
		lp_ri[i]=(int)(0.5+lp_riPr[i]);
		lp_nt[i]=(int)(0.5+lp_ntPr[i]); 
		lp_t1[i]=(int)(0.5+lp_t1Pr[i]); 
		lp_t2[i]=(int)(0.5+lp_t2Pr[i]); 
		lp_dec[i]=(int)(0.5+lp_decPr[i]);
		lp_nfir[i]=(int)(0.5+lp_nfirPr[i]);  		
			}


		/* Create a matrix for the return argument */
		COVAR_RE_OUT = mxCreateDoubleMatrix(1, 1, mxREAL);
		COVAR_IM_OUT = mxCreateDoubleMatrix(1, 1, mxREAL);

		covarRe = mxGetPr(COVAR_RE_OUT);
		covarIm = mxGetPr(COVAR_IM_OUT);
	

		covar33Calc(addr1,addr2, 
                 signallength,signalvcs,vc_signalPr,
				 nlp,lp_vc,lp_dt,lp_ra,lp_ri,
				 lp_nt,lp_t1,lp_t2,lp_dec,
				 lp_nfir,
				 maxfir,lp_firPr,
				 covarRe,covarIm);
			
}
