#ifndef _GUISDAP_H
#ifdef ANSI_C
extern void Transf(double *pPr,double *nin0Pr,double *tit0Pr,double *mim0Pr,double *psiPr,double *viPr,double *p_m0,long nion);
extern void specCalc(double *pldfvPr,double *pldfvPi,double *nin0Pr,double *tit0Pr,long nion,double *mim0Pr,double *psiPr,double *viPr,double kd2Pr,double *scr,long nom,double *omPr,double *resPr,long ifref);
extern void mymul(char transa,double *mat1,long mat1m,long mat1n,char transb,double *mat2,long mat2m,long mat2n, double *res);
extern void DirtheCalc(long ns,long aaN,double *aaPr,double *coefPr,long womM,double *womPr,double *kd2Pr,long nom,double *omPr,double *pldfvPr,double *pldfvPi,double *acfPr,long use_reference,double *p_m0,long nion,double *bwomPr,double *scr,double *scr1);
extern void MrqmndiagCalc(long ns,long aaN,double *aPr,double* ymPr,double *variancePr,long varianceM,long varianceN,double* ftolPr,double *itMaxPr,double *coefPr,long coefM,long coefN,long womM,double *womPr,double *kd2Pr,long nom,double *omPr,double *aaOutPr,double *chi2Pr,double *itsPr,double *alphaPr,double *pldfvPr,double *pldfvPi,double *physlimPr,double *p_m0,long nion,double *bwomPr);
extern void covar33Calc (long addr1,long addr2,unsigned long signallength,unsigned long signalvcs,double *vc_signal,unsigned long nlp,long *lp_vc,long *lp_dt,long *lp_ra,long *lp_ri,long *lp_nt,long *lp_t1,long *lp_t2,long *lp_dec,long *lp_nfir,unsigned long maxfir,double *lp_fir,double *covarRe,double *covarIm);
#else
extern void Transf();
extern void specCalc();
extern void mymul();
extern void DirtheCalc();
extern void MrqmndiagCalc();
extern void covar33Calc();
#endif
#define _GUISDAP_H
#endif
