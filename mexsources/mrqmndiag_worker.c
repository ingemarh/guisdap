#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mex.h"

#include <time.h>
#ifdef UNIX
#include <sys/types.h>
#include <sys/times.h>
#endif
#include "guisdap.h"
#include "GULIPS.h"

#ifdef GUPTHREAD
#include <pthread.h>
#endif
struct pth {
	long ns;
	long aaN;
	double *aaPr;
	double *coefPr;
	long womM;
	double *womPr;
	double *kd2Pr;
	long nom;
	double *omPr;
	double *pldfvPr;
	double *pldfvPi;
	double *acfPr;
	unsigned long nag;
	unsigned long *ag;
	unsigned long nas;
	unsigned long *as;
	double *dydaPr;
	unsigned long nvarOK;
	unsigned long *varOK;
	double *yaPr;
	unsigned long i;
	pthread_mutex_t val_lock;
	double *aaOutPr;
	unsigned long yaSize;
	unsigned long *afree;
	unsigned long nscr;
	unsigned long nscr1;
	double *scr;
	double *scr1;
};
void *dirthe_loop(struct pth *);
#ifdef THTIME
#ifdef __linux__
/* Only used for linux, a high resolution timer operating at the CPU clock
 * speed is returned */
__inline__ unsigned long long int gethrtime (void)
{
  unsigned long long int x = 0;
  __asm__ volatile (".byte 0x0f, 0x31":"=A" (x));
  return x;
}
#endif
#endif

#ifdef ANSI_C
void MrqmndiagCalc(long ns,long aaN,double *aPr,double* ymPr,double *variancePr,long varianceM,long varianceN, 
	double* ftolPr,double *itMaxPr,double *coefPr,long coefM,long coefN,
	long womM,double *womPr,double *kd2Pr,long nom,double *omPr, 
	double *aaOutPr,double *chi2Pr,double *itsPr,double *alphaPr,double *pldfvPr,double *pldfvPi,double *physlimPr)
#else
void MrqmndiagCalc(ns,aaN,aPr,ymPr,variancePr,varianceM,varianceN,ftolPr,itMaxPr,coefPr,coefM,coefN,womM,womPr,kd2Pr,nom,omPr,aaOutPr,chi2Pr,itsPr,alphaPr,pldfvPr,pldfvPi,physlimPr)
long ns,aaN,varianceM,varianceN,coefM,coefN,womM,nom;
double *aPr,*ymPr,*variancePr,*ftolPr,*itMaxPr,*coefPr,*womPr,*kd2Pr,*omPr,*aaOutPr,*chi2Pr,*itsPr,*alphaPr,*pldfvPr,*pldfvPi,*physlimPr;
#endif
	{
	unsigned long i,j,nR;
	mxArray *ya,*dyda,*aa2,*tempYa,*R,*Y,*KhiSqr,*dA,*chi2Old,*yaOld,*aaOld,*tempDyda,*errorBar;
	double *yaPr,*dydaPr,*aa2Pr,*tempYaPr,*RPr,*YPr,*KhiSqrPr,*dAPr,*chi2OldPr,*yaOldPr,*aaOldPr;
	double *tempDydaPr,*errorBarPr;
	double *scr,*scr1;
	unsigned long nscr,nscr1;
	double lambda = 0.001,maxDeviation;
	long validDer = 0,its = 0,itMax,*indicesVector;
	mxArray *storage;

	unsigned long *varOK,nvarOK,*afree,nafree;
	mxArray *tempAlpha,*plim;
	double *tempAlphaPr,*plimPr;
	unsigned long nag=4,ag[]={0,1,2,3},nas=1,as[]={4};

	struct pth pth;
#ifdef THTIME
#ifdef __linux__
	unsigned long long int start1, start2, end1=0, end2=0;
#else /* if solaris */
	hrtime_t start1, start2, end1=0, end2=0; 
#endif
  	start1=gethrtime();
#endif

/* Get the max number of iterations from the matrix */

	itMax=(long)itMaxPr[0];

	storage=mxCreateDoubleMatrix(0, 0, mxREAL);	/* Do not save the angles in GULIPS_addm*/				

/* Check for free pars IH*/
	varOK=(unsigned long *)mxCalloc(varianceM*varianceN,sizeof(long));
	for(i=0,nvarOK=0;i<(varianceM*varianceN);i++)
		if(variancePr[i]!=0) varOK[nvarOK++]=i;
	afree=(unsigned long *)mxCalloc(aaN,sizeof(long));
	for(i=0,nafree=0;i<aaN;i++)
		if(variancePr[i+coefM]!=0) afree[nafree++]=i;
	tempAlpha=mxCreateDoubleMatrix(nafree,nafree,mxREAL);
	tempAlphaPr=mxGetPr(tempAlpha);
	for(i=0;i<aaN*aaN;i++) alphaPr[i]=0.;

/* Copy a into aaOut */
	for(i=0;i<aaN;i++)
		aaOutPr[i]=aPr[i];

	plim=mxCreateDoubleMatrix(2, aaN, mxREAL);
	plimPr=mxGetPr(plim);
	for(i=0;i<2*aaN;i++)
		plimPr[i]=physlimPr[i];

	aaOld=mxCreateDoubleMatrix(1, aaN, mxREAL);
	aaOldPr=mxGetPr(aaOld);

	chi2Old=mxCreateDoubleMatrix(1,1,mxREAL);
	chi2OldPr=mxGetPr(chi2Old);

/* Create matrices ya and yaOld and call Dirthe */

	ya=mxCreateDoubleMatrix((coefM + aaN), coefN, mxREAL);
	yaPr=mxGetPr(ya);

	yaOld=mxCreateDoubleMatrix((coefM + aaN), coefN, mxREAL);
	yaOldPr=mxGetPr(yaOld);

	nscr=(nion+2)*(3+4*nom); nscr1=((nion+1)*5+nom+aaN)*ns;
#ifdef GUPTHREAD
	scr = (double *)mxCalloc(nscr*nafree,sizeof(double));
	scr1= (double *)mxCalloc(nscr1*nafree,sizeof(double));
	tempYa=mxCreateDoubleMatrix((coefM + aaN),coefN*nafree,mxREAL);
	aa2=mxCreateDoubleMatrix(1,aaN*nafree,mxREAL);
#else
	scr = (double *)mxCalloc(nscr,sizeof(double));
	scr1= (double *)mxCalloc(nscr1,sizeof(double));
	tempYa=mxCreateDoubleMatrix((coefM + aaN),coefN,mxREAL);
	aa2=mxCreateDoubleMatrix(1,aaN,mxREAL);
#endif
	tempYaPr=mxGetPr(tempYa);
	aa2Pr=mxGetPr(aa2);

	DirtheCalc(ns,aaN,aaOutPr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,yaPr,1,scr,scr1);

/* Create dyda and the temporaty copy of dyda matrix for the spectrum derivatives */

	dyda=mxCreateDoubleMatrix(nvarOK,nafree, mxREAL);
	dydaPr=mxGetPr(dyda);

	tempDyda=mxCreateDoubleMatrix(nvarOK,nafree, mxREAL);
	tempDydaPr=mxGetPr(tempDyda);

	/* Copy the sqrt(variance) to the errorbar (for the GULIPS) */ 
	errorBar=mxCreateDoubleMatrix(nvarOK,1,mxREAL);
	errorBarPr=mxGetPr(errorBar);

	chi2Pr[0]=0;
	for (i=0;i<nvarOK;i++) {
		j=varOK[i];
		chi2Pr[0]+=(ymPr[j]-yaPr[j])*(ymPr[j]-yaPr[j])/variancePr[j];
		errorBarPr[i]=sqrt(variancePr[j]);
	}

	nR=(nafree*(nafree+1))/2;
	R=mxCreateDoubleMatrix(nR,1,mxREAL);	/* Note: this is right triangular matrix!!! */
	RPr=mxGetPr(R);

	Y=mxCreateDoubleMatrix(nafree,1,mxREAL);	/* Note: this is number of unknowns (m) times 1) */
	YPr=mxGetPr(Y);

	KhiSqr=mxCreateDoubleMatrix(1,1,mxREAL);
	KhiSqrPr=mxGetPr(KhiSqr);

	dA = mxCreateDoubleMatrix(1,nafree,mxREAL);	/* Matrix for the solution of the inverse problem */
	dAPr = mxGetPr(dA);

	indicesVector=(long *)mxCalloc(nafree,sizeof(long));
	for(i=0;i<nafree;i++)
		indicesVector[i]=i;

/* logscale pars IH*/
	for(j=0;j<nag;j++) {
		i=ag[j];
		aaOutPr[i]=log(aaOutPr[i]);
		plimPr[IND2(0,i,2)]=log(plimPr[IND2(0,i,2)]);
		plimPr[IND2(1,i,2)]=log(plimPr[IND2(1,i,2)]);
	}
	for(j=0;j<nas;j++) {
		i=as[j];
		aaOutPr[i]=asinh(aaOutPr[i]/2.);
		plimPr[IND2(0,i,2)]=asinh(plimPr[IND2(0,i,2)]/2.);
		plimPr[IND2(1,i,2)]=asinh(plimPr[IND2(1,i,2)]/2.);
	}

	/* set up thread structure*/
#ifdef GUPTHREAD
	pthread_attr_t sched_glob;
	pthread_t *thread;
	void *retval;
	pthread_attr_init(&sched_glob);
	pthread_mutex_init(&pth.val_lock,NULL);
	pthread_attr_setscope(&sched_glob,PTHREAD_SCOPE_SYSTEM);
	thread=(pthread_t *)mxCalloc(nafree,sizeof(pthread_t));
#endif
	pth.ns=ns;
	pth.aaN=aaN;
	pth.aaPr=aa2Pr;
	pth.coefPr=coefPr;
	pth.womM=womM;
	pth.womPr=womPr;
	pth.kd2Pr=kd2Pr;
	pth.nom=nom;
	pth.omPr=omPr;
	pth.pldfvPr=pldfvPr;
	pth.pldfvPi=pldfvPi;
	pth.acfPr=tempYaPr;
	pth.nag=nag;
	pth.ag=ag;
	pth.nas=nas;
	pth.as=as;
	pth.dydaPr=dydaPr;
	pth.nvarOK=nvarOK;
	pth.varOK=varOK;
	pth.yaPr=yaPr;
	pth.aaOutPr=aaOutPr;
	pth.yaSize=(coefM+aaN)*coefN;
	pth.afree=afree;
	pth.nscr=nscr;
	pth.nscr1=nscr1;
	pth.scr=scr;
	pth.scr1=scr1;
/* The actual fitting loop */
	while(its<itMax) {
		if(!validDer) {
			its++;
#ifdef THTIME
			start2=gethrtime(); end1+=(start2-start1);
#endif
			for(i=0;i<nafree;i++) {
#ifdef GUPTHREAD
				pthread_mutex_lock(&pth.val_lock);
				pth.i=i;
				if(pthread_create(&thread[i],&sched_glob,(void *(*)(void *)) dirthe_loop,(void *)(&pth)))
					mexErrMsgTxt("mrqmndiag: cannot thread");
#else
				pth.i=i;
				dirthe_loop(&pth);
#endif
			}
#ifdef GUPTHREAD
			for(i=0;i<nafree;i++)
				if(pthread_join(thread[i],&retval))
					mexErrMsgTxt("mrqmndiag: cannot join thread");
#endif
#ifdef THTIME
			start1=gethrtime(); end2+=(start1-start2);
#endif
		}

		/*Initialise R matrix */
		for(i=0;i<nR;i++)
			RPr[i]=0;

		/*Put sqrt(lambda) in the diagonal elements of R*/
		for(i=0;i<nafree;i++)
			RPr[IND(i,i,nafree)]=sqrt(lambda);

		/*Initialise Y matrix */
		for(i=0;i<nafree;i++)
			YPr[i]=0;

		/* Initialise KhiSqr */
		KhiSqrPr[0]=0;

		for(i=0;i<nvarOK*nafree;i++)
			tempDydaPr[i]=dydaPr[i];

		/* Calculate ya-ym (DirtheCalculated measumenets - real measurements)*/ 
		for(i=0;i<nvarOK;i++)
		 	tempYaPr[i]=(yaPr[varOK[i]]-ymPr[varOK[i]]);

		/* Calculate the solution using GULIPS */
 
		GULIPS_addmCalc(RPr,YPr,tempDydaPr,tempYaPr,KhiSqrPr,nvarOK,nafree,1,storage,errorBarPr);
		GULIPS_invRCalc(RPr,nafree);
		GULIPS_mulCalc(RPr,YPr,dAPr,1,nafree);
		GULIPS_covCalc(RPr,indicesVector,nafree,1,nafree,tempAlphaPr);
		for(i=0;i<nafree;i++) for(j=0;j<nafree;j++)
			alphaPr[IND2(afree[j],afree[i],aaN)]=tempAlphaPr[IND2(j,i,nafree)];

		/* Store old values chi2, ya and aa*/
		chi2OldPr[0]=chi2Pr[0];
	 	for(i=0;i<(coefM+aaN);i++)
			yaOldPr[i]=yaPr[i];
		for(i=0;i<aaN;i++)
			aaOldPr[i]=aaOutPr[i];

		for(i=0;i<nafree;i++) {
			j=afree[i];
			aaOutPr[j]+=dAPr[i];
			if(aaOutPr[j]<plimPr[IND2(0,j,2)]) aaOutPr[j]=plimPr[IND2(0,j,2)];
			else if(aaOutPr[j]>plimPr[IND2(1,j,2)]) aaOutPr[j]=plimPr[IND2(1,j,2)];
		}

/* logscale pars IH*/
		for(j=0;j<aaN;j++) aa2Pr[j]=aaOutPr[j];
		for(j=0;j<nag;j++) aa2Pr[ag[j]]=exp(aa2Pr[ag[j]]);
		for(j=0;j<nas;j++) aa2Pr[as[j]]=2.*sinh(aa2Pr[as[j]]);

		DirtheCalc(ns,aaN,aa2Pr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,yaPr,0,scr,scr1);
		chi2Pr[0]=0;
		for(i=0;i<nvarOK;i++) {
			j=varOK[i];
			chi2Pr[0]+=(ymPr[j]-yaPr[j])*(ymPr[j]-yaPr[j])/variancePr[j];
		}

		maxDeviation=0;
		for(i=0;i<nafree;i++)
			maxDeviation=max(ddabs(dAPr[i]),maxDeviation);

		if(chi2Pr[0]>=chi2OldPr[0]) {
			chi2Pr[0]=chi2OldPr[0];
			for(i=0;i<(coefM+aaN);i++)
				yaPr[i]=yaOldPr[i];
			for(i=0;i<aaN;i++)
				aaOutPr[i]=aaOldPr[i];
			lambda*=10;
			validDer=1;
			if(maxDeviation<ftolPr[0]) break;
		} else {
			lambda/=10;
			validDer=0;
			if(maxDeviation<ftolPr[0]) break;
		}
	}
	itsPr[0]=its;

/* logscale pars IH*/
	for(j=0;j<aaN;j++) aa2Pr[j]=1.;
	for(j=0;j<nag;j++) {
		aaOutPr[ag[j]]=exp(aaOutPr[ag[j]]);
		aa2Pr[ag[j]]=aaOutPr[ag[j]];
	}
	for(j=0;j<nas;j++) {
		aaOutPr[as[j]]=2.*sinh(aaOutPr[as[j]]);
		aa2Pr[as[j]]=sqrt(4.+aaOutPr[as[j]]*aaOutPr[as[j]]);
	}
	for(i=0;i<nafree;i++) for(j=0;j<nafree;j++)
		alphaPr[IND2(afree[j],afree[i],aaN)]*=(aa2Pr[afree[j]]*aa2Pr[afree[i]]);

#ifdef THTIME
	start2=gethrtime(); end1+=(start2-start1+end2);
	printf("Spec loop:%.3f %.1f\n",((float)end2)/((float)end1),end1/1e6);
#endif
}

void *dirthe_loop(struct pth *pth)
{
	unsigned long i,j,k=0,k1=0,k2=0,k3=0;
	i=pth->i;
#ifdef GUPTHREAD
	pthread_mutex_unlock(&pth->val_lock);
	k=i*pth->aaN;
	k1=i*pth->yaSize;
	k2=i*pth->nscr;
	k3=i*pth->nscr1;
#endif
	/*Copy aaOut into aa2*/
	for(j=0;j<pth->aaN;j++)
		pth->aaPr[j+k]=pth->aaOutPr[j];
	/* Change one of the parameters... */
	pth->aaPr[pth->afree[i]+k]+=0.0001;
	/* ...and calculate the derivatives using DirtheCalc 
	and one loop which calculates the erotusosam‰‰r‰*/

/* logscale pars IH*/
	for(j=0;j<pth->nag;j++)
		pth->aaPr[pth->ag[j]+k]=exp(pth->aaPr[pth->ag[j]+k]);
	for(j=0;j<pth->nas;j++)
		pth->aaPr[pth->as[j]+k]=2.*sinh(pth->aaPr[pth->as[j]+k]);

	DirtheCalc(pth->ns,pth->aaN,pth->aaPr+k,pth->coefPr,pth->womM,pth->womPr,pth->kd2Pr,pth->nom,pth->omPr,pth->pldfvPr,pth->pldfvPi,pth->acfPr+k1,0,pth->scr+k2,pth->scr1+k3);

	for(j=0;j<pth->nvarOK;j++)
		pth->dydaPr[IND2(j,i,pth->nvarOK)]=((pth->yaPr[pth->varOK[j]]-pth->acfPr[pth->varOK[j]+k1])/0.0001);
}
