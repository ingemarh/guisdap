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
#include <unistd.h>
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
	double *dyda;
	unsigned long nvarOK;
	unsigned long *varOK;
	double *ya;
	unsigned long i;
	pthread_mutex_t val_lock;
	double *aaOutPr;
	unsigned long yaSize;
	unsigned long *afree;
	unsigned long nscr;
	unsigned long nscr1;
	double *scr;
	double *scr1;
	unsigned long nth;
	double *p_m0;
	unsigned long nion;
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
	double *aaOutPr,double *chi2Pr,double *itsPr,double *alphaPr,double *pldfvPr,double *pldfvPi,double *physlimPr,double *p_m0,long nion)
#else
void MrqmndiagCalc(ns,aaN,aPr,ymPr,variancePr,varianceM,varianceN,ftolPr,itMaxPr,coefPr,coefM,coefN,womM,womPr,kd2Pr,nom,omPr,aaOutPr,chi2Pr,itsPr,alphaPr,pldfvPr,pldfvPi,physlimPr,p_m0,nion)
long ns,aaN,varianceM,varianceN,coefM,coefN,womM,nom,nion;
double *aPr,*ymPr,*variancePr,*ftolPr,*itMaxPr,*coefPr,*womPr,*kd2Pr,*omPr,*aaOutPr,*chi2Pr,*itsPr,*alphaPr,*pldfvPr,*pldfvPi,*physlimPr;
#endif
{
	unsigned long i,j,nR,nscr,nscr1;
	double *ya,*dyda,*aa2,*tempYa,*R,*Y,*KhiSqr,*dA,*yaOld,*aaOld;
	double *tempDyda,*errorBar,*scr,*scr1,*RowStorage;
	double lambda=0.001,maxDeviation,chi2Old;
	long validDer=0,its=0,itMax,*indicesVector;
	mxArray *storage;

	unsigned long *varOK,nvarOK,*afree,nafree,nth=1;
	double *tempAlpha,*plim;
	unsigned long nag=4,ag[]={0,1,2,3},nas=3,as[]={4,0,0};
	struct pth pth;

#ifdef THTIME
#ifdef __linux__
	unsigned long long int start1, start2, end1=0, end2=0;
#else /* if solaris */
	hrtime_t start1, start2, end1=0, end2=0; 
#endif
  	start1=gethrtime();
#endif

	for(i=1;i<3;i++)
		as[i]=3+nion+i;
/* Get the max number of iterations from the matrix */

	itMax=(long)itMaxPr[0];

	storage=mxCreateDoubleMatrix(0, 0, mxREAL);	/* Do not save the angles in GULIPS_addm*/				

/* Check for free pars IH*/
	afree=(unsigned long *)mxCalloc(aaN+varianceM*varianceN,sizeof(long));
	for(i=0,nafree=0;i<aaN;i++)
		if(variancePr[i+coefM]!=0) afree[nafree++]=i;
	varOK=afree+aaN;
	for(i=0,nvarOK=0;i<(varianceM*varianceN);i++)
		if(variancePr[i]!=0) varOK[nvarOK++]=i;
	for(i=0;i<aaN*aaN;i++) alphaPr[i]=0.;

/* Copy a into aaOut */
	for(i=0;i<aaN;i++)
		aaOutPr[i]=aPr[i];

	aaOld=(double *)mxCalloc(3*aaN+nafree*nafree+(coefM+aaN)*coefN*2,sizeof(double));
	plim=aaOld+aaN;
	for(i=0;i<2*aaN;i++)
		plim[i]=physlimPr[i];
	tempAlpha=plim+2*aaN;

	ya=tempAlpha+nafree*nafree;
	yaOld=ya+(coefM+aaN)*coefN;

#ifdef GUPTHREAD
/* Do not thread if only one CPU */
	if(sysconf(_SC_NPROCESSORS_ONLN)>1) nth=nafree;
	pthread_attr_t sched_glob;
	pthread_t *thread;
	void *retval;
	if(nth>1) {
		pthread_attr_init(&sched_glob);
		pthread_mutex_init(&pth.val_lock,NULL);
		pthread_attr_setscope(&sched_glob,PTHREAD_SCOPE_SYSTEM);
		thread=(pthread_t *)mxCalloc(nth,sizeof(pthread_t));
	}
#endif
	nscr=(nion+2)*(3+4*nom); nscr1=((nion+1)*5+nom+aaN)*ns;
	scr=(double *)mxCalloc((nscr+nscr1)*nth,sizeof(double));
	scr1=scr+nscr*nth;
	aa2=(double *)mxCalloc((aaN+(coefM+aaN)*coefN)*nth,sizeof(double));
	tempYa=aa2+aaN*nth;

	DirtheCalc(ns,aaN,aaOutPr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,ya,1,p_m0,nion,scr,scr1);

/* Create dyda and the temporaty copy of dyda matrix for the spectrum derivatives */

	dyda=(double *)mxCalloc(nvarOK*nafree*2,sizeof(double));
	tempDyda=dyda+nvarOK*nafree;

	/* Copy the sqrt(variance) to the errorbar (for the GULIPS) */ 
	nR=(nafree*(nafree+1))/2;
	errorBar=(double *)mxCalloc(nvarOK+nR+nafree+1+nafree+1+nafree,sizeof(double));
	chi2Pr[0]=0;
	for (i=0;i<nvarOK;i++) {
		j=varOK[i];
		chi2Pr[0]+=(ymPr[j]-ya[j])*(ymPr[j]-ya[j])/variancePr[j];
		errorBar[i]=sqrt(variancePr[j]);
	}

	R=errorBar+nvarOK;	/* Note: this is right triangular matrix!!! */
	Y=R+nR;		/* Note: this is number of unknowns (m) times 1) */
	KhiSqr=Y+nafree;
	dA=KhiSqr+1;	/* Matrix for the solution of the inverse problem */
	RowStorage=dA+nafree; /*For GULIPS internal use*/

	indicesVector=(long *)mxCalloc(nafree,sizeof(long));
	for(i=0;i<nafree;i++)
		indicesVector[i]=i;

/* logscale pars IH*/
	for(j=0;j<nag;j++) {
		i=ag[j];
		aaOutPr[i]=log(aaOutPr[i]);
		plim[IND2(0,i,2)]=log(plim[IND2(0,i,2)]);
		plim[IND2(1,i,2)]=log(plim[IND2(1,i,2)]);
	}
	for(j=0;j<nas;j++) {
		i=as[j];
		aaOutPr[i]=asinh(aaOutPr[i]/2.);
		plim[IND2(0,i,2)]=asinh(plim[IND2(0,i,2)]/2.);
		plim[IND2(1,i,2)]=asinh(plim[IND2(1,i,2)]/2.);
	}

	/* set up thread structure*/
	pth.ns=ns;
	pth.aaN=aaN;
	pth.aaPr=aa2;
	pth.coefPr=coefPr;
	pth.womM=womM;
	pth.womPr=womPr;
	pth.kd2Pr=kd2Pr;
	pth.nom=nom;
	pth.omPr=omPr;
	pth.pldfvPr=pldfvPr;
	pth.pldfvPi=pldfvPi;
	pth.acfPr=tempYa;
	pth.nag=nag;
	pth.ag=ag;
	pth.nas=nas;
	pth.as=as;
	pth.dyda=dyda;
	pth.nvarOK=nvarOK;
	pth.varOK=varOK;
	pth.ya=ya;
	pth.aaOutPr=aaOutPr;
	pth.yaSize=(coefM+aaN)*coefN;
	pth.afree=afree;
	pth.nscr=nscr;
	pth.nscr1=nscr1;
	pth.scr=scr;
	pth.scr1=scr1;
	pth.nth=nth;
	pth.p_m0=p_m0;
	pth.nion=nion;
/* The actual fitting loop */
	while(its<itMax) {
		if(!validDer) {
			its++;
#ifdef THTIME
			start2=gethrtime(); end1+=(start2-start1);
#endif
			for(i=0;i<nafree;i++) {
#ifdef GUPTHREAD
				if(nth>1) {
					pthread_mutex_lock(&pth.val_lock);
					pth.i=i;
					if(pthread_create(&thread[i],&sched_glob,(void *(*)(void *)) dirthe_loop,(void *)(&pth)))
						mexErrMsgTxt("mrqmndiag: cannot thread");
				} else {
					pth.i=i;
					dirthe_loop(&pth);
				}
#else
				pth.i=i;
				dirthe_loop(&pth);
#endif
			}
#ifdef GUPTHREAD
			if(nth>1)
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
			R[i]=0;

		/*Put sqrt(lambda) in the diagonal elements of R*/
		for(i=0;i<nafree;i++)
			R[IND(i,i,nafree)]=sqrt(lambda);

		/*Initialise Y matrix */
		for(i=0;i<nafree;i++)
			Y[i]=0;

		/* Initialise KhiSqr */
		KhiSqr[0]=0;

		for(i=0;i<nvarOK*nafree;i++)
			tempDyda[i]=dyda[i];

		/* Calculate ya-ym (DirtheCalculated measumenets - real measurements)*/ 
		for(i=0;i<nvarOK;i++)
		 	tempYa[i]=(ya[varOK[i]]-ymPr[varOK[i]]);

		/* Calculate the solution using GULIPS */
 
		GULIPS_addmCalc(R,Y,tempDyda,tempYa,KhiSqr,nvarOK,nafree,1,storage,errorBar,RowStorage);
		GULIPS_invRCalc(R,nafree);
		GULIPS_mulCalc(R,Y,dA,1,nafree);
		GULIPS_covCalc(R,indicesVector,nafree,1,nafree,tempAlpha);
		for(i=0;i<nafree;i++) for(j=0;j<nafree;j++)
			alphaPr[IND2(afree[j],afree[i],aaN)]=tempAlpha[IND2(j,i,nafree)];

		/* Store old values chi2, ya and aa*/
		chi2Old=chi2Pr[0];
	 	for(i=0;i<(coefM+aaN);i++)
			yaOld[i]=ya[i];
		for(i=0;i<aaN;i++)
			aaOld[i]=aaOutPr[i];

		for(i=0;i<nafree;i++) {
			j=afree[i];
			aaOutPr[j]+=dA[i];
			if(aaOutPr[j]<plim[IND2(0,j,2)]) aaOutPr[j]=plim[IND2(0,j,2)];
			else if(aaOutPr[j]>plim[IND2(1,j,2)]) aaOutPr[j]=plim[IND2(1,j,2)];
		}

/* logscale pars IH*/
		for(j=0;j<aaN;j++) aa2[j]=aaOutPr[j];
		for(j=0;j<nag;j++) aa2[ag[j]]=exp(aa2[ag[j]]);
		for(j=0;j<nas;j++) aa2[as[j]]=2.*sinh(aa2[as[j]]);

		DirtheCalc(ns,aaN,aa2,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,ya,0,p_m0,nion,scr,scr1);
		chi2Pr[0]=0;
		for(i=0;i<nvarOK;i++) {
			j=varOK[i];
			chi2Pr[0]+=(ymPr[j]-ya[j])*(ymPr[j]-ya[j])/variancePr[j];
		}

		maxDeviation=0;
		for(i=0;i<nafree;i++)
			maxDeviation=max(ddabs(dA[i]),maxDeviation);

		if(chi2Pr[0]>=chi2Old) {
			chi2Pr[0]=chi2Old;
			for(i=0;i<(coefM+aaN);i++)
				ya[i]=yaOld[i];
			for(i=0;i<aaN;i++)
				aaOutPr[i]=aaOld[i];
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
	for(j=0;j<aaN;j++) aa2[j]=1.;
	for(j=0;j<nag;j++) {
		aaOutPr[ag[j]]=exp(aaOutPr[ag[j]]);
		aa2[ag[j]]=aaOutPr[ag[j]];
	}
	for(j=0;j<nas;j++) {
		aaOutPr[as[j]]=2.*sinh(aaOutPr[as[j]]);
		aa2[as[j]]=sqrt(4.+aaOutPr[as[j]]*aaOutPr[as[j]]);
	}
	for(i=0;i<nafree;i++) for(j=0;j<nafree;j++)
		alphaPr[IND2(afree[j],afree[i],aaN)]*=(aa2[afree[j]]*aa2[afree[i]]);

#ifdef THTIME
	start2=gethrtime(); end1+=(start2-start1+end2);
	printf("Spec loop:%.3f %.1f\n",((float)end2)/((float)end1),end1/1e6);
#endif
}

void *dirthe_loop(struct pth *pth)
{
	unsigned long i,j=0;
	double *aa2,*acf,*scr,*scr1;
	i=pth->i;
#ifdef GUPTHREAD
	if(pth->nth>1) {
		pthread_mutex_unlock(&pth->val_lock);
		j=i;
	}
#endif
	aa2=j*pth->aaN+pth->aaPr;
	acf=j*pth->yaSize+pth->acfPr;
	scr=j*pth->nscr+pth->scr;
	scr1=j*pth->nscr1+pth->scr1;
	/*Copy aaOut into aa2*/
	for(j=0;j<pth->aaN;j++)
		aa2[j]=pth->aaOutPr[j];
	/* Change one of the parameters... */
	aa2[pth->afree[i]]+=0.0001;
	/* ...and calculate the derivatives using DirtheCalc 
	and one loop which calculates the erotusosam‰‰r‰*/

/* logscale pars IH*/
	for(j=0;j<pth->nag;j++)
		aa2[pth->ag[j]]=exp(aa2[pth->ag[j]]);
	for(j=0;j<pth->nas;j++)
		aa2[pth->as[j]]=2.*sinh(aa2[pth->as[j]]);

	DirtheCalc(pth->ns,pth->aaN,aa2,pth->coefPr,pth->womM,pth->womPr,pth->kd2Pr,pth->nom,pth->omPr,pth->pldfvPr,pth->pldfvPi,acf,0,pth->p_m0,pth->nion,scr,scr1);

	for(j=0;j<pth->nvarOK;j++)
		pth->dyda[IND2(j,i,pth->nvarOK)]=((pth->ya[pth->varOK[j]]-acf[pth->varOK[j]])/0.0001);
	return NULL;
}
