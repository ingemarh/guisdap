#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mex.h"

#include <time.h>
#ifdef UNIX
#include <sys/types.h>
#include <sys/times.h>
#endif

#define	max(A, B)	((A) > (B) ? (A) : (B))
#define	min(A, B)	((A) < (B) ? (A) : (B))
#define ddabs(A) ((A)<(0) ? (-A) : (A))
#define IND(k1,k2,N) (k1*N - ((k1+2)*(k1-1))/2L + k2-1)	/* Index of right triangular matrix */
#define IND2(k1,k2,N) ((k1+N*k2))	/* Index of regular matrix */


#ifdef ANSI_C
extern void DirtheCalc(long ns,long aaN,double *aaPr,double *coefPr,long womM,double *womPr,double *kd2Pr,long nom,double *omPr,double *pldfvPr,double *pldfvPi,double *acfPr,long use_reference);
extern void GULIPS_addmCalc(double *R, double *y, double *A, double *m,double *KhiSqr,unsigned long nnew,unsigned long npar,unsigned long nmeas,mxArray* storage, double *errorBarPr);
extern void GULIPS_covCalc(double *R,long *indices,unsigned long indiceslength,unsigned long code,unsigned long npar, double *cov);
extern void GULIPS_mulCalc(double *R,double *y,double *sol,unsigned long yn,unsigned long ym);
extern void GULIPS_invRCalc(double *R,unsigned long npar);
extern void MrqmndiagCalc(long ns,long aaN,double *aPr,double* ymPr,double *variancePr,long varianceM,long varianceN, 
	double* ftolPr,double *itMaxPr,double *coefPr,long coefM,long coefN,
	long womM,double *womPr,double *kd2Pr,long nom,double *omPr, 
	double *aaOutPr,double *chi2Pr,double *itsPr,double *alphaPr,double *pldfvPr,double *pldfvPi,double *physlimPr);
#else
extern void DirtheCalc();
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
	double lambda = 0.001,maxDeviation;
	long validDer = 0,its = 0,itMax,*indicesVector;
	mxArray *storage;

	unsigned long *varOK,nvarOK,*afree,nafree;
	mxArray *tempAlpha,*plim;
	double *tempAlphaPr,*plimPr;
	unsigned long nag=4,ag[]={0,1,2,3},nas=1,as[]={4};

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

	DirtheCalc(ns,aaN,aaOutPr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,yaPr,1);

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

	aa2=mxCreateDoubleMatrix(1,aaN,mxREAL);
	aa2Pr=mxGetPr(aa2);

	tempYa=mxCreateDoubleMatrix((coefM + aaN),coefN,mxREAL);
	tempYaPr=mxGetPr(tempYa);

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

/* The actual fitting loop */
	while(its<itMax)
		{
		if(!validDer)
			{
			its++;
			for(i=0;i<nafree;i++)
				{
				/*Copy aaOut into aa2*/
				for(j=0;j<aaN;j++)
					aa2Pr[j]=aaOutPr[j];

				/* Change one of the parameters... */
				aa2Pr[afree[i]]+=0.0001;

				/* ...and calculate the derivatives using DirtheCalc 
				and one loop which calculates the erotusosamäärä*/

/* logscale pars IH*/
				for(j=0;j<nag;j++)
					aa2Pr[ag[j]]=exp(aa2Pr[ag[j]]);
				for(j=0;j<nas;j++)
					aa2Pr[as[j]]=2.*sinh(aa2Pr[as[j]]);

				DirtheCalc(ns,aaN,aa2Pr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,tempYaPr,0);

				for(j=0;j<nvarOK;j++)
					dydaPr[IND2(j,i,nvarOK)]=((yaPr[varOK[j]]-tempYaPr[varOK[j]])/0.0001);
				}
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

		DirtheCalc(ns,aaN,aa2Pr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,yaPr,0);
		chi2Pr[0]=0;
		for(i=0;i<nvarOK;i++) {
			j=varOK[i];
			chi2Pr[0]+=(ymPr[j]-yaPr[j])*(ymPr[j]-yaPr[j])/variancePr[j];
		}

		maxDeviation=0;
		for(i=0;i<nafree;i++)
			maxDeviation=max(ddabs(dAPr[i]),maxDeviation);

		if(chi2Pr[0]>=chi2OldPr[0])
			{
			chi2Pr[0]=chi2OldPr[0];
			for(i=0;i<(coefM+aaN);i++)
				yaPr[i]=yaOldPr[i];
			for(i=0;i<aaN;i++)
				aaOutPr[i]=aaOldPr[i];
			lambda*=10;
			validDer=1;
			if(maxDeviation<ftolPr[0]) break;
			}
		else
			{
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

	}
