# include <stdlib.h>
# include <stdio.h>
# include <string.h>
# include <math.h>
# include "mex.h"


#ifdef ESSL
#include "/usr/include/essl.h"
#endif

#define	max(A, B)	((A) > (B) ? (A) : (B))
#define	min(A, B)	((A) < (B) ? (A) : (B))
#define ddabs(A) ((A)<(0) ? (-A) : (A))
#define IND2(k1,k2,N) ((k1+N*k2))								/* Index of regular matrix */

#ifdef ANSI_C
extern void Transf(double *pPr,double *nin0Pr,double *tit0Pr,double *mim0Pr,double *psiPr,double *viPr);
extern void specCalc(double *pldfvPr,double *pldfvPi,double *nin0Pr,double *tit0Pr,long nion,double *mim0Pr,double *psiPr,double *viPr,double kd2Pr,double *scr,long nom,double *omPr,double *resPr,long ifref);
extern void mymul(char transa,double *mat1,long mat1m,long mat1n,char transb,double *mat2,long mat2m,long mat2n, double *res);
extern void DirtheCalc(long ns,long aaN,double *aaPr,double *coefPr,long womM,double *womPr,double *kd2Pr,long nom,double *omPr,double *pldfvPr,double *pldfvPi,double *acfPr,long use_reference,double *scr,double *scr1);
#else
	extern void Transf();
	extern void specCalc();
	extern void mymul();
	extern void DirtheCalc();
#endif

extern unsigned long nion;

void mymul(char transa,double *mat1,long mat1m,long mat1n,char transb,double *mat2,long mat2m,long mat2n, double *res)
{
  register long resM,resN,i,j,k;
  register double sum;
  double *ptr1,*ptr2;
 
  
  if((transa=='N') && (transb=='N'))
    {
      if(mat1n != mat2m)
	mexErrMsgTxt("field: matrix sizes incompatible in matrix product!");
      resM=mat1m;resN=mat2n;
      for(i=0;i<resN;i++)
	{
	  for(j=0;j<resM;j++)
	    {
	      sum=0.0;
	      ptr1 = &(mat1[IND2(j,0,mat1m)]);
	      ptr2 = &(mat2[IND2(0,i,mat2m)]);
	      for(k=0;k<mat2m;k++)
		{
		  sum+=(*ptr1)*(*ptr2);
		  ptr1 += mat1m;
		  ptr2++;
		}
	      res[IND2(j,i,resM)]=sum;
	    }
	}
    }
  else if ((transa=='T') && (transb=='N'))
    {
     if(mat1m != mat2m)
	mexErrMsgTxt("field: matrix sizes incompatible in matrix product!");
      resM=mat1n;resN=mat2n;
      for(i=0;i<resN;i++)
	{
	  for(j=0;j<resM;j++)
	    {
	      sum=0.0;
	      ptr1 = &(mat1[IND2(0,j,mat1m)]);
	      ptr2 = &(mat2[IND2(0,i,mat2m)]);
	      for(k=0;k<mat2m;k++)
		{
		  sum+=(*ptr1)*(*ptr2);
		  ptr1++;
		  ptr2++;
		}
	      res[IND2(j,i,resM)]=sum;
	    }	
	}
   }
  else if ((transa=='N') && (transb=='T'))
    {
      if(mat1n != mat2n)
	mexErrMsgTxt("field: matrix sizes incompatible in matrix product!");
      resM=mat1m;resN=mat2m;
      for(i=0;i<resN;i++)
	{
	  for(j=0;j<resM;j++)
	    {
	      sum=0.0;
	      ptr1 = &(mat1[IND2(j,0,mat1m)]);
	      ptr2 = &(mat2[IND2(i,0,mat2m)]);
	      for(k=0;k<mat2n;k++)
		{
		  sum+=(*ptr1)*(*ptr2);
		  ptr1 += mat1m;
		  ptr2 += mat2m;
		}
	      res[IND2(j,i,resM)]=sum;
	    }	
	}
    }
  else
    {
      if(mat1m != mat2n)
	mexErrMsgTxt("field:mn matrix sizes incompatible in matrix product!");
      resM=mat1n;resN=mat2m;
      for(i=0;i<resN;i++)
	{
	  for(j=0;j<resM;j++)
	    {
	      sum=0.0;
	      ptr1 = &(mat1[IND2(0,j,mat1m)]);
	      ptr2 = &(mat2[IND2(i,0,mat2m)]);
	      for(k=0;k<mat2n;k++)
		{
		  sum+=(*ptr1)*(*ptr2);
		  ptr1++;
		  ptr2 += mat2m;
		}
	      res[IND2(j,i,resM)]=sum;
	    }	
	}     
    }
}

#ifdef ANSI_C
void DirtheCalc(long ns,long aaN,double *aaPr,double *coefPr,long womM,
			   double *womPr,double *kd2Pr,long nom,double *omPr,
			   double *pldfvPr,double *pldfvPi,double *acfPr,long use_reference,double *scr,double *scr1)
#else
void DirtheCalc(ns,aaN,aaPr,coefPr,womM,womPr,kd2Pr,nom,omPr,pldfvPr,pldfvPi,acfPr,use_reference,scr,scr1)
		long ns,aaN,womM,nom,use_reference;
		double *aaPr,*coefPr,*womPr,*kd2Pr,*omPr,*acfPr,*pldfvPr,*pldfvPi,scr,scr1;
#endif	
	{
	unsigned long i;
	double *nin0Pr,*tit0Pr,*mim0Pr,*psiPr,*viPr,*specPr;
	double *pPr;
	register double *acf_ptr,*coeffg_ptr;

	/* Copy matrix AA_IN into p and then use p */
	
	nin0Pr = scr1;
	tit0Pr = nin0Pr+ns*(nion+1);
	mim0Pr = tit0Pr+ns*(nion+1);
	psiPr = mim0Pr+ns*(nion+1);
	viPr = psiPr+ns*(nion+1);
	specPr = viPr+ns*(nion+1);
	pPr = specPr+nom*ns;
	for(i=0;i<(aaN*ns);i++)
		pPr[i] = aaPr[i];
	
	pPr[3]=pPr[3]/sqrt(pPr[1]);

	Transf(pPr,nin0Pr,tit0Pr,mim0Pr,psiPr,viPr);
        specCalc(pldfvPr,pldfvPi,nin0Pr,tit0Pr,nion,mim0Pr,psiPr,viPr,kd2Pr[0],scr,nom,omPr,specPr,use_reference);
	/*acf=[p_coeffg.*(f_womega*s);col(aa)];*/
	
#ifdef ESSL
	long acfN = 1;
	dgemul(womPr,womM,"N",specPr,nom,"N",acfPr,womM,womM,nom,acfN);
/* Ambiguity fuction is already multiplied by the coefficients, so we don't have to do that any more here!*/
	acf_ptr = acfPr;coeffg_ptr = coefPr;
	for(i=0;i<womM;i++)
	  {
	    (*acf_ptr) *= (*coeffg_ptr);
	    acf_ptr++;coeffg_ptr++;
	  }
	for(i=0;i<aaN;i++)
	  acfPr[i+womM]= aaPr[i];

#else	
/*	for(i=0;i<womM;i++)
		{
		sum = 0;
		for(j=0;j<nom;j++)
			{
			sum += womPr[IND2(i,j,womM)]*specPr[j];
			}
		acfPr[i]=coefPr[i]*sum;
		}
	for(i=womM;i<(womM+aaN-1);i++)
		{
		acfPr[i]= aaPr[i-womM];
		}*/
		
	mymul('N',womPr,womM,nom,'N',specPr,nom,1,acfPr);
/* Ambiguity fuction is already multiplied by the coefficients, so we don't have to do that any more here!*/
	acf_ptr = acfPr;coeffg_ptr = coefPr;
	for(i=0;i<womM;i++,acf_ptr++,coeffg_ptr++)
	    (*acf_ptr) *= (*coeffg_ptr);
	for(i=0;i<aaN;i++)
	  acfPr[i+womM]= aaPr[i];

#endif	
      }
