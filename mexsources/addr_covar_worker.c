#include <math.h>
#include "mex.h"

#define llabs(A) ((A)<(0) ? (-A) : (A))

extern void covar33Calc (long addr1, long addr2,
			 unsigned long signallength, unsigned long signalvcs,
			 double *vc_signal, unsigned long nlp, long *lp_vc,
			 long *lp_dt, long *lp_ra, long *lp_ri, long *lp_nt,
			 long *lp_t1, long *lp_t2, long *lp_dec,
			 long *lp_nfir, unsigned long maxfir, double *lp_fir,
			 double *covarRe, double *covarIm);

#ifdef ANSI_C
void
covar33Calc (long addr1, long addr2, unsigned long signallength, unsigned long signalvcs, double *vc_signal, unsigned long nlp,	/* common length of following variables */
	     long *lp_vc, long *lp_dt, long *lp_ra, long *lp_ri, long *lp_nt, long *lp_t1, long *lp_t2, long *lp_dec, long *lp_nfir, unsigned long maxfir,	/* size of longest filter */
	     double *lp_fir, double *covarRe, double *covarIm)
#else
void
covar33Calc (addr1, addr2, signallength, signalvcs, vc_signal,
	     nlp, lp_vc, lp_dt, lp_ra, lp_ri, lp_nt, lp_t1, lp_t2, lp_dec,
	     lp_nfir, maxfir, lp_fir, covarRe, covarIm)
     long addr1, addr2;
     unsigned long signallength, signalvcs;
     double *vc_signal;
     unsigned long nlp;		/* common length of following variables */
     long *lp_vc, *lp_dt, *lp_ra, *lp_ri, *lp_nt, *lp_t1, *lp_t2, *lp_dec,
       *lp_nfir;
     unsigned long maxfir;	/* size of longest filter */
     double *lp_fir, *covarRe, *covarIm;
#endif
{

  register long itime, itau, ero1, ero2;
#ifdef MAC
  register extended co1, co2;
  double apureal;
#else
  register double co1, co2, apureal;
#endif
  long i, lps1len, lps2len, lp1, lp2, ilp1, ilp2, dt1, dt2, vc, apu, time1,
    time2, tau1, tau2;
  long maxfirlp1, maxfirlp2, vcsignallength;

  long *lps1;
  long *lps2;
  lps1 = (long *) mxCalloc (nlp, sizeof (long));
  lps2 = (long *) mxCalloc (nlp, sizeof (long));
  lps1len = 0;
  lps2len = 0;

  for (i = 0; i < nlp; i++)
    {
/*	printf("%f ",((double)((addr1-lp_ra[i])/lp_ri[i])));
	printf("%f ",((double)((addr1-lp_ra[i])/lp_ri[i]))-((double)(addr1-lp_ra[i])/lp_ri[i]));
	printf("%f \n",((double)(addr1-lp_ra[i])/lp_ri[i]));
	printf("%ld %ld\n",addr1,lp_ra[i]);*/
      if ((lp_ra[i] <= addr1)
	  && (addr1 <= (lp_ra[i] + (lp_nt[i] - 1) * lp_ri[i]))
	  && (((double) ((addr1 - lp_ra[i]) / lp_ri[i])) ==
	      ((double) (addr1 - lp_ra[i]) / lp_ri[i])))
	{
	  lps1[lps1len] = i;
	  lps1len++;
	}
      if ((lp_ra[i] <= addr2)
	  && (addr2 <= (lp_ra[i] + (lp_nt[i] - 1) * lp_ri[i]))
	  && (((double) ((addr2 - lp_ra[i]) / lp_ri[i])) ==
	      ((double) (addr2 - lp_ra[i]) / lp_ri[i])))
	{
	  lps2[lps2len] = i;
	  lps2len++;
	}
    }

/*	printf("%ld %ld\n",lps1len,lps2len);*/


  co1 = 0;
  co2 = 0;
  for (ilp1 = 0; ilp1 < lps1len; ilp1++)
    {
      lp1 = lps1[ilp1];
      dt1 = lp_dt[lp1];
      maxfirlp1 = maxfir * lp1;
      for (ilp2 = 0; ilp2 < lps2len; ilp2++)
	{
	  lp2 = lps2[ilp2];
	  dt2 = lp_dt[lp2];
	  if (lp_vc[lp1] == lp_vc[lp2])
	    {
	      vc = lp_vc[lp1] - 1;
	      vcsignallength = vc * signallength;
	      maxfirlp2 = maxfir * lp2;

	      apu = lp_dec[lp1] * dt1 * (addr1 - lp_ra[lp1]) / lp_ri[lp1];
	      time1 = lp_t1[lp1] + apu;
	      time2 = lp_t2[lp1] + apu;
	      apu = lp_dec[lp2] * dt2 * (addr2 - lp_ra[lp2]) / lp_ri[lp2];
	      tau1 = lp_t1[lp2] + apu;
	      tau2 = lp_t2[lp2] + apu;
/*printf(	"%ld %ld %ld %ld \n\n ",time1, time2, tau1, tau2);	*/
	      for (itime = 0; itime < lp_nfir[lp1]; itime++)
		{
		  if (lp_fir[itime + maxfirlp1] != 0)
		    {
		      if (addr1 != addr2)
			{
			  for (itau = 0; itau < lp_nfir[lp2]; itau++)
			    {
			      if (lp_fir[itau + maxfirlp2] != 0)
				{
				  apu = itau * dt2 - itime * dt1;
				  apureal =
				    lp_fir[itime + maxfirlp1] * lp_fir[itau +
								       maxfirlp2];
				  ero1 = tau1 - time1 + apu;
				  if (ero1 < 0)
				    ero1 = -ero1;
				  ero2 = time2 - tau2 - apu;
				  if (ero2 < 0)
				    ero2 = -ero2;
				  if ((ero1 < signallength)
				      && (ero2 < signallength))
				    co1 +=
				      apureal * vc_signal[ero1 +
							  vcsignallength] *
				      vc_signal[ero2 + vcsignallength];
				  ero1 = tau2 - time1 + apu;
				  if (ero1 < 0)
				    ero1 = -ero1;
				  ero2 = time2 - tau1 - apu;
				  if (ero2 < 0)
				    ero2 = -ero2;
				  if ((ero1 < signallength)
				      && (ero2 < signallength))
				    co2 +=
				      apureal * vc_signal[ero1 +
							  vcsignallength] *
				      vc_signal[ero2 + vcsignallength];
				}
			    }
			}
		      else
			{
			  for (itau = itime; itau < lp_nfir[lp2]; itau++)
			    {
			      if (lp_fir[itau + maxfirlp2] != 0)
				{
				  apureal =
				    lp_fir[itime + maxfirlp1] * lp_fir[itau +
								       maxfirlp2];
				  apu = itau * dt2 - itime * dt1;
				  if (apu < 0)
				    apu = -apu;
				  if ((apu < signallength))
				    {
				      if (itau == itime)
					co1 +=
					  apureal * vc_signal[apu +
							      vcsignallength]
					  * vc_signal[apu + vcsignallength];
				      else
					co1 +=
					  2 * apureal * vc_signal[apu +
								  vcsignallength]
					  * vc_signal[apu + vcsignallength];
				    }
				  ero1 = tau2 - time1 + apu;
				  if (ero1 < 0)
				    ero1 = -ero1;
				  ero2 = time2 - tau1 - apu;
				  if (ero2 < 0)
				    ero2 = -ero2;
				  if ((ero1 < signallength)
				      && (ero2 < signallength))
				    {
				      if (itau == itime)
					co2 +=
					  apureal * vc_signal[ero1 +
							      vcsignallength]
					  * vc_signal[ero2 + vcsignallength];
				      else
					co2 +=
					  2 * apureal * vc_signal[ero1 +
								  vcsignallength]
					  * vc_signal[ero2 + vcsignallength];
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  *covarRe = (co1 + co2) / 2;
  *covarIm = (co1 - co2) / 2;
}
