#include "fintrf.h"
#ifndef mwPointer
#define mwPointer mwpointer
#define mwSize mwpointer
#endif

      SUBROUTINE mexFunction(NLHS, PLHS, NRHS, PRHS)
C par=iri95(rp,tim,pos,h)
      mwPointer PLHS(*),PRHS(*),mxGetPr,mxCreateDoubleMatrix
      INTEGER*4 NLHS, NRHS
      mwSize n,nrp,len,mxGetN,mxGetM
      real*8 mxGetNaN
C KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
C IN ALL YOUR FORTRAN MEX FILES.
C---------------------------------------------------------------------
      INTEGER parp,rp(12)
      logical jf(50)
      real*8 par(4500),dum(12)
      real outf(20,1000),oarr(100),tim(2),pos(2),h(3)
      save jf,nrp,rp,tim,pos,h
c jf tttff ftttt ttttt ttttt ftftt ttfff ttftf ----- ----- -----
      data jf/3*.true.,3*.false.,14*.true.,.false.,.true.,.false.,
     ,4*.true.,3*.false.,2*.true.,3*.false.,15*.true./,nrp/3/,
     ,rp/1,4,3,9*0/,tim/15552090.,1996./,pos/69.2,19.2/,h/100.,590.,0./
C CHECK FOR PROPER NUMBER OF ARGUMENTS
      IF(NLHS.GT.1)CALL MEXERRMSGTXT('Only one output argument')
      if(nrhs.gt.0)then
       n=mxGetN(prhs(1))
       if(n.gt.0)then
        nrp=n
        call mxcopyptrtoreal8(mxGetPr(prhs(1)),dum,n)
        do i=1,n
         rp(i)=nint(dum(i))
        enddo
       endif
      endif
      if(nrhs.gt.1) call mygetreal(prhs(2),tim,2,dum)
      if(nrhs.gt.2) call mygetreal(prhs(3),pos,2,dum)
      if(nrhs.gt.3) call mygetreal(prhs(4),h,3,dum)
      id=-tim(1)/86400.
      ut=tim(1)/3600.+id*24
      iy=nint(tim(2))
      if(h(3).eq.0.) h(3)=(h(2)-h(1))/99.
      len=(h(2)-h(1))/h(3)+1
      if(len.GT.500)CALL MEXERRMSGTXT('Max 500 heights')
      if(len*nrp.gt.4500)CALL MEXERRMSGTXT('Max 9par*500 heights')
      call initialize
      call iri_sub(jf,0,pos(1),pos(2),iy,id-1,ut+25.,h(1),h(2),h(3),
     ,outf,oarr)
      i=1
      do m=1,nrp
       if(rp(m).eq.12)then
        ut=h(1)
        do n=1,len
         par(i)=ut
         ut=ut+h(3)
         i=i+1
        enddo
       else
        do n=1,len
         par(i)=outf(rp(m),n)
         if(par(i).eq.-1.)par(i)=mxGetNaN()
         i=i+1
        enddo
       endif
      enddo
C CREATE MATRIX FOR RETURN ARGUMENT
      PLHS(1) = mxCreateDoubleMatrix(len,nrp,0)
C ASSIGN POINTER
      call mxcopyreal8toptr(par,mxGetPr(PLHS(1)),len*nrp)
      RETURN
      END
      subroutine mygetreal(prhs,v,maxn,dum)
      mwPointer prhs,mxGetPr
      mwSize n,mxGetN
      real*8 dum(*)
      real v(*)
      n=mxGetN(prhs)
      if(n.gt.0)then
       call mxcopyptrtoreal8(mxGetPr(prhs),dum,n)
       n=max(n,maxn)
       do i=1,n
        v(i)=dum(i)
       enddo
      endif
      return
      end
