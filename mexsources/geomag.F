#include "fintrf.h"
#ifndef mwPointer
#define mwPointer mwpointer
#define mwSize mwpointer
#endif
      SUBROUTINE MEXFUNCTION(NLHS, PLHS, NRHS, PRHS)
C This subroutine is the main gateway to MATLAB.  When a MEX function
C  is executed MATLAB calls the USRFCN subroutine in the corresponding
C  MEX file.  
C par=iri95(rp,tim,pos,h)
      mwPointer PLHS(*),PRHS(*),mxGetPr,mxCreateDoubleMatrix,m1,magp
      INTEGER*4 NLHS, NRHS
      mwSize mxGetN,mxGetM
C KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
C IN ALL YOUR FORTRAN MEX FILES.
C---------------------------------------------------------------------
      integer ttim(9),tim(6)
      real*8 par(6)
      real pos(3)
      data pos/69.2,19.2,200./
C CHECK FOR PROPER NUMBER OF ARGUMENTS
c     CALL MEXSETCANONICALMODE
      IF(NLHS.GT.1)CALL mexErrMsgTxt('Only one output argument')
      call gmtime(time(),ttim)
      do i=1,6
       tim(i)=ttim(7-i)
      enddo
      tim(1)=1900+tim(1)
      tim(2)=1+tim(2)
      if(nrhs.gt.1)then
       n=mxGetN(prhs(2))*mxGetM(prhs(2));
       if(n.gt.6)then
        CALL mexErrMsgTxt('Max six numbers in array')
       elseif(n.gt.0)then
        call mxcopyptrtoreal8(mxgetpr(prhs(2)),par,n)
        do i=1,n
         tim(i)=nint(par(i))
        enddo
       endif
      endif
      year=tim(1)+(tim(2)-1)/12.+
     +((tim(3)-1)+(tim(4)+(tim(5)+tim(6)/60.)/60.)/24.)/365.
      CALL INITIZE
      CALL FELDCOF(YEAR,DIMO)
      if(nrhs.eq.0.or.mxGetM(prhs(1)).ne.3)then
        CALL mexErrMsgTxt('Exact three numbers in row')
      endif
      n=mxGetN(prhs(1));
C CREATE MATRIX FOR RETURN ARGUMENT
      PLHS(1)=mxCreateDoubleMatrix(4,n,0)
C ASSIGN POINTER
      magp=mxGetPr(PLHS(1))
      m1=mxGetPr(PRHS(1))
      do i=0,n-1
        call mxcopyptrtoreal8(m1+i*3*8,par,3)
        do j=1,3
         pos(j)=par(j)
        enddo
        call myigrf(pos(1),pos(2),pos(3),par)
        call mxcopyreal8toptr(par,magp+i*4*8,4)
      enddo
      RETURN
      END
      subroutine myigrf(xlat,xlong,height,out)
      real*8 out(*),mexgetnan
      CALL FELDG(xlat,xlong,HEIGHT,BNORTH,BEAST,BDOWN,BABS)
c     CALL SHELLG(xlat,xlong,HEIGHT,DIMO,XL,ICODE,BAB1)
c     DIP=ASIN(BDOWN/BABS)/UMR
c     DEC=ASIN(BEAST/SQRT(BEAST*BEAST+BNORTH*BNORTH))/UMR
       out(1)=BEAST*1e-4
       out(2)=BNORTH*1e-4
       out(3)=-BDOWN*1e-4
       DIPL=ATAN(BDOWN/2.0/sqrt(BNORTH*BNORTH+BEAST*BEAST))
       out(4)=DIPL
c      if(icode.eq.2)out(4)=mexgetnan()
      RETURN
      END
