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
      mwPointer PLHS(*),PRHS(*),mxGetPr,mxCreateDoubleMatrix
      INTEGER*4 NLHS, NRHS
      mwSize mxGetN,mxGetM
C KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
C IN ALL YOUR FORTRAN MEX FILES.
C---------------------------------------------------------------------
      integer magp,ttim(9),tim(6)
      real*8 par(6)
      real pos(3)
      data pos/69.2,19.2,200./
C CHECK FOR PROPER NUMBER OF ARGUMENTS
c     CALL MEXSETCANONICALMODE
      IF(NLHS.GT.1)CALL mexErrMsgTxt('Only one output argument')
      if(nrhs.gt.0)then
       n=mxGetN(prhs(1))*mxGetM(prhs(1));
       if(n.gt.3)then
        CALL mexErrMsgTxt('Max three numbers in array')
       elseif(n.gt.0)then
        call mxcopyptrtoreal8(mxgetpr(prhs(1)),par,n)
        do i=1,n
         pos(i)=par(i)
        enddo
       endif
      endif
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
      call myigrf(pos(1),pos(2),year,pos(3),xl,icode,par)
C CREATE MATRIX FOR RETURN ARGUMENT
      PLHS(1)=mxCreateDoubleMatrix(4,1,0)
C ASSIGN POINTER
      magp = mxGetPr(PLHS(1))
      call mxcopyreal8toptr(par,magp,4)
      RETURN
      END
      subroutine myigrf(xlat,xlong,year,height,xl,icode,out)
      real*8 out(*),mexgetnan
      CALL INITIZE
C     umr=ATAN(1.0)*4./180.
      CALL FELDCOF(YEAR,DIMO)
      CALL FELDG(xlat,xlong,HEIGHT,BNORTH,BEAST,BDOWN,BABS)
      CALL SHELLG(xlat,xlong,HEIGHT,DIMO,XL,ICODE,BAB1)
c     DIP=ASIN(BDOWN/BABS)/UMR
c     DEC=ASIN(BEAST/SQRT(BEAST*BEAST+BNORTH*BNORTH))/UMR
       out(1)=BNORTH*1e-4
       out(2)=BEAST*1e-4
       out(3)=BDOWN*1e-4
       out(4)=xl
       if(icode.eq.2)out(4)=mexgetnan()
      RETURN
      END
