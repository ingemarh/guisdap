#include "fintrf.h"
#ifndef mwPointer
#define mwPointer mwpointer
#define mwSize mwpointer
#endif
#define maxheights 4096

      SUBROUTINE MEXFUNCTION(NLHS, PLHS, NRHS, PRHS)
C [profile,status]=msis(height,time,position,indices,cgs)
      mwPointer PLHS(*),PRHS(*),mxGetPr,mxCreateDoubleMatrix
      INTEGER*4 NLHS, NRHS
      mwSize mxGetN,mxGetM,nm
C---------------------------------------------------------------------
C This function will call GTD6 for evaluation of the MSIS90 neutral 
C atmosphere model. 
C Mikael Hedin <micce@irf.se>
      integer day,sec
      integer cgs
      integer nheights,pheights
      real time(2)
      real height(maxheights)
      real stl
      real pos(2),glat,glong
      real indices(3),f107a,f107,ap
      real*8 dd(maxheights,9),tt(2),dummy
      save indices
C
      data indices/100.,100.,5./
C-----------------------------------------------------------------------
C  check for proper number of arguments
      if(nlhs.gt.2)call mexErrMsgTxt('Max two output arguments')
      if(nrhs.lt.3)call mexErrMsgTxt('Min three input arguments')
C  check that arguments are numbers
      do i=1,nrhs
        if(mxIsNumeric(prhs(i)).eq.0)then
          call mexErrMsgTxt('All argument must be numbers')
        endif
      enddo
C  check the number of heights, and set a pointer
      nheights=mxGetN(prhs(1))*mxGetM(prhs(1))
      pheights=mxGetPr(prhs(1))
      if (nheights .gt. maxheights) then
        write (*,*) 'Maximum ',maxheights,' allowed, ',nheights, 
     *   ' is too much.'
        call mexErrMsgTxt('Too many heights')
      endif
      call myCopyToReal(prhs(1),height,nheights)
C  msis want height in [km] 
      do i=1,nheights
        height(i)=height(i)/1000.
      enddo
C  chech that time is a vector
      if(mxGetN(prhs(2))*mxGetM(prhs(2)).ne.2)then
        call mexErrMsgTxt('Time not a 2-vector')
      endif
      call myCopyToReal(prhs(2),time,2)
      day=time(1)
      sec=time(2)
C  check that position is a vector
      if(mxGetN(prhs(3))*mxGetM(prhs(3)).ne.2)then
        call mexErrMsgTxt('Position not a 2-vector')
      endif
      call myCopyToReal(prhs(3),pos,2)
      glat=pos(1)
      glong=pos(2)
C  check if indices given
      if(nrhs.gt.3)then
        nindices=mxGetN(prhs(4))*mxGetM(prhs(4))
        if(nindices.ne.3)then
          if(nindices.ne.0)then
            call mexErrMsgTxt('Indices not a 3-vector or empty')
          endif
        else
          call myCopyToReal(prhs(4),indices,3)
        endif
      endif
      f107a=indices(1)
      f107=indices(2)
      ap=indices(3)
C     check if cgs-flag is given
      call meters(.true.)
      if(nrhs.gt.4)then
         nm=1
         call mxCopyPtrToReal8(mxGetPr(prhs(5)),dummy,nm1)
         if(dummy.eq.0)then
            cgs=0
            call meters(.true.)
         else
            cgs=1
            call meters(.false.)
         endif
      endif
C  calculate solar local time
      stl=sec/3600+glong/15
C  initialize return arrays
      nm=nheights
      nm1=9 
      plhs(1)=mxCreateDoubleMatrix(nm,nm1,0)
      nm=2
      nm1=1
      plhs(2)=mxCreateDoubleMatrix(nm,nm1,0)
C
      call msis90(day,sec,height,glat,glong,stl,f107a,f107,
     ,ap,nheights,dd,tt)
      tt(2)=cgs                                                          cgs?
      nm=9*nheights
      call mxCopyReal8ToPtr(dd,mxGetPr(plhs(1)),nm)
      nm=2
      call mxCopyReal8ToPtr(tt,mxGetPr(plhs(2)),nm)
      return
      end
C
      subroutine myCopyToReal(prhs,var,n)
      mwPointer prhs,mxGetPr
      mwSize nm
      real*8 dum(maxheights)
      real var(*)
      nm=n
      call mxCopyPtrToReal8(mxGetPr(prhs),dum,nm)
      do i=1,n
        var(i)=dum(i)
      enddo
      return
      end
      subroutine msis90(day,sec,height,glat,glong,stl,f107a,f107,
     ,ap,nheights,dd,tt)
C  call the msis90 computation for each height (gtd7 is smart 
C  if just height changes)
      integer day,sec
      real height(*)      
      real*8 dd(nheights,9),tt(*)
      real d(8),t(2)
      do i=1,nheights
        call gtd7(day,sec,height(i),glat,glong,stl,f107a,f107,
     ,ap,48,d,t)
        dd(i,1)=d(1)                                                     He
        dd(i,2)=d(2)                                                     O
        dd(i,3)=d(3)                                                     N2
        dd(i,4)=d(4)                                                     O2  
        dd(i,5)=d(5)                                                     Ar  
        dd(i,6)=d(7)                                                     H    
        dd(i,7)=d(8)                                                     N
        dd(i,8)=d(6)                                                     mass
        dd(i,9)=t(2)                                                     T
      enddo
      tt(1)=t(1)                                                         ex T
      return
      end
