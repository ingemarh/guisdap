function [ R1,R2,R3 ] = RIGHT( context, X,Y,Z, DS3 )
%RIGHT Similar to SUBR RHAND from GEOPACK-1996 but RIGHT takes into account
%  only internal sources
%      SUBROUTINE RIGHT(X,Y,Z,R1,R2,R3)
%  *********************************************************************
%  Similar to SUBR RHAND from GEOPACK-1996 but RIGHT takes into account
%  only internal sources
%  The code is re-written from Tsyganenko's subroutine RHAND
%  by Natalia and Vladimir Papitashvili in mid-1980s
%  *********************************************************************

%      COMMON /A5/DS3
%      COMMON /NM/NM
%      COMMON /IYR/IYR

  [R,T,F] = IGRF.SPHCAR(X,Y,Z,IGRF.CARTESIAN_INPUT);
  [BR,BT,BF] = IGRF.IGRFM(context.IYR,context.NM,R,T,F);
  [BX,BY,BZ] = IGRF.BSPCAR(T,F,BR,BT,BF);
  mag = sqrt(BX^2+BY^2+BZ^2);
  if mag ~= 0
    B = DS3/mag;
    R1 = BX*B;
    R2 = BY*B;
    R3 = BZ*B;
  else
    R1 = BX;
    R2 = BY;
    R3 = BZ;
  end

end

