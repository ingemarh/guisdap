function [ X,Y,Z ] = SHAG( context, X,Y,Z, DS )
%SHAG Similar to SUBR STEP from GEOPACK-1996 but SHAG takes into account
%  only internal sources
%      SUBROUTINE SHAG(X,Y,Z,DS)
%  *********************************************************************
%  Similar to SUBR STEP from GEOPACK-1996 but SHAG takes into account
%  only internal sources
%  The code is re-written from Tsyganenko's subroutine STEP by
%  Natalia and Vladimir Papitashvili in mid-1980s
%  *********************************************************************

%      COMMON/A5/DS3

  DS3 = -DS/3.;
  [R11,R12,R13] = context.RIGHT(X,Y,Z,DS3);
  [R21,R22,R23] = context.RIGHT(X+R11,Y+R12,Z+R13,DS3);
  [R31,R32,R33] = context.RIGHT(X+.5*(R11+R21), ...
                                Y+.5*(R12+R22), ...
                                Z+.5*(R13+R23),DS3);
  [R41,R42,R43] = context.RIGHT(X+.375*(R11+3.*R31), ...
                                Y+.375*(R12+3.*R32), ...
                                Z+.375*(R13+3.*R33),DS3);
  [R51,R52,R53] = context.RIGHT(X+1.5*(R11-3.*R31+4.*R41), ...
                                Y+1.5*(R12-3.*R32+4.*R42), ...
                                Z+1.5*(R13-3.*R33+4.*R43),DS3);
  X = X+.5*(R11+4.*R41+R51);
  Y = Y+.5*(R12+4.*R42+R52);
  Z = Z+.5*(R13+4.*R43+R53);

end

