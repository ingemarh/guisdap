function [ XM,YM,ZM ] = DPMTRX( context,~,~ )
%DPMTRX calculates othonormal matrix (columns XM,YM,ZM) for transformation 
% from geographic to magnetic coordinates
%       SUBROUTINE DPMTRX(IYYYY,DDD,XM,YM,ZM)
%--------------------------------------------------------------------------
%      calculates othonormal matrix (columns XM,YM,ZM) for transformation 
%      from geographic to magnetic coordinates
%      Inputs:
%             IYYYY..year
%               DDD..day of year (1.1 = 0)
%      Outputs:
%               XM,YM,ZM..colums of the matrix
%      Notes:
%      MX(N),MY(N),MZ(N)..coordinates of the B vector in geographic system 
%                for years stored in YR(N)
%      N..number of elements of arrays MX,MY,MZ and YR
%--------------------------------------------------------------------------

%       INTEGER IYYYY,DDD
%       REAL XM(3),YM(3),ZM(3)
%       REAL YR(10),MX(10),MY(10),MZ(10)
%       REAL INTERP,YEAR
%       REAL M,MXI,MYI,MZI,ZM12
%       INTEGER N
%
%       COMMON /DIPOL/ GHI1,GHI2,GHI3
  %persistent N;
  %if isempty(N)
  %  N = 10;
  %end

  % IGRF coefficients (dipole) calculated in FELDCOF in IGRF.FOR
  MXI = -context.GHI2;
  MYI = -context.GHI3;
  MZI = -context.GHI1;

  % normalization of the vector of the dipole exis of the magnetic field
  M=sqrt(MXI*MXI+MYI*MYI+MZI*MZI);
  %MYZ=sqrt(MYI*MYI+MZI*MZI);
  if M ~= 0.
    ZM(1)=MXI/M;
    ZM(2)=MYI/M;
    ZM(3)=MZI/M;
  else
    ZM(1)=0.;
    ZM(2)=0.;
    ZM(3)=1.;
  end
  ZM12=sqrt(ZM(1)*ZM(1)+ZM(2)*ZM(2));
  if ZM12 ~= 0.
    YM(1)=-ZM(2)/ZM12;
    YM(2)=ZM(1)/ZM12;
    YM(3)=0.;
  else
    YM(1)=0.;
    YM(2)=1.;
    YM(3)=0.;
  end
  XM(1)=YM(2)*ZM(3)-YM(3)*ZM(2);
  XM(2)=YM(3)*ZM(1)-YM(1)*ZM(3);
  XM(3)=YM(1)*ZM(2)-YM(2)*ZM(1);

end

