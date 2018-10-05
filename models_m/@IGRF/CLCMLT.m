function [ MLT ] = CLCMLT( context,IYYYY,DDD,UTHR,GLAT,GLON )
%CLCMLT calculates magnetic local time
%
%       SUBROUTINE CLCMLT(IYYYY,DDD,UTHR,GLAT,GLON,MLT)
%--------------------------------------------------------------------
%      calculates magnetic local time
%      Inputs:
%             IYYYY..Year as YYYY, e.g. 1998
%             DDD..day of year (1.1. = 0)
%             UTHR..universal time in decimal hours
%             GLAT,GLON..latitude north and longitude east in degrees
%      Output:
%             MLT..magnetic local time in decimal hours
%--------------------------------------------------------------------

%       INTEGER IYYYY,DDD
%       REAL UTHR,GLAT,GLON,MLT
%       REAL DTOR,PI,XG,YG,ZG
%       REAL XXM(3),YYM(3),ZZM(3)
%       INTEGER IHOUR,MIN,ISEC
%       REAL GST,SLONG,SRASN,SDEC
%       REAL BE,CAL,SA(3),S,C,SG(3),SM(3)
%       REAL LAM,LAMS,DELLAM 
  SA = zeros(1,3);
  SG = zeros(1,3);
  SM = zeros(1,3);

  XG=cos(GLAT*IGRF.UMR)*cos(GLON*IGRF.UMR);
  YG=cos(GLAT*IGRF.UMR)*sin(GLON*IGRF.UMR);
  ZG=sin(GLAT*IGRF.UMR);
  [XXM,YYM,~] = context.DPMTRX(IYYYY,DDD);
       
  %       transform
  XM=XXM(1)*XG+XXM(2)*YG+XXM(3)*ZG;
  YM=YYM(1)*XG+YYM(2)*YG+YYM(3)*ZG;
  %ZM=ZZM(1)*XG+ZZM(2)*YG+ZZM(3)*ZG;

  IHOUR=floor(UTHR);
  MIN=floor((UTHR-IHOUR)*60);
  ISEC=floor((UTHR-IHOUR-MIN/60.0)*3600);
  [GST,~,SRASN,SDEC] = IGRF.SUN (IYYYY,DDD+1,IHOUR,MIN,ISEC);
  BE=GST;
  CAL=cos(SRASN);
  SA(3)=sin(SDEC);
  SA(1)=cos(SDEC);
  SA(2)=SA(1)*sin(SRASN);
  SA(1)=SA(1)*CAL;
  S=sin(BE);
  C=cos(BE);
  SG(1)=C*SA(1)+S*SA(2);
  SG(2)=C*SA(2)-S*SA(1);
  SG(3)=SA(3);
%       transform
  SM(1)=XXM(1)*SG(1)+XXM(2)*SG(2)+XXM(3)*SG(3);
  SM(2)=YYM(1)*SG(1)+YYM(2)*SG(2)+YYM(3)*SG(3);
  %SM(3)=ZZM(1)*SG(1)+ZZM(2)*SG(2)+ZZM(3)*SG(3);
%      
  LAM=atan2(YM,XM);
  LAMS=atan2(SM(2),SM(1));
  DELLAM=LAM-LAMS;
  if DELLAM < 0.
    DELLAM=DELLAM+IGRF.twopi;
  end
  MLT=mod(DELLAM/IGRF.pi*12.+12.,24.);

end

