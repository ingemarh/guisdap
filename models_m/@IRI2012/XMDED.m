function [ XMDED ] = XMDED( XHI,R,YW )
%XMDED ELECTRON DENSITY OF D MAXIMUM
%
%        REAL FUNCTION XMDED(XHI,R,YW)                
% D. BILITZA, 1978, CALCULATES ELECTRON DENSITY OF D MAXIMUM.                   
% XHI/DEG. IS SOLAR ZENITH ANGLE, R SMOOTHED ZURICH SUNSPOT NUMBER              
% AND YW/M-3 THE ASSUMED CONSTANT NIGHT VALUE.     
% [REF.: D.BILITZA, WORLD DATA CENTER A REPORT UAG-82,7,BOULDER,1981]
% corrected 4/25/97 - D. Bilitza

%        COMMON/CONST/UMR
%
  if XHI < 90
    y = 6.05E8 + 0.088E8 * R;
    yy = cos ( XHI * IRI2012.UMR );
    yyy = -0.1 / ( yy^2.7 );
    if yyy < -40.
      ymd=0.0;
    else
      ymd = y * exp(yyy);
    end
    if ymd < YW
      ymd = YW;
    end
    XMDED=ymd;
  else

    XMDED=YW;
  end

end

