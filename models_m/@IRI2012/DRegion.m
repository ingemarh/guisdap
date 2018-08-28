function [ elg ] = DRegion( z,it,f,vKp,f5SW,f6WA )
%DRegion D region parameters
%
%      Subroutine DRegion(z,it,f,vKp,f5SW,f6WA,elg)
%-----------------------------------------------------------------------
% Reference: Danilov, Rodevich, and Smirnova, Adv. Space Res.  
%     15, #2, 165, 1995.
%
% Input:     z    - solar zenith angle in degrees
%            it   - season (month)
%            f    - F10.7 solar radio flux (daily)
%            vKp  - Kp magnetic index (3-hour)
%            f5SW - indicator for Stratospheric Warming (SW) conditions
%                   =0 no SW, =0.5 minor SW, =1 major SW
%            f6WA - indicator for Winter Anomaly (WA) conditions
%                   =0 no WA, =0.5 weak WA, =1 strong WA
% Criteria for SW and WA indicators:
%      SW minor:  Temperature increase at the 30 hPa level by 10 deg.
%      SA major:  The same but by 20 degrees.
%         Temperature data for each year are published  
%         in Beilage zur Berliner Wetterkarte (K. Labitzke et al.).
%      WA weak:   An increase of the absorption in the 2-2.8 MHz  
%                 range at short A3 paths by 15 dB
%      WA strong: The same by 30 dB.
% 
%       Only for month 12 to 2 (winter).
%
% Output:      elg(7)  alog10 of electron density [cm-3] at h=60,65,
%                  70,75,80,85, and 90km
%-----------------------------------------------------------------------

%or   dimension h(7),A0(7),A1(7),A2(7),A3(7),A4(7),A5(7),A6(7),elg(7)
%      dimension A0(7),A1(7),A2(7),A3(7),A4(7),A5(7),A6(7),elg(7)
  persistent A0 A1 A2 A3 A4 A5 A6;
  if isempty(A0)
    A0 = [1.0,1.2,1.4,1.5,1.6,1.7,3.0];
    A1 = [0.6,0.8,1.1,1.2,1.3,1.4,1.0];
    A2 = [0.,0.,0.08,0.12,0.05,0.2,0.];
    A3 = [0.,0.,0.,0.,0.,0.,1.];
    A4 = [0.,0.,-0.30,0.10,0.20,0.30,0.15];
    A5 = [0.,-0.10,-0.20,-0.25,-0.30,-.30,0.];
    A6 = [0.,0.1,0.3,0.6,1.,1.,0.7];
  end
  elg = zeros(7,1);
  if z <= 45
    f1z=1.;
  else
    if z < 90
      f1z=1.1892*(cos(z*IRI2012.UMR))^0.5;
    else
      f1z=0.;
    end
  end
  if (it >= 5) && (it <= 9) % May, Jun, Jul, Aug, Sep
    f4S=0.;
    f5SW=0.;
    f6WA=0.;
  elseif (it == 3) || (it == 4) || (it == 10) || (it == 11) % Mar, Apr, Oct, Nov
    f4S=0.5;
    f5SW=0.;
    f6WA=0.;
  else
    f4S=1.; % Jan, Feb, Dec
  end
  if (vKp > 2)
    f2Kp=2.;
  else
    f2Kp=double(vKp);
  end
  f3F=(double(f)-60.)/300.*f1z;
  for i=1:7
    elg(i)=A0(i)+A1(i)*f1z+A2(i)*f2Kp+A3(i)*f3F+A4(i)*f4S ...
      +A5(i)*f5SW+A6(i)*f6WA;
  end

end

