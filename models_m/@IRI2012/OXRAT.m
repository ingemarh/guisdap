function [ R4S,R2D,R2P ] = OXRAT( E )
%OXRAT electron impact branching ratios
%:::::::::::::::::::::::::::::::::::: OXRAT ::::::::::::::::::::::::::::::::
%      SUBROUTINE OXRAT(E,R4S,R2D,R2P)
%....... This subroutine returns the electron impact branching ratios
%....... for atomic oxygen from Burnett and Rountree Phys. Rev. A. 20
%....... 1979 page 1468
  R4S=1.0;
  R2D=0.0;
  %R2P=0.0;
  EV=E;
  if(E > 100.0)
    EV=100.0;
  end
  if(EV > 17)
    R4S=-1.6E-3*EV+0.56;
  end
  if(EV > 17)
    R2D=1.067E-3*EV+0.2933;
  end
  R2P=1-R4S-R2D;
  if(EV < 22)
     R2P=0.0;
     RTOT=R4S+R2D;
     R4S=R4S/RTOT;
     R2D=R2D/RTOT;
  end

end

