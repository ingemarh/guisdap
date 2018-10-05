function [ YIELD ] = YLDISS( ISW,ZLAM )
%YLDISS determination of dissociative yield of n+
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%        SUBROUTINE YLDISS(ISW,ZLAM,YIELD)
%..... determination of dissociative yield of n+, refer to kirby et al
%..... page 66 and table b

%      IMPLICIT NONE
%      INTEGER ISW,I,IPTS
%      REAL X(9),Y(9),ZLAM,YIELD
  persistent IPTS X Y;
  if isempty(IPTS)
    IPTS = 9;
    X = [50.,210.,240.,302.,387.,477.,496.,509.,2000.];
    Y = [.36,.36,.346,.202,.033,.041,.024,0.0,0.0];
  end
%
%
  for I=1:IPTS
% kjh 6/22/92   NOTE:  I realize the following statement is strange
%   looking, but its purpose is to prevent the CRAY compiler from
%   vectorizing this loop.  (Which it does incorrectly).
  %if(i.eq.25)write(6,*)' '
    if(ZLAM >= X(I) && ZLAM < X(I+1))
      break;
    end
  end
  if ~(ZLAM > 387 && ZLAM < 477)
%....... linear interpolation
    YIELD=(ZLAM-X(I))/(X(I+1)-X(I))*(Y(I+1)-Y(I))+Y(I);
  else
%...... parabolic interpolation see formula page 66 kirby et al
    YIELD=.0329+8.13E-6*(ZLAM-442)^2;
  end


end

