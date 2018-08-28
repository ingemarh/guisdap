function [ context,retrieveFirst ] = TSELEC( context, SV )
%TSELEC SET SWITCHES
%      SUBROUTINE TSELEC(SV)
%-----------------------------------------------------------------------
%        SET SWITCHES
%        Output in  COMMON/CSW/SW(25),ISW,SWC(25)
%        SW FOR MAIN TERMS, SWC FOR CROSS TERMS
%  
%        TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SV),
%        WHERE SV IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1. 
%        FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
%
%        To get current values of SW: CALL TRETRV(SW)
%-----------------------------------------------------------------------

%      DIMENSION SV(1),SAV(25),SVV(1)
%      COMMON/CSW/SW(25),ISW,SWC(25)
%      SAVE
  if nargout > 1
    retrieveFirst = context.SAV;
  end
  if nargin > 1
    for I = 1:CIRA.maxSW
      context.SAV(I)=SV(I);
      context.SW(I)=mod(SV(I),2.);
      if abs(SV(I)) == 1 || abs(SV(I)) == 2.
        context.SWC(I)=1.;
      else
        context.SWC(I)=0.;
      end
    end
    context.ISW=CIRA.ISWinitialized;
  end
end

