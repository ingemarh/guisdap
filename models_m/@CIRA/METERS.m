function [ context ] = METERS( context, METER )
%METERS Convert outputs to kg & meters if METER true or g and cm if false
%      SUBROUTINE METERS(METER)
%-----------------------------------------------------------------------
%      Convert outputs to Kg & Meters if METER true
%-----------------------------------------------------------------------

%      LOGICAL METER
%      COMMON/METSEL/IMR
%      SAVE
  context.IMR=0;
  if METER
    context.IMR=1;
  end

end

