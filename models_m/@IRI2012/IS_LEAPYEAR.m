function [ leap ] = IS_LEAPYEAR( yyyy )
%IS_LEAPYEAR Checks whether a Gregorian year is a leap year
%
% leap year if evenly divisible by 4 and not by 100, except if evenly
% divisible by 400. Thus 2000 will be a leap year.
%
  iyyyy = floor(yyyy);
  is_4div = (mod(iyyyy,4) == 0);
  is_100div = (mod(iyyyy,100) == 0);
  is_400div = (mod(iyyyy,400) == 0);
  if (is_4div && ~is_100div) || is_400div
    leap = true;
  else
    leap = false;
  end

end

