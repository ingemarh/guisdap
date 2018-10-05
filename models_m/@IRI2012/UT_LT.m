function [ ut,slt,iyyy,ddd ] = UT_LT( mode,ut,slt,glong,iyyy,ddd )
%UT_LT Converts Universal Time UT (decimal hours) into Solar Local Time
%
%       subroutine ut_lt(mode,ut,slt,glong,iyyy,ddd)
% -----------------------------------------------------------------
% Converts Universal Time UT (decimal hours) into Solar Local Time
% SLT (decimal hours) for given date (iyyy is year, e.g. 1995; ddd
% is day of year, e.g. 1 for Jan 1) and geodetic longitude in degrees.
% For mode=0 UT->LT and for mode=1 LT->UT
% Please NOTE that iyyy and ddd are input as well as output parameters
% since the determined LT may be for a day before or after the UT day.
% ------------------------------------------------- bilitza nov 95

%        integer         ddd,dddend

  if(glong > 180)
    xlong=glong-360;
  else
    xlong=glong;
  end
  if(mode == IRI2012.UT_TO_LT)
    %
    % UT ---> LT
    %
    slt=ut+xlong/15.;
    if slt < 0.
      slt=slt+24.;
      ddd=ddd-1;
      if(ddd < 1.)
        iyyy=iyyy-1;
        ddd=365;
        if IRI2012.IS_LEAPYEAR(iyyy)
          ddd=ddd + 1;
        end
      end
    elseif slt > 24.
      slt=slt-24.;
      ddd=ddd+1;
      dddend=365;
      if IRI2012.IS_LEAPYEAR(iyyy)
        dddend=dddend + 1;
      end
      if(ddd > dddend)
        iyyy=iyyy + 1;
        ddd=1;
      end
    end
  else
    %
    % LT ---> UT
    %
    ut=slt-xlong/15.;
    if ut < 0.
      ut=ut+24.;
      ddd=ddd-1;
      if(ddd < 1.)
        iyyy=iyyy-1;
        ddd=365;
        if IRI2012.IS_LEAPYEAR(iyyy)
          ddd=ddd + 1;
        end
      end
    elseif ut > 24.
      ut=ut-24.;
      ddd=ddd+1;
      dddend=365;
      if IRI2012.IS_LEAPYEAR(iyyy)
        dddend=dddend + 1;
      end
      if(ddd > dddend)
        iyyy=iyyy+1;
        ddd=1;
      end
    end
  end

end

