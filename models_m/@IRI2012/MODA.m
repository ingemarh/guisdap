function [ MONTHout,IDAYout,IDOY,NRDAYMO,IYEAR ] = MODA( IN,IYEAR,MONTHin,IDAYin )
%MODA CALCULATES DAY OF YEAR (IDOY, ddd)
%
%        SUBROUTINE MODA(IN,IYEAR,MONTH,IDAY,IDOY,NRDAYMO)
%-------------------------------------------------------------------
% CALCULATES DAY OF YEAR (IDOY, ddd) FROM YEAR (IYEAR, yy or yyyy), 
% MONTH (MONTH, mm) AND DAY OF MONTH (IDAY, dd) IF IN=0, OR MONTH 
% AND DAY FROM YEAR AND DAY OF YEAR IF IN=1. NRDAYMO is an output 
% parameter providing the number of days in the specific MONTH.
%-------------------------------------------------------------------
%        DIMENSION       MM(12)
  persistent MM MOSUM;
  if isempty(MM)
    MM = [31,28,31,30,31,30,31,31,30,31,30,31];
    MOSUM = zeros(13,1);
    MOSUM(1)=0;
    for i=1:12
      MOSUM(i+1)=MOSUM(i)+MM(i);
    end
  end

%
%  leap year rule: years evenly divisible by 4 are leap years, except
%  years also evenly divisible by 100 are not leap years, except years 
%  also evenly divisible by 400 are leap years. The year 2000 therefore 
%  is a leap year. The 100 and 400 year exception rule
%     if((iyear/4*4.eq.iyear).and.(iyear/100*100.ne.iyear)) mm(2)=29
%  will become important again in the year 2100 which is not a leap 
%  year.
%
  leap = IRI2012.IS_LEAPYEAR( IYEAR );

  if IN <= 0
    n = floor((MONTHin-1)/12); % check if MONTHin out of range
    IYEAR = IYEAR + n;
    MONTHout = MONTHin - n*12;
    IDAYout = IDAYin;
    NRDAYMO=MM(MONTHout);
    if MONTHout == 2 && leap
      NRDAYMO=NRDAYMO+1;
    end
    while IDAYout < 1 % check if IDAYin out of range
      MONTHout = MONTHout - 1;
      n = floor((MONTHout-1)/12); % check if MONTHout out of range
      IYEAR = IYEAR + n;
      leap = IRI2012.IS_LEAPYEAR( IYEAR );
      MONTHout = MONTHout - n*12;
      NRDAYMO=MM(MONTHout);
      if MONTHout == 2 && leap
        NRDAYMO=NRDAYMO+1;
      end
      IDAYout = IDAYout + NRDAYMO;
    end
    while IDAYout > NRDAYMO % check if IDAYin out of range
      IDAYout = IDAYout - NRDAYMO;
      MONTHout = MONTHout + 1;
      n = floor((MONTHout-1)/12); % check if MONTHout out of range
      IYEAR = IYEAR + n;
      leap = IRI2012.IS_LEAPYEAR( IYEAR );
      MONTHout = MONTHout - n*12;
      NRDAYMO=MM(MONTHout);
      if MONTHout == 2 && leap
        NRDAYMO=NRDAYMO+1;
      end
    end
    mosum=MOSUM(MONTHout);
    if MONTHout >= 2 && leap
      mosum=mosum+1;
    end
    IDOY=mosum+IDAYout;
  else
    IDOY = MONTHin;
    IMO=0;
    MOBE=0;
    MOBEspillover = 0;
    while MOBE >= IDOY
      if IMO < 1 % check if IMO out of range
        IMO = IMO + 12;
        IYEAR = IYEAR - 1;
        NRDAYMO=MOSUM(IMO+1);
        leap = IRI2012.IS_LEAPYEAR( IYEAR );
        if leap
          NRDAYMO=NRDAYMO+1;
        end
        MOBEspillover = MOBE - NRDAYMO;
      end
      MOOLD=MOBE;
      NRDAYMO=MM(IMO);
      if IMO == 2 && leap
        NRDAYMO=NRDAYMO+1;
      end
      MOBE=MOBE-NRDAYMO;
      IMO=IMO-1;
    end
    while MOBE < IDOY
      IMO=IMO+1;
      if IMO > 12 % check if IMO out of range
        IMO = IMO - 12;
        IYEAR = IYEAR + 1;
        leap = IRI2012.IS_LEAPYEAR( IYEAR );
        MOBEspillover = MOBE;
      end
      MOOLD=MOBE;
      NRDAYMO=MM(IMO);
      if IMO == 2 && leap
        NRDAYMO=NRDAYMO+1;
      end
      MOBE=MOBE+NRDAYMO;
    end
    MONTHout=IMO;
    IDAYout=IDOY-MOOLD;
    IDOY = IDOY - MOBEspillover;
  end

end

