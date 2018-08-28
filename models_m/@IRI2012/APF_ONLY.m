function [ F107D,F107PD,F107_81,F107_365,IAPDA ] = APF_ONLY( context,IYYYY,IMN,ID )
%APF_ONLY Finds daily F10.7, daily Ap, and 81-day and 365-day F10.7 index
%
%        SUBROUTINE APF_ONLY(IYYYY,IMN,ID,F107D,F107PD,F107_81,F107_365,
%     *        IAPDA)
%-----------------------------------------------------------------------
% Finds daily F10.7, daily Ap, and 81-day and 365-day F10.7 index: 
%
%    INPUTS: 	IYYYY (yyyy)	year 
% 				IMN (mm)		month 
%				ID (dd)			day 
%    OUTPUT:    F107D			F10.7 index for the day (adjusted 
%								to 1AU)
%               F107PD  		F10.7 index for one day prior (used in MSIS)
%				F107_81			F10.7 average over 3 solar rotations
%                               (81 days, centered on the current day) 
%               F107_365        F10.7 12-month running mean
%				IAPDA			Daily Ap
% 
% Using APF107.DAT file (see subroutine APF) on UNIT=13. 
%
% Is used for vdrift and foeedi.
%
% If date is outside the range of indices file than F107D=F107_81=-11.1  
%-----------------------------------------------------------------------

%        DIMENSION iiap(8),lm(12)
%
%        common /iounit/konsol
  persistent LM IAPDAs F107Ds F107_81s F365s IYBEG IYEND IMEND IDEND;
  if isempty(LM)
    LM = [31,28,31,30,31,30,31,31,30,31,30,31];
    IAPDAs = IRI2012.getAP('IAPDA');
    F107Ds = IRI2012.getAP('F107D');
    F107_81s = IRI2012.getAP('F107_81');
    F365s = IRI2012.getAP('F365');
    rtn=IRI2012.getAP('BEGEND');
    IYBEG = rtn{1};
    IYEND = rtn{2};
    IMEND = rtn{3};
    IDEND = rtn{4};
  end

  if IYYYY > IYEND
    beforeEnd = false;
  elseif IYYYY == IYEND
    if IMN > IMEND
      beforeEnd = false;
    elseif IMN == IMEND
      if ID <= IDEND
        beforeEnd = true;
      else
        beforeEnd = false;
      end
    else
      beforeEnd = true;
    end
  else
    beforeEnd = true;
  end
  if IYYYY >= IYBEG && beforeEnd   % APF107.DAT starts at Jan 1, 1958

%-web-sepcial vfor web version
%      OPEN(13,FILE='/usr/local/etc/httpd/cgi-bin/models/IRI/apf107.dat',
%     *    ACCESS='DIRECT',RECL=55,FORM='FORMATTED',STATUS='OLD')

    IS=0;
    for i=IYBEG:IYYYY-1
      nyd=365;
      if IRI2012.IS_LEAPYEAR( i )
        nyd=nyd+1;  % leap year
      end
      IS=IS+nyd;
    end

    LM(2)=28;
    if IRI2012.IS_LEAPYEAR( IYYYY )
      LM(2)=LM(2)+1; % leap year
    end
    for i=1:IMN-1
      IS=IS+LM(i);
    end

    IS=IS+ID;

    %JY = JYs(IS);
    %JMN = JMNs(IS);
    %JD = JDs(IS);
    %for i=1:8
    %  iiap(i) = iiaps{i}(IS);
    %end
    IAPDA = floor(IAPDAs(IS));
    %IR = IRs(IS);
    F107D = F107Ds(IS);
    F107_81 = F107_81s(IS);
    F107_365 = F365s(IS);
    %READ(13,10,REC=IS,ERR=21) JY,JMN,JD,iiap,IAPDA,IR,F107D,F107_81, F365

    if F107_81 < -4.
      F107_81=F107D;
    end
    if F107_365 < -4.
      F107_365=F107D;
    end

    F107PD=F107D;
    if IS > 1
      %JY = JYs(IS-1);
      %JMN = JMNs(IS-1);
      %JD = JDs(IS-1);
      %for i=1:8
      %  iiap(i) = iiaps{i}(IS-1);
      %end
      F107PD = F107Ds(IS-1);
      %READ(13,10,REC=IS-1,ERR=21) JY,JMN,JD,iiap,idum1,idum2,F107PD,fdum1,fdum2;
    end
                 
%10      FORMAT(3I3,9I3,I3,3F5.1)
  else
    if context.KONSOL > 0
      fprintf(context.KONSOL, ...
        'Date is outside range of F10.7D indices file (F10.7D = F10.7_81 = F10.7RM12).\n');
    end
    F107D = -11.1;
    F107_81 = -11.1;
    F107_365 = -11.1;
    F107PD = -11.1;
    IAPDA = -11;
  end


end

