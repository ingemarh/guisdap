function [ IAP ] = APF( context, IYYYY,IMN,ID,HOUR )
%APF Finds 3-hourly Ap indices for IRI-STORM model
%
%        SUBROUTINE APF(IYYYY,IMN,ID,HOUR,IAP)
%-----------------------------------------------------------------------
% Finds 3-hourly Ap indices for IRI-STORM model
%    INPUTS: 	IYYYY (yyyy)	year 
% 				IMN (mm)		month 
%				ID (dd)			day 
%				HOUR			UT in decimal hours
%    OUTPUT:    IAP(13)			3-hourly Ap index
%								IAP(13) Ap index for current UT
%								IAP(1) AP index for UT-39 hours.
%
% Reads APF107.DAT file (on UNIT=13) that is structured as follows:
% 		JY(I3),JMN(I3),JD(I3)	year, month, day 
%		IIAP(8)	(8I3)			3-HOUR Ap indices for the UT intervals 
%								(0-3),)3-6),)6-9), .., )18-21),)21-24(
%		IAPD (I3)				daily Ap
%		IR (I3)					sunspot number for the day (empty)
%		F107 (F5.1)				F10.7 radio flux for the day
%		F107_81 (F5.1)			81-day average of F10.7 radio flux 
%       F107_365 (F5.1)         365-day average of F10.7 centered on 
%                               the date of interest. At start and end  
%								of index file it takes all available  
%                               indices, e.g. for the first date the 
%                               average is only over 40 F10.7 values  
%                               and over 41 values on the 2nd date.  
%
% If date is outside the range of the Ap indices file than IAP(1)=-5  
%-----------------------------------------------------------------------

%        DIMENSION iiap(8),iap(13),lm(12)
%        COMMON /iounit/konsol
  persistent LM iiaps IYBEG IYEND IMEND IDEND NI NJ APHOURS;
  if isempty(LM)
    LM = [31,28,31,30,31,30,31,31,30,31,30,31];
    iiaps = IRI2012.getAP('iiap');
    rtn=IRI2012.getAP('BEGEND');
    IYBEG = rtn{1};
    IYEND = rtn{2};
    IMEND = rtn{3};
    IDEND = rtn{4};
    APHOURS = 3.0;
    NI = floor(24/APHOURS);
    NJ = NI+5;
  end
  IAP = zeros(NJ,1,IRI2012.float_t);
  for i=1:NJ
    IAP(i)=IRI2012.BAD_AP;
  end
  iiap = zeros(NI,1,IRI2012.float_t);
       
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

  if IYYYY >= IYBEG && beforeEnd  % file starts at Jan 1, 1958

    %-web-sepcial vfor web version
    %      OPEN(13,FILE='/usr/local/etc/httpd/cgi-bin/models/IRI/apf107.dat',
    %     *    ACCESS='DIRECT',RECL=55,FORM='FORMATTED',STATUS='OLD')
                
    IS=0;
    if IYYYY > IYBEG
      for i=IYBEG:IYYYY-1
        nyd=365;
        if IRI2012.IS_LEAPYEAR( i )
          nyd=nyd+1;
        end
        IS=IS+nyd;
      end
    end   

    LM(2)=28;
    if IRI2012.IS_LEAPYEAR( IYYYY )
      LM(2)=LM(2)+1;
    end
    for i=1:IMN-1
      IS=IS+LM(i);
    end

    IS=IS+ID;

    ihour=floor(HOUR/APHOURS)+1;
    if ihour > NI
      ihour=NI;
    end

    if IS*NI+ihour >= NJ % at least 13 indices available	
      %READ(13,10,REC=IS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
      jump = false;
      for i9=1:NI
        iiap(i9) = floor(iiaps{i9}(IS));
        if iiap(i9) < -2
          jump = true;
          break;
        end
      end
      if ~jump
        j1=NJ-ihour;
        for i=1:ihour
          IAP(j1+i)=iiap(i);
        end
        %iss=IS-1;
        %READ(13,10,REC=ISS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
        jump = false;
        for i9=1:NI
          iiap(i9) = floor(iiaps{i9}(IS-1));
          if iiap(i9) < -2
            jump = true;
            break;
          end
        end
        if ~jump
          if ihour > NJ-NI-1
            for i=1:j1
              IAP(i)=iiap(NI-j1+i);
            end
          else           
            j2=NJ-NI-ihour;
            for i=1:NI
              IAP(j2+i)=iiap(i);
            end
            %iss=IS-2;
            %READ(13,10,REC=ISS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
            jump = false;
            for i9=1:NI
              iiap(i9) = floor(iiaps{i9}(IS-2));
              if iiap(i9) >= -2
                jump = true;
                break;
              end
            end
            if ~jump
              for i=1:j2
                IAP(i)=iiap(NI-j2+i);
              end
            end
          end
        end
      end
    end
  else
    if context.KONSOL > 0
      fprintf(context.KONSOL,...
        'Date is outside range of Ap indices file. STORM model is turned off.\n');
    end
    IAP(1)=IRI2012.BAD_AP;
  end

end

