function [ IAPO ] = APFMSIS( context,IYYYY,IMN,ID,HOUR )
%APFMSIS Finds 3-hourly Ap indices for NRLMSIS00 model for given year IYYYY
%
%        SUBROUTINE APFMSIS(IYYYY,IMN,ID,HOUR,IAPO)
%-----------------------------------------------------------------------
% Finds 3-hourly Ap indices for NRLMSIS00 model for given year IYYYY 
% (yyyy), month (IMN), day (ID), and UT (HOUR, decimal hours). The 
% indices are stored in IAP(13) providing the 13 3-hourly indices 
% prior to HOUR. 
%   IAPO(1) DAILY AP
%   IAPO(2) 3-HR AP INDEX FOR CURRENT TIME			   	STORM  MSIS
%   IAPO(3) 3-HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME	STORM  MSIS
%   IAPO(4) 3-HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME	STORM  MSIS
%   IAPO(5) 3-HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME	STORM  MSIS  
%   IAPO(6) AVERAGE OF EIGHT 3-HR AP INDICIES FROM 12 TO 33 HRS PRIOR
%                    TO CURRENT TIME
%   IAPO(7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
%                    TO CURRENT TIME
%
% The 3-hour UT intervals during the day are: (0-3),)3-6),)6-9),)9-12),
% )12-15),)15-18),)18-21),)21-24(.
% 
% If date is outside the range of the Ap indices file than IAPO(1)=-5  
%-----------------------------------------------------------------------

%		REAL IAPO
%        DIMENSION iiap(8),iap(21),lm(12),iapo(7)
%
%        common /iounit/konsol
  persistent LM iiaps IYBEG IYEND IMEND IDEND NI NJ INTER;
  if isempty(LM)
    LM = [31,28,31,30,31,30,31,31,30,31,30,31];
    iiaps = IRI2012.getAP('iiap');
    rtn=IRI2012.getAP('BEGEND');
    IYBEG = rtn{1};
    IYEND = rtn{2};
    IMEND = rtn{3};
    IDEND = rtn{4};
    INTER = 3.0;
    NI = 24/INTER;
    NJ = 2*NI+5;
  end
  IAPO = zeros(CIRA.maxAP,1);
  iap = zeros(NJ,1);
  iiap = zeros(NI,1);
  for i=1:NJ
    iap(i)=IRI2012.BAD_AP;
  end
  for i=1:CIRA.maxAP
    IAPO(i)=IRI2012.BAD_AP;
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
  if IYYYY < IYBEG || ~beforeEnd
    % file starts at Jan 1, 1958
    if context.KONSOL > 0
      fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
    end
    return;
  end

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

  ihour=floor(HOUR/INTER)+1;
  if ihour > NI
    ihour=NI;
  end
  %
  % calculate daily Ap for day of interest
  %
  %READ(13,10,REC=IS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
  iapsum=0;
  for ijk=1:NI
    iiap(ijk) = iiaps{ijk}(IS);
    iapsum=iapsum+iiap(ijk);
  end
  %		IAPO(1)=int(iapsum/8.+.5)
  IAPO(1)=iapsum/NI;
		
  %
  % There must be at least 20 indices available
  %
  if IS*NI+ihour < NJ-1
    if context.KONSOL > 0
      fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
    end
    return;
  end
  %
  % Read indices; first record was already read above
  %
  j1=ihour+1;
  for i=1:ihour
    if iiap(i) < -2
      if context.KONSOL > 0
        fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
      end
      return;
    end
    iap(j1-i)=iiap(i);
  end

  ISS=IS-1;
  %READ(13,10,REC=ISS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
  j1=ihour+NI+1;
  for i=1:NI
    iiap(i) = iiaps{i}(ISS);
    if iiap(i) < -2
      if context.KONSOL > 0
        fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
      end
      return;
    end
    iap(j1-i)=iiap(i);
  end

  ISS=IS-2;
  %READ(13,10,REC=ISS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
  j1=ihour+2*NI+1;
  j2=NI-(NJ-1-ihour-NI)+1;
  if j2 < 1
    j2=1;
  end
  for i=j2:NI
    iiap(i) = iiaps{i}(ISS);
    if iiap(i) < -2
      if context.KONSOL > 0
        fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
      end
      return;
    end
    iap(j1-i)=iiap(i);
  end

  if ihour < NJ-2*NI-1
    ISS=IS-3;
    %READ(13,10,REC=ISS,ERR=21) JY,JMN,JD,iiap,iapd,IR,F,F81,F365
    j1=ihour+NI*3+1;
    j2=NI-(NJ-NI*2-1-ihour)+1;
    for i=j2:NI
      iiap(i) = iiaps{i}(ISS);
      if iiap(i) < -2
        if context.KONSOL > 0
          fprintf(context.KONSOL,'MSIS: Only Ap-daily dependence not history\n');
        end
        return;
      end
      iap(j1-i)=iiap(i);
    end
  end

  for i=1:NJ-2*NI-1
    IAPO(i+1)=iap(i);
  end
  sum1=0.;
  sum2=0.;
  for i=1:NI
    sum1=sum1+iap(NJ-2*NI-1+i);
    sum2=sum2+iap(NJ-NI-1+i);
  end
  %        IAPO(6)=floor(sum1/8.+.5)
  %        IAPO(7)=floor(sum2/8.+.5)
  IAPO(6)=sum1/NI;
  IAPO(7)=sum2/NI;

end

