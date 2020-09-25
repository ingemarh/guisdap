function [ rz,ig,rsn,nmonth ] = TCON( context, yr,mm,day,idn )
%TCON retrieve latest rz and ig indices
%
%           subroutine tcon(yr,mm,day,idn,rz,ig,rsn,nmonth)
%----------------------------------------------------------------
% input:        yr,mm,day       year(yyyy),month(mm),day(dd)
%               idn             day of year(ddd)
% output:       rz(3)           12-month-smoothed solar sunspot number
%               ig(3)           12-month-smoothed IG index
%               rsn             interpolation parameter
%               nmonth          previous or following month depending
%                               on day
%
% Requires I/O UNIT=12 to read the Rz12 and IG12 indices file IG_RZ.DAT 
% 
% rz(1) & ig(1) contain the indices for the month mm and rz(2) & ig(2)
% for the previous month (if day less than 15) or for the following
% month (otherwise). These indices are for the mid of the month. The
% indices for the given day are obtained by linear interpolation and
% are stored in rz(3) and ig(3).
%
% The indices file IG_RZ.DAT is structured as follows (values are 
% separated by comma): 
%   day, month, year of the last update of this file,
%   a blank line
%   start month, start year, end month, end year,
%   a blank line
%   the IG index for December of start year - 1 (needed for interpolation)
%   the 12 IG indices (13-months running mean) for start year, 
%   the 12 IG indices for the second year 
%       .. and so on until the end year,
%   the IG index for January of end year + 1 (needed for interpolation)
%   a blank line
%   the Rz index for December of start year - 1 (needed for interpolation)
%   the 12 Rz indices (13-months running mean) for the start year,
%   the 12 Rz indices for the second year 
%       .. and so on until the end year.
%   the Rz index for January of end year + 1 (needed for interpolation)
% 
% A negative Rz index means that the given index is the 13-months-
% running mean of the solar radio flux (F10.7). The close correlation 
% between (Rz)12 and (F10.7)12 is used to compute the (Rz)12 indices.
%
% An IG index of -111 indicates that no IG values are available for the
% time period. In this case a correlation function between (IG)12 and 
% (Rz)12 is used to obtain (IG)12.
%
% The computation of the 13-month-running mean for month M requires the
% indices for the six months preceeding M and the six months following 
% M (month: M-6, ..., M+6). To calculate the current running mean one 
% therefore requires predictions of the indix for the next six months. 
% Starting from six months before the UPDATE DATE (listed at the top of 
% the file) and onward the indices are therefore based on indices 
% predictions.
%----------------------------------------------------------------

%           integer      yr, mm, day, iflag, iyst, iyend,iymst
%           integer      imst,iymend
%           real         ionoindx(722),indrz(722)
%           real         ig(3),rz(3)

%           common /iounit/konsol

%           save         ionoindx,indrz,iflag,iyst,iymst,iymend,imst
  persistent ionoindx indrz iyst iymst iymend imst s;
  ig = zeros(1,3);
  rz = zeros(1,3);
  if isempty(ionoindx)
    %s = 'ftp://spdf.gsfc.nasa.gov/pub/models/iri/iri2012/ig_rz.dat';
    fmt = '%f';
    %[f,status] = urlread(s);
    %if status == 0 % bad Internet, try local file
    if true
      global path_GUP
      s = fullfile(path_GUP,'share','iri','ig_rz.dat');
      f = fopen(s,'r');
      if f == -1
        status = 0;
      else
        status = 1;
        sc = textscan(f,fmt,'delimiter', ',');
        fclose(f);
      end
    else
      sc = textscan(f,fmt,'delimiter', ',');
    end
    if status == 1
      sc = sc{1};
      %iupm = sc(1);
      %iupd = sc(2);
      %iupy = sc(3);
      imst = sc(4);
      iyst = sc(5);
      imend = sc(6);
      iyend = sc(7);
      inum_vals= 3-imst + (iyend-iyst)*12 + imend;
      expectedLen = 2*inum_vals+7;
      if length(sc) == expectedLen
        ionoindx = zeros(1,inum_vals,IRI2012.float_t);
        indrz = zeros(1,inum_vals,IRI2012.float_t);
        % read all the IG12 (ionoindx) and Rz12 (indrz) values
        for i=1:inum_vals
          ionoindx(i) = sc(i+7);
        end
        for i=1:inum_vals
          indrz(i) = sc(i+inum_vals+7);
        end
      else
        if context.KONSOL > 0
          fprintf(context.KONSOL,'WARNING bad length (%d/%d) in %s\n', ...
            expectedLen,length(sc),s);
        end
      end
    else
      if context.KONSOL > 0
        fprintf(context.KONSOL,'WARNING %s not in directory\n',s);
      end
      rsn=0.0;
      nmonth=-1;
      return;
    end

    iymst=iyst*100+imst;
    iymend=iyend*100+imend;

    % inum_vals= 12-imst+1+(iyend-iyst-1)*12 +imend + 2
    % 1st year \ full years       \last y\ before & after



    for jj=1:inum_vals
      rrr=indrz(jj);
      if rrr < 0.0
        covr=abs(rrr);
        rrr=33.52*sqrt(covr+85.12)-408.99;
        if rrr < 0.0
          rrr=0.0;
        end
        indrz(jj)=rrr;
      end
      if ionoindx(jj) <= -90.
        p = [-2.67690893e-03,1.4683266,-12.349154];
        zi=polyval(p,rrr);
        if zi > 274.0
          zi=274.0;
        end
        ionoindx(jj)=zi;
      end
    end
  end

  iytmp=yr*100+mm;
  if iytmp < iymst || iytmp > iymend
    if context.KONSOL > 0
      fprintf(context.KONSOL,' %10d ** OUT OF RANGE **\n',iytmp);
      fprintf(context.KONSOL,...
        '     The file %s which contains the indices Rz12 and IG12\n',s);
      fprintf(context.KONSOL,...
        '     currently only covers the time period (yymm) : %6d-%6d\n', ...
        iymst,iymend);
    end
    rsn=0.0;
    nmonth=-1;
    return;
  end

  %       num=12-imst+1+(yr-iyst-1)*12+mm+1;
  num=2-imst+(yr-iyst)*12+mm;

  rz(1)=indrz(num);
  ig(1)=ionoindx(num);
  midm=15;
  if mm == 2
    midm=midm-1;
  end
  [mm,midm,idd1,~] = IRI2012.MODA(0,yr,mm,midm);
  if day >= midm
    %
    % day is at or after mid of month
    %
    imm2=mm+1;
    if imm2 > 12
      imm2=1;
      %iyy2=yr+1;
      idd2=365+15;            % =365+15 mid-January
      if IRI2012.IS_LEAPYEAR( yr )
        idd2=idd2+1;
      end
    else
      iyy2=yr;
      midm=15;
      if imm2 == 2
        midm=midm-1;
      end
      [imm2,~,idd2,~] = IRI2012.MODA(0,iyy2,imm2,midm);
    end
    rz(2)=indrz(num+1);
    ig(2)=ionoindx(num+1);
    rsn=(double(idn)-double(idd1))/(double(idd2)-double(idd1));
    rz(3)=rz(1)+(rz(2)-rz(1))*rsn;
    ig(3)=ig(1)+(ig(2)-ig(1))*rsn;
  else
    imm2=mm-1;
    if imm2 < 1
      imm2=12;
      idd2=-16;
      %iyy2=yr-1;
    else
      iyy2=yr;
      midm=15;
      if imm2 == 2
        midm=midm-1;
      end
      [imm2,~,idd2,~] = IRI2012.MODA(0,iyy2,imm2,midm);
    end
    rz(2)=indrz(num-1);
    ig(2)=ionoindx(num-1);
    rsn=(double(idn)-double(idd2))/(double(idd1)-double(idd2));
    rz(3)=rz(2)+(rz(1)-rz(2))*rsn;
    ig(3)=ig(2)+(ig(1)-ig(2))*rsn;
  end
  nmonth=imm2;

end

