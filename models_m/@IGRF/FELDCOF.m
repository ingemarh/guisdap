function [ DIMO,context ] = FELDCOF( context, YEAR )
%FELDCOF DETERMINES COEFFICIENTS AND DIPOL MOMENT FROM IGRF MODELS
%        SUBROUTINE FELDCOF(YEAR,DIMO)
%-----------------------------------------------------------------------        
%  DETERMINES COEFFICIENTS AND DIPOL MOMENT FROM IGRF MODELS
%
%       INPUT:  YEAR    DECIMAL YEAR FOR WHICH GEOMAGNETIC FIELD IS TO
%                       BE CALCULATED
%       OUTPUT: DIMO    GEOMAGNETIC DIPOL MOMENT IN GAUSS (NORMALIZED 
%                       TO EARTH'S RADIUS) AT THE TIME (YEAR)
% 05/31/2000 updated to IGRF-2000 version (###) 
% 03/24/2000 updated to IGRF-2005 version (###) 
% 07/22/2009 NMAX=13 for DGRF00 and IGRF05; H/G-arrays(195)
% 02/26/2010 updated to IGRF-2010 version (###)  
% 10/05/2011 added COMMON/DIPOL/ for MLT computation in DPMTRX (IRIFUN)
%-----------------------------------------------------------------------        

%        CHARACTER*13    FILMOD, FIL1, FIL2           
% ### FILMOD, DTEMOD array-size is number of IGRF maps
%        DIMENSION       GH1(196),GH2(196),GHA(196),FILMOD(15)
%        DIMENSION		DTEMOD(15)
%        DOUBLE PRECISION X,F0,F 
%        COMMON/MODEL/   NMAX,TIME,GH1,FIL1
%        COMMON/IGRF1/   UMR,ERAD,AQUAD,BQUAD
%        COMMON/DIPOL/	GHI1,GHI2,GHI3
  persistent FILMOD DTEMOD GHAs ERAs NMAXs INCYEAR NUMYE;
  if isempty(FILMOD)
    INCYEAR = 5.0;
    %url = sprintf('http://www.ngdc.noaa.gov/IAGA/vmod/igrf%2dcoeffs.txt',IGRF.version);
    %[f,status] = urlread(url);
    status=0;
    if status == 1
      lns = textscan(f,'%s','delimiter','\n');
      numLines = length(lns{1});
      start = 4;
      colOffset = 3;
      yrln = lns{1}(start);
      years = textscan(yrln{1},'%s');
      years = years{1};
      NUMYE=length(years)-colOffset-1;
      DTEMOD = zeros(NUMYE+1,1);
      GHAs = cell(NUMYE+1,1);
      ERAs = zeros(NUMYE+1,1);
      NMAXs = zeros(NUMYE+1,1);
      FILMOD = cell(NUMYE+1,1);
      for i=1:NUMYE
        DTEMOD(i) = str2double(years{colOffset+i});
      end
      DTEMOD(NUMYE+1) = DTEMOD(NUMYE)+INCYEAR;
      numCoeff = numLines-start;
      nmax = floor(sqrt(numCoeff+1)-1);
      for L = 1:NUMYE+1
        FILMOD{L} = url;
        NMAXs(L) = nmax;
        ERAs(L) = context.ERA;
        GHAs{L} = zeros(numCoeff,1);
      end
      for i=1:numCoeff
        cln = lns{1}(start+i);
        coeffs = textscan(cln{1},'%s');
        coeffs = coeffs{1};
        for L=1:NUMYE+1
          GHAs{L}(i) = str2double(coeffs{colOffset+L});
        end
      end
    else
      % ### updated coefficient file names and corresponding years
      FILMOD =  {'dgrf1945.dat','dgrf1950.dat','dgrf1955.dat', ...
                 'dgrf1960.dat','dgrf1965.dat','dgrf1970.dat','dgrf1975.dat', ...
                 'dgrf1980.dat','dgrf1985.dat','dgrf1990.dat','dgrf1995.dat', ...
                 'dgrf2000.dat','dgrf2005.dat','dgrf2010.dat','dgrf2015.dat', ...
                 'igrf2020.dat','igrf2020s.dat'};
%                'dgrf2000.dat','dgrf2005.dat','igrf2010.dat','igrf2010s.dat'};
      DTEMOD = [1945., 1950., 1955., 1960., 1965., ...
                1970., 1975., 1980., 1985., 1990., 1995., 2000.,2005., ...
		2010., 2015., 2020., 2025.];
%               2010., 2015.];
      %
      % ### numye is number of IGRF coefficient files minus 1
      %
      NUMYE=length(DTEMOD)-1;
      GHAs = cell(NUMYE+1,1);
      ERAs = zeros(NUMYE+1,1);
      NMAXs = zeros(NUMYE+1,1);
      for L = 1:NUMYE+1
        [NMAXs(L),ERAs(L),GHAs{L}, IER] = context.GETSHC ( FILMOD{L} );
        if IER ~= 0
          %STOP
        else
        end
      end
    end
  end
  %
  %  IS=0 FOR SCHMIDT NORMALIZATION   IS=1 GAUSS NORMALIZATION
  %  IU  IS INPUT UNIT NUMBER FOR IGRF COEFFICIENT SETS
  %
  %IU = 14;
  IS = 0;
  %-- DETERMINE IGRF-YEARS FOR INPUT-YEAR
  context.TIME = double(YEAR);
  IYEA = floor(double(YEAR)/INCYEAR)*INCYEAR;
  L = floor((IYEA - DTEMOD(1))/INCYEAR) + 1;
  if L < 1
    L=1;
  end
  if L > NUMYE
    L=NUMYE;
  end
  DTE1 = DTEMOD(L);
  context.FIL1 = FILMOD{L};
  %-- GET IGRF COEFFICIENTS FOR THE BOUNDARY YEARS
  context.GH1 = GHAs{L};
  context.ERA = ERAs(L);
  NMAX1 = NMAXs(L);
  
  DTE2 = DTEMOD(L+1);
  GH2 = GHAs{L+1};
  context.ERA = ERAs(L+1);
  NMAX2 = NMAXs(L+1);
  %-- DETERMINE IGRF COEFFICIENTS FOR YEAR
  if L <= NUMYE-1
    [context.NMAX, GHA] = IGRF.INTERSHC (YEAR, DTE1, NMAX1, context.GH1, DTE2, ...
          NMAX2, GH2);
  else
    [context.NMAX, GHA] = IGRF.EXTRASHC (YEAR, DTE1, NMAX1, context.GH1, ...
          NMAX2, GH2);
  end
  %-- DETERMINE MAGNETIC DIPOL MOMENT AND COEFFIECIENTS G
  F0=0.0;
  for J=1:3
    F = GHA(J) * 1.0e-5;
    F0 = F0 + F * F;
  end
  DIMO = sqrt(F0);
  context.GHI1=GHA(1);
  context.GHI2=GHA(2);
  context.GHI3=GHA(3);

  context.GH1(1) = 0.0;
  I=2;
  F0=1.0e-5;
  if IS == 0
    F0=-F0;
  end
  SQRT2=sqrt(2.);      

  for N=1:context.NMAX           
    X = N;
    F0 = F0 * X * X / (4.0 * X - 2.0);
    if IS == 0
      F0 = F0 * (2.0 * X - 1.0) / X;
    end
    F = F0 * 0.5;
    if IS == 0
      F = F * SQRT2;
    end
    context.GH1(I) = GHA(I-1) * F0;
    I = I+1;
    for M=1:N                                    
      F = F * (X + M) / (X - M + 1.0);
      if IS == 0
        F = F * sqrt((X - M + 1.0) / (X + M));
      end
      context.GH1(I) = GHA(I-1) * F;
      context.GH1(I+1) = GHA(I) * F;
      I=I+2;
    end
  end

end

