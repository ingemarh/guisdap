function [ B0_98 ] = B0_98( HOUR, SAX, SUX, NSEASN, R, ZLO, ZMODIP )
%B0_98 Interpolation procedure for bottomside thickness parameter B0
%      
%        REAL FUNCTION B0_98 ( HOUR, SAX, SUX, NSEASN, R, ZLO, ZMODIP)
%-----------------------------------------------------------------
% Interpolation procedure for bottomside thickness parameter B0.
% Array B0F(ILT,ISEASON,IR,ILATI) distinguishes between day and
% night (ILT=1,2), four seasons (ISEASON is northern season with
% ISEASON=1 northern spring), low and high solar activity Rz12=10,
% 100 (IR=1,2), and modified dip latitudes of 0, 18 and 45
% degress (ILATI=1,2,3). In the DATA statement the first value
% corresponds to B0F(1,1,1,1), the second to B0F(2,1,1,1), the
% third to B0F(1,2,1,1) and so on.
%
% input:
%       hour    LT in decimal hours
%       SAX     time of sunrise in decimal hours
%       SUX     time of sunset in decimal hours
%       NSEASN  season in northern hemisphere (1=spring)
%       R       12-month running mean of sunspot number
%       ZLO     longitude
%       ZMODIP  modified dip latitude
%
% JUNE 1989 --------------------------------------- Dieter Bilitza
%
% Updates (B0_new -> B0_98):
%
% 01/98 corrected to include a smooth transition at the modip equator
%       and no discontinuity at the equatorial change in season.
% 09/98 new B0 values incl values at the magnetic equator
% 10/98 longitude as input to determine if magnetic equator in northern 
%         or southern hemisphere
%-------------------------------------------------------------------

%      REAL      NITVAL
%      DIMENSION B0F(2,4,2,3),BFR(2,2,3),BFD(2,3),zx(5),g(6),dd(5)
  persistent B0F zx dd numAlt num_lat num_hemi num_day num_seasons;
  if isempty(B0F)
    dat = [201,68,210, 61,192, 68,199, 67,240, 80,245, 83, ...
           233,71,230, 65,108, 65,142, 81,110, 68, 77, 75, ...
           124,98,164,100,120, 94, 96,112, 78, 81, 94, 84, ...
            81,81, 65, 70,102, 87,127, 91,109, 88, 81, 78];
    num_lat=3;
    num_hemi=2;
    num_day=2;
    num_seasons=4;
    B0F = zeros(num_day,num_seasons,2,num_lat);
    M = 1;
    for L=1:num_lat
      for K=1:2
        for J=1:num_seasons
          for I=1:num_day
            B0F(I,J,K,L) = dat(M);
            M = M + 1;
          end
        end
      end
    end
    zx = [45.,72.,90.,108.,135.];
    dd = [3.0,3.0,3.0,3.0,3.0];
    numAlt = length(zx);
  end
  BFR = zeros(num_day,num_hemi,num_lat);
  BFD = zeros(num_hemi,num_lat);
  G = zeros(numAlt+1,1);


  % jseasn is southern hemisphere season
  jseasn=NSEASN+floor(num_seasons/2);
  if (jseasn > num_seasons)
    jseasn=jseasn-num_seasons;
  end

  zz = ZMODIP + 90.;
  zz0 = 0.;

  % Interpolation in Rz12: linear from 10 to 100
  for ISL=1:num_lat
    for ISD=1:num_day
      BFR(ISD,1,ISL) = B0F(ISD,NSEASN,1,ISL) + ...
        (B0F(ISD,NSEASN,2,ISL) - B0F(ISD,NSEASN,1,ISL))/90.*(R-10.);
      BFR(ISD,2,ISL) = B0F(ISD,jseasn,1,ISL) + ...
        (B0F(ISD,jseasn,2,ISL) - B0F(ISD,jseasn,1,ISL))/90.*(R-10.);
    end
    % Interpolation day/night with transitions at SAX (sunrise)
    % and SUX (sunset) for northern/southern hemisphere iss=1/2
    for ISS=1:num_hemi
      DAYVAL = BFR(1,ISS,ISL);
      NITVAL = BFR(2,ISS,ISL);
      BFD(ISS,ISL) = IRI2012.HPOL(HOUR,DAYVAL,NITVAL,SAX,SUX,1.,1.);
    end
  end

  % Interpolation with epstein-transitions in modified dip latitude.
  % Transitions at +/-18 and +/-45 degrees; constant above +/-45.
  %
  % g(1:5) are the latitudinal slopes of B0;
  %       g(1) is for the region from -90 to -45 degrees
  %       g(2) is for the region from -45 to -18 degrees
  %       g(3) is for the region from -18 to   0 degrees
  %       g(4) is for the region from   0 to  18 degrees
  %       g(5) is for the region from  18 to  45 degrees
  %       g(6) is for the region from  45 to  90 degrees
  %
  % B0 =  BFD(2,3) at modip = -45,
  %       BFD(2,2) at modip = -18,
  %       BFD(2,1) or BFD(1,1) at modip = 0,
  %       BFD(1,2) at modip = 20,
  %       BFD(1,3) at modip = 45.
  % If the Longitude is between 200 and 320 degrees than the modip 
  % equator is in the southern hemisphere and BFD(2,1) is used at the 
  % equator, otherwise BFD(1,1) is used.
  %
  zx1=BFD(2,3);
  zx2=BFD(2,2);
  if (ZLO > 200.0 && ZLO < 320)
    zx3=BFD(2,1);
  else
    zx3=BFD(1,1);
  end
  zx4=BFD(1,2);
  zx5=BFD(1,3);
  G(1) = 0.;
  G(2) = ( zx2 - zx1 ) / 27.;
  G(3) = ( zx3 - zx2 ) / 18.;
  G(4) = ( zx4 - zx3 ) / 18.;
  G(5) = ( zx5 - zx4 ) / 27.;
  G(6) = 0.;

%        bb0 = BFD(2,3);
%      SUM = bb0;
  SUM=zx1;
  for I=1:numAlt
    AA = IRI2012.EPTR(zz ,dd(I),zx(I));
    BB = IRI2012.EPTR(zz0,dd(I),zx(I));
    DSUM = (G(I+1) - G(I)) * (AA-BB) * dd(I);
    SUM = SUM + DSUM;
  end
  B0_98 = SUM;

end

