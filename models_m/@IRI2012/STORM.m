function [ cf,rgma ] = STORM( ap,rga,rgo,coor,ut,doy )
%STORM foF2 storm-time correction factor
%
%      SUBROUTINE STORM(ap,rga,rgo,coor,rgma,ut,doy,cf)
%----------------------------------------------------------------------
%      Fortran code to obtain the foF2 storm-time correction factor at 
%      a given location and time, using the current and the 12 previous
%      ap values as input.
%
%      ap ---> (13 elements integer array). Array with the preceeding
%              13 value of the 3-hourly ap index. The 13th value
%              in the array will contain the ap at the UT of interest,
%              the 12th value will contain the 1st three hourly interval
%              preceeding the time of interest, and so on to the 1st
%              ap value at the earliest time.
%     coor --> (integer). If coor = 2, rga should contain the 
%                         geomagnetic latitude.
%                         If coor = 1, rga should contain the 
%                         geographic latitude.
%     rga ---> (real, -90 to 90) geographic or geomagnetic latitude.
%     rgo ---> (real, 0 to 360, positive east from Greenwich.)
%                           geographic longitude, only used if coor=1.
%     ut  ---> (integer, hours 00 to 23) Universal Time of interest.
%     doy ---> (integer, 1 to 366)Day of the year.
%     cf  ---> (real) The output; the storm-time correction factor used
%              to scale foF2, foF2 * cf.
%
%     This model and computer code was developed by E. Araujo-Pradere,
%     T. Fuller-Rowell and M. Condrescu, SEC, NOAA, Boulder, USA
%     Ref: 
%     T. Fuller-Rowell, E. Araujo-Pradere, and M. Condrescu, An 
%       Empirical Ionospheric Storm-Time Ionospheric Correction Model,
%       Adv. Space Res. 8, 8, 15-24, 2000.
%----------------------------------------------------------------------

%     DIMENSIONS AND COEFFICIENTS VALUES
  persistent c4 c3 c2 c1 c0 fap code numAps;
  if isempty(c4)
    numAps = 13;
    c4 = [0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00, ...
      0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00, ...
      0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00];

    c3 = [0.00E+00,0.00E+00,0.00E+00,0.00E+00,0.00E+00,-9.44E-12, ...
      0.00E+00,3.04E-12,0.00E+00,9.32E-12,-1.07E-11,0.00E+00,0.00E+00, ...
      0.00E+00,1.09E-11,0.00E+00,0.00E+00,0.00E+00,0.00E+00,-1.01E-11];

    c2 = [1.16E-08,0.00E+00,0.00E+00,-1.46E-08,0.00E+00,9.86E-08, ...
      2.25E-08,-1.67E-08,-1.62E-08,-9.42E-08,1.17E-07,4.32E-08,3.97E-08, ...
      3.13E-08,-8.04E-08,3.91E-08,2.58E-08,3.45E-08,4.76E-08,1.13E-07];

    c1 = [-9.17E-05,-1.37E-05,0.00E+00,7.14E-05,0.00E+00,-3.21E-04, ...
      -1.66E-04,-4.10E-05,1.36E-04,2.29E-04,-3.89E-04,-3.08E-04, ...
      -2.81E-04,-1.90E-04,4.76E-05,-2.80E-04,-2.07E-04,-2.91E-04, ...
      -3.30E-04,-4.04E-04];

    c0 = [1.0136E+00,1.0478E+00,1.00E+00,1.0258E+00,1.00E+00, ...
      1.077E+00,1.0543E+00,1.0103E+00,9.9927E-01,9.6876E-01,1.0971E+00, ...
      1.0971E+00,1.0777E+00,1.1134E+00,1.0237E+00,1.0703E+00,1.0248E+00, ...
      1.0945E+00,1.1622E+00,1.1393E+00];

    fap = [0.,0.,0.037037037,0.074074074,0.111111111,0.148148148, ...
      0.185185185,0.222222222,0.259259259,0.296296296,0.333333333, ...
      0.37037037,0.407407407,0.444444444,0.481481481,0.518518519, ...
      0.555555556,0.592592593,0.62962963,0.666666667,0.703703704, ...
      0.740740741,0.777777778,0.814814815,0.851851852,0.888888889, ...
      0.925925926,0.962962963,1.,0.66666667,0.33333334,0.,0.333333, ...
      0.666666,1.,0.7];

      %integer code(8,6)
    code = transpose([ 3, 4, 5, 4, 3, 2, 1, 2; ...
                       3, 2, 1, 2, 3, 4, 5, 4; ...
                       8, 7, 6, 7, 8, 9,10, 9; ...
                      13,12,11,12,13,14,15,14; ...
                      18,17,16,17,18,19,20,19; ...
                      18,17,16,17,18,19,20,19]);
  end

%      INTEGER ape(39)
%      INTEGER ap(13)
%      INTEGER ut,doy,dayno,coor,s1,s2,l1,l2
%      REAL rgma, rap, rga, rgo, rs, rl
%
%      CALLING THE PROGRAM TO CONVERT TO GEOMAGNETIC COORDINATES
  ape = zeros(length(fap)+3,1,'int32');
  cf = 1.0; % no change in foF2 by default
  rgma = rga; % uncorrected magnetic latitude by default
  if (coor  ==  1)

    rgma = IRI2012.CONVER (rga,rgo);

  elseif (coor  ==  2)
    rgma = rga;

  else
    fprintf (1,'\n\n   Wrong Coordinates Selection -------- >> %d\n\n', coor);
    return;
  end

% FROM 3-HOURLY TO HOURLY ap (New, interpolates between the three hourly ap values)

  ape(1)=ap(1);
  ape(2)=ap(1);
  ape(length(fap)+2)=ap(numAps);
  ape(length(fap)+3)=ap(numAps);

  for k = 1:numAps
    i = (k * 3) - 1;
    ape(i) = ap(k);
  end

  for k = 1:numAps-1
    i = k * 3;
    ape(i) = (ap(k)*2 + ap(k+1))/3.0;
  end

  for k = 2:numAps
    i = (k * 3) - 2;
    ape(i) = (ap(k-1) + ap(k)*2)/3.0;
  end

%     FROM 3-HOURLY TO HOURLY ap (old version without interpolation)
%      i = 1;
%      for k = 1:13
%         for j = 1:3
%            ape(i) = ap(k);
%            i = i + 1;
%         end
%      end

%     TO OBTAIN THE INTEGRAL OF ap.
%     INTEGRAL OF ap
  ut = floor(ut);
  if(ut == 24)
    ut=0;
  end
  if mod(ut,3) == 0
    k = 1;
  elseif mod(ut,3) == 1
    k = 2;
  elseif mod(ut,3) == 2
    k = 3;

  else

    fprintf (1,'\n\n  Wrong Universal Time value -------- >> %d\n\n', ut);
    return;

  end

  rap = 0;

  for j = 1:length(fap)
    rap = rap + fap(j) * double(ape(k+j));
  end

  if(rap <= 200.)
    return;
  end

  if(doy > 366 || doy < 1)
    fprintf (1,'\n\n\n      Wrong Day of Year value --- >> %f\n\n', doy);
    return;
  end

  if(rgma > 90.0 || rgma < -90.0)
    fprintf (1,'\n\n\n   Wrong GEOMAGNETIC LATITUDE value --- >> %f\n\n', rgma);
    return;
  end

  %      fprintf(1,'%f\n',rgma);

  dayno=doy;
  if(rgma < 0.0)
    dayno=doy+172;
    if(dayno > 365)
      dayno=dayno-365;
    end
  end

  if (dayno >= 82)
    rs=(dayno-82.)/45.6+1.;
  end
  if (dayno < 82)
    rs=(dayno+283.)/45.6+1.;
  end
  s1=floor(rs);
  facs=rs-s1;
  s2=s1+1;
  if(s2 == 9)
    s2=1;
  end
  %      fprintf(1,'%f %f %f\n',s1,s2,rs);

  rgma = abs(rgma);

  rl=(rgma+10.)/20.+1;
  if(rl == 6.0)
    rl=5.9;
  end
  l1=floor(rl);
  facl=rl-l1;
  l2=l1+1;
  %      fprintf(6,*)l1,l2,rl

  %     FACTORS CALCULATIONS

  if(rap < 300.)
    rapf=300.;
    n1=code(s1,l1);
    cf1=c4(n1)*(rapf^4)+c3(n1) * (rapf^3) + c2(n1) * (rapf^2) + ...
     c1(n1) * rapf + c0(n1);
    n2=code(s1,l2);
    cf2=c4(n2)*(rapf^4)+c3(n2) * (rapf^3) + c2(n2) * (rapf^2) + ...
     c1(n2) * rapf + c0(n2);
    n3=code(s2,l1);
    cf3=c4(n3)*(rapf^4)+c3(n3) * (rapf^3) + c2(n3) * (rapf^2) + ...
     c1(n3) * rapf + c0(n3);
    n4=code(s2,l2);
    cf4=c4(n4)*(rapf^4)+c3(n4) * (rapf^3) + c2(n4) * (rapf^2) + ...
     c1(n4) * rapf + c0(n4);

    %     INTERPOLATION

    cf300=cf1*(1 - facs) * (1 - facl) + cf2 * (1 - facs) * (facl) + ...
     cf3 * (facs) * (1 - facl) + cf4 * (facs) * (facl);

    cf = (cf300-1.0)*rap/100.-2.*cf300+3.;
  else

    n1=code(s1,l1);
    %      fprintf(1,'%f\n',n1);
    cf1 = c4(n1) * (rap^4) + c3(n1) * (rap^3) + c2(n1) * (rap^2) + ...
     c1(n1) * rap + c0(n1);
    n2=code(s1,l2);
    cf2 = c4(n2) * (rap^4) + c3(n2) * (rap^3) + c2(n2) * (rap^2) + ...
     c1(n2) * rap + c0(n2);
    n3=code(s2,l1);
    cf3 = c4(n3) * (rap^4) + c3(n3) * (rap^3) + c2(n3) * (rap^2) + ...
     c1(n3) * rap + c0(n3);
    n4=code(s2,l2);
    cf4 = c4(n4) * (rap^4) + c3(n4) * (rap^3) + c2(n4) * (rap^2) + ...
     c1(n4) * rap + c0(n4);

    %     INTERPOLATION

    cf = cf1 * (1 - facs) * (1 - facl) + cf2 * (1 - facs) * (facl) + ...
     cf3 * (facs) * (1 - facl) + cf4 * (facs) * (facl);
  end

end

