% decodeparblock: Transfers the EISCAT parameter block data to GUISDAP variables
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
% function  decodeparblock
% The relevant information in the parameter block is now transferred to 
% GUISDAP variables
% NOTE: This is a EISCAT specific script
%
% See also: an_start integr_data

if d_parbl(128)>4 & d_parbl(128)<=10,
  % The site information
  names=['K';'T';'V';'S'];
  ant_id=[5 3 4 6]; ant_id=ant_id(d_parbl(1));
  if isempty(name_site) | name_site~='V'
    name_site=names(d_parbl(1));
  end
  if name_site=='V', ant_id=4; end

  % Put the remote receiver locations to GUP variables
  % Why? Because the remote sites may have common initialization file
  if name_site=='K'
    p_RECloc=[67.863, 20.44, .412];
  elseif name_site=='S'
    p_RECloc=[67.367, 26.65, .180];
  end

  % The radar controller program number
  d_rcprog=d_parbl(92);
  % endtime of integration 
  [time,year]=tosecs(d_parbl(2:4)); 
  d_time(2,:)=toYMDHMS(year,time);
  % starttime of integration 
  time=time-d_parbl(94);
  d_time(1,:)=toYMDHMS(year,time);

  % Antenna pointing direction
  if name_site=='V' % VHF data
    % Valid for CP4B. Note that only the first azimuth is used by GUISDAP 1.60 and 2.0
    ch_az(1:2:8)=d_parbl(14)*ones(1,4)/10; % Calculated beam azimuth "W" for odd channels
    ch_az(2:2:8)=d_parbl(15)*ones(1,4)/10; % Calculated beam azimuth "E" for odd channels
    ch_el(1:2:8)=d_parbl(8)*ones(1,4)/10; % Read hardware elevation "W" for odd channels
    ch_el(2:2:8)=d_parbl(10)*ones(1,4)/10; % Read hardware elevation "E" for odd channels
  else
    ch_az=(d_parbl(6)/10)*ones(1,8);
    ch_el=(d_parbl(9)/10)*ones(1,8);
  end
  
  % transmitter power
  ch_Pt=ones(1,8)*d_parbl(99)*1000;
  if rem(d_parbl(127),2)==1,
    ch_Pt=ones(1,8)*max(d_parbl([99,101]))*1000;
  end
  if all(ch_Pt)==0 % for some old data
    fprintf('\n\n No transmitter power information available, power set to 1.4 MW\n\n')
    ch_Pt=1.4e6*ones(1,8);
  end
  
  % Range to the common volume (for remotes on multistatic radars only)
  range=d_parbl(11);
  if range==0
    if (name_site=='K' | name_site=='S')
      range=[];
    else
      range=1000;
    end
  end
  ch_range=range*ones(1,8)/10;
  if (name_site=='K' | name_site=='S')
    calTemp=30;
  else
    calTemp=210;
  end
 
else
  error('Unknown parameter block version, update decodeparblock')
end
