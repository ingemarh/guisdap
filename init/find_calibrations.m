% find_calibrations: Associates backgr/cal groups to lag profile groups
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Script to associate each lag profile groups with its background and 
% calibration groups. Guisdap required that a calibration group is found
% It is not necessary to find a background groups exists, if the lag value 
% is non-zero
% Change in version 1.54:
% If more than one background of calibration group is found, 
% all groups are stored in the variables lpg_bac and lpg_cal
%
% See also: init_GUP
% 
% find_calibrations.m

% The program logic is based on treating one lpg_code value at a time
fprintf('\nLooking for calibration and background ....\n\n')
codes=sort(lpg_code);
codes(find(diff(codes)==0))=[]; % find all different values

% lpg_bac stores the background lag profile group number  (0 means no background)
% lpg_cal stores the calibration lag profile group number
lpg_bac=zeros(1,length(lpg_ra),'int32');
lpg_cal=zeros(1,length(lpg_ra),'int32');
for code=codes;

  % First we look for background and calibration measurements with the same code number
  sig_gr=find(lpg_code==code & lpg_bcs=='s');
  xxx_gr=find(lpg_code==code & lpg_bcs=='x');
  bac_gr=find(lpg_code==code & lpg_bcs=='b');
  cal_gr=find(lpg_code==code & lpg_bcs=='c');
  off_gr=find(lpg_code==code & lpg_bcs=='o');
  all_gr=[bac_gr, cal_gr, sig_gr, xxx_gr, off_gr];
  % If no calibration data for a code, look for other codes, which
  % might be on the same real channel. If any is found, use their calibrations.
  if length(cal_gr)==0,
    if isempty(sig_gr)
      ch=diff_val(vc_ch(lp_vc(lpg_lp(off_gr(1))))); % These are the real channels used
    else
      ch=diff_val(vc_ch(lp_vc(lpg_lp(sig_gr(1))))); % These are the real channels used
    end
    for other=codes(find(codes~=code))
      lpg_other=find(lpg_code==other);
      ch_other=diff_val(vc_ch(lp_vc(lpg_lp(lpg_other(1))))); % These are channels used
      if length(ch)==length(ch_other) % Make sure that same channels are used
        if all(ch==ch_other) % Continue testing
          bac_gr=find(lpg_code==other & lpg_bcs=='b');
          cal_gr=find(lpg_code==other & lpg_bcs=='c');
          if length(cal_gr)>0, % Be happy, if calibration group was found
            fprintf(' Using calibrations of group %g for group %g\n',other,code)
            break, 
          end
        end
      end
    end
  end 

    %******* find background LPG for all LPG:s ********************
    for lpg=all_gr,
      % Find out first if a background measurement is needed in the first place
      % This is done by calculating the sum of all filter coefficients
      % If the sum is zero, the background cancels out automatically
      % This is the case for alternating code experiments
      lps=lpg_lp(lpg);
      if sum(sum(lp_fir(:,lps)))==0,
        fprintf(' No background needed for lpg %3.0f, lag value %3.0f us\n',lpg,lpg_lag(lpg)*p_dtau)
      else
        % bac contains all possible background measurements for the given lpg
        %bac=find(abs(lpg_lag(bac_gr)-lpg_lag(lpg))<=1000*eps);
        bac=find(lpg_lag(bac_gr)==lpg_lag(lpg));
        lenbac=length(bac);
        if lenbac>=1, 
          % First the case when at least one background measurement was found
          lpg_bac(1:lenbac,lpg)=bac_gr(bac)'; 
        elseif lenbac==0 & lpg_lag(lpg)>0 & length(off_gr)>0;
          % If background not found, use offset for non-zero lags, if it exists
          % This background system is used in many uniprog-type experiments
          % UHF1 and UHF2, ELSA-T4, UP3A (=GEN6B)
          % UP3A has two offset profiles, use the latter which is completely
          % free of signal contributions.
          len=length(off_gr);
          lpg_bac(1,lpg)=off_gr(len);
        elseif lenbac==0 & lpg_lag(lpg)>0
          % Multipulse experiments do not have background for non-zero lags
          fprintf(' No background for lpg %3.0f, lag value %3.0f us\n',lpg,lpg_lag(lpg)*p_dtau)
        elseif lenbac==0 & lpg_lag(lpg)==0  % Something in error in the background measurement
          fprintf('\nERROR: No background measurement found for lpg %3.0f\n',lpg)
          fprintf(' The lag value of this lpg is zero and\n therefore data will be regarded as garbage \n\n')
          if lpg_bcs(lpg)=='c'; % Remove from the calibration group
            ind=find(cal_gr==lpg);cal_gr(ind)=[]; 
          end
          lpg_bcs(lpg)='g'; 
        end      
      end
    end  

  %******* find the LPG that gives the calibration power *********** 
  cal=find(lpg_lag(cal_gr)==0);
  if exist('nocal','var') & nocal
    lpg_cal(all_gr)=NaN;
  elseif length(cal)==0
    warning('GUISDAP:init','No calibration found for code %d, all those lag profile groups treated as garbage',code)
    lpg_bcs(all_gr)='g';
  else
    % Store the found calibration lpg to all lag profile groups with this code number
    lpg_cal(1:length(cal),all_gr)=cal_gr(cal)'*ones(1,length(all_gr));
  end

end

if size(lpg_bac,1)>1
  ind=find(lpg_bac(2,:));
  fprintf('\n More than one background lag profile group found for lpg(s) \n')
  fprintf(' %.0f', ind)
  fprintf('\n SORRY, but only ONE can be used at present, taking the first one from each\n\n')
  lpg_bac=lpg_bac(1,:);
end

if size(lpg_cal,1)>1
  ind=find(lpg_cal(2,:));
  fprintf('\n More than one calibration lag profile group found for lpg(s) \n')
  fprintf(' %.0f', ind)
  fprintf('\n SORRY, but only ONE can be used at present, taking the first one from each\n\n')
  lpg_cal=lpg_cal(1,:);
end

fprintf('\n...      Calibration and background lpg''s located\n\n')

clear all_gr sig_gr xxx_gr bac_gr cal_gr off_gr code codes cal bac lenbac len lpg
clear ch lpg_other ch_other other_codes other
