% COR_status: Internal routine used by the COR_xxx routines
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This is an internal routine called by COR_XX routines. It displays
% - the number of lag profiles defined by the present call
% - total number of lag profiles specified up to this call
% - Number of samples required and total number of samples taken (design package only)
% - The last result memory address used
% In the design use, the routine calculates the sampling end time 
% Input parameters:
% routine: Name of calling routine as string for output 
% vc     : virtual channel number
% type   : lag profile type
% index  : lag profile numbers

% function COR_status(routine,vc,type,index)
  function COR_status(routine,vc,type,index)
  
global vc_sampling vc_adcint vc_ba vc_ch bm_next ra_next p_dtau vc_routine
global lp_t2 lp_nfir lp_nt lp_dec lp_ind

string=' '*ones(1,11);len=length(routine);
string(1:len)=routine;string(len+1)=':';
fprintf(1,[string,' %3.0f lps, total %5.0f; '],length(index), lp_ind)

typeind=type2ind(type);
adcint=vc_adcint(vc);
[st_sampl,end_sampl,ind]=sample_times(vc,typeind);

% Calculate the sampling interval, if not already there
% i.e. calculate when used by experiment design package
if end_sampl==0 & length(ind)==1, % This is executed in experiment design
  end_sampl=max(lp_t2(index)+((lp_nfir(index)+(lp_nt(index)-1).*lp_dec(index))-1)*adcint);
  vc_sampling(ind,4)=end_sampl;
% Number of samples required for calculations
  Nsamples=1+(end_sampl-st_sampl)/adcint;
  vc_sampling(ind,5:6)=bm_next+[0 Nsamples-1];
  bm_next=bm_next+Nsamples;
  fprintf(' %3.0f samples, total %5.0f;',Nsamples, vc_sampling(ind,6))

  % Check now that no other channel is transmitting at the time of reception
  [chs,RECch,CALon]=active_ch((st_sampl:end_sampl)');
  if length(chs)
    fprintf('\n\n  ERROR\nClash in the design:\n')
    fprintf('Channel %.0f was asked to receive  ',vc_ch(vc))
    fprintf('from %.1f to %.1f, but\n', st_sampl*p_dtau, end_sampl*p_dtau)
    fprintf('channel %.0f is transmitting during that period\n',chs)
    error(' ')
  else
    if type~='c' & CALon
      fprintf('\nWARNING:\nThe required reception period\n')
      fprintf('from %.1f to %.1f\n', st_sampl*p_dtau, end_sampl*p_dtau)
      fprintf('will be contaminated by the calibration injection\n')
    end
  end
elseif end_sampl==0 & length(ind)>1, % One never should end up here
  error([' Error in ',routine,' Contact a GUIZARD'])
end
vc_routine(vc)=cellstr(routine);
fprintf(' ResMem used to %5.0f\n', ra_next-1)
