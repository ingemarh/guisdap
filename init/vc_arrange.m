% vc_arrange.m: routine that checks vc-variable timing
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% 
% This script checks if GUP variables vc_sampling vc_t1 and vc_t2 have values 
% exceeding p_rep and subtracts p_rep is necessary. 
% This cancels the operations done by td_arrange.
%
% Be careful: This is NOT meant to be a user callable routine
%             Meaningful only in the sequence of call in init-EISCAT
%
% See also: td_arrange init_EISCAT
%
ind=find(vc_t1>p_rep);
if length(ind)
  fprintf(' Virtual channels:'),fprintf(' %.0f',ind),
		fprintf('\n have start times exceeding REP, which is not allowed\n')
		error(' ')
end

ind=find(vc_t2>p_rep);
if length(ind)
  fprintf('\n Virtual channels:'),fprintf(' %.0f',ind),
		fprintf('\n have end times exceeding REP. Subtracting REP from the end times\n')
  vc_t2(ind)=vc_t2(ind)-p_rep;
end

ind=find(vc_envo>p_rep);
if length(ind)
  fprintf('\n Virtual channels:'),fprintf(' %.0f',ind),
		fprintf('\n have transmission after REP. Subtracting REP from the transmission times\n')
  vc_envo(ind)=vc_envo(ind)-p_rep;
end

ind=find(vc_sampling(:,3)>p_rep & vc_sampling(:,4)>p_rep);
if length(ind)
  fprintf('\n Virtual channels:'),fprintf(' %.0f',vc_sampling(ind,1)),
		fprintf('\n have reception after REP. Subtracting REP from the reception times\n')
  vc_sampling(ind,3:4)=vc_sampling(ind,3:4)-p_rep;
end
