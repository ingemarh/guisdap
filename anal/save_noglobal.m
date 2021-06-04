% save_noglobal.m: function to store results without being globals
% GUISDAP v.8.3 03-11-27 Copyright EISCAT
%
function save_noglobal(varfile,varargin)
varstruct=[];
for varno=1:length(varargin)
 vartemp=varargin{varno};
 if ~isempty(vartemp)
  varname=inputname(varno+1);
  eval(['varstruct.' varname '=vartemp;']);
 end
end
save(varfile,'-struct','varstruct','-mat')
