% save_noglobal.m: function to store results without being globals
% GUISDAP v.8.3 03-11-27 Copyright EISCAT
%
function save_noglobal(varfile,varargin)
global local
varlist=[];
varflags=' -mat';
for varno=1:length(varargin)
 vartemp=varargin{varno};
 if ~isempty(vartemp)
  varname=inputname(varno+1);
  eval([varname '=vartemp;']);
  varlist=[varlist ' ' varname];
 end
end
if local.matlabversion>=14, varflags=[' -v6' varflags]; end
eval(['save ' varfile varlist varflags])
