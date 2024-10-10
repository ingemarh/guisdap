function fn=helpers()
fn.gs=sysexist([],'gs',fullfile(matlabroot,'sys','ghostscript','bin','gs'));
r=getenv('EISCATSITE'); if r, r='send_www'; end 
fn.curl=sysexist(r,'curl');
fn.exif=sysexist([],'exiftool','exiv2');
fn.bunzip=sysexist([],'lbunzip2','bunzip2','bzcat','7z');
fn.gzip=sysexist('save_GUPvar','gzip');
return

function fn=sysexist(routine,varargin)

for k=1:length(varargin)
 if ispc
  [~,fn]=gupsystem(['where /r c:\"Program Files" ' varargin{k} '.exe']);
 elseif isunix
  [~,fn]=gupsystem(['which ' varargin{k}]);
 end 
 if ~isempty(fn)
  fn=splitlines(fn);
  fn=fn{1};
  if ~isempty(fn) & ispc, fn=['"' fn '"']; end
  %if ~isempty(fn) & ispc, fn=replce(fn,' ','^ ']; end
  return
 end
end
if isempty(routine) | exist(routine)
 helpers=varargin{1};
 for k=2:length(varargin), helpers=[helpers '|' varargin{k}]; end
 warning('GUISDAP:helper',[helpers ' not found, please install'])
end
