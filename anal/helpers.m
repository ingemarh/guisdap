function fn=helpers()
fn.gs=sysexist('gs',fullfile(matlabroot,'sys','ghostscript','bin','gs'));
%fn.curl=sysexist('curl','wget')
fn.curl=sysexist('curl');
fn.exif=sysexist('exiftool','exiv2');
return

function fn=sysexist(varargin)

for k=1:length(varargin)
 if ispc
  [~,fn]=gupsystem(['where ' varargin{k} '.exe']);
 elseif isunix
  [~,fn]=gupsystem(['which ' varargin{k}]);
 end 
 if ~isempty(fn)
  fn=varargin{k};
  return
 end
end
helpers=varargin{1};
for k=2:length(varargin), helpers=[helpers '|' varargin{k}]; end
warning('GUISDAP:helper',[helpers ' not found, please install'])
