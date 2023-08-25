function r=minput(Q,D,s)
persistent in
if ispc & (isstr(D) | iscellstr(D)),D=strrep(D,'\','\\');end
if nargin>2
 a=input([Q '? [' row(D') '] '],'s');
 anan=[];
else
 if isempty(in) && length(D)>1
  disp('Enter NaN to shortern the vector')
  in=1;
 end 
 if iscell(D)
  a=strsplit(input([Q '? [ ' sprintf('%s ',strjoin(D)) ']'],'s'));
  anan=contains(a,{'NaN'});
 else
  a=str2num(input([Q '? [ ' sprintf('%g ',D) ']'],'s'));
  anan=find(isnan(a));
 end
end
if ispc & (isstr(D) | iscellstr(D)),D=strrep(D,'\\','\');end
if isempty(a) | iscell(D) & isempty(a{1})
 r=D;
elseif nargin>2 || ~isempty(anan)
 r=a;
 r(anan)=[];
elseif ndims(D)==2 && size(D,2)==2 && size(a,2)==2
 r=a; 
else
 r=D;
 r(1:length(a))=a;
end
