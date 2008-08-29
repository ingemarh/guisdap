function r=minput(Q,D,s)
if nargin>2
 a=input([Q '? [' row(D') '] '],'s');
else
 a=str2num(input([Q '? [ ' sprintf('%g ',D) ']'],'s'));
end
if isempty(a)
 r=D;
elseif nargin>2 | any(isnan(a))
 r=a(~isnan(a));
else
 r=D;
 r(1:length(a))=a;
end
