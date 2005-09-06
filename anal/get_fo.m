function [day,fo]=get_fo(y,m)
global local
t=clock;
if nargin==0
 y=t(1); m=t(2);
end
of=local.tfile;

%get the foF2 file
www='http://dynamite.eiscat.uit.no/';
%wget=(['wget -O ' of ' %s%s >/dev/null 2>&1']);
wget=(['wget -O ' of ' %s%s']);
fof2_link=sprintf('a-a/jww/AUTO/archive/DSND%02d%02d.FEF',rem(y,100),m);
[i,devnull]=system(sprintf(wget,www,fof2_link));
if i & y==t(1)
 fof2_link='jww/AUTO/autodsnd.fef';
 [i,devnull]=system(sprintf(wget,www,fof2_link));
end

if ~i
 form='%s%*s%s%*s%*s%s%*[^\n]';
 [day,foE,foF2]=textread(of,form,1);
 fprintf('*** Reading %s %s %s ***\n',char(day),char(foE),char(foF2))
 form(strfind(form,'s'))='f';
 [day,foE,foF2]=textread(of,form,'headerlines',1);
 fo=[foE foF2];
 fo(find(fo==0))=NaN;
 [day,d]=sort(day); fo=fo(d,:);
 d=find(diff(day)==0);
 day(d)=[]; fo(d,:)=[];
 day=day+datenum(y,1,1)-1;
else
 day=[]; fo=[];
end
delete(of)
