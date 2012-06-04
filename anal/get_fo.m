function [day,fo]=get_fo(t1,t2,site)
global local
if nargin==0, t1=[]; end
if isempty(t1), t1=now, end
if nargin<2, t2=[]; end
if isempty(t2), t2=t1; end
if nargin<3, site='tromso'; end
if findstr(site(1),'TV'), site='tromso'; end
if site(1)=='L', site='svalbard'; end
site3=site(1:3);
if strcmp(site3,'32m') | strcmp(site3,'42m') | strcmp(site3,'esr'), site='svalbard'; end 
if strcmp(site3,'uhf') | strcmp(site3,'vhf'), site='tromso'; end 
of=local.tfile;
day=[]; fo=[];

%get the foF2 file
%wget=(['wget -O ' of ' %s%s >/dev/null 2>&1']);
wget=(['wget -O ' of ' %s%s']);
if t1>datenum(2010,11,22)
 www='http://dynserv.eiscat.uit.no/';
 for t=fix(t1):fix(t2)
  t1=datevec(t);
  fof2_link=sprintf('DD/myque.php?q=select\\ dDay,foE,foF2\\ from\\ %s.resul%d_%02d_%02d\\ where\\ foE\\>0\\ or\\ foF2\\>0',site,t1(1:3));
  [i,devnull]=system(sprintf(wget,www,fof2_link));
  if ~i
   form='%f%f%f%*[^\n]';
   [d,foE,foF2]=textread(of,form,'headerlines',2);
   fo=[fo;[foE foF2]];
   fo(find(fo==0))=NaN;
   day=[day;d+datenum(t1(1),1,1)-1];
  end
 end
else
 t1=datevec(t1); y=t1(1); m=t1(2);
 www='http://www.eiscat.uit.no/';
 fof2_link=sprintf('DataBases/Dynasonde/dsnd/DSND%02d%02d.FEF',rem(y,100),m);
 [i,devnull]=system(sprintf(wget,www,fof2_link));
 if i & y==t(1)
  fof2_link='DataBases/Dynasonde/dsnd/autodsnd.FEF';
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
 end
end
delete(of)
