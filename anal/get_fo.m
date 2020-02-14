function [day,fo]=get_fo(t1,t2,site,epar,fpar)
%
% Retrieve DSND data from database
% Update 20160408: E and F layer parameters selectable by user for new
% (SQL) data.  
% Default: foE and foF2. 
% Suggested: sometimes FMXE and FMXF are better.
%  
% Usage: [day,fo]=get_fo(t1,t2,site,epar,fpar)
% t1, t2: Time interval as Matlab datenum. Empty: default today
% site: empty: default Tromsø | '(tromso|uhf|vfh)' | '(svalbard|32m|42m)'
% epar: empty: default foE    |  1: FMXE           | '<parameter name>'
% fpar: empty: default foF2   |  1: FMXF           | '<parameter name>'
%
% Last modified C-F Enell 20160408

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

if nargin<4, epar='foE'; end
if epar==1, epar='FMXE'; end
if isempty(epar), epar='foE'; end

if nargin<5, fpar='foF2'; end
if fpar==1, fpar='FMXF'; end
if isempty(fpar), fpar='foF2'; end

if strcmp(site3,'quj')
 ifile=minput('Enter ionosonde monthly foF2 table','KMG',1);
 fof2table=xlsread(ifile);
 %assumeing 00:00 01:00 ... 23:00, and local Kunming time (-7h)
 d=datevec(t1);
 ti1=datenum([d(1:2) 1 0 0 0]);
 fof2=fof2table(:)/10;
 ti=(ti1+(0:length(fof2)-1)'/24)-7/24;
 d=find(ti>t1-3599/86400 & ti<t2+3599/86400);
 day=ti(d); fo=[ones(length(d),1)*NaN fof2(d)];
 return
end
day=[]; fo=[];

%get the foF2 data
if t1>datenum(1995,07,12)
 www='https://dynserv.eiscat.uit.no/';
 for t=fix(t1):fix(t2)
 tt=datevec(t); 
  fof2_link=sprintf('DD/myque.php?q=select dDay,%s,%s from %s.resul%d_%02d_%02d where %s>0 or %s>0',epar,fpar,site,tt(1:3),epar,fpar);
  [of,i]=urlread([www strrep(fof2_link,' ','%20')]);
  if i
   form='%f%f%f%*[^\n]';
   d=textscan(of,form,'headerlines',2);
   fo=[fo;[d{2} d{3}]];
   fo(find(fo==0))=NaN;
   day=[day;d{1}+datenum(tt(1),1,1)-1];
  end
 end
else
 t1=datevec(t1); y=t1(1); m=t1(2);
 www='https://www.eiscat.uit.no/';
 fof2_link=sprintf('DataBases/Dynasonde/dsnd/DSND%02d%02d.FEF',rem(y,100),m);
 [of,i]=urlread([www fof2_link]);
 if ~i && y==t(1)
  fof2_link='DataBases/Dynasonde/dsnd/autodsnd.FEF';
  [of,i]=urlread([www fof2_link]);
 end
 if i
  form='%s%*s%s%*s%*s%s%*[^\n]';
  d=textscan(of,form,1);
  fprintf('*** Reading %s %s %s ***\n',char(d{1}),char(d{2}),char(d{3}))
  form(strfind(form,'s'))='f';
  d=textscan(of,form,'headerlines',1);
  fo=[d{2} d{3}];
  fo(find(fo==0))=NaN;
  [day,d]=sort(d{1}); fo=fo(d,:);
  d=find(diff(day)==0);
  day(d)=[]; fo(d,:)=[];
  day=day+datenum(y,1,1)-1;
 end
end
