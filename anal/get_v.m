function [day,v]=get_v(t1,t2,site,r)
% Retrieve DSND data from database
%  
% Usage: [day,v]=get_v(t1,t2,site,region)
% t1, t2: Time interval as Matlab datenum. Empty: default today
% site: empty: default Tromsø | '(tromso|uhf|vfh)' | '(svalbard|32m|42m)'
% region: empty: default F | E
% v: Matrix [ v ev ]

if nargin==0 | isempty(t1), t1=now, end
if nargin<2 | isempty(t2), t2=t1; end
if nargin<3 | isempty(site), site='tromso'; end

if findstr(site(1),'TV'), site='tromso'; end
if site(1)=='L', site='svalbard'; end
site3=site(1:3);
if strcmp(site3,'32m') | strcmp(site3,'42m') | strcmp(site3,'esr'), site='svalbard'; end 
if strcmp(site3,'uhf') | strcmp(site3,'vhf'), site='tromso'; end 

if nargin<4, r='F'; end

day=[]; v=[];
%par='VX%s,VY%s,VZ%s,PQ%s,kmZTR%s';
par='VX%s,VY%s,VZ%s,PQ%s';
form='%f%f%f%f%f%*[^\n]';
www='https://dynserv.eiscat.uit.no/';
qform=['DD/myque.php?q=select dDay,' par ' from %s.resul%d_%02d_%02d where PQ%s>0'];

%get the vector data
for t=fix(t1):fix(t2)
 tt=datevec(t);
 query=sprintf(qform,r,r,r,r,site,tt(1:3),r);
 [of,i]=urlread([www strrep(query,' ','%20')]);
 if i
   d=textscan(of,form,'headerlines',2);
   v=[v;[d{2:end}]];
   day=[day;d{1}+datenum(tt(1),1,1)-1];
 end
end
