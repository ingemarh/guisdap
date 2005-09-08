function tx=get_tx(filename,par)
% Read parameters from the tx technical log
global local d_time
if nargin<2, par=53; end %Default is HV
if d_time(1)<2001
 head=char(textread(filename,'%s',1,'delimiter','-'));
else
 head=char(textread(filename,'%s',1,'delimiter',''));
end
lp=length(par);
i=zeros(1,lp);
for j=1:lp
 c=num2str(par(j)); lc=length(c);
 [dd,hh]=strtok(head,9);
 while ~isempty(dd) & ~strcmp(dd(1:lc),c)
  [dd,hh]=strtok(hh,9);
  i(j)=i(j)+1;
  if isempty(dd)
   error(['Did not find parameter ' c])
  end
 end
 fprintf('Found "%s" on position %d\n',dd,i(j))
end

unix(['cat ' filename ' | sed s/["\.":]/\ /g | sed s/,/./g >' local.tfile]);
d=textread(local.tfile,'','headerlines',1);
delete(local.tfile)
if d_time(1)<2001
 c19=find(d(:,6)>80); d(c19,3)=d(c19,3)+1900;
 c20=find(d(:,6)<80); d(c20,6)=d(c20,6)+2000; d(:,4)=-d(:,4);
 tx=[datenum(d(:,[6 5 4 1:3])) d(:,i+6)];  % for May 2000 and Sept 2000
else
 c19=find(d(:,3)>80); d(c19,3)=d(c19,3)+1900;
 c20=find(d(:,3)<80); d(c20,3)=d(c20,3)+2000;
 tx=[datenum(d(:,[3 2 1 4:6])) d(:,i+6)];
end
