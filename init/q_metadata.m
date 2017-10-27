function q_metadata(yrs)
global path_GUP
qmeta_new=[];
for y=yrs
 qmeta_new=[qmeta_new qget(sprintf('%d.xlsx',y))];
end
try
 load(fullfile(path_GUP,'matfiles','q_metadata'))
catch
 qmeta=[];
end
qmeta=[qmeta_new;qmeta];
[c1,ia1,ic1] = unique(qmeta(:,1));
qmeta=qmeta(ia1,:);
save(fullfile(path_GUP,'matfiles','q_metadata'),'qmeta','-v6')

function n=qget(q_xls_file)
%date1 date2 az el mode# nscans r0(ms) r0_raw(km)
[n,t]=xlsread(q_xls_file);
%remove empty lines
d=find(isnan(n(:,1)));
n(d,:)=[]; t([1;d+1],:)=[];
sn=size(n);
%fix date
dates=datestr(n(:,1)+693960);
h=split(t(:,3),'-');
for i=1:sn(1)
 for j=1:2
  n(i,j)=datenum([dates(i,:) ' ' char(h(i,j))]);
 end
end
n(:,1:3)=n(:,1:3)-8/24;
n(:,3)=[];
%fix mode, need also ipp/baud ?
for i=1:sn(1)
 n(i,5)=sscanf(char(t(i,6)),'%d');
end
%use hard ipp baud for now
ipplist=[2.8 5.4 2.8 5.4 5.4 8 10 5.4 8 12 10 12 15 8 12 12 20 ...
         2.8 5.4 5.4 8 8 12 ...
	 2.8 5.4 5.4 8 5.4 8 8 12 15 12 20 ...
	 5.4 8 12 20]'*1e-3;
baudlen=[20 20 50 50 80 80 80 100 100 100 200 200 200 300 300 480 480 ...
         8 8 15 15 30 30 ...
	 8 8 10 10 15 15 20 20 20 30 30 ...
	 7.5 7.5 15 15]'*1e-6;
n(:,6:8)=[ipplist(n(:,5)) baudlen(n(:,5)) n(:,6)];
%r0, need also rend ?
for i=1:sn(1)
 for j=1:2
  n(i,8+j)=sscanf(strtok(char(t(i,7+j)),'-'),'%g');
 end
end
n(:,10)=round(n(:,10)/.15)*1e-6;

if find(isnan(n))
 warning(['Some fields not converted in ' q_xls_file])
end 
