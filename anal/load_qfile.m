function [d_parbl,d_data]=load_qfile(file)

if strfind(file,'1.14-42-00-000_000000.')
  [d_parbl,d_data]=load_qraw(file);
  return
end

fid=fopen(file,'rb');

ipp=12e-3; nsc=32; %huihui

[p,f,e]=fileparts(file);
a=sscanf(f,'ch%1d_%4d%2d%2d_%2d%2d%2d');

bjt_h= double(fread(fid, 1, '*ubit2', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be'));
bjt_m= double(fread(fid, 1, '*ubit3', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be'));
bjt_s= double(fread(fid, 1, '*ubit3', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be'));
bjt_ms=sum(double(fread(fid, 3, '*ubit4', 'ieee-be')).*[.1 .01 1]');
begin_time=[bjt_h bjt_m bjt_s+bjt_ms/1000.];

mode = double(fread(fid, 1, 'int32', 'ieee-be'));
numofsound = double(fread(fid, 1, 'int32', 'ieee-be'));
wavegate = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
range0 = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
range1 = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
numofrange = double(fread(fid, 1, 'int32', 'ieee-be'));
deltalag= fread(fid, 1, 'float32', 'ieee-be')*1e-6;
filesizeofACF= fread(fid, 1, 'int32', 'ieee-be');
filesizeofPSD= fread(fid, 1, 'int32', 'ieee-be');
filesizeofPP= fread(fid, 1, 'int32', 'ieee-be');
if deltalag==15e-6 || deltalag==20e-6
 ipp=8e-3;
end

%fread(fid, 1024-44, 'int8', 'ieee-be')'
fseek(fid,1024,'bof');

lengthofACF=filesizeofACF/numofrange/4;
lengthofPSD=filesizeofPSD/numofrange/4;
%ACF=reshape(fread(fid, numofrange*lengthofACF, 'float32', 'ieee-be'),lengthofACF,numofrange);
ACF=fread(fid, numofrange*lengthofACF, 'float32', 'ieee-be');
if lengthofACF==lengthofPSD
 ACF=complex(ACF(1:2:end),ACF(2:2:end));
end
PSD=fread(fid, numofrange*lengthofPSD, 'float32', 'ieee-be');
b=sort(PSD); BACK=median(b(1:end/2));
PSD=spec2acf(reshape(PSD-BACK,lengthofPSD,numofrange));
PP=fread(fid, numofrange, 'float32', 'ieee-be');
fclose(fid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
d_parbl(64)=numofsound;
d_parbl(43:54)=[a(1) mode wavegate range0 range1 numofrange length(ACF)/numofrange size(PSD,1) deltalag begin_time];
d_parbl(41)=9;
d_parbl(1:6)=datevec(datenum(a(2:7)')-8/24);
d_parbl(7)=numofsound*ipp*nsc;
d_data=[PP;ACF;PSD(:);BACK];
return

function [d_parbl,d_data]=load_qraw(file)

global a_lagprofiling
%setup
Fs=6.25e6;	% sampling freq
IPP=0.012;
Pulsewidth=480e-6;	% 16*30us
Prtperfile=512;	% no profiles/file
Pperprof=32760;	% (32768-8)
Data_Raw=complex(zeros(Pperprof,Prtperfile));
tt=Fs*30e-6;
%d_date=[2015 3 3];

%read data
d=dir(file);
fid=fopen(file,'r');
for i=1:Prtperfile
  prt_cnt_Vdspb(i) = fread(fid, 1, 'uint32'); %²¨ÃÅ¼ÆÊý
  prt_cnt_Mdspd(i) = fread(fid, 1, 'uint32');  %prt ¼ÆÊý
  time_gate_l(i) = fread(fid, 1, 'uint32');
  time_gate_h = fread(fid, 1, 'uint32');
  time_prt_l(i) = fread(fid, 1, 'uint32');
  time_prt_h = fread(fid, 1, 'uint32');
  time_50mhz_count= fread(fid, 1, 'uint32');  %50hzÊ±ÖÓ¼ÆÊ±
  %time_code(i)= fread(fid, 1, 'uint32');  %Ê±¼äÂë¼ÆÊ±,
  t(i,:)=[sum([fread(fid,4,'ubit4');fread(fid,1,'ubit3')].*[0.001;.01;.1;1;10]) ...
  sum([fread(fid, 1, 'ubit4') fread(fid, 1, 'ubit3')].*[1 10]) ...
  sum([fread(fid, 1, 'ubit4') fread(fid, 1, 'ubit2')].*[1 10])];
  Data_IQ= fread(fid,Pperprof*2,'short');
  Data_Raw(:,i)=complex(Data_IQ(1:2:end),Data_IQ(2:2:end));
end
if ftell(fid)~=d.bytes, disp('not at eof'), end
fclose(fid);
ns=floor(Pperprof/tt);
%if ns-174, error('no resampled changed'), end

%parameter block
d_parbl(64)=Prtperfile;
d_parbl(41)=9;
d=dir(file); d_date=datevec(d.date);
t1=datenum([ones(2,1)*d_date(1:3) t([1 end],[3 2 1])]);
d_parbl(1:6)=datevec(t1(2,:)-8/24);
%d_parbl(7)=diff(t1)*86400;
d_parbl(7)=IPP*Prtperfile;
d_parbl(43)=ns;

%resample
d_raw_rs=zeros(ns,Prtperfile);
ii=0;
for i=1:tt:Pperprof-tt+1
  ii=ii+1;
  d_raw_rs(ii,:)=mean(Data_Raw(floor(i):ceil(i+tt-1),:),1);
end

%correlate
%load q16x30.par
[d_data,upar]=plwin(a_lagprofiling.par,d_parbl,d_raw_rs(:));
return
