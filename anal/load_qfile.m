function [d_parbl,d_data]=load_qfile(file)

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
if deltalag==15e-6
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
