function [d_parbl,d_data]=load_qfile(file,t,ch)

fid=fopen(file,'rb');

bjt_h= fread(fid, 1, '*ubit2', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be');
bjt_m= fread(fid, 1, '*ubit3', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be');
bjt_s= fread(fid, 1, '*ubit3', 'ieee-be')*10+fread(fid, 1, '*ubit4', 'ieee-be');
bjt_ms=sum(double(fread(fid, 3, '*ubit4', 'ieee-be')).*[.1 .01 1]');

mode = double(fread(fid, 1, 'int32', 'ieee-be'));
numofsound = fread(fid, 1, 'int32', 'ieee-be');
wavegate = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
range0 = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
range1 = double(fread(fid, 1, 'int32', 'ieee-be'))*1e-6;
numofrange = fread(fid, 1, 'int32', 'ieee-be');
deltalag= fread(fid, 1, 'float32', 'ieee-be')*1e-6;
filesizeofACF= fread(fid, 1, 'int32', 'ieee-be');
filesizeofPSD= fread(fid, 1, 'int32', 'ieee-be');
filesizeofPP= fread(fid, 1, 'int32', 'ieee-be');

fseek(fid,1024,'bof');

lengthofACF=filesizeofACF/numofrange/4;
lengthofPSD=filesizeofPSD/numofrange/4;
%ACF=reshape(fread(fid, numofrange*lengthofACF, 'float32', 'ieee-be'),lengthofACF,numofrange);
ACF=fread(fid, numofrange*lengthofACF, 'float32', 'ieee-be');
PSD=spec2acf(reshape(fread(fid, numofrange*lengthofPSD, 'float32', 'ieee-be'),lengthofPSD,numofrange));
PP=fread(fid, numofrange, 'float32', 'ieee-be');
fclose(fid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
d_parbl(64)=double(numofsound);
d_parbl(43:55)=[ch mode wavegate range0 range1 double([numofrange lengthofACF]) size(PSD,1) deltalag double([bjt_h bjt_m bjt_s]) bjt_ms];
d_parbl(41)=9;
d_parbl(1:6)=datevec(t/86400);
d_parbl(7)=double(numofsound);
d_data=[PP;ACF;PSD(:)];
