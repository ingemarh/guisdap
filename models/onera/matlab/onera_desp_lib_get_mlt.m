function MLT = onera_desp_lib_get_mlt(matlabd,xGEO)
% function MLT = onera_desp_lib_get_mlt(matlabd,xGEO)
% returns magnetic local time
% xGEO is a Nx3 matrix of geographic cartesian coordinates
% MLT is Nx1

matlabd = datenum(matlabd);

onera_desp_lib_load;

if size(xGEO,2)==1,
    xGEO = xGEO';
end

ntime = size(xGEO,1);

if length(matlabd)==1,
    matlabd = repmat(matlabd,ntime,1);
end

MLT = repmat(nan,ntime,1);
[iyear,idoy,UT] = onera_desp_lib_matlabd2yds(matlabd);
mlt = nan;
MLTPtr = libpointer('doublePtr',mlt);
for i = 1:ntime,
    calllib('onera_desp_lib','get_mlt1_',iyear(i),idoy(i),UT(i),xGEO(i,:),MLTPtr);
    % have to do this next bit because Ptr's aren't really pointers
    MLT(i) = get(MLTPtr,'value');
end

% the flag value is actually -1d31
MLT(MLT<-1e30) = nan;
