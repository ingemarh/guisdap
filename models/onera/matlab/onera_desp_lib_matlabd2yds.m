function [iyear,idoy,UT] = onera_desp_lib_matlabd2yds(matlabd)
% function [iyear,idoy,UT] = onera_desp_lib_matlabd2yds(matlabd);
% helper function to convert between matlab date number and
% date format expected by onera_desp_lib routines
matlabd = datenum(matlabd);
dvec = datevec(matlabd);
iyear = dvec(:,1);
idoy = floor(matlabd)-datenum(iyear,1,1)+1;
UT = floor(dvec(:,4)*60*60+dvec(:,5)*60+dvec(:,6));
