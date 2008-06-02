function hemi = onera_desp_lib_get_hemi(kext,options,sysaxes,matlabd,x1,x2,x3,maginput)
%***************************************************************************************************
% Copyright 2008, T.P. O'Brien
%
% This file is part of ONERA_DESP_LIB.
%
%    ONERA_DESP_LIB is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Lesser General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    ONERA_DESP_LIB is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Lesser General Public License for more details.
%
%    You should have received a copy of the GNU Lesser General Public License
%    along with ONERA_DESP_LIB.  If not, see <http://www.gnu.org/licenses/>.
%
%***************************************************************************************************
%
% function hemi = onera_desp_lib_get_hemi(kext,options,sysaxes,matlabd,x1,x2,x3,maginput)
if nargin < 8,
    maginput = [];
end

sysaxes = onera_desp_lib_sysaxes(sysaxes);
sysaxesGEO = onera_desp_lib_sysaxes('GEO');
xGEO = onera_desp_lib_coord_trans([x1(:) x2(:) x3(:)],[sysaxes sysaxesGEO],matlabd);
[Bgeo,B] = onera_desp_lib_get_field(kext,options,sysaxesGEO,matlabd,xGEO(:,1),xGEO(:,2),xGEO(:,3),maginput);
xGEO2 = xGEO+1e-4*Bgeo./repmat(B,1,3); % take a 0.0001 Re step along field line
[Bgeo2,B2] = onera_desp_lib_get_field(kext,options,sysaxesGEO,matlabd,xGEO2(:,1),xGEO2(:,2),xGEO2(:,3),maginput);
hemi = nan(size(B));
hemi(B2>B) = 1;
hemi(B2<B) = -1;


