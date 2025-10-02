% startup.m: calls start_GUP
% GUISDAP v.8.5 07-03-27 Copyright Huuskonen&Lehtinen and EISCAT
%
vn=version; vn=str2num(vn(1:3));
if vn>=25
 guplash()
end
start_GUP
