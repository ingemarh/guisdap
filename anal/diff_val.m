% diff_val.m: Function returns differing elements of a vector in ascending order
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function val=diff_val(vec);

function val=diff_val(vec);

val=sort(vec);
val(find(diff(val)==0))=[];
