% path_expr: creates path to the experiment folders.
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% creates path to the experiment folders. This function will come into use 
% when the corresponding parameter has not been defined in the workspace.
%
%function path=path_expr

function pathexpr=path_expr

global path_exps name_expr

pathexpr=canon([path_exps name_expr filesep],0);
