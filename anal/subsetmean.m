function aver = subsetmean(data,lowlim,highlim) 
% aver = subsetmean(data,lowlim,highlim) 
% 
% Caluclates the average for a given subset of a dataset 'data', where the
% subset is defined by 'lowlim' and 'highlim' (0<lim<1), such that the
% subset excludes the lowlim*100 % lowest values and (1-highlim)*100 %
% highest values.
%
% Default is a dataset compiled by quartile ranges 2 and 3 of 'data'.

if nargin<3, highlim = 0.75; end
if nargin<2, lowlim  = 0.25; end
if nargin<1, error('Error: no data was given'), end

data_sort = sort(data(:));

liml = ceil(length(data(:))*lowlim);
limh = round(length(data(:))*highlim);

aver = mean(data_sort(liml:limh));

    