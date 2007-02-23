%% 	function strings = makelabels(arg1,arg2,arg3,arg4)
%%
%% The function makelabels() creates a set of labels from numerical
%% input, returning them each in one row of the output. The output
%% array is regular, padded out with white space where necessary.
%% The strings can optionally be given a minimum length (the second
%% argument). Certain elements of the input array can be explicitly
%% associated with strings, supplied as the second and third or
%% third and fourth arguments respectively e.g.
%%
%%	array = 0:9;
%%	makelabels(array)
%%	makelabels(array,10)
%%	makelabels(array,10,[0 1 2]',['ab','cd','ef'])
%%	makelabels(array,[0 1 2]',['ab';'cd';'ef']
%%
%% The second (optional) argument is understood as the minimum string
%% length if it only contains one element, otherwise as the first of
%% the two (optional) string association arguments.

function strings = makelabels(arg1,arg2,arg3,arg4)

	min_len = 1;
	assoc = [];
	iatio = [];	
	if (nargin > 1)			%% ..... optional arguments
		if     (nargin > 3)
			min_len = arg2;
			if (rem(length(arg3),2) > 0)
				warning('Ignoring arg3 and arg4')
			else
				assoc = arg3;
				iatio = arg4;
			end
		elseif (nargin > 2)
			assoc = arg2;
			iatio = arg3;
		else
			min_len = arg2;
		end
	end

	%%  Sort through associations ....
	if (size(assoc,1) ~= size(iatio,1))
		warning('Association arguments must be of the same length')
		assoc = [];
		iatio = [];
	end
	if (~isempty(assoc) & ~isempty(iatio))
		remove = find(diff(assoc==0));
		assoc(remove)=[];
		iatio(remove)=[];
	end
	min_len = max(min_len,size(iatio,2));

	%% Find the length of the longest string to be created ...
	for i = 1:length(arg1)
		min_len = max(length(num2str(arg1(i))),min_len);
	end

	%% Construct an array of the right size ...
	strings = char(zeros(length(arg1),min_len)); % AH 1997-08-05 for Matlab5 compatibility

	%% Pad out entries with white space ...
	for i=1:length(arg1)
		if ~isempty(assoc)
			test = duprow(arg1(i),length(assoc))==assoc;
			if (any(test) > 0)
				num = iatio(find(test),:);
			else
				num = num2str(arg1(i));
			end
		else
			num = num2str(arg1(i));
		end
		tmp = length(num); if (tmp < min_len)
			num = [num dupcol(' ',min_len-tmp)];
		end
		strings(i,:) = num;
	end
%end
