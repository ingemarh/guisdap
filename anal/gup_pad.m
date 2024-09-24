% Function c = gup_pad(a,b,n)
%
% pad() adds the columns of b to the columns of a, regardless
% of whether a and b have different sizes. pad() can be called
% with 2 arguments or 3. If called with 2 arguments of the same
% dimension, the output will be sparse if either a or b is. If
% called with 2 arguments of different dimensions, the output
% will be sparse by default. If called with the third argument
% then the output will be a full array, where "missing" elements
% are replaced by the argument n. If n is empty but given, then
% the default n=0 is chosen. Examples:
% a = [1 2 3 4]';	b=[1 2]';
%
% 1a) pad(a,b) = 1   1	            1b) pad(a,b,0) = 1   1
%	          2   2		                     2   2
%   (SPARSE)     3  (0)	        (FULL)       3   0
%	          4  (0)	                     4   0
%
% 2a) pad(a,[b b]) = 1   1   1     2b) pad(a,[b b],NaN) = 1   1   1
%                    2   2   2                            2   2   2
%   (SPARSE)	      3  (0) (0)        (FULL)             3   NaN NaN
%		      4  (0) (0)                           4   NaN NaN
%
% Written by N.G.J.Gazey

function c = pad(a,b,n)

	[ar,ac] = size(a);
	[br,bc] = size(b);

	if isempty(a)

		c = b;	%% c is sparse OR full depending on b ...

	elseif (ar==br)

		%% If either a or b is sparse, c will be ...
		c = [a b];

	elseif (nargin > 2)

		%% Full matrix output, padded out with number "n" ...
		if issparse(a), a = full(a); end
		if issparse(b), b = full(b); end
		if isempty(n),  n = 0;       end
                if (ar > br)
			if isempty(b)
				c = a;
			else
				c = ones(ar,ac+bc);
	                	c(1:ar,1:ac)=a;
	                	c(1:br,ac+1:ac+bc)=b;
       		        	c(br+1:ar,ac+1:ac+bc)=ones(ar-br,bc)*n;
			end
                elseif (br > ar)
			c = ones(br,ac+bc);
			c(1:ar,1:ac)=a;
			c(ar+1:br,1:ac)=ones(br-ar,ac)*n;
			c(1:br,ac+1:ac+bc)=b;
		end

	else

		%% Sparse matrix output
		if (ar > br)
			c = sparse(ar,ac+bc);
		elseif (br > ar)
			c = sparse(br,ac+bc);
		else
			c = sparse(ar,ac+bc);
		end
		c(1:ar,1:ac) = a;
		c(1:br,ac+1:ac+bc) = b;

	end
