%% y = function(mat,n,multiplier)
%%
%% Function to duplicate each row of the input matrix n times
%% placing each duplicated row n rows away from the original.
%% The output is a matrix with n times as many rows as the
%% input matrix.
%% a = 
%% 	1  2  3  4  5
%% 	6  7  8  9  0
%%
%% duprow2(a,2) =
%% 	1  2  3  4  5
%%      6  7  8  9  0
%%
%%	1  2  3  4  5
%%	6  7  8  9  0
%%
%% duprow2(a,3,10)
%%      1  2  3  4  5
%%      6  7  8  9  0
%%
%%     11 12 13 14 15
%%     16 17 18 19 10
%%
%%     21 22 23 24 25
%%     26 27 28 29 20
%%
%% i.e. with 3 input arguments, duprow2 adds on to the result
%% of the plain duplication (n-1)*multiplier. See also dupcol
%% dupcol2, and duprow. N G J Gazey

function y = duprow2(mat,n,multiplier)

	if (n==0)

		y = [];

	elseif (nargin==3)

		[r,c] = size(mat);
		y = ones(r*n,c);
		for i=1:r
			rows = i : r : (n-1)*r + i;
			for j = find(rows)
				y(rows(j),:) = mat(i,:)+(j-1)*multiplier;
			end
		end

	else

		[r,c] = size(mat);
		y = ones(r*n,c);
		for i=1:r
			rows = i : r : (n-1)*r + i;
			y(rows,:)  = ones(n,1)*mat(i,:);
		end

	end
