%% Function to duplicate each column of the input matrix n times
%% placing each duplicated column n columns away from the original.
%% The output is a matrix with n times as many columns as the
%% input matrix. 
%% a =
%%     	1  2  
%% 	6  7  
%%
%% dupcol2(a,3) =
%% 	1  2    1  2    1  2
%%	6  7    6  7    6  7
%%
%% dupcol(a,3,10) = 
%%      1  2   11 12   21 22
%%      6  7   16 17   26 27
%%
%% i.e. with 3 input arguments, dupcol2 adds on to the result
%% of the plain duplication (n-1)*multiplier. See also dupcol
%% duprow, and duprow2. N G J Gazey

function y = dupcol(mat,n,multiplier)

	if (n==0)

		y = [];

	elseif (nargin==3)

		[r,c] = size(mat);
		y = ones(r,c*n);
		for i=1:c
			cols = i : c : (n-1)*c + i;
			for j = find(cols)
				y(:,cols(j)) = mat(:,i)+(j-1)*multiplier;
			end
		end

	else

		[r,c] = size(mat);
		y = ones(r,c*n);
		for i=1:c
			cols = i : c : (n-1)*c + i;
			y(:,cols)  = mat(:,i)*ones(1,n);
		end

	end
