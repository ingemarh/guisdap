%% Function to duplicate each column of the input matrix n times
%% placing all n duplicated columns next to each other. The output
%% is a matrix with n times as many columns as the input matrix.
%% Examples: a =
%%     	       1  2  3  4  5
%% 	       6  7  8  9  0
%% dupcol(a,2) =
%% 	1  1   2  2   3  3   4  4   5  5
%%	6  6   7  7   8  8   9  9   0  0
%%
%% dupcol(a,[1 2 3 2 1]) = 
%%      1    2  2    3  3  3    4  4    5
%%      6    7  7    8  8  8    9  9    0
%%
%% This last way of functioning works only if the second argument
%% is a row matrix! See also dupcol2,duprow,duprow2.  N G J Gazey

function y = dupcol(mat,n)

	if (n==0)

		y = [];

	elseif ((size(n,1)==1) & (size(n,2) > 1))

		if (size(n,2)==size(mat,2))
			[r,c] = size(mat);
			y = ones(r,sum(n));
			cn = 0; for i = 1:c
				cols = cn+1:cn+n(i);
				y(:,cols) = mat(:,i)*ones(1,n(i));
				cn = max(cols);
			end
		else
			disp('''n'' must have same number of columns as ''mat''')
		end		
		
	else

		[r,c] = size(mat);
		y = ones(r,c*n);
		for i=1:c
			cols = (i-1)*n+1 : i*n;
			y(:,cols)  = mat(:,i)*ones(1,n);
		end

	end
%end
