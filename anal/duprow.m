%% Function to duplicate each row of the input matrix n times
%% and place all n duplicated columns next to each other.
%% The output is a matrix with n times as many rows as the
%% input matrix. Examples: a=
%% 	1  2  3  4  5
%% 	6  7  8  9  0
%% duprow(a,2) =
%% 	1  2  3  4  5
%%	1  2  3  4  5
%%	6  7  8  9  0
%%	6  7  8  9  0
%%
%% duprow(a,[2 3]') =
%%	1  2  3  4  5
%%      1  2  3  4  5
%%
%%      6  7  8  9  0
%%	6  7  8  9  0
%%	6  7  8  9  0
%% This last way of functioning works only if the second argument
%% is a column matrix! See also dupcol,dupcol2,duprow2.  N G J Gazey

function y = duprow(mat,n)

	if (n==0)

		y = [];

	elseif ((size(n,1) > 1) & (size(n,2)==1))

		if (size(n,1)==size(mat,1))
			[r,c] = size(mat);
			y = ones(sum(n),c);
			rn = 0; for i = 1:r
				rows = rn+1:rn+n(i);
				y(rows,:) = ones(n(i),1)*mat(i,:);
				rn = max(rows);
			end
		else
			disp('''n'' must have same number of rows as ''mat''')
		end		

	else
		[r,c] = size(mat);
		y = ones(r*n,c);
		for i=1:r
			rows = (i-1)*n+1 : i*n;
			y(rows,:)  = ones(n,1)*mat(i,:);
		end
	end
%end
