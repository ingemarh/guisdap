function a = remscale(b,factor)

	a = rem(b,factor);
	if any(a==0)
		a(a==0) = dupcol(factor,length(find(a==0)));
	end

end
