function [err,th] = fit_prof(x,dat_c,th);
th = th * x;
err = (dat_c - th);
err = norm(err);
