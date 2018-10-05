function [ pot ] = centrifugalPotential( position, w )
%CENTRIFUGALPOTENTIAL centrifugal effect on potential due to rotation

  pot = 0.5*w*w* ...
        (position(1)*position(1)+position(2)*position(2));

end

