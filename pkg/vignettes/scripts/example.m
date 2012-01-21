%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example file for the R package RcppOctave
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [a,b,c] = fun1()

	a = rand(3,4);
	b = rand(6,2);
c = "it works sdsad";

end

function [a] = fun2()

	a = rand(3,4);

end

function fun_noargout() 
	x = 1; 
end

function [u, s, v] = fun_varargout() 

	if (nargout == 1) u = 99; 
	elseif (nargout == 3)
		u = 1; s = 99; v = 2; 
	endif; 
endfunction

function s = passarg(varargin)
  if (nargin==0)
	s = 0;
  else
	s = varargin{1} + plus (varargin{2:nargin});
  endif
endfunction
