% Parser for Java programs
% Jim Cordy, January 2008

% Using Java 5 grammar
include "java.grm"

function main
    replace [program]
	P [program]
    by
	P	
end function
