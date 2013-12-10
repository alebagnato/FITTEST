define line
	[repeat id] ';
end define

define program
	[repeat line]
end define

function main

match [program]
	_ [program]
	
end function