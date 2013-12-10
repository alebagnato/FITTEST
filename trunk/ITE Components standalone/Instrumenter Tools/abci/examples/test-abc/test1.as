package {

public function test1() : int {
	var x : int = 3;
	var y : int = 4;
	if (x < 5) {
		y = 2;
	} else {
		if (y < 2) {
			x = 5;
		} else {
			x = 1;
		}
	}
	
	if (x > 0) {
		++x;
	}
	
	++y;
	return y;
}

}