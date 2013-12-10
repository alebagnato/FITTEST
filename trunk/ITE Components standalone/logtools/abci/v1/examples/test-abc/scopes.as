package {
	function scopes() : void {
			trace("hello");
			
			function f() : void {
			}
			
			function g() : void {
			}
			
			function h() : void {
				function s() : void {
				}
				
				function t() : void {
					scopes();
				}
				
				function u() : void {
					f();
				}
				
				function v() : void {
					trace("hi");
				}
			}
	}
	
	scopes();
}