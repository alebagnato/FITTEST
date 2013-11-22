package
{
    public dynamic class test1
    {
        var a = 19;
        function test1() {
          a = 18;
        }

        function f(x, y) {
          this.b = 4;
          var c = 5;
          var g = function(z) {
            h(x,z,1,g,f,a,this.b,c);
          };
        }

        function h(x,z,k,g,f,a,b,c) {
        }
    }
}
