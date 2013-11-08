// java -jar ~/Work/asc/lib/asc.jar -import ~/Work/asc/abc/builtin.abc test-abc/test4.as
//
// ./runtests.py -v --avm=~/Work/tamarin-central/platform/mac/avmshell/build/Release/shell --builtinabc=~/Work/asc/abc/builtin.abc --shellabc=~/Work/asc/abc/toplevel.abc --rebuildtests --asc=~/Work/asc/lib/asc.jar

// test some simple arithmetic and booelan expressions

x = 0.0001;
y = 10;
z = -(x * y - y / 1 + 10) * -1.0;
u = x % y;

a = x >= y;
b = y <= z;
c = x > y;
d = y < z;
e = x == y;
e = !e;
f = x != y;
g = a || b;
h = g && c;
i = !h;
j = i != h;