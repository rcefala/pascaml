program fibproc ;
var a,b:int;

procedure test(a1,a2,a3,a4: int);
    var n :int;
        procedure intest(a1,b:int);
        var a :int;
        begin
            a := 10;
            b := a + a1;
            writeln b;
            dumpenv;
        end;
    begin
        a := a + 1;
        n := 100;
        writeln a;
        dumpenv;
        intest(n,a);
    end;

begin
    a := 1000;
    writeln a;
    dumpenv;
    test(1,2,3,4);    
    dumpenv;
    writeln a;
    end
.

