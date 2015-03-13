program fibonacci;
var i,a,b,c : int ;

begin
    a := 0;
    b := 1;
    c := 0;
    i := 0;

    while i < 10 do
        begin                                 
            c := a + b;
            a := b;
            b := c;
            i := i + 1;
            writeln c;
        end
end .
