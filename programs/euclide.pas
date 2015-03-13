program euclide ;
var a,b :int;

    begin
        a := 12;
        b := 9;
        while a <> b do
            if a > b then
                a := a - b
            else
                b := b - a;
        writeln a
    end .

