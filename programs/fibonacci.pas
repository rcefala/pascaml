program fibproc;
var x, y, times, res, quanti : int;

    procedure fibonacci (x, y, times : int);
    var temp,i : int;
        begin
            i := 0;
            while i < (times - 2) do
            begin
                temp := x + y;
                x := y;
                y := temp;
                i := i + 1;
            end;
            res := y
        end;


    procedure fibonacciList (x, y, times : int);
    var temp,i : int;
        begin
            i := 0;
            write x;
            write y;
            while i < (times - 3) do
            begin
                temp := x + y;
                x := y;
                y := temp;
                write y;
                i := i + 1;
            end;
            writeln y+x
        end;


    procedure fiboRec (x, y, times : int);
        var i:int;

        procedure fiboRecAux (x, y, i: int);
            begin
                write x;
                if i <= times then  fiboRecAux (y, x + y, i + 1);
            end;

        begin
            times := times - 2;
            fiboRecAux (x,y,0);
	    
        end;




begin
    quanti := 10;
    fibonacci(0,1,quanti);
    writeln res;
    fibonacciList(0,1,quanti);
    fiboRec(0,1,quanti);
end.

