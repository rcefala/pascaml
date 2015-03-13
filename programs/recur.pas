program recur ;

procedure test2(p1, p2: int);
    begin
        if p1 < p2 then
            begin
                test2(p1 + 1, p2);                
                writeln p1;
            end;
    end;

begin
    test2(1,4);
end
.

