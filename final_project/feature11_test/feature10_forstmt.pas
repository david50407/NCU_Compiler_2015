PROGRAM feature;

VAR 
    a:integer;
    sum:integer;

BEGIN
    sum:=10;
    FOR a := 10 to 20 do
    BEGIN
        sum := sum + a;
    END;
    writeln(sum);
END.
