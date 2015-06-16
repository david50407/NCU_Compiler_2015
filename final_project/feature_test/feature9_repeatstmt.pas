PROGRAM feature;

VAR sum:integer;

BEGIN
    sum := 10;
    REPEAT 
        sum := sum - 1;
    UNTIL sum = 0;
END.
