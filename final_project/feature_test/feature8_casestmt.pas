PROGRAM feature;

VAR n:integer;

BEGIN
    case n of
        0 : write('0');
        1,2,3 : write('other')
    end;
END.
