with HAT;

procedure Series is
  use HAT;
  max_x : constant := 30;
  sum : Real := 0.0;
  x : constant Real := 0.8;
begin
  Put_Line (+"x = " & x);
  Put_Line (+"sum:  1 + x + x^2 + ... + x^n:");
  for n in 0 .. max_x loop
    sum := sum + x ** n;
    Put_Line (+"n = " & n & "; sum = " & sum);
  end loop;
  Put_Line (+"n -> infinity; sum = " & 1.0 / (1.0 - x));
end Series;
