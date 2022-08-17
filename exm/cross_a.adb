--  Example of cross-dependency in bodies

with Cross_B;

with HAT;

package body Cross_A is

  procedure A (n : Natural) is
    use HAT;
  begin
    if n > 0 then
      Put_Line ("A: " & Image (n));
      Cross_B.B (n - 1);
    end if;
  end A;

end Cross_A;
