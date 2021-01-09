procedure exit_nested_loops is
begin
  L: for x in 1 .. 10 loop
    M: for y in 1 .. 10 loop
      declare
        procedure P is
        begin
          null;  --  exit;  --  "cannot exit from body or accept statement enclosed in loop (RM 5.7 (4))"
          L: for x in 1 .. 10 loop  --  Another L
            exit L;
          end loop L;
        end;
      begin
        exit L;
      end;
    end loop M;
  end loop L;
  null;  --  exit;  --  "no loop to exit from"
end exit_nested_loops;