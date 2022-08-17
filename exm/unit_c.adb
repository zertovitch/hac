with HAT;

use HAT;

function Unit_C (title_1, title_2 : VString; n : Integer) return Real is
  c_msg : VString;
  procedure Z is
  begin
   c_msg := +".C";
   Put (+"z[" & n & "]");
  end Z;
begin
  c_msg := +".c";
  Put (+"(" & title_1 & c_msg & ">");
  Z;
  Put (+"<" & title_2 & c_msg & ")");
  return Real (n);
end Unit_C;
