package body HAC_Sys.Targets.AMD64_Windows_Console_FASM is

  use Defs, HAT;

  asm_name : constant String := "hac_generated.asm";

  procedure Instruction
    (m        : Machine;
     instr    : String;
     operands : String)
  is
  begin
    Put_Line
      (m.asm_file, "         " & instr & (20 - instr'Length) * ' ' & operands);
  end Instruction;

  overriding procedure Initialize_Code_Emission (m : in out Machine) is
  begin
    Create (m.asm_file, asm_name);
    Put_Line (m.asm_file, ";  Assembler file for the Flat Assembler - https://flatassembler.net/");
    New_Line (m.asm_file);
    Put_Line (m.asm_file, "format PE64 console");
    Put_Line (m.asm_file, "entry _start");
    Put_Line (m.asm_file, "include 'include\win64a.inc'");
    New_Line (m.asm_file);
    Put_Line (m.asm_file, "section '.code' code readable executable");
    New_Line (m.asm_file);
    Put_Line (m.asm_file, "_start:");
  end Initialize_Code_Emission;

  overriding procedure Finalize_Code_Emission
    (m       : in out Machine;
     strings :        String)
  is
    procedure Dump_Strings is
      printable : Boolean := True;
      col : Integer;
      function Needs_New_Line return Boolean is (col mod 60 = 10);
      procedure Separate_with_Comma is
      begin
        if col > strings'First then
          Put (m.asm_file, ", ");
        end if;
        if Needs_New_Line then
          Put_Line (m.asm_file, "\ ");
          Put (m.asm_file, "    ");
        end if;
      end Separate_with_Comma;
    begin
      Put_Line (m.asm_file, "_hac_end_of_line  db 10, 0");
      Put_Line (m.asm_file, "_hac_decimal_format  db ""%d"", 0");
      Put (m.asm_file, "_hac_strings_pool db ""X");
      for i in strings'Range loop
        col := i;
        if Character'Pos (strings (i)) in 32 .. 127 then
          if printable then
            if Needs_New_Line then
              Put_Line (m.asm_file, """, \");
              Put (m.asm_file, "    """);
            end if;
          else
            Separate_with_Comma;
            Put (m.asm_file, '"');
            printable := True;
          end if;
          Put (m.asm_file, strings (i));
        else
          if printable then
            Put (m.asm_file, '"');
            printable := False;
          end if;
          Separate_with_Comma;
          Put (m.asm_file, Character'Pos (strings (i)), 0);
        end if;
      end loop;
      if printable then
        Put (m.asm_file, '"');
      end if;
      New_Line (m.asm_file);
    end Dump_Strings;
  begin
    if strings'Length > 0 then
      Put_Line (m.asm_file, "section '.data' data readable writeable");
      Dump_Strings;
      New_Line (m.asm_file);
    end if;
    Put_Line (m.asm_file, "section '.idata' import data readable");
    Put_Line (m.asm_file, "library kernel,'kernel32.dll',\");
    Put_Line (m.asm_file, "        msvcrt,'msvcrt.dll'");
    Put_Line (m.asm_file, "import  kernel,\");
    Put_Line (m.asm_file, "        ExitProcess,'ExitProcess'");
    Put_Line (m.asm_file, "import  msvcrt,\");
    Put_Line (m.asm_file, "        printf,'printf'");
    Close (m.asm_file);
  end Finalize_Code_Emission;

  overriding procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  :        Defs.Arithmetic_Binary_Operator;
     base_typ  :        Defs.Numeric_Typ)
  is
  begin
    case base_typ is
      when Floats =>
        case operator is
          when Plus    => raise combination_not_supported;
          when Minus   => raise combination_not_supported;
          when Times   => raise combination_not_supported;
          when Divide  => raise combination_not_supported;
          when Power   => raise combination_not_supported;
        end case;
      when Ints   =>
        Instruction (m, "pop", "r11");  --  Right
        Instruction (m, "pop", "rax");  --  Left
        case operator is
          when Plus    => Instruction (m, "add",  "rax, r11");
          when Minus   => Instruction (m, "sub",  "rax, r11");
          when Times   => Instruction (m, "imul", "rax, r11");
          when Divide  =>
            Instruction (m, "xor", "rdx, rdx");
            Instruction (m, "idiv", "r11");
          when Power   => raise combination_not_supported;
        end case;
        Instruction (m, "push", "rax");
    end case;
  end Emit_Arithmetic_Binary_Instruction;

  overriding procedure Emit_Halt (m : in out Machine) is
  begin
    Instruction (m, "stdcall", "[ExitProcess],0");
    New_Line (m.asm_file);
  end Emit_Halt;

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer) is
  begin
    Instruction (m, "push", HAC_Image (x));
  end Emit_Push_Discrete_Literal;

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer) is
  begin
    Instruction (m, "push", HAC_Image (x));
    Instruction (m, "push", HAC_Image (y));
  end Emit_Push_Discrete_Literals;

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer)
  is
  begin
    case builtin_proc is
      when SP_Put .. SP_Put_Line =>
        case Defs.Typen'Val (parameter) is
          --  Register numbering is parameter position, plus 10.
          when String_Literals =>
            Instruction (m, "pop", "r12");  --  String Position in the strings pool
            Instruction (m, "add", "r12, _hac_strings_pool");
            Instruction (m, "pop", "r11");  --  String Length, discarded
            Instruction (m, "ccall", "[printf], r12");
          when Ints =>
            Instruction (m, "pop", "r13");  --  Base  - support it !!
            Instruction (m, "pop", "r12");  --  Width - support it !!
            Instruction (m, "pop", "r11");  --  Integer value
            Instruction (m, "ccall", "[printf], _hac_decimal_format, r11");
          when others =>
            raise combination_not_supported;
        end case;
        if builtin_proc = SP_Put_Line then
          m.Emit_HAT_Builtin_Procedure (SP_New_Line, 0);
        end if;
      when SP_New_Line =>
        Instruction (m, "ccall", "[printf], _hac_end_of_line");
      when others =>
        raise combination_not_supported;
    end case;
  end Emit_HAT_Builtin_Procedure;

  function Assembler_File_Name (m : Machine) return String is (asm_name);

end HAC_Sys.Targets.AMD64_Windows_Console_FASM;
