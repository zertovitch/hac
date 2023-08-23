package body HAC_Sys.Targets.AMD64_Windows_Console_FASM is

  --  Technical quotations are from:
  --  AMD64 Architecture Programmer's Manual
  --  24594 - Rev. 3.35 - June 2023

  use Defs, HAT;

  asm_name : constant String := "hac_generated.asm";

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

  procedure Output (m : Machine; asm : Assembler_Line) is
    instr_img : constant String :=
      (case asm.instr is
       when push_immediate => "push",
       when xor_i          => "xor",
       when others         => asm.instr'Image);
  begin
    Put_Line
      (m.asm_file,
       asm.label & Integer'Max (0, 9 - Length (asm.label)) * ' ' &
       To_Lower (+instr_img) & (20 - instr_img'Length) * ' ' &
       asm.operand_1 &
       (if asm.operand_2 = "" then +"" else ", " & asm.operand_2));
  end Output;

  procedure Peephole_Optimization (m : in out Machine) is
    aux_line : Assembler_Line;
    --
    procedure Pass is
      lines : Natural := Integer (m.asm_buf.Length);
      current : Positive := 1;
      cur_instr : Instruction;
      --
      procedure Replace_Pair is
      begin
        m.asm_buf.Delete (current);
        m.asm_buf (current) := aux_line;
        lines := lines - 1;
      end Replace_Pair;
      --
      procedure Swap_Pair is
      begin
        aux_line := m.asm_buf (current);
        m.asm_buf (current) := m.asm_buf (current + 1);
        m.asm_buf (current + 1) := aux_line;
      end Swap_Pair;
      --
    begin
      while current <= lines loop
        cur_instr := m.asm_buf (current).instr;
        if current < lines
          and then  m.asm_buf (current + 1).label = ""
        then
          --  Optimization involving this line and the next one.
          case cur_instr is
            when push | push_immediate =>
              --  Displace or simplify PUSH instruction.
              if cur_instr = push_immediate
                or else m.asm_buf (current).operand_1 = "rax"
              then
                case m.asm_buf (current + 1).instr is
                  when pop =>
                    --  Fold "push x; pop y;" into "mov y, x"
                    aux_line :=
                      (label     => m.asm_buf (current).label,
                       instr     => mov,
                       operand_1 => m.asm_buf (current + 1).operand_1,
                       operand_2 => m.asm_buf (current).operand_1);
                    Replace_Pair;
                  when mov | add | sub | imul | idiv | xor_i =>
                    --  !!  check it is not an operation involving the stack pointer !!
                    --  Move the "push" one line further. Perhaps it meets a "pop"...
                    if m.asm_buf (current).operand_1 = "rax"
                      and then
                        (m.asm_buf (current + 1).operand_1 = "rax"
                         or else m.asm_buf (current + 1).instr = idiv)
                    then
                      --  The pushed register *** is modified on next instruction,
                      --  so we can't displace the current "push *** " instrction after it.
                      null;
                    else
                      Swap_Pair;
                    end if;
                  when others =>
                    null;
                end case;
              end if;
            when others =>
              null;
          end case;
        end if;
        current := current + 1;
      end loop;
    end Pass;
  begin
    for pass_count in 1 .. 10 loop
      Pass;
    end loop;
  end Peephole_Optimization;

  procedure Flush_Assembler (m : in out Machine) is
  begin
    Peephole_Optimization (m);
    for l of m.asm_buf loop
      Output (m, l);
    end loop;
    m.asm_buf.Clear;
  end Flush_Assembler;

  procedure Emit
    (m         : in out Machine;
     instr     :        Instruction;
     operand_1 :        String := "";
     operand_2 :        String := "";
     label     :        String := "")
  is
  begin
    m.asm_buf.Append
      ((label     => +label,
        instr     => instr,
        operand_1 => +operand_1,
        operand_2 => +operand_2));
  end Emit;

  overriding procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  :        Defs.Arithmetic_Binary_Operator;
     base_typ  :        Defs.Numeric_Typ)
  is
  begin
    case base_typ is
      when Floats =>
        --  case operator is
        --    when Plus    => raise combination_not_supported;
        --    when Minus   => raise combination_not_supported;
        --    when Times   => raise combination_not_supported;
        --    when Divide  => raise combination_not_supported;
        --    when Power   => raise combination_not_supported;
        --  end case;
        raise
          combination_not_supported
            with base_typ'Image & ' ' & operator'Image;
      when Ints   =>
        Emit (m, pop, "r11");  --  Right
        Emit (m, pop, "rax");  --  Left
        case operator is
          when Plus    => Emit (m, add,  "rax", "r11");
          when Minus   => Emit (m, sub,  "rax", "r11");
          when Times   => Emit (m, imul, "rax", "r11");
          when Divide  =>
            Emit (m, xor_i, "rdx, rdx");
            Emit (m, idiv, "r11");
          when Power   =>
            raise
              combination_not_supported
                with base_typ'Image & ' ' & operator'Image;
        end case;
        Emit (m, push, "rax");
    end case;
  end Emit_Arithmetic_Binary_Instruction;

  overriding procedure Emit_Halt (m : in out Machine) is
  begin
    Emit (m, stdcall, "[ExitProcess], 0");
    Flush_Assembler (m);
    New_Line (m.asm_file);
  end Emit_Halt;

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer) is
  begin
    --  P.327: "In 64-bit mode, the operand size of all
    --          PUSH instructions defaults to 64 bits"
    Emit (m, push_immediate, HAC_Image (x));
  end Emit_Push_Discrete_Literal;

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer) is
  begin
    Emit (m, push_immediate, HAC_Image (x));
    Emit (m, push_immediate, HAC_Image (y));
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
            Emit (m, pop, "r12");  --  String Position in the strings pool
            Emit (m, add, "r12, _hac_strings_pool");
            Emit (m, pop, "r11");  --  String Length, discarded
            Emit (m, ccall, "[printf], r12");
          when Ints =>
            Emit (m, pop, "r13");  --  Base  - support it !!
            Emit (m, pop, "r12");  --  Width - support it !!
            Emit (m, pop, "r11");  --  Integer value
            Emit (m, ccall, "[printf], _hac_decimal_format, r11");
          when others =>
            raise combination_not_supported;
        end case;
        if builtin_proc = SP_Put_Line then
          m.Emit_HAT_Builtin_Procedure (SP_New_Line, 0);
        end if;
      when SP_New_Line =>
        Emit (m, ccall, "[printf], _hac_end_of_line");
      when others =>
        raise combination_not_supported with builtin_proc'Image;
    end case;
  end Emit_HAT_Builtin_Procedure;

  function Assembler_File_Name (m : Machine) return String is (asm_name);

end HAC_Sys.Targets.AMD64_Windows_Console_FASM;
