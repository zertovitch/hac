--  This is a demo of multiple instances of HAC running in parallel.
--  Run as:   hac_multi >res_multi.csv
--  and open the CSV file in your preferred spreadsheet software.
--
--  See HAC for the full version of the command-line tool.

with HAC_Sys.Compiler,
     HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.PCode.Interpreter;

with HAL;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Numerics.Float_Random,
     Ada.Streams.Stream_IO,
     Ada.Text_IO;

procedure HAC_Multi is

  procedure Launch_Tasks is

    use Ada.Text_IO;
    sep : constant Character := ';';

    task type HAC_Instance is
      entry Start (id : Positive);
    end HAC_Instance;

    task body HAC_Instance is
      use HAC_Sys.Compiler, HAC_Sys.Co_Defs, HAC_Sys.PCode.Interpreter;

      procedure No_Put (Item : Character) is null;
      procedure No_New_Line (Spacing : Positive_Count := 1) is null;

      package Current_IO_Console is new
        Console_Traits
           (Ada.Text_IO.End_Of_File,
            Ada.Text_IO.End_Of_Line,
            Current_IO_Get_Needs_Skip_Line,
            HAC_Sys.Defs.IIO.Get,
            HAC_Sys.Defs.RIO.Get,
            Ada.Text_IO.Get,
            Ada.Text_IO.Get_Immediate,
            Ada.Text_IO.Get_Line,
            Ada.Text_IO.Skip_Line,
            HAC_Sys.Defs.IIO.Put,
            HAC_Sys.Defs.RIO.Put,
            HAC_Sys.Defs.BIO.Put,
            No_Put,  --  Ada.Text_IO.Put
            Ada.Text_IO.Put,
            No_New_Line  --  Ada.Text_IO.New_Line
           );

      package Custom_System_Calls is new
        System_Calls_Traits
           (Ada.Command_Line.Argument_Count,
            Ada.Command_Line.Argument,
            Ada.Command_Line.Command_Name,    --  Wrong but not used anyway in this demo.
            HAL.Shell_Execute,
            HAL.Directory_Separator
           );

      use Ada.Calendar, Ada.Numerics.Float_Random, Ada.Streams.Stream_IO;

      task_id : Positive;
      tick : Time;
      gen : Generator;

      procedure Multi_Feedback (
        Stack_Current, Stack_Total : in     Natural;
        Wall_Clock                 : in     Ada.Calendar.Time;
        User_Abort                 :    out Boolean
      )
      is
      pragma Unreferenced (Stack_Current, Stack_Total);
      begin
        User_Abort := False;
        if Wall_Clock - tick >= 0.005 then
          if Random (gen) > 0.999 then
            User_Abort := True;
            Put_Line ("A1" & sep & " Task" & sep & task_id'Image & sep & " wants to abort the HAC VM.");
          end if;
          tick := Wall_Clock;
        end if;
      end Multi_Feedback;

      procedure Interpret_for_Multi is new
        Interpret
           (Multi_Feedback,
            Current_IO_Console,
            Custom_System_Calls
          );

      Ada_file_name : constant String := "exm/mandelbrot.adb";
      --
      f : Ada.Streams.Stream_IO.File_Type;
      CD : Compiler_Data;
      unhandled : Exception_Propagation_Data;
    begin
      accept Start (id : Positive) do
        task_id := id;
      end Start;
      tick := Clock;
      Reset (gen);
      --
      Open (f, In_File, Ada_file_name);
      Set_Source_Stream (CD, Stream (f), Ada_file_name);
      Compile_Main (CD);
      Close (f);
      --
      if Unit_Compilation_Successful (CD) then
        Put_Line ("S" & sep & " Task" & sep & task_id'Image & sep & " successful compilation. Running the VM.");
        Interpret_for_Multi (CD, unhandled);
        if Image (unhandled) = "User_Abort" then
          Put_Line ("A2" & sep & " Task" & sep & task_id'Image & sep & " got ""User_Abort"" exception from HAC VM.");
        else
          Put_Line ("D" & sep & " Task" & sep & task_id'Image & sep & " is done.");
        end if;
      end if;
    end HAC_Instance;

    hacs : array (1 .. 20) of HAC_Instance;

  begin
    Put_Line ("Event" & sep & "  Task #" & sep & "  Message");
    for T in hacs'Range loop
      hacs (T).Start (T);
      delay 0.01;
    end loop;
  end Launch_Tasks;

begin
  Launch_Tasks;
end HAC_Multi;
