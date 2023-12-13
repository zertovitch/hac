--  Solution to Advent of Code 2022, Day 19
-------------------------------------------
--  Not Enough Minerals
--
--  https://adventofcode.com/2022/day/19
--  Copy of questions in: aoc_2022_19_questions.txt

with AoC_Toolbox;

--  Note: this solution takes a large amount of time
--  with GNAT and an even larger one with HAC.
--  See AoC_2022_19_Full_Ada for a fast solution.

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

procedure AoC_2022_19 is
  use AoC_Toolbox, HAT;

  verbosity_level : constant Natural := 1;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  f : File_Type;

  --  Parseable copy of the example (part after column with '|'):
  --
  --  |Blueprint 1:
  --  | Each ore robot costs 4 ore.
  --  | Each clay robot costs 2 ore.
  --  | Each obsidian robot costs 3 ore and 14 clay.
  --  | Each geode robot costs 2 ore and 7 obsidian.
  --  |
  --  |Blueprint 2:
  --  | Each ore robot costs 2 ore.
  --  | Each clay robot costs 3 ore.
  --  | Each obsidian robot costs 3 ore and 8 clay.
  --  | Each geode robot costs 3 ore and 12 obsidian.

  type Resource_Type is (ore, clay, obsidian, geode);

  type Portfolio_Type is array (Resource_Type) of Natural;

  type State_Type is record
    resource : Portfolio_Type;
    robot    : Portfolio_Type;
  end record;

  subtype Cost_Type is Portfolio_Type;

  --  Costs of a robot producing a certain resource.
  type Blueprint_Type is array (Resource_Type) of Cost_Type;

  function Best_Geode_Opening
    (blueprint : Blueprint_Type; total_time : Positive) return Natural
  is
    initial : State_Type;

    obsidian_cost_geode_robot, clay_cost_obsidian_robot : Positive;

    greedy : constant Boolean := True;

    procedure Visit (state : State_Type; time_left : Natural; geodes : out Natural) is
      robot_creation_possible, any_possible : Boolean;
      score_build_robot : array (Resource_Type) of Natural;
      score_same_robots : Natural;
      new_state : State_Type;
      result : Natural;
    begin
      case time_left is
        when 0 =>
          geodes := state.resource (geode);
          return;

        --  Some recursion breakers on desperately unefficient scenarios
        when 1 =>
          if state.robot (geode) = 0 then
            --  There is not even a single geode-breaking robot in the last minute.
            geodes := 0;
            return;
          end if;
        when 2 =>
          if state.robot (obsidian) = 0 then
            --  There is not even a single obsidian-collecting robot
            --  in the needed time to construct the first geode-breaking robot
            --  and produce the first geode.
            geodes := 0;
            return;
          end if;
          if state.robot (geode) = 0 then
            --  No geode-breaking robot? Fair enough, we contruct one.
            if state.resource (obsidian) < obsidian_cost_geode_robot then
              --  There is not enough obsidian to construct the
              --  first geode-breaking robot.
              geodes := 0;
              return;
            end if;
          end if;
        when 3 =>
          if state.robot (clay) = 0 then
            --  Too late for the first clay robot.
            geodes := 0;
            return;
          end if;
          if state.robot (obsidian) = 0 then
            if state.resource (clay) < clay_cost_obsidian_robot then
              --  Not enough clay to construct the first obsidian-collecting robot.
              geodes := 0;
              return;
            end if;
          end if;
          if state.robot (geode) = 0 then
            if state.resource (obsidian) + state.robot (obsidian) < obsidian_cost_geode_robot then
              --  There won't be not enough obsidian on next step to construct the
              --  first geode-breaking robot.
              geodes := 0;
              return;
            end if;
          end if;
        when 4 =>
          if state.robot (obsidian) = 0 then
            if state.resource (clay) + state.robot (clay) < clay_cost_obsidian_robot then
              --  Not enough clay to construct the first obsidian-collecting
              --  robot in the last possible minute.
              geodes := 0;
              return;
            end if;
          end if;
          if state.robot (geode) = 0 then
            case state.robot (obsidian) is
              when 0 =>
                if state.resource (obsidian) < obsidian_cost_geode_robot - 1 then
                  geodes := 0;
                  return;
                end if;
              when 1 =>
                if state.resource (obsidian) < obsidian_cost_geode_robot - 3 then
                  geodes := 0;
                  return;
                end if;
              when 2 =>
                if state.resource (obsidian) < obsidian_cost_geode_robot - 6 then
                  geodes := 0;
                  return;
                end if;
              when others =>
                null;
            end case;
          end if;
        when 5 =>
          if state.robot (geode) = 0 then
            case state.robot (obsidian) is
              when 0 =>
                --  Best case : one more obsidian robot is constructed at each
                --  subsequent time step, allowing for constructin a geode robot.
                if state.resource (obsidian) < obsidian_cost_geode_robot - 3 then
                  geodes := 0;
                  return;
                end if;
              when 1 =>
                if state.resource (obsidian) < obsidian_cost_geode_robot - 7 then
                  geodes := 0;
                  return;
                end if;
              when others =>
                null;
            end case;
          end if;
        when 6 =>
          if state.robot (geode) = 0
            and then state.robot (obsidian) = 0
            and then state.resource (obsidian) < obsidian_cost_geode_robot - 6
          then
            geodes := 0;
            return;
          end if;
        when others =>
          null;
      end case;

      any_possible := False;
      for new_robot in Resource_Type loop
        score_build_robot (new_robot) := 0;
        robot_creation_possible := False;
        if state.resource (ore) >= blueprint (new_robot)(ore) then
          case new_robot is
            when ore | clay =>
              new_state := state;
              new_state.resource (ore) := new_state.resource (ore) - blueprint (new_robot)(ore);
              robot_creation_possible := True;
            when obsidian =>
              if state.resource (clay) >= blueprint (obsidian)(clay) then
                new_state := state;
                new_state.resource (ore) := new_state.resource (ore) - blueprint (obsidian)(ore);
                new_state.resource (clay) := new_state.resource (clay) - blueprint (obsidian)(clay);
                robot_creation_possible := True;
              end if;
            when geode =>
              if state.resource (obsidian) >= blueprint (geode)(obsidian) then
                new_state := state;
                new_state.resource (ore) := new_state.resource (ore) - blueprint (geode)(ore);
                new_state.resource (obsidian) := new_state.resource (obsidian) - blueprint (geode)(obsidian);
                robot_creation_possible := True;
              end if;
          end case;
        end if;
        if robot_creation_possible then
          --  Earn new resources
          for res in Resource_Type loop
            new_state.resource (res) := new_state.resource (res) + new_state.robot (res);
          end loop;
          new_state.robot (new_robot) := new_state.robot (new_robot) + 1;
          Visit (new_state, time_left - 1, score_build_robot (new_robot));
        end if;
        any_possible := any_possible or robot_creation_possible;
      end loop;
      --  Variant without constructing a new robot.
      if greedy and then any_possible then
        --  We ignore this variant if it is possible to construct a robot.
        score_same_robots := 0;
      else
        new_state := state;
        --  Earn new resources
        for res in Resource_Type loop
          new_state.resource (res) := new_state.resource (res) + new_state.robot (res);
        end loop;
        Visit (new_state, time_left - 1, score_same_robots);
      end if;
      --  Find max:
      result := score_same_robots;
      for res in Resource_Type loop
        result := Max (result, score_build_robot (res));
      end loop;
      geodes := result;
    end Visit;

    max_geodes : Natural;

  begin
    for r in Resource_Type loop
      initial.robot (r) := 0;
      initial.resource (r) := 0;
    end loop;
    initial.robot (ore) := 1;
    obsidian_cost_geode_robot := blueprint (geode)(obsidian);
    clay_cost_obsidian_robot  := blueprint (obsidian)(clay);

    --  put(min_time_to_collect_enough_obsidian);
    --  put(min_time_to_collect_enough_clay);
    --  new_line;
    Visit (initial, total_time, max_geodes);
    return max_geodes;
  end Best_Geode_Opening;

  last : Natural := 0;

  blueprint : array (1 .. 30) of Blueprint_Type;

  best : Natural;

begin
  Open (f, "aoc_2022_19.txt");
Read_Data :
  while not End_Of_File (f) loop
    Skip_till_Space (f, 6);
    exit Read_Data when End_Of_File (f);
    last := last + 1;
    for robot in Resource_Type loop
      for cost in Resource_Type loop
        blueprint (last)(robot)(cost) := 0;
      end loop;
    end loop;
    Get (f, blueprint (last)(ore)(ore));
    Skip_till_Space (f, 6);
    Get (f, blueprint (last)(clay)(ore));
    Skip_till_Space (f, 6);
    Get (f, blueprint (last)(obsidian)(ore));
    Skip_till_Space (f, 3);
    Get (f, blueprint (last)(obsidian)(clay));
    Skip_till_Space (f, 6);
    Get (f, blueprint (last)(geode)(ore));
    Skip_till_Space (f, 3);
    Get (f, blueprint (last)(geode)(obsidian));
    Skip_till_Space (f, 1);
  end loop Read_Data;
  Close (f);

  r (1) := 0;
  r (2) := 1;
  for b in 1 .. last loop
    best := Best_Geode_Opening (blueprint (b), 24);
    if verbosity_level > 0 then
      Put_Line
        (+"In 24 steps (minutes), blueprint #" & b & ": " & best & " geodes cracked, T = " & (Clock - T0));
    end if;
    r (1) := r (1) + b * best;
    if b <= 3 then
      best := Best_Geode_Opening (blueprint (b), 32);
      if verbosity_level > 0 then
        Put_Line
          (+"In 32 steps (minutes), blueprint #" & b & ": " & best & " geodes cracked, T = " & (Clock - T0));
      end if;
      r (2) := r (2) * best;
    end if;
  end loop;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1 (24 minutes):" & r (1)'Image);
    Put_Line (+"Part 2 (32 minutes):" & r (2)'Image);
    --  Part 1: validated by AoC: 1192
    --  Part 2: validated by AoC: 14725
  end if;
end AoC_2022_19;
