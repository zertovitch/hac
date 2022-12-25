--  Solution to Advent of Code 2022, Day 19
-------------------------------------------
--  Not Enough Minerals
--
--  https://adventofcode.com/2022/day/19
--  Copy of questions in: aoc_2022_19_questions.txt

--  !!! Only part 1 (in a reasonable time) so far !!!

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

--  --  Interfaces is needed for compiling on both
--  --  HAC and GNAT (64-bit integer: Integer_64):
--  with Interfaces;

procedure AoC_2022_19 is
  use AoC_Toolbox, HAT;

  verbosity_level : constant Natural := 1;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  f : File_Type;

  --  Parseable copy of the example (part after '|'):
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

  subtype Cost_Type is Portfolio_Type;

  --  Costs of a robot producing a certain resource.
  type Blueprint_Type is array (Resource_Type) of Cost_Type;

  function Best_Geode_Opening
    (blueprint : Blueprint_Type; total_time : Positive) return Natural
  is
    robot : Portfolio_Type;
    resource : Portfolio_Type;

    obsidian_cost_geode_robot, clay_cost_obsidian_robot : Positive;
    min_time_to_collect_enough_obsidian : Positive;
    min_time_to_collect_enough_clay : Positive;

    function Visit (time_left : Natural) return Natural is
      enough : Boolean;
      score_build_robot : array (Resource_Type) of Natural;
      score_same_robots : Natural;
      mem_resource : Portfolio_Type;
      result : Natural;
    begin
      case time_left is
        when 0 =>
          return resource (geode);

        --  Some recursion breakers on desperately unefficient scenarios
        when 1 =>
          if robot (geode) = 0 then
            --  There is not even a single geode-breaking robot in the last minute.
            return 0;
          end if;
        when 2 =>
          if robot (obsidian) = 0 then
            --  There is not even a single obsidian-collecting robot
            --  in the needed time to construct the first geode-breaking robot
            --  and produce the first geode.
            return 0;
          end if;
          if robot (geode) = 0 then
            --  No geode-breaking robot? Fair enough, we contruct one.
            if resource (obsidian) < obsidian_cost_geode_robot then
              --  There is not enough obsidian to construct the
              --  first geode-breaking robot.
              return 0;
            end if;
          end if;
        when 3 =>
          if robot (clay) = 0 then
            --  Too late for the first clay robot.
            return 0;
          end if;
          if robot (obsidian) = 0 then
            if resource (clay) < clay_cost_obsidian_robot then
              --  Not enough clay to construct the first obsidian-collecting robot.
              return 0;
            end if;
          end if;
          if robot (geode) = 0 then
            if resource (obsidian) + robot (obsidian) < obsidian_cost_geode_robot then
              --  There won't be not enough obsidian on next step to construct the
              --  first geode-breaking robot.
              return 0;
            end if;
          end if;
        when 4 =>
          if robot (obsidian) = 0 then
            if resource (clay) + robot (clay) < clay_cost_obsidian_robot then
              --  Not enough clay to construct the first obsidian-collecting
              --  robot in the last possible minute.
              return 0;
            end if;
          end if;
          if robot (geode) = 0 then
            case robot (obsidian) is
              when 0 =>
                if resource (obsidian) < obsidian_cost_geode_robot - 1 then
                  return 0;
                end if;
              when 1 =>
                if resource (obsidian) < obsidian_cost_geode_robot - 3 then
                  return 0;
                end if;
              when 2 =>
                if resource (obsidian) < obsidian_cost_geode_robot - 6 then
                  return 0;
                end if;
              when others =>
                null;
            end case;
          end if;
        when 5 =>
          if robot (geode) = 0 then
            case robot (obsidian) is
              when 0 =>
                --  Best case : one more obsidian robot is constructed at each
                --  subsequent time step, allowing for constructin a geode robot.
                if resource (obsidian) < obsidian_cost_geode_robot - 3 then
                  return 0;
                end if;
              when 1 =>
                if resource (obsidian) < obsidian_cost_geode_robot - 7 then
                  return 0;
                end if;
              when others =>
                null;
            end case;
          end if;
        when 6 =>
          if robot (geode) = 0
            and then robot (obsidian) = 0
            and then resource (obsidian) < obsidian_cost_geode_robot - 6
          then
            return 0;
          end if;
        when others =>
          null;
      end case;

      if robot (obsidian) = 0
        and then time_left < 2 + min_time_to_collect_enough_obsidian
      then
        return 0;
      end if;

      if robot (clay) = 0
        and then time_left < 2 + min_time_to_collect_enough_obsidian
                               + min_time_to_collect_enough_clay
      then
        return 0;
      end if;

      for new_robot in Resource_Type loop
        score_build_robot (new_robot) := 0;
        enough := True;
        --  Are costs covered?
        for res in Resource_Type loop
          enough := enough and then resource (res) >= blueprint (new_robot)(res);
          exit when not enough;
        end loop;
        if enough then
          mem_resource := resource;
          --  Pay for the new robot
          for res in Resource_Type loop
            resource (res) := resource (res) - blueprint (new_robot)(res);
          end loop;
          --  Earn new resources
          for res in Resource_Type loop
            resource (res) := resource (res) + robot (res);
          end loop;
          robot (new_robot) := robot (new_robot) + 1;
          score_build_robot (new_robot) := Visit (time_left - 1);
          robot (new_robot) := robot (new_robot) - 1;
          resource := mem_resource;
        end if;
      end loop;
      --  Variant without constructing a new robot.
      mem_resource := resource;
      --  Earn new resources
      for res in Resource_Type loop
        resource (res) := resource (res) + robot (res);
      end loop;
      score_same_robots := Visit (time_left - 1);
      resource := mem_resource;
      --  Find max:
      result := score_same_robots;
      for res in Resource_Type loop
        result := Max (result, score_build_robot (res));
      end loop;
      return result;
    end Visit;

    any_res : Natural := 0;

  begin
    for r in Resource_Type loop
      robot (r) := 0;
      resource (r) := 0;
    end loop;
    robot (ore) := 1;
    obsidian_cost_geode_robot := blueprint (geode)(obsidian);
    clay_cost_obsidian_robot  := blueprint (obsidian)(clay);

    for n in 1 .. 100 loop
      any_res := any_res + n;
      --  The steepest production of any new resource
      --  is by constructing a new robot every minute.
      --  Then the cumulative resource is the
      --  triangualar suite: 1, 3, 6, 10, 15, 21, ...
      if any_res >= obsidian_cost_geode_robot then
        min_time_to_collect_enough_obsidian := n;
        exit;
      end if;
    end loop;
    for n in 1 .. 100 loop
      any_res := any_res + n;
      if any_res >= clay_cost_obsidian_robot then
        min_time_to_collect_enough_clay := n;
        exit;
      end if;
    end loop;
    --  put(min_time_to_collect_enough_obsidian);
    --  put(min_time_to_collect_enough_clay);
    --  new_line;
    return Visit (total_time);
  end Best_Geode_Opening;

  last : Natural := 0;

  blueprint : array (1 .. 30) of Blueprint_Type;

  best : Natural;

begin
  Open (f, "aoc_2022_19.txt");
Read_Data :
  while not End_Of_File (f) loop
    Skip_till_Space (f, 6);
    exit when End_Of_File (f);
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
  for b in 1 .. last loop
    best := Best_Geode_Opening (blueprint (b), 24);
    if verbosity_level > 0 then
      Put_Line
        (+"In 24 steps (minutes), blueprint #" & b & ": " & best & " geodes cracked, T = " & (Clock - T0));
    end if;
    r (1) := r (1) + b * best;
  end loop;

  r (2) := 1;
  for b in 1 .. Min (3, last) loop
    best := Best_Geode_Opening (blueprint (b), 32);
    if verbosity_level > 0 then
      Put_Line
        (+"In 32 steps (minutes), blueprint #" & b & ": " & best & " geodes cracked, T = " & (Clock - T0));
    end if;
    r (2) := r (2) * best;
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
    Put_Line (+"Part 1: bla bla:" & Integer'Image (r (1)));
    Put_Line (+"Part 2: bli bli:" & Integer'Image (r (2)));
    --  Part 1: validated by AoC: 1192
    --  Part 2: validated by AoC:
  end if;
end AoC_2022_19;
