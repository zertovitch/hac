--  Preparation of Advent of Code 2020, Day 21
----------------------------------------------
--
--  https://adventofcode.com/2020/day/21
--
with Ada.Containers.Ordered_Maps, Ada.Text_IO;

with HAT;  --  in ../../../src

procedure AoC_2020_21_full_Ada_Preproc is
  use HAT;

  package Name_Mapping is new Ada.Containers.Ordered_Maps (VString, VString);

  ingredient_map, allergen_map : Name_Mapping.Map;

  i : Integer;
  f : File_Type;
  s, s1, s2, key : VString;
  paren : Integer;
begin
  Open (f, "aoc_2020_21.txt");
  while not End_Of_File (f) loop
    Get_Line (f, s);
    paren := Index (s, "(contains ");
    s1 := Slice (s, 1, paren - 1);
    s2 := Slice (s, paren + 10, Length (s));
    loop
      i := Index (s1, " ");
      exit when i = 0;
      key := Slice (s1, 1, i - 1);
      ingredient_map.Include (key, key);
      s1 := Slice (s1, i + 1, Length (s1));
    end loop;
    loop
      i := Index (s2, ", ");
      exit when i = 0;
      key := Slice (s2, 1, i - 1);
      allergen_map.Include (key, key);
      s2 := Slice (s2, i + 2, Length (s2));
    end loop;
    key := Slice (s2, 1, Length (s2) - 1);
    allergen_map.Include (key, key);
  end loop;
  Close (f);
  Put ("  type Ingredient is (");
  for ingr of ingredient_map loop
    Put (ingr & ", ");
    if Integer (Ada.Text_IO.Col) > 70 then
      New_Line;
      Put ("    ");
    end if;
  end loop;
  New_Line;
  Put ("  type Allergen is (");
  for aller of allergen_map loop
    Put (aller & ", ");
  end loop;
  New_Line;
end AoC_2020_21_full_Ada_Preproc;
