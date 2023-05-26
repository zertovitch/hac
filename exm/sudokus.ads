with HAT;

package Sudokus is

  subtype Sudigit is Integer range 1 .. 9;

  type Sudoset is array (Sudigit) of Boolean;

  function Count (s : Sudoset) return Natural;

  type Cell is record
    solved : Boolean;
    value  : Sudigit;
    set    : Sudoset;
  end record;

  type Grid is array (Sudigit, Sudigit) of Cell;

  function Count_Solved (u : Grid) return Natural;

  function Is_Solved (u : Grid) return Boolean;

  function Is_Valid (u : Grid) return Boolean;

  --  Once a cell (i, j) is solved, we need to remove
  --  the digit in the possibility sets of the row i,
  --  the column j and the 3x3 box containing (i, j).
  --
  procedure Adapt_Sets (u : in out Grid; i, j : Sudigit);

  procedure Adapt_All_Sets (u : in out Grid);

  --  Internals - for build with HAC, this stuff stays public.
  --  We associate combinations of possible Sudigits
  --  with the binary representation of numbers.
  --  Example: if we want to consider digits 2 and 4,
  --  the number associated is 2 ** 1 + 2 ** 3 = 10.
  --
  --           ----- Binary ----- |#Sudigits = count
  --  Decimal: Bit :  >987654321< |of binary '1's   |Sequence:
  ------------------------------------------------------------
  --        1         >        1<           1       | 1
  --        2         >       10<           1       | 2
  --        3         >       11<           2       | 1, 2
  --        4         >      100<           1       | 3
  --        5         >      101<           2       | 1, 3
  --        6         >      110<           2       | 2, 3
  --        7         >      111<           3       | 1, 2, 3
  --        8         >     1000<           1       | 4
  --        9         >     1001<           2       | 1, 4
  --  >>>> 10         >     1010<           2       | 2, 4
  --       11         >     1011<           3       | 1, 2, 4

  type Sequence_Type is array (Sudigit) of Sudigit;

  type Combination_Table is array (Sudigit, 1 .. 126) of Sequence_Type;

  --  Say c is of type Combination_Table. Then, in the example above,
  --  we see that the pair {2, 4} is the 5th combination with two Sudigits.
  --  Then c (2, 5)(1) = 2 and c (2, 5)(2) = 4.

  type Max_Combinations_Type is array (Sudigit) of Natural;

  --  The data in the following record is invariant and independent of puzzles.

  type Sudo_Help is record
    full      : Sudoset;
    empty     : Sudoset;
    table     : Combination_Table;
    max_combi : Max_Combinations_Type;
  end record;

  ------------------------------------
  --  ===  Solving techniques  ===  --
  ------------------------------------

  ---------------------------------------------------------------
  --  Singles. Some cells have only one possibility.           --
  --  This case is called "single". It can be either "naked",  --
  --  when the only possibility is alone, or "hidden" when     --
  --  the possibility in question is not alone in its cell     --
  --  but is nowhere else in a house (row, column or box).     --
  --  When a single is found, the cell is solved.              --
  ---------------------------------------------------------------

  --  The "naked single" case is the most simple case to solve when
  --  the possibilities for each cell are counted: the cell (i, j)
  --  has only one possible digit. Note that for humans, this case
  --  is not easily identifiable visually.
  --
  procedure Handle_Naked_Singles (u : in out Grid; found : out Natural);

  --  A "hidden single" is not the only left on its cell but
  --  the only possible in one of the houses (row, column, box)
  --  the cells belongs to.
  --
  procedure Handle_Hidden_Singles (u : in out Grid; found : out Natural);

  --------------------
  --  Locked cells  --
  --------------------

  procedure Handle_Locked_Cells_Outside_Boxes (u : in out Grid; found : out Natural);

  procedure Handle_Locked_Cells_Inside_Boxes (u : in out Grid; found : out Natural);

  ------------------------------
  --  Multiple possibilities  --
  ------------------------------

  --  TBD: Handle_Naked_Multiples

  procedure Handle_Hidden_Multiples
    (u       : in out Grid;
     multi   : in     Sudigit;
     h       : in out Sudo_Help;
     verbose : in     Boolean;
     found   :    out Natural);

  -------------------------------------------------------
  --  Organization of different resolution techniques  --
  -------------------------------------------------------

  type Resolution_Technique is
    (naked_single,
     hidden_single,
     locked_cell_outside_box,
     locked_cell_inside_box,
     hidden_double,
     hidden_triple,
     hidden_quadruple,
     hidden_quintuple,
     hidden_sextuple,
     hidden_septuple,
     hidden_octuple);

  function Technique_Image (t : Resolution_Technique) return HAT.VString;

  type Technique_Count is array (Resolution_Technique) of Natural;

  procedure Zero (count : out Technique_Count);

  procedure Cumulate
    (total      : in out Technique_Count;
     additional :        Technique_Count);

  procedure Show_Total (total : Technique_Count; title : HAT.VString);

  --  Output of a grid on console.

  procedure Show (u : Grid; title : HAT.VString);

  procedure Show_Detailed_Possibilities (u : Grid);

  type Sudo_Pack is record
    u        : Grid;
    total    : Technique_Count;
    stalling : Boolean;
  end record;

  procedure Resolution_Round
    (pack            : in out Sudo_Pack;
     help            : in out Sudo_Help;
     title           : in     HAT.VString;
     verbosity_level : in     Natural);

  procedure Solve
    (pack            : in out Sudo_Pack;
     help            : in out Sudo_Help;
     name            : in     HAT.VString;
     verbosity_level : in     Natural);

  procedure Initialize (pack : out Sudo_Pack);

  procedure Initialize_Helper (help : out Sudo_Help);

  --  Input of puzzles as strings.

  type Sudo_Strings is array (Sudigit) of HAT.VString;

  procedure Convert_Data
    (s    : in     Sudo_Strings;
     pack : in out Sudo_Pack;
     help : in out Sudo_Help);

end Sudokus;
