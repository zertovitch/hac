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
  procedure Handle_Naked_Single
    (u : in out Grid; i, j : Sudigit; found : in out Natural);

  procedure Handle_Naked_Singles (u : in out Grid; found : out Natural);

  --  A "hidden single" is not the only left on its cell but
  --  the only possible in one of the houses (row, column, box)
  --  the cells belongs to.
  --
  procedure Handle_Hidden_Single
    (u : in out Grid; i, j : Sudigit; found : in out Natural);

  procedure Handle_Hidden_Singles (u : in out Grid; found : out Natural);

  --------------------
  --  Locked cells  --
  --------------------

  procedure Handle_Locked_Cells_Outside_A_Box
    (u : in out Grid; i, j : Sudigit; found : in out Natural);

  procedure Handle_Locked_Cells_Outside_Boxes (u : in out Grid; found : out Natural);

  procedure Handle_Locked_Cells_Inside_A_Box
    (u : in out Grid; i, j : Sudigit; found : in out Natural);

  procedure Handle_Locked_Cells_Inside_Boxes (u : in out Grid; found : out Natural);

  --  Output of a grid on console.

  procedure Show (u : Grid; title : HAT.VString);

  procedure Show_Detailed_Possibilities (u : Grid);

  --  Input of puzzles as strings.

  type Sudostrings is array (Sudigit) of HAT.VString;

  procedure Convert_Data (s : in Sudostrings; data : out Grid);

end Sudokus;
