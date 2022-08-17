--    ***  This HAC demo is a version of the Three_Lakes
--    ***  program in MathPaqs. The program is scaled down for
--    ***  meeting HAC v.0.072 syntax subset.
--    ***  We mark the downscaling with "!" in comments.
--    ***
--    ***  For the original verion in full Ada, see "three_lakes.adb" @
--    ***    https://mathpaqs.sourceforge.io/ or
--    ***    https://github.com/zertovitch/mathpaqs .
----------------------------------------------------------------------------
--  This program solves a vectorial ordinary differential equation
--  (or a system of ordinary differential equations).
--
--  * The unknown is a vector containing the levels of three lakes.
--  * The lakes are connected by two channels.
--  * There is an initial condition: the levels at t = 0.
--  * Boundary conditions take the form of natural inflows into the lakes,
--    and a single, controlled outflow out of one of the lakes.
--
--  Related publication:
--    Evolution simulee des niveaux dans le systeme des Trois-Lacs,
--    F. & G. de Montmollin,
--    Bulletin de la Societe vaudoise des sciences naturelles.
--    88.2: 121-129, ISSN 0037-9603, 2002
--
--  Related post:
--    https://gautiersblog.blogspot.com/2020/05/the-three-lakes-problem.html
--

with HAT; use HAT;
--  ! with Ada.Text_IO,
--  !      Ada.Integer_Text_IO,
--  !      Ada.Numerics.Generic_Elementary_Functions;

procedure Three_Lakes_S is

  --  ! type Real is digits 15;

  --  ! package PFIO is new Ada.Text_IO.Float_IO (Real);
  --  ! package PFEF is new Ada.Numerics.Generic_Elementary_functions (Real);

  type Lake is (Morat, Neuchatel, Bienne);

  type Lake_Vector is array (Lake) of Real;

  --  ! Full Ada: programmable operators (*, +, ...).

  --  ! function "*" (l : Real; v : Lake_Vector) return Lake_Vector is
  --  !   r : Lake_Vector;
  --  ! begin
  --  !   for i in v'Range loop r(i) := v(i) * l; end loop;
  --  !   return r;
  --  ! end "*";

  --  ! function "+" (a, b : Lake_Vector) return Lake_Vector is
  --  !   r : Lake_Vector;
  --  ! begin
  --  !   for i in a'Range loop r(i) := a(i) + b(i); end loop;
  --  !   return r;
  --  ! end "+";

  procedure Times (l : Real; v : Lake_Vector; r : out Lake_Vector) is
  begin
    for i in Lake loop r (i) := v (i) * l; end loop;
  end Times;

  procedure Plus (a, b : Lake_Vector; r : out Lake_Vector) is
  begin
    for i in Lake loop r (i) := a (i) + b (i); end loop;
  end Plus;

  function Sign (i : Real) return Real is
  begin
    if    i < 0.0 then  return -1.0;
    elsif i = 0.0 then  return  0.0;
    else                return  1.0;
    end if;
  end Sign;

  --  ! ivs: constant Lake_Vector:=
  --  !    (Morat     => 1.0 / 2.2820e7,
  --  !     Neuchatel => 1.0 / 2.1581e8,
  --  !     Bienne    => 1.0 / 4.0870e7);
  --
  --  ! Full Ada: aggregates

  ivs : Lake_Vector;

  procedure Init_Sensitivity is
  begin
    ivs (Morat)     := 1.0 / 2.2820e7;
    ivs (Neuchatel) := 1.0 / 2.1581e8;
    ivs (Bienne)    := 1.0 / 4.0870e7;
  end Init_Sensitivity;

  --  We solve numerically   x' (t) = f (x (t), t)   over the time step h.
  --
  procedure Evolution (x : in out Lake_Vector; q_e : Lake_Vector; q_sb, h : Real) is
    --
    --  ! function f (x : Lake_Vector) return Lake_Vector is
    --  ! Full Ada: functions with non-atomic results.

    procedure f (x : Lake_Vector; res_f : out Lake_Vector) is
      q_tr_mn, q_tr_nb : Real;
      --
      procedure Flux_tansfert is
        --  ! use PFEF;
      begin
        q_tr_mn :=
          --  Canal de la Broye: Morat -> Neuchatel.
          Sign (x (Morat) - x (Neuchatel)) *                           --  sens d'ecoulement
          15.223 *                                                      --  facteur de debit
          (((x (Morat) + x (Neuchatel)) * 0.5 - 426.0)**1.868) *   --  effet du niveau moyen
          ((abs (x (Morat) - x (Neuchatel)))**0.483);       --  effet de la diff. de niveaux
        --
        q_tr_nb :=
          --  Canal de la Thielle: Neuchatel -> Bienne.
          Sign (x (Neuchatel) - x (Bienne)) *                          --  sens d'ecoulement
          18.582 *                                                      --  facteur de debit
          (((x (Neuchatel) + x (Bienne)) * 0.5 - 426.0)**2.511) *  --  effet du niveau moyen
          ((abs (x (Neuchatel) - x (Bienne)))**0.482);      --  effet de la diff. de niveaux
      end Flux_tansfert;
    begin
      Flux_tansfert;
      res_f (Morat) := (q_e (Morat)     - q_tr_mn) * ivs (Morat);
      res_f (Neuchatel) := (q_e (Neuchatel) + q_tr_mn - q_tr_nb) * ivs (Neuchatel);
      res_f (Bienne) := (q_e (Bienne)              + q_tr_nb - q_sb) * ivs (Bienne);
    end f;
    k1, k2, k3, k4, tmp_a, tmp_b, dbk2, dbk3 : Lake_Vector;
  begin
    --  Runge-Kutta, Order 4
    --
    --  ! Full Ada: sooooo much simpler with operators!
    --
    --  !  k1 := f (x               );
    --  !  k2 := f (x + h * 0.5 * k1);
    --  !  k3 := f (x + h * 0.5 * k2);
    --  !  k4 := f (x + h *       k3);
    --  !  x := x + h * (1.0/6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
    --
    f (x, k1);
    --
    Times (h * 0.5, k1, tmp_a);
    Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * 0.5 * k1
    f (tmp_b, k2);
    --
    Times (h * 0.5, k2, tmp_a);
    Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * 0.5 * k2
    f (tmp_b, k3);
    --
    Times (h, k3, tmp_a);
    Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * k3
    f (tmp_b, k4);
    --
    Times (2.0, k2, dbk2);
    Times (2.0, k3, dbk3);
    Plus (k1, dbk2, tmp_a);
    Plus (tmp_a, dbk3, tmp_b);
    Plus (tmp_b, k4, tmp_a);     --  tmp_a = (k1 + 2.0 * k2 + 2.0 * k3 + k4)
    Times (h * (1.0 / 6.0), tmp_a, tmp_b);
    Plus (x, tmp_b, tmp_a);
    x := tmp_a;
  end Evolution;

  procedure Simulation (sim_output : VString) is
    --  ! use Ada.Text_IO, Ada.Integer_Text_IO, PFIO;
    x, q_e : Lake_Vector;
    q_sb, h : Real;
    n_iter : Integer;
    out_step : Integer;
    rf : File_Type;
    sep : constant Character := ';';
  begin
    h := 3600.0;
    n_iter := 24 * 20;
    out_step := 3;

    --  ! x := (Morat => 428.2, Neuchatel => 429.0, Bienne => 429.4);  --  Lake levels at time t = 0.
    --  ! q_e := (Morat => 40.0, Neuchatel => 70.0, Bienne => 100.0);  --  Inflows (could be dynamic).
    x (Morat)     := 428.2;
    x (Neuchatel) := 429.0;
    x (Bienne)    := 429.4;
    q_e (Morat)     := 40.0;
    q_e (Neuchatel) := 70.0;
    q_e (Bienne)    := 100.0;
    q_sb := 200.0;                                               --  Outflow (could be dynamic).

    Create (rf, sim_output);
    Put (rf, "t");
    for l in Lake loop
      Put (rf, sep);
      Put (rf, Lake'Image (l));
    end loop;
    New_Line (rf);
    for i in 0 .. n_iter loop
      if i mod out_step = 0 then
        Put (rf, i);
        for l in Lake loop
          Put (rf, sep);
          Put (rf, x (l), 4, 5, 0);
        end loop;
        New_Line (rf);
      end if;
      Evolution (x, q_e, q_sb, h);
    end loop;
    Close (rf);
  end Simulation;

  sim_output : constant VString := +"3_lakes_s.csv";

begin
  Put_Line ("Three_Lakes_S Simulation");
  Put_Line ("  -  predicting the levels of 3 interconnected lakes.");
  Put_Line ("Output in : " & sim_output);
  Init_Sensitivity;
  Simulation (sim_output);
  Put_Line ("Done");
end Three_Lakes_S;
