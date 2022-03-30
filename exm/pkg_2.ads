with Pkg_1;

package Pkg_2 is

  subtype F is Pkg_1.E;

  use Pkg_1.Sub_Pkg_1;

  subtype G is D range 22 .. 99;

end Pkg_2;
