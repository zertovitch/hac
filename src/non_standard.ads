--  Here is the only non-Ada-standard stuff in HAC_Pack - and
--  the whole HAC system, by the way!

package Non_Standard is

  procedure Sys (Command : String; Result : out Integer);

  function Directory_Separator return Character;

end Non_Standard;
