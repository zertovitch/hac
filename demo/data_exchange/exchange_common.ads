-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------
--  Package used by both HAC and         --
--  Native sides                         --
-------------------------------------------

package Exchange_Common is

  type Animal is (ant, bat, cat, dog);

  subtype Beast is Animal;
  subtype Insect is Animal range ant .. ant;
  subtype Mammal is Animal range bat .. dog;

end Exchange_Common;
