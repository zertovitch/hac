with HAL; use HAL;

PROCEDURE PGM1 IS

TASK STD_1;

TASK STD_2;

TASK STD_3;

TASK TEACHER IS
   ENTRY CHECK_GRADE(STDNT : INTEGER;
                     GRADE   : OUT INTEGER);
END TEACHER;

TASK BODY STD_1 IS
   MY_GRADE : INTEGER;
BEGIN
   TEACHER.CHECK_GRADE(1,MY_GRADE);
END STD_1;

TASK BODY STD_2 IS
   MY_GRADE : INTEGER;
BEGIN
   TEACHER.CHECK_GRADE(2,MY_GRADE);
END STD_2;

TASK BODY STD_3 IS
   MY_GRADE : INTEGER;
BEGIN
   TEACHER.CHECK_GRADE(3,MY_GRADE);
END STD_3;

TASK BODY TEACHER IS
   CLASS_GRADES : ARRAY(1..4) OF INTEGER;
   I : INTEGER;
BEGIN
   CLASS_GRADES(1) := 35;
   CLASS_GRADES(2) := 97;
   CLASS_GRADES(3) := 85;
   CLASS_GRADES(4) := 77;
   ACCEPT CHECK_GRADE(
        STDNT   : INTEGER;
        GRADE   : OUT INTEGER) 
   DO
      I := STDNT;
      GRADE := CLASS_GRADES(I);
   END CHECK_GRADE;
END TEACHER;

BEGIN
   NULL;
END PGM1;
