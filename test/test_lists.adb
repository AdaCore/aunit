with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

with AUnit.Assertions; use AUnit.Assertions;

with AUnit.Lists;
package body Test_Lists is

   package Integer_Lists is new AUnit.Lists (Integer);
   use Integer_Lists;

   L : List;

   procedure Set_Up (T : in out Test_Case) is
   begin
      if not Before (L) then
         Back (L);
      end if;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Wipe_Out (L);
   end Tear_Down;


   -- Test Routines:

   procedure Test_Creation (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert (Before (L), "Cursor not properly set on initialization");
      Assert (Empty (L), "Initial list not empty");
   end Test_Creation;

   procedure Test_Back (T : in out AUnit.Test_Cases.Test_Case'Class) is
      I : Natural;
   begin
      Extend (L, 1);
      Extend (L, 2);

      Finish (L);
      Assert (Is_Last (L), "Finish did not put cursor at end of list");

      I := Index (L);
      Back (L);
      Assert
        (Index (L) = I - 1,
          "Cursor not moved backwards: expected " &
          Integer'Image (I - 1) & ", got " & Integer'Image (Index (L)));
   end Test_Back;

   procedure Test_Finish (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Finish (L);
      Assert (Before (L), "Finish on empty list /= Before");

      Extend (L, 1);
      Finish (L);
      Assert
        (Is_Last (L),
         "Finish failed to place cursor on last element of the list");
   end Test_Finish;


   procedure Test_Forth (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Extend (L, 1);
      Extend (L, 2);
      Forth (L);
      Assert
        (Index (L) = 1,
         "Forth failed to advance cursor: expected " &
         Integer'Image (1) &
         " got " &
         Integer'Image (Index (L)));
      Forth (L);
      Assert
        (Index (L) = 2,
         "Forth failed to advance cursor: expected " &
         Integer'Image (2) &
         " got " &
         Integer'Image (Index (L)));
   end Test_Forth;

   procedure Test_Go_I_Th (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Extend (L, 1);
      Extend (L, 2);
      Extend (L, 3);

      for I in 0 .. Count (L) + 1 loop
         Go_I_Th (L, I);
         Assert
           (Index (L) = I,
            "Go_I_Th failed to place cursor correctly: expected " &
            Integer'Image (I) &
            " got " &
            Integer'Image (Index (L)));
      end loop;
   end Test_Go_I_Th;

   procedure Test_Move (T : in out AUnit.Test_Cases.Test_Case'Class) is
      I : Natural;
   begin
      Extend (L, 1);
      Extend (L, 2);
      Extend (L, 3);

      Move (L, 4);
      Assert (Off (L), "Move beyond end of list did not result in Off");

      Move (L, -4);
      Assert (Off (L), "Move before beginning of list did not result in Off");

      Start (L);
      I := Index (L);
      Move (L, 2);
      Assert (not Off (L), "Test written incorrectly: expected not Off");
      Assert
        (Index (L) = I + 2,
         "Move failed to position cursor: expected " &
         Integer'Image (I + 2) &
         " got " &
         Integer'Image (Index (L)));

      Finish (L);
      I := Index (L);
      Move (L, -2);
      Assert (not Off (L), "Test written incorrectly: expected not Off");
      Assert
        (Index (L) = I + (-2),
         "Move failed to position cursor: expected " &
         Integer'Image (I + (-2)) &
         " got " &
         Integer'Image (Index (L)));

      Start (L);
      Move (L, -1);
      Assert
        (Before (L), "Move prior to first element failed to indicate Before");

      Finish (L);
      Move (L, 1);
      Assert
        (After (L), "Move beyond last element failed to indicate After");
   end Test_Move;

   procedure Test_Start (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Start (L);
      Assert (After (L), "Start on empty list failed to indicate After");

      Extend (L, 1);
      Extend (L, 2);
      Finish (L);
      Start (L);
      Assert
        (Is_First (L), "Start on non-empty list failed to indicate Is_First");
   end Test_Start;

   procedure Test_Put_Front (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Old_Count : Natural := 0;
   begin
      Put_Front (L, 1);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Front failed to increment count on initial list");
      Assert
        (First (L) = 1,
         "Put_Front inserted element incorrectly on empty list");

      Old_Count := Count (L);
      Put_Front (L, 2);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Front failed to increment count on non-empty list");
      Assert
        (First (L) = 2,
         "Put_Front inserted element incorrectly on non-empty list");
   end Test_Put_Front;


   procedure Test_Put_Left (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Old_Count, Old_Index : Natural;
   begin
      Extend (L, 1);
      Start (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Put_Left (L, 2);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Left failed to increment Count for single element list");
      Assert
        (Index (L) = Old_Index + 1,
         "Put_Left failed to adjust index for single element list");

      Finish (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Put_Left (L, 3);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Left failed to increment Count for multi-element list");
      Assert
        (Index (L) = Old_Index + 1,
         "Put_Left failed to adjust index for multi-element list");
   end Test_Put_Left;

   procedure Test_Put_Right (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Old_Count, Old_Index : Natural;
   begin
      Extend (L, 1);
      Start (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Put_Right (L, 2);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Right failed to increment Count for single element list");
      Assert
        (Index (L) = Old_Index,
         "Put_Right failed to maintain index for single element list");

      Start (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Put_Right (L, 3);
      Assert
        (Count (L) = Old_Count + 1,
         "Put_Right failed to increment Count for multi-element list");
      Assert
        (Index (L) = Old_Index,
         "Put_Right failed to maintain index for multi-element list");
   end Test_Put_Right;

   procedure Test_Replace (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Extend (L, 1);
      Start (L);
      Replace (L, 2);
      Assert (Item (L) = 2, "Replace failed for one element list");

      Extend (L, 1);
      Finish (L);
      Replace (L, 3);
      Assert (Item (L) = 3, "Replace failed at end of list");

      Extend (L, 1);
      Start (L);
      Forth (L);
      Replace (L, 4);
      Assert (Item (L) = 4, "Replace failed in middle of list");
   end Test_Replace;

   procedure Test_Remove (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Extend (L, 1);
      Extend (L, 2);
      Extend (L, 3);

      Start (L);
      Forth (L);
      Remove (L);
      Assert
        (Count (L) = 2,
         "Remove failed to adjust Count when removing from middle of list");

      Finish (L);
      Remove (L);
      Assert
        (Count (L) = 1,
         "Remove failed to adjust Count when removing from end of list");

      Start (L);
      Remove (L);
      Assert (Empty (L), "Removal of last element failed to empty list");
      Assert (After (L), "Removal of last element failed to indicate After");
   end Test_Remove;

   procedure Test_Remove_Left (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Old_Count, Old_Index : Natural;
   begin
      Extend (L, 1);
      Extend (L, 2);
      Extend (L, 3);
      Extend (L, 4);

      Finish (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Remove_Left (L);
      Assert
        (Count (L) = Old_Count - 1,
         "Remove_Left failed to adjust Count when removing before last element");
      Assert
        (Index (L) = Old_Index - 1,
         "Remove_Left failed to adjust Index when removing before last element");

      Start (L);
      Forth (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Remove_Left (L);
      Assert
        (Count (L) = Old_Count - 1,
         "Remove_Left failed to adjust Count when removing first element");
      Assert
        (Index (L) = Old_Index - 1,
         "Remove_Left failed to adjust Index when removing first element");
   end Test_Remove_Left;

   procedure Test_Remove_Right (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Old_Count, Old_Index : Natural;
   begin
      Extend (L, 1);
      Extend (L, 2);
      Extend (L, 3);
      Extend (L, 4);

      Start (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Remove_Right (L);
      Assert
        (Count (L) = Old_Count - 1,
         "Remove_Right failed to adjust Count when removing after first element");
      Assert
        (Index (L) = Old_Index,
         "Remove_Right failed to maintain Index when removing after first element");

      Finish (L);
      Back (L);
      Old_Count := Count (L);
      Old_Index := Index (L);
      Remove_Right (L);
      Assert
        (Count (L) = Old_Count - 1,
         "Remove_Right failed to adjust Count when removing last element");
      Assert
        (Index (L) = Old_Index,
         "Remove_Right failed to maintain Index when removing last element");
   end Test_Remove_Right;

   procedure Test_Wipe_Out (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in 1 .. 10 loop
         Extend (L, I);
      end loop;

      Wipe_Out (L);
      Assert (Empty (L), "Wipe_Out failed to empty list");

      Wipe_Out (L);
   exception
      when others =>
         Assert (False, "Wipe_Out fails when called on empty list");
   end Test_Wipe_Out;

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      --  Repeat for each test routine.
      Register_Routine (T, Test_Creation'Access, "Test Creation");
      Register_Routine (T, Test_Back'Access, "Test Back");
      Register_Routine (T, Test_Finish'Access, "Test Finish");
      Register_Routine (T, Test_Forth'Access, "Test Forth");
      Register_Routine (T, Test_Go_I_Th'Access, "Test Go_I_Th");
      Register_Routine (T, Test_Move'Access, "Test Move");
      Register_Routine (T, Test_Start'Access, "Test Start");
      Register_Routine (T, Test_Put_Front'Access, "Test Put_Front");
      Register_Routine (T, Test_Put_Left'Access, "Test Put_Left");
      Register_Routine (T, Test_Put_Right'Access, "Test Put_Right");
      Register_Routine (T, Test_Replace'Access, "Test Replace");
      Register_Routine (T, Test_Remove'Access, "Test Remove");
      Register_Routine (T, Test_Remove_Left'Access, "Test Remove_Left");
      Register_Routine (T, Test_Remove_Right'Access, "Test Remove_Right");
      Register_Routine (T, Test_Wipe_Out'Access, "Test Wipe_Out");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Test_Lists");
   end Name;

end Test_Lists;
