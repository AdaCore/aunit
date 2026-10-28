------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ R E S U L T S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2011, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Memory.Utils;

--  Record test results.

package body AUnit.Test_Results is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Alloc_Failure is new
     AUnit.Memory.Utils.Gen_Alloc (Test_Failure, Test_Failure_Access);

   function Alloc_Error is new
     AUnit.Memory.Utils.Gen_Alloc (Test_Error, Test_Error_Access);

   function Alloc_Location is new
     AUnit.Memory.Utils.Gen_Alloc (Tested_Location, Tested_Location_Access);

   Package_List : Message_List_Access;

   function Matches_Package
     (Package_Name, Name : Message_String) return Boolean;

   procedure Iterate_Package_Name
     (Position : Result_Lists.Cursor; Name : String);
   --  Get a set of all the tests package

   function Is_Error
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean;
   function Is_Failure
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean;
   function Is_Success
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean;

   generic
      with
        function Test
          (Position : Result_Lists.Cursor; Name : Message_String)
           return Boolean;
   procedure Gen_Extract
     (R : Result;
      E : in out Result_Lists.List;
      N : Message_String := All_Packages);

   generic
      with
        function Test
          (Position : Result_Lists.Cursor; Name : Message_String)
           return Boolean;
   function Gen_Count (R : Result; Name : Message_String) return Count_Type;

   ---------------------
   -- Matches_Package --
   ---------------------

   function Matches_Package
     (Package_Name, Name : Message_String) return Boolean is
   begin
      if Name = null then
         return Package_Name = null or else Package_Name.all'Length = 0;
      else
         return
           Name.all'Length = 0
           or else (Package_Name /= null and then Package_Name.all = Name.all);
      end if;
   end Matches_Package;

   ---------------
   -- Gen_Count --
   ---------------

   function Gen_Count (R : Result; Name : Message_String) return Count_Type is
      use Result_Lists;
      C     : Cursor := First (R.Result_List);
      Count : Count_Type := 0;
   begin
      while Has_Element (C) loop
         if Test (C, Name) then
            Count := Count + 1;
         end if;
         Next (C);
      end loop;
      return Count;
   end Gen_Count;

   --------------------------
   -- Iterate_Package_Name --
   --------------------------

   procedure Iterate_Package_Name
     (Position : Result_Lists.Cursor; Name : String)
   is
      use Message_List;
      Elem         : constant Test_Result := Result_Lists.Element (Position);
      --  Empty and null package names share the unnamed suite. Use null
      --  here because the empty-string filter selects every package.
      Package_Name : constant Message_String :=
        (if Elem.Package_Name = null or else Elem.Package_Name.all'Length = 0
         then null
         else Elem.Package_Name);
      Present      : Boolean := False;
      C            : Cursor := First (Package_List.all);
      pragma Unreferenced (Name);
   begin
      while Has_Element (C) and not Present loop
         if (Element (C) = null and then Package_Name = null)
           or else
             (Element (C) /= null
              and then Package_Name /= null
              and then Element (C).all = Package_Name.all)
         then
            Present := True;
         end if;
         C := Next (C);
      end loop;
      if not Present then
         Package_List.all.Append (Package_Name);
      end if;
   end Iterate_Package_Name;

   -----------------
   -- Gen_Extract --
   -----------------

   procedure Gen_Extract
     (R : Result;
      E : in out Result_Lists.List;
      N : Message_String := All_Packages)
   is
      C : Result_Lists.Cursor;
      use Result_Lists;
   begin
      C := First (R.Result_List);

      while Has_Element (C) loop
         if Test (C, N) then
            E.Append (Element (C));
         end if;
         Next (C);
      end loop;
   end Gen_Extract;

   --------------
   -- Is_Error --
   --------------

   function Is_Error
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean
   is

      Elem : constant Test_Result := Result_Lists.Element (Position);
   begin
      if not Matches_Package (Elem.Package_Name, Name) then
         return False;
      end if;
      return Elem.Error /= null;
   end Is_Error;

   ----------------
   -- Is_Failure --
   ----------------

   function Is_Failure
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean
   is
      Elem : constant Test_Result := Result_Lists.Element (Position);
   begin
      if not Matches_Package (Elem.Package_Name, Name) then
         return False;
      end if;
      return Elem.Failure /= null;
   end Is_Failure;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success
     (Position : Result_Lists.Cursor; Name : Message_String) return Boolean
   is
      Elem : constant Test_Result := Result_Lists.Element (Position);
   begin
      if not Matches_Package (Elem.Package_Name, Name) then
         return False;
      end if;
      return
        not Is_Error (Position, Name) and then not Is_Failure (Position, Name);
   end Is_Success;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (R               : in out Result;
      Test_Name       : Message_String;
      Routine_Name    : Message_String;
      Error           : Test_Error;
      Elapsed         : Time;
      Package_Name    : Message_String := null;
      Test_File       : Message_String := null;
      Standard_Output : Message_String := null;
      Standard_Error  : Message_String := null;
      Location        : Tested_Location := No_Location;
      Suffix          : Test_Suffix_Access := null)
   is
      Val : constant Test_Result :=
        (Test_Name       => Test_Name,
         Routine_Name    => Routine_Name,
         Failure         => null,
         Error           => Alloc_Error,
         Elapsed         => Elapsed,
         Package_Name    => Package_Name,
         Test_File       => Test_File,
         Standard_Output => Standard_Output,
         Standard_Error  => Standard_Error,
         Location        =>
           (if Location = No_Location then null else Alloc_Location),
         Suffix          => Suffix);
      use Result_Lists;
   begin
      if Location /= No_Location then
         Val.Location.all := Location;
      end if;
      Val.Error.all := Error;
      Append (R.Result_List, Val);
   end Add_Error;

   -----------------
   -- Add_Failure --
   -----------------

   procedure Add_Failure
     (R               : in out Result;
      Test_Name       : Message_String;
      Routine_Name    : Message_String;
      Failure         : Test_Failure;
      Elapsed         : Time;
      Package_Name    : Message_String := null;
      Test_File       : Message_String := null;
      Standard_Output : Message_String := null;
      Standard_Error  : Message_String := null;
      Location        : Tested_Location := No_Location;
      Suffix          : Test_Suffix_Access := null)
   is

      Val : constant Test_Result :=
        (Test_Name       => Test_Name,
         Routine_Name    => Routine_Name,
         Failure         => Alloc_Failure,
         Error           => null,
         Elapsed         => Elapsed,
         Package_Name    => Package_Name,
         Test_File       => Test_File,
         Standard_Output => Standard_Output,
         Standard_Error  => Standard_Error,
         Location        =>
           (if Location = No_Location then null else Alloc_Location),
         Suffix          => Suffix);
      use Result_Lists;
   begin
      if Location /= No_Location then
         Val.Location.all := Location;
      end if;
      Val.Failure.all := Failure;
      Append (R.Result_List, Val);
   end Add_Failure;

   -----------------
   -- Add_Success --
   -----------------

   procedure Add_Success
     (R               : in out Result;
      Test_Name       : Message_String;
      Routine_Name    : Message_String;
      Elapsed         : Time;
      Package_Name    : Message_String := null;
      Test_File       : Message_String := null;
      Standard_Output : Message_String := null;
      Standard_Error  : Message_String := null;
      Location        : Tested_Location := No_Location;
      Suffix          : Test_Suffix_Access := null)
   is

      Val : constant Test_Result :=
        (Test_Name       => Test_Name,
         Routine_Name    => Routine_Name,
         Failure         => null,
         Error           => null,
         Elapsed         => Elapsed,
         Package_Name    => Package_Name,
         Test_File       => Test_File,
         Standard_Output => Standard_Output,
         Standard_Error  => Standard_Error,
         Location        =>
           (if Location = No_Location then null else Alloc_Location),
         Suffix          => Suffix);
      use Result_Lists;

   begin
      if Location /= No_Location then
         Val.Location.all := Location;
      end if;
      Append (R.Result_List, Val);

   end Add_Success;

   -----------------
   -- Set_Elapsed --
   -----------------

   procedure Set_Elapsed (R : in out Result; T : Time_Measure.Time) is
   begin
      R.Elapsed_Time := T;
   end Set_Elapsed;

   -----------------
   -- Error_Count --
   -----------------

   function Error_Count
     (R : Result; N : Message_String := All_Packages) return Count_Type
   is
      function Count is new Gen_Count (Is_Error);
   begin
      return Count (R, N);
   end Error_Count;

   ------------
   -- Errors --
   ------------

   procedure Errors
     (R : Result;
      E : in out Result_Lists.List;
      N : Message_String := All_Packages)
   is
      procedure Extract is new Gen_Extract (Is_Error);
   begin
      Extract (R, E, N);
   end Errors;

   -------------------
   -- Failure_Count --
   -------------------

   function Failure_Count
     (R : Result; N : Message_String := All_Packages) return Count_Type
   is
      function Count is new Gen_Count (Is_Failure);
   begin
      return Count (R, N);
   end Failure_Count;

   --------------
   -- Failures --
   --------------

   procedure Failures
     (R : Result;
      F : in out Result_Lists.List;
      N : Message_String := All_Packages)
   is
      procedure Extract is new Gen_Extract (Is_Failure);
   begin
      Extract (R, F, N);
   end Failures;

   -------------
   -- Elapsed --
   -------------

   function Elapsed (R : Result) return Time_Measure.Time is
   begin
      return R.Elapsed_Time;
   end Elapsed;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (R : in out Result; Subtest_Count : Count_Type) is
   begin
      R.Tests_Run := R.Tests_Run + Subtest_Count;
   end Start_Test;

   -------------------
   -- Success_Count --
   -------------------

   function Success_Count
     (R : Result; N : Message_String := All_Packages) return Count_Type
   is
      function Count is new Gen_Count (Is_Success);
   begin
      return Count (R, N);
   end Success_Count;

   ---------------
   -- Successes --
   ---------------

   procedure Successes
     (R : Result;
      S : in out Result_Lists.List;
      N : Message_String := All_Packages)
   is
      procedure Extract is new Gen_Extract (Is_Success);
   begin
      Extract (R, S, N);
   end Successes;

   ----------------
   -- Successful --
   ----------------

   function Successful (R : Result) return Boolean is
   begin
      return Success_Count (R) = Test_Count (R);
   end Successful;

   ----------------
   -- Test_Count --
   ----------------

   function Test_Count (R : Result) return Ada_Containers.Count_Type is
   begin
      return R.Tests_Run;
   end Test_Count;

   -----------------
   -- Total_Count --
   -----------------

   function Total_Count
     (R : Result; Name : Message_String := All_Packages) return Count_Type
   is
      use Result_Lists;
      C     : Cursor := First (R.Result_List);
      Count : Count_Type := 0;
   begin
      while Has_Element (C) loop
         if Matches_Package (Element (C).Package_Name, Name) then
            Count := Count + 1;
         end if;
         Next (C);
      end loop;
      return Count;
   end Total_Count;

   -----------
   -- Clear --
   -----------

   procedure Clear (R : in out Result) is
   begin
      R.Tests_Run := 0;
      R.Elapsed_Time := Time_Measure.Null_Time;
      Result_Lists.Clear (R.Result_List);
   end Clear;

   ------------------
   -- Get_Packages --
   ------------------

   function Get_Packages (R : Result) return Message_List_Access is
   begin
      Package_List := new Message_List.List;
      Result_Lists.Iterate (R.Result_List, Iterate_Package_Name'Access);
      return Package_List;
   end Get_Packages;

end AUnit.Test_Results;
