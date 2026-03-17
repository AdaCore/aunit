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

   E_Count      : Count_Type;
   F_Count      : Count_Type;
   S_Count      : Count_Type;
   Package_List : Message_List_Access;

   procedure Iterate_Error
     (Position : Result_Lists.Cursor; Name : String := "");
   --  Find all the test that ended in an error.
   --  A name arg can be added to filter to only tests from a specific package.
   procedure Iterate_Failure
     (Position : Result_Lists.Cursor; Name : String := "");
   --  Find all the test that ended in an failure.
   --  A name arg can be added to filter to only tests from a specific package.
   procedure Iterate_Success
     (Position : Result_Lists.Cursor; Name : String := "");
   --  Find all the test that ended succesfully.
   --  A name arg can be added to filter to only tests from a specific package.
   procedure Iterate_Name
     (Position : Result_Lists.Cursor; Name : String := "");
   --  Get all the tests from a given package name
   procedure Iterate_Package_Name
     (Position : Result_Lists.Cursor; Name : String);
   --  Get a set of all the tests package

   function Is_Error
     (Position : Result_Lists.Cursor; Name : String) return Boolean;
   function Is_Failure
     (Position : Result_Lists.Cursor; Name : String) return Boolean;
   function Is_Success
     (Position : Result_Lists.Cursor; Name : String) return Boolean;

   generic
      with
        function Test
          (Position : Result_Lists.Cursor; Name : String) return Boolean;
   procedure Gen_Extract
     (R : Result; N : String := ""; E : in out Result_Lists.List);

   -------------------
   -- Iterate_Error --
   -------------------

   procedure Iterate_Error
     (Position : Result_Lists.Cursor; Name : String := "") is
   begin
      if (Name'Length = 0
          or else Result_Lists.Element (Position).Package_Name.all = Name)
        and then Result_Lists.Element (Position).Error /= null
      then
         E_Count := E_Count + 1;
      end if;
   end Iterate_Error;

   ---------------------
   -- Iterate_Failure --
   ---------------------

   procedure Iterate_Failure
     (Position : Result_Lists.Cursor; Name : String := "") is
   begin
      if (Name'Length = 0
          or else Result_Lists.Element (Position).Package_Name.all = Name)
        and then Result_Lists.Element (Position).Failure /= null
      then
         F_Count := F_Count + 1;
      end if;
   end Iterate_Failure;

   ---------------------
   -- Iterate_Success --
   ---------------------

   procedure Iterate_Success
     (Position : Result_Lists.Cursor; Name : String := "") is
   begin
      if (Name'Length = 0
          or else Result_Lists.Element (Position).Package_Name.all = Name)
        and then Result_Lists.Element (Position).Error = null
        and then Result_Lists.Element (Position).Failure = null
      then
         S_Count := S_Count + 1;
      end if;
   end Iterate_Success;

   ------------------
   -- Iterate_Name --
   ------------------

   procedure Iterate_Name (Position : Result_Lists.Cursor; Name : String := "")
   is
   begin
      if Name'Length = 0
        or else Result_Lists.Element (Position).Package_Name.all = Name
      then
         S_Count := S_Count + 1;
      end if;
   end Iterate_Name;

   --------------------------
   -- Iterate_Package_Name --
   --------------------------

   procedure Iterate_Package_Name
     (Position : Result_Lists.Cursor; Name : String)
   is
      use Message_List;
      Package_Name : constant Message_String :=
        Result_Lists.Element (Position).Package_Name;
      Present      : Boolean := False;
      C            : Cursor := First (Package_List.all);
      pragma Unreferenced (Name);
   begin
      if Package_Name /= null then
         while Has_Element (C) loop
            if Element (C).all = Package_Name.all then
               Present := True;
            end if;
            C := Next (C);
         end loop;
         if not Present then
            Package_List.all.Append (Package_Name);
         end if;
      end if;
   end Iterate_Package_Name;

   -----------------
   -- Gen_Extract --
   -----------------

   procedure Gen_Extract
     (R : Result; N : String := ""; E : in out Result_Lists.List)
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
     (Position : Result_Lists.Cursor; Name : String) return Boolean is
   begin
      if Name'Length /= 0
        and then Result_Lists.Element (Position).Package_Name.all /= Name
      then
         return False;
      end if;
      return Result_Lists.Element (Position).Error /= null;
   end Is_Error;

   ----------------
   -- Is_Failure --
   ----------------

   function Is_Failure
     (Position : Result_Lists.Cursor; Name : String) return Boolean is
   begin
      if Name'Length /= 0
        and then Result_Lists.Element (Position).Package_Name.all /= Name
      then
         return False;
      end if;
      return Result_Lists.Element (Position).Failure /= null;
   end Is_Failure;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success
     (Position : Result_Lists.Cursor; Name : String) return Boolean is
   begin
      if Name'Length /= 0
        and then Result_Lists.Element (Position).Package_Name.all /= Name
      then
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
      Package_Name    : Message_String;
      Test_File       : Message_String;
      Routine_Name    : Message_String;
      Standard_Output : Message_String;
      Standard_Error  : Message_String;
      Location        : Tested_Location;
      Error           : Test_Error;
      Suffix          : Test_Suffix_Access;
      Elapsed         : Time)
   is
      Val : constant Test_Result :=
        (Test_Name,
         Package_Name,
         Test_File,
         Routine_Name,
         Standard_Output,
         Standard_Error,
         Alloc_Location,
         Suffix,
         null,
         Alloc_Error,
         Elapsed);
      use Result_Lists;
   begin
      Val.Location.all := Location;
      Val.Error.all := Error;
      Append (R.Result_List, Val);
   end Add_Error;

   -----------------
   -- Add_Failure --
   -----------------

   procedure Add_Failure
     (R               : in out Result;
      Test_Name       : Message_String;
      Package_Name    : Message_String;
      Test_File       : Message_String;
      Routine_Name    : Message_String;
      Standard_Output : Message_String;
      Standard_Error  : Message_String;
      Location        : Tested_Location;
      Failure         : Test_Failure;
      Suffix          : Test_Suffix_Access;
      Elapsed         : Time)
   is

      Val : constant Test_Result :=
        (Test_Name,
         Package_Name,
         Test_File,
         Routine_Name,
         Standard_Output,
         Standard_Error,
         Alloc_Location,
         Suffix,
         Alloc_Failure,
         null,
         Elapsed);
      use Result_Lists;
   begin

      Val.Location.all := Location;
      Val.Failure.all := Failure;
      Append (R.Result_List, Val);
   end Add_Failure;

   -----------------
   -- Add_Success --
   -----------------

   procedure Add_Success
     (R               : in out Result;
      Test_Name       : Message_String;
      Package_Name    : Message_String;
      Test_File       : Message_String;
      Routine_Name    : Message_String;
      Standard_Output : Message_String;
      Standard_Error  : Message_String;
      Location        : Tested_Location;
      Suffix          : Test_Suffix_Access;
      Elapsed         : Time)
   is

      Val : constant Test_Result :=
        (Test_Name,
         Package_Name,
         Test_File,
         Routine_Name,
         Standard_Output,
         Standard_Error,
         Alloc_Location,
         Suffix,
         null,
         null,
         Elapsed);
      use Result_Lists;

   begin
      Val.Location.all := Location;
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

   function Error_Count (R : Result; N : String := "") return Count_Type is
      use Result_Lists;
   begin
      E_Count := 0;
      Iterate (R.Result_List, Iterate_Error'Access, N);
      return E_Count;
   end Error_Count;

   ------------
   -- Errors --
   ------------

   procedure Errors
     (R : Result; N : String := ""; E : in out Result_Lists.List)
   is
      procedure Extract is new Gen_Extract (Is_Error);
   begin
      Extract (R, N, E);
   end Errors;

   -------------------
   -- Failure_Count --
   -------------------

   function Failure_Count (R : Result; N : String := "") return Count_Type is
      use Result_Lists;
   begin
      F_Count := 0;
      Iterate (R.Result_List, Iterate_Failure'Access, N);
      return F_Count;
   end Failure_Count;

   --------------
   -- Failures --
   --------------

   procedure Failures
     (R : Result; N : String := ""; F : in out Result_Lists.List)
   is
      procedure Extract is new Gen_Extract (Is_Failure);
   begin
      Extract (R, N, F);
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

   function Success_Count (R : Result; N : String := "") return Count_Type is
   begin
      S_Count := 0;
      Result_Lists.Iterate (R.Result_List, Iterate_Success'Access, N);
      return S_Count;
   end Success_Count;

   ---------------
   -- Successes --
   ---------------

   procedure Successes
     (R : Result; N : String := ""; S : in out Result_Lists.List)
   is
      procedure Extract is new Gen_Extract (Is_Success);
   begin
      Extract (R, N, S);
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

   function Total_Count (R : Result; Name : String := "") return Count_Type is
   begin
      S_Count := 0;
      Result_Lists.Iterate (R.Result_List, Iterate_Name'Access, Name);
      return S_Count;
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
