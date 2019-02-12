with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada_Containers;        use Ada_Containers;
with AUnit.Time_Measure;    use AUnit.Time_Measure;

package body AUnit.Reporter.JUnit is
   
   function Special_Chars (S : String) return String is
      Result : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"' => Append (Result, "&quot;");
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '&' => Append (Result, "&amp;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Special_Chars;
   
   procedure Report (Engine  : JUnit_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options)
   is
      package Count_Type_IO is new Ada.Text_IO.Integer_IO (Count_Type);
      use Count_Type_IO;
      
      File : File_Type renames Engine.File.all;
      
      procedure Put_To_File (Item : String) is
      begin
         Put (File, Item);
      end Put_To_File;
      
      procedure Put_To_File (Item : Integer) is
      begin
         Put (File, Item, 0);
      end Put_To_File;      
      
      procedure Put_Measure is
        new Gen_Put_Measure_In_Seconds (Put_To_File, Put_To_File);
   
      procedure Report_Test (Test : Test_Result) is
      begin
         Put (File, "<testcase name=""");
         Put (File, Special_Chars (Test.Test_Name.all &
              (if Test.Routine_Name = null then ""
                 else " : " & Test.Routine_Name.all)));
         Put (File, """ time=""");
         Put_Measure (Get_Measure (Test.Elapsed));
         if Test.Failure /= null then
            Put_Line (""">"); 
            Put_Line (File, "<failure>");
            Put_Line (File, Special_Chars (Test.Failure.Message.all));
            Put_Line (File, "</failure>");
            Put_Line (File, "</testcase>");
         elsif Test.Error /= null then
            Put_Line (""">"); 
            Put_Line (File, "<error>");
            Put_Line (File, Special_Chars (Test.Error.Exception_Name.all));
            Put_Line (File, "</error>");
            Put_Line (File, "</testcase>");
         else
            Put_Line (""" />");
         end if;      
      end Report_Test;
   
      procedure Dump_Result_List (L : Result_Lists.List) is
         use Result_Lists;
         C : Cursor := First (L);
      begin
         while Has_Element (C) loop
            Report_Test (Element (C));
            Next (C);
         end loop;
      end Dump_Result_List;
      
      T : constant Time := Elapsed (R);
   begin
      Put_Line (File, "<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line (File, "<testsuites>");
      Put (File, "<testsuite name=""AUnit testsuite"" tests=""");
      Put (File, Test_Count (R), 0);
      Put (File, """ failures=""");
      Put (File, Failure_Count (R), 0);
      Put (File, """ errors=""");
      Put (File, Error_Count (R), 0);
      if T /= Time_Measure.Null_Time then
         Put (File, """ time=""");
         Put_Measure (Get_Measure (T));
      end if;
      Put_Line (File, """>");      
      if Options.Report_Successes then
         declare
            S : Result_Lists.List;
         begin
            Successes (R, S);
            Dump_Result_List (S);
         end;
      end if;      
      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (F);
      end;
      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (E);
      end;      
      Put_Line (File, "</testsuite>");
      Put_Line (File, "</testsuites>");
   end Report;

end AUnit.Reporter.JUnit;
