with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body AUnit.IO is

   procedure Redirect_Standard_Streams
     (Name             : String;
      Output_File      : in out File_Type;
      Error_File       : in out File_Type;
      Capture_Standard : Boolean) is
   begin
      if not Capture_Standard then
         return;
      end if;
      Create (Output_File, Out_File, ".tmp_stdout_" & Name);
      Create (Error_File, Out_File, ".tmp_stderr_" & Name);
      Set_Output (Output_File);
      Set_Error (Error_File);
   end Redirect_Standard_Streams;

   procedure Restore_Standard_Streams
     (Output_File      : in out File_Type;
      Error_File       : in out File_Type;
      Capture_Standard : Boolean) is
   begin
      if not Capture_Standard then
         return;
      end if;
      Flush (Output_File);
      Flush (Error_File);
      --  Flush in case there is still some characters in the
      --  file buffer.

      Set_Output (Standard_Output);
      Set_Error (Standard_Error);
      Close (Output_File);
      Close (Error_File);
   end Restore_Standard_Streams;

   function Read_Error
     (Name : String; Capture_Standard : Boolean) return Message_String is
   begin
      if not Capture_Standard then
         return null;
      end if;
      return Read_File (".tmp_stderr_" & Name);
   end Read_Error;

   function Read_Output
     (Name : String; Capture_Standard : Boolean) return Message_String is
   begin
      if not Capture_Standard then
         return null;
      end if;
      return Read_File (".tmp_stdout_" & Name);
   end Read_Output;

   function Read_File (Name : String) return Message_String is
      File        : File_Type;
      Output_Text : Unbounded_String;
   begin
      Open (File, In_File, Name);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Output_Text := Output_Text & Line & ASCII.LF;
         end;
      end loop;
      Delete (File);
      if To_String (Output_Text)'Length /= 0 then
         return Format (To_String (Output_Text));
      else
         return null;
      end if;
   end Read_File;

end AUnit.IO;
