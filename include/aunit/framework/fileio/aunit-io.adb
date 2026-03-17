with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

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
      File_Size : constant Natural := Natural (Size (Name));
   begin
      --  On some targets, Unbounded_String is unusable, so to avoid this 
      --  problem, we first get the size of the file to read and then 
      --  read it line by line. 
      --  StreamIO could be used to read the whole file at once but 
      --  it was preferable to avoid having others dependencies.
      if File_Size = 0 then
         return null;
      end if;

      declare
         Buffer : constant Message_String :=
           new String (1 .. File_Size + 10); --  Extra Padding
         File   : File_Type;
         Last   : Natural := 0;
      begin
         Open (File, In_File, Name);
         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
            begin
               -- Check if adding this line + LF exceeds our buffer
               if Last + Line'Length + 1 <= Buffer'Last then
                  Buffer (Last + 1 .. Last + Line'Length) := Line;
                  Last := Last + Line'Length;

                  -- Manually re-insert the Line Feed that Get_Line stripped
                  Last := Last + 1;
                  Buffer (Last) := ASCII.LF;
               end if;
            end;
         end loop;
         Delete (File);

         return Format (Buffer.all);
      end;
   end Read_File;

end AUnit.IO;
