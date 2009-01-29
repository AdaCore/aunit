--
--  Copyright (C) 2008-2009, AdaCore
--

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with System.OS_Lib;         use System.OS_Lib;
pragma Warnings (Off);
with System.Regpat;         use System.Regpat;
pragma Warnings (On);
with GNAT.Expect;           use GNAT.Expect;

with Gprconfig_Finder;      use Gprconfig_Finder;
with String_Utils;          use String_Utils;

-------------------
-- Setup_Utility --
-------------------

procedure Setup_Utility is

   Gprconfig_Root : constant String := Find_Gprconfig_Root;
   Targets        : Argument_List_Access;
   File           : Ada.Text_IO.File_Type;

   Status         : aliased Integer;

   type Compiler_Path;
   type Compiler_Path_Access is access all Compiler_Path;

   type Compiler_Path is record
      Target  : String_Access;
      Runtime : String_Access;
      Version : String_Access;
      Path    : String_Access;
      Next    : Compiler_Path_Access := null;
   end record;

   Compilers : Compiler_Path_Access := null;

   ----------------
   -- Add_Target --
   ----------------

   procedure Add_Target
     (Target  : String;
      Path    : String;
      Runtime : String;
      Version : String)
   is
      Comp : Compiler_Path_Access := Compilers;
      Last : Compiler_Path_Access;

      function Same_Target (T1, T2 : String) return Boolean is
      begin
         if T1 = T2
           or else (T1 = "pentium-mingw32msv" and then T2 = "i686-pc-mingw32")
           or else (T2 = "pentium-mingw32msv" and then T1 = "i686-pc-mingw32")
         then
            return True;
         else
            return False;
         end if;
      end Same_Target;

   begin
      if Ada.Command_Line.Argument_Count = 2
        and then Ada.Command_Line.Argument (1) = "-path"
        and then System.OS_Lib.Normalize_Pathname
          (Ada.Command_Line.Argument (2) & "\\bin",
           Case_Sensitive => False) /=
         System.OS_Lib.Normalize_Pathname (Path, Case_Sensitive => False)
      then
         return;
      end if;

      if Compilers = null then
         Compilers := new Compiler_Path'
           (Target  => new String'(Target),
            Runtime => new String'(Runtime),
            Version => new String'(Version),
            Path    => new String'(Path),
            Next    => null);

         return;

      else
         while Comp /= null loop
            if Same_Target (Comp.Target.all, Target)
              and then Comp.Runtime.all = Runtime
            then
               return;
            end if;

            Last := Comp;
            Comp := Comp.Next;
         end loop;

         Last.Next := new Compiler_Path'
           (Target  => new String'(Target),
            Runtime => new String'(Runtime),
            Version => new String'(Version),
            Path    => new String'(Path),
            Next    => null);
      end if;
   end Add_Target;

   ---------------
   -- Int_Image --
   ---------------

   function Int_Image (I : Integer) return String is
      Img : constant String := Integer'Image (I);
   begin
      if I < 0 then
         return Img;
      else
         return Img (Img'First + 1 .. Img'Last);
      end if;
   end Int_Image;

   -------------------
   -- Num_Compilers --
   -------------------

   function Num_Compilers return Natural is
      Cur  : Compiler_Path_Access := Compilers;
      Num  : Natural := 0;
   begin
      while Cur /= null loop
         Num := Num + 1;
         Cur := Cur.Next;
      end loop;

      return Num;
   end Num_Compilers;

begin
   --  First retrieve gprconfig full path
   if Gprconfig_Root = "" then
      Ada.Text_IO.Put_Line ("Error: Cannot find gprconfig");
      Set_Exit_Status (1);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-findgprbuild"
   then
      Ada.Text_IO.Create (File, Name => "aunit-pre.ini");
      Ada.Text_IO.Put_Line (File, "[Settings]");
      Ada.Text_IO.Put_Line (File, "Install=" & Gprconfig_Root);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.CLose (File);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 2
     and then Ada.Command_Line.Argument (1) = "-path"
   then
      declare
         Orig : String_Access := System.OS_Lib.Getenv ("PATH");
         Path : constant String :=
                  System.OS_Lib.Normalize_Pathname
                    (Ada.Command_Line.Argument (2) & "\\bin",
                     Case_Sensitive => False) & ";" &
                  Orig.all;
      begin
         System.OS_Lib.Setenv ("PATH", Path);
         Free (Orig);
      end;
   end if;

   --  Now we launch it with --show-targets to retrieve the list of available
   --  targets
   declare
      Parameters   : Argument_List_Access :=
                       new Argument_List'
                         (1 => new String'("--show-targets"));
      Targets_Str  : constant String :=
                       GNAT.Expect.Get_Command_Output
                         (Gprconfig_Root & "\bin\gprconfig.exe",
                          Parameters.all, "",
                          Status'Access);
      Iter         : String_Iter;
      Index        : Natural;
      First_Line   : Boolean;

   begin
      Free (Parameters);

      if Status /= 0 then
         Ada.Text_IO.Put_Line ("Error: Problem while executing gprconfig");
         Set_Exit_Status (Exit_Status (Status));
         return;
      end if;

      Iter := Start (Targets_Str);
      Index := 0;

      while Has_More (Iter) loop
         Index := Index + 1;
         Next (Iter);
      end loop;
      --  Remove the first line that is not a target
      Index := Index - 1;
      Free (Iter);

      Targets := new Argument_List (1 .. Index);

      Index := 1;
      First_Line := True;
      Iter := Start (Targets_Str);

      while Has_More (Iter) loop
         if First_Line then
            First_Line := False;
         else
            declare
               Line       : constant String := Get_Line (Iter);
               Initialized : Boolean := False;
            begin
               for J in Line'Range loop
                  if Line (J) = ' ' then
                     Targets (Index) :=
                       new String'(Line (Line'First .. J - 1));
                     Initialized := True;
                     exit;
                  end if;
               end loop;

               if not Initialized then
                  Targets (Index) := new String'(Line);
               end if;
            end;

            Index := Index + 1;
         end if;

         Next (Iter);
      end loop;
   end;

   --  Now we launch gprconfig to retrieve all Ada compilers.
   for J in Targets'Range loop
      declare
         Parameters   : Argument_List_Access :=
                          new Argument_List'
                           ((1 => new String'("--target=" & Targets (J).all)));
         Targets_Str  : constant String :=
                          GNAT.Expect.Get_Command_Output
                            (Gprconfig_Root & "\bin\gprconfig.exe",
                             Parameters.all, "",
                             Status'Access,
                             True);
         Expression   : Pattern_Matcher :=
                          Compile
                            (" +[^ ]+ GNAT .* in (.*) version" &
                             " ([0-9.a-zA-Z]*) \(([^ ]*) runtime\) *$");
         Matched      : Match_Array (0 .. 3);
         Iter         : String_Iter;
      begin
         Free (Parameters);

         if Status /= 0 then
            Ada.Text_IO.Put_Line ("Error: Problem while executing gprconfig");
            Set_Exit_Status (Exit_Status (Status));
            return;
         end if;

         Iter := Start (Targets_Str);
         while Has_More (Iter) loop
            Match (Expression, Get_Line (Iter), Matched);
            if Matched (0) /= No_Match then
               declare
                  Path    : constant String :=
                              Get_Line (Iter)
                                (Matched (1).First .. Matched (1).Last);
                  Version : constant String :=
                              Get_Line (Iter)
                                (Matched (2).First .. Matched (2).Last);
                  Runtime : constant String :=
                              Get_Line (Iter)
                                (Matched (3).First .. Matched (3).Last);
               begin
                  if Runtime = "sjlj" then
                     Add_Target (Target  => Targets (J).all,
                                 Path    => Path,
                                 Runtime => "default",
                                 Version => Version);
                  else
                     Add_Target (Target  => Targets (J).all,
                                 Path    => Path,
                                 Runtime => Runtime,
                                 Version => Version);
                  end if;
               end;
            end if;
            Next (Iter);
         end loop;
      end;
   end loop;

   if Compilers = null then
      Ada.Text_IO.Put_Line ("Cannot find any compiler in the selected path");
      Set_Exit_Status (1);
      return;
   end if;

   declare
      Comp       : Compiler_Path_Access := Compilers;
      Tmp        : Compiler_Path_Access;
      Prj        : Ada.Text_IO.File_Type;
      Tmpl       : Ada.Strings.Unbounded.Unbounded_String;
      Tgts       : Ada.Strings.Unbounded.Unbounded_String;
      Idx        : Natural;
      Num        : Natural := 0;
      Native     : Boolean := False;
      Duplicated : Boolean;

   begin
      --  First read the aunit.gpr template
      Ada.Text_IO.Open
        (Prj, Mode => Ada.Text_IO.In_File,
         Name => "support/aunit_shared.gpr.in");

      begin
         loop
            Ada.Strings.Unbounded.Append
              (Tmpl, Ada.Text_IO.Get_Line (Prj) & ASCII.LF);
         end loop;

      exception
         when others =>
            Ada.Text_IO.Close (Prj);
      end;

      --  Get the actual list of all possible targets
      Comp := Compilers;
      Tgts := Ada.Strings.Unbounded.To_Unbounded_String ("""native""");

      while Comp /= null loop
         if Comp.Target.all /= "pentium-mingw32msv"
           and then Comp.Target.all /= "i686-pc-mingw32"
         then
            Tmp := Compilers;
            Duplicated := False;

            while Tmp /= Comp loop
               if Tmp.Target.all = Comp.Target.all then
                  Duplicated := True;
                  exit;
               end if;
               Tmp := Tmp.Next;
            end loop;

            if not Duplicated then
               Ada.Strings.Unbounded.Append (Tgts, ", ");
               Ada.Strings.Unbounded.Append
                 (Tgts, """" & Comp.Target.all & """");
            end if;
         end if;

         Comp := Comp.Next;
      end loop;

      --  And create the actual project
      Idx := Ada.Strings.Unbounded.Index (Tmpl, "@TARGETS@");
      Ada.Strings.Unbounded.Replace_Slice
        (Tmpl, Idx, Idx + 8, Ada.Strings.Unbounded.To_String (Tgts));
      Ada.Text_IO.Create (Prj, Name => "support/aunit_shared.gpr");
      Ada.Text_IO.Put (Prj, Ada.Strings.Unbounded.To_String (Tmpl));
      Ada.Text_IO.Close (Prj);

      --  Now create the config file for the NSIS installer
      Ada.Text_IO.Create (File, Name => "aunit.ini");
      Ada.Text_IO.Put_Line (File, "[Settings]");
      Ada.Text_IO.Put_Line
        (File, "NumCompilers=" & Int_Image (Num_Compilers));
      Ada.Text_IO.New_Line (File);

      Comp := Compilers;

      while Comp /= null loop
         Num := Num + 1;
         Ada.Text_IO.Put_Line (File, "[Compiler " & Int_Image (Num) & "]");
         Ada.Text_IO.Put_Line (File, "Target=" & Comp.Target.all);

         if Comp.Target.all = "pentium-mingw32msv"
           or else Comp.Target.all = "i686-pc-mingw32"
         then
            Ada.Text_IO.Put_Line (File, "XPLATFORM=native");
         else
            Ada.Text_IO.Put_Line (File, "XPLATFORM=" & Comp.Target.all);
         end if;

         Ada.Text_IO.Put_Line (File, "Runtime=" & Comp.Runtime.all);

         if Comp.Runtime.all = "default" then
            Ada.Text_IO.Put_Line (File, "XRUNTIME=full");
         else
            Ada.Text_IO.Put_Line (File, "XRUNTIME=" & Comp.Runtime.all);
         end if;

         Ada.Text_IO.Put_Line (File, "Version=" & Comp.Version.all);
         --  Remove trailing '\'
         Ada.Text_IO.Put_Line
           (File, "Path=" & Comp.Path (Comp.Path'First .. Comp.Path'Last - 1));
         --  Print path whithout the trailing '\bin\'
         Ada.Text_IO.Put_Line
           (File,
            "BasePath=" & Comp.Path (Comp.Path'First .. Comp.Path'Last - 5));
         Ada.Text_IO.New_Line (File);

         Comp := Comp.Next;
      end loop;
   end;

end Setup_Utility;
