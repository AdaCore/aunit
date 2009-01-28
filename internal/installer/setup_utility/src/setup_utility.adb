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

   Status         : aliased Integer;

   type Compiler_Path;
   type Compiler_Path_Access is access all Compiler_Path;

   type Build_Target;
   type Build_Target_Access is access all Build_Target;

   type Compiler_Path is record
      Path     : String_Access;
      Targets  : Build_Target_Access;
      Next     : Compiler_Path_Access := null;
   end record;

   type Build_Target is record
      Target  : String_Access;
      Runtime : String_Access;
      Version : String_Access;
      Next    : Build_Target_Access := null;
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
      Tgt  : Build_Target_Access;
      Last : Build_Target_Access;

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
      if Compilers = null then
         Compilers := new Compiler_Path'
           (Path => new String'(Path),
            Targets => new Build_Target'
              (Target  => new String'(Target),
               Runtime => new String'(Runtime),
               Version => new String'(Version),
               Next    => null),
            Next    => null);

         return;

      else
         while Comp.Next /= null
           and then Comp.Path.all /= Path
         loop
            Comp := Comp.Next;
         end loop;

         if Comp.Path.all /= Path then
            Comp.Next := new Compiler_Path'
              (Path => new String'(Path),
               Targets => new Build_Target'
                 (Target  => new String'(Target),
                  Runtime => new String'(Runtime),
                  Version => new String'(Version),
                  Next    => null),
               Next    => null);

            return;
         end if;

         Tgt := Comp.Targets;
         while Tgt /= null loop
            if Same_Target (Tgt.Target.all, Target)
              and then Tgt.Runtime.all = Runtime
            then
               return;
            end if;

            Last := Tgt;
            Tgt := Tgt.Next;
         end loop;

         Last.Next := new Build_Target'
           (Target  => new String'(Target),
            Runtime => new String'(Runtime),
            Version => new String'(Version),
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

   -----------------
   -- Num_Targets --
   -----------------

   function Num_Comp_Paths return Natural is
      Cur  : Compiler_Path_Access := Compilers;
      Num  : Natural := 0;
   begin
      while Cur /= null loop
         Num := Num + 1;
         Cur := Cur.Next;
      end loop;

      return Num;
   end Num_Comp_Paths;

   -------------------
   -- Num_Compilers --
   -------------------

   function Num_Targets (Path : Compiler_Path_Access) return Natural is
      Cur : Build_Target_Access := Path.Targets;
      Num : Natural := 0;
   begin
      while Cur /= null loop
         Num := Num + 1;
         Cur := Cur.Next;
      end loop;

      return Num;
   end Num_Targets;

begin
   --  First retrieve gprconfig full path
   if Gprconfig_Root = "" then
      Ada.Text_IO.Put_Line ("Error: Cannot find gprconfig");
      Set_Exit_Status (1);
      return;
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

   declare
      Path   : Compiler_Path_Access := Compilers;
      Tgt    : Build_Target_Access;
      File   : Ada.Text_IO.File_Type;
      Prj    : Ada.Text_IO.File_Type;
      Tmpl   : Ada.Strings.Unbounded.Unbounded_String;
      Tgts   : Ada.Strings.Unbounded.Unbounded_String;
      Idx    : Natural;
      Num    : Natural := 0;
      NTgt   : Natural;
      Native : Boolean := False;

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
      Path := Compilers;
      Tgts := Ada.Strings.Unbounded.To_Unbounded_String ("""native""");

      while Path /= null loop
         Tgt := Path.Targets;
         while Tgt /= null loop
            if Tgt.Target.all /= "pentium-mingw32msv"
              and then Tgt.Target.all /= "i686-pc-mingw32"
            then
               Ada.Strings.Unbounded.Append (Tgts, ", ");
               Ada.Strings.Unbounded.Append
                 (Tgts, """" & Tgt.Target.all & """");
            end if;

            Tgt := Tgt.Next;
         end loop;

         Path := Path.Next;
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
        (File, "NumCompilerPaths=" & Int_Image (Num_Comp_Paths));
      Ada.Text_IO.Put_Line (File, "Install=" & Gprconfig_Root);
      Ada.Text_IO.New_Line (File);

      Path := Compilers;

      while Path /= null loop
         Num := Num + 1;
         Ada.Text_IO.Put_Line (File, "[Compiler " & Int_Image (Num) & "]");
         --  Remove trailing '/'
         Ada.Text_IO.Put_Line
           (File, "Path=" & Path.Path (Path.Path'First .. Path.Path'Last - 1));
         --  Print path whithout the trailing '/bin/'
         Ada.Text_IO.Put_Line
           (File,
            "BasePath=" & Path.Path (Path.Path'First .. Path.Path'Last - 5));
         Ada.Text_IO.Put_Line
           (File, "NumTargets=" & Int_Image (Num_Targets (Path)));
         Ada.Text_IO.New_Line;

         Tgt := Path.Targets;
         NTgt := 0;

         while Tgt /= null loop
            NTgt := NTgt + 1;
            Ada.Text_IO.Put_Line
              (File, "[Target" & Int_Image (Num) & NTgt'Img & "]");
            Ada.Text_IO.Put_Line
              (File, "Target=" & Tgt.Target.all);

            if Tgt.Target.all = "pentium-mingw32msv"
              or else Tgt.Target.all = "i686-pc-mingw32"
            then
               Ada.Text_IO.Put_Line (File, "XPLATFORM=native");
            else
               Ada.Text_IO.Put_Line (File, "XPLATFORM=" & Tgt.Target.all);
            end if;

            Ada.Text_IO.Put_Line (File, "Runtime=" & Tgt.Runtime.all);

            if Tgt.Runtime.all = "default" then
               Ada.Text_IO.Put_Line (File, "XRUNTIME=full");
            else
               Ada.Text_IO.Put_Line (File, "XRUNTIME=" & Tgt.Runtime.all);
            end if;

            Ada.Text_IO.Put_Line (File, "Version=" & Tgt.Version.all);
            Ada.Text_IO.New_Line (File);

            Tgt := Tgt.Next;
         end loop;

         Path := Path.Next;
      end loop;
   end;

end Setup_Utility;
