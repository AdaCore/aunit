--
--  Copyright (C) 2008, AdaCore
--

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Expect;      use GNAT.Expect;
with System.OS_Lib;    use System.OS_Lib;
pragma Warnings (Off);
with System.Regpat;    use System.Regpat;
pragma Warnings (On);

with Gprconfig_Finder; use Gprconfig_Finder;
with String_Utils;     use String_Utils;

-------------------
-- Setup_Utility --
-------------------

procedure Setup_Utility is

   Gprconfig_Root : constant String := Find_Gprconfig_Root;
   Targets        : Argument_List_Access;

   Status         : aliased Integer;

   type Build_Target;
   type Build_Target_Access is access all Build_Target;

   type Build_Target is record
      Target  : String_Access;
      Path    : String_Access;
      Runtime : String_Access;
      Version : String_Access;
      Next    : Build_Target_Access;
   end record;

   Build_Targets  : Build_Target_Access := null;

   ----------------
   -- Add_Target --
   ----------------

   procedure Add_Target
     (Target  : String;
      Path    : String;
      Runtime : String;
      Version : String)
   is
      Cur  : Build_Target_Access := Build_Targets;
      Last : Build_Target_Access := null;
   begin
      if Build_Targets = null then
         Build_Targets := new Build_Target'
           (Target  => new String'(Target),
            Path    => new String'(Path),
            Runtime => new String'(Runtime),
            Version => new String'(Version),
            Next    => null);
      else
         while Cur /= null loop
            if Cur.Target.all = Target
              and then Cur.Runtime.all = Runtime
            then
               return;
            end if;

            Last := Cur;
            Cur := Cur.Next;
         end loop;

         Last.Next := new Build_Target'
           (Target  => new String'(Target),
            Path    => new String'(Path),
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

   function Num_Targets return Natural is
      Cur  : Build_Target_Access := Build_Targets;
      Num  : Natural := 0;
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
            Targets (Index) := new String'(Get_Line (Iter));
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
      Target : Build_Target_Access := Build_Targets;
      File   : Ada.Text_IO.File_Type;
      Num    : Natural := 0;

   begin
      if Ada.Command_Line.Argument_Count > 0 then
         Ada.Text_IO.Create (File, Name => Ada.Command_Line.Argument (1));
      else
         Ada.Text_IO.Create (File, Name => "aunit.ini");
      end if;
      Ada.Text_IO.Put_Line (File, "[Settings]");
      Ada.Text_IO.Put_Line (File, "NumFields=" & Int_Image (Num_Targets));
      Ada.Text_IO.Put_Line (File, "Install=" & Gprconfig_Root);
      Ada.Text_IO.New_Line (File);

      while Target /= null loop
         Num := Num + 1;
         Ada.Text_IO.Put_Line (File, "[Field " & Int_Image (Num) & "]");
         Ada.Text_IO.Put_Line (File, "Name=" & Target.Target.all & " (" &
                               Target.Runtime.all & ")");
         Ada.Text_IO.Put_Line (File, "Target=" & Target.Target.all);
         Ada.Text_IO.Put_Line (File, "Runtime=" & Target.Runtime.all);
         Ada.Text_IO.Put_Line (File, "Version=" & Target.Version.all);
         --  We remove the trailing '\' as it is not understood by gprconfig
         Ada.Text_IO.Put_Line
           (File,
            "Path=" & Target.Path (Target.Path'First .. Target.Path'Last - 1));

         if Target.Runtime.all = "default" then
            Ada.Text_IO.Put_Line (File, "XRUNTIME=full");
         else
            Ada.Text_IO.Put_Line (File, "XRUNTIME=" & Target.Runtime.all);
         end if;

         if Target.Target.all = "pentium-mingw32msv" then
            Ada.Text_IO.Put_Line (File, "XPLATFORM=native");
         else
            Ada.Text_IO.Put_Line (File, "XPLATFORM=" & Target.Target.all);
         end if;

         Ada.Text_IO.New_Line (File);
         Target := Target.Next;
      end loop;
   end;
end Setup_Utility;
