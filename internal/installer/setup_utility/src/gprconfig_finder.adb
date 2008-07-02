--
--  Copyright (C) 2008, AdaCore
--

with System.OS_Lib; use System.OS_Lib;
with GNAT_Registry; use GNAT_Registry;

package body Gprconfig_Finder is

   procedure For_Key_Action
     (Key     : HKEY;
      Sub_Key : String;
      Quit    : in out Boolean);

   procedure Loop_Keys is new For_Every_Key (For_Key_Action);
   --  Lookup every key value

   Gprconfig_Root : String_Access;

   --------------------
   -- For_Key_Action --
   --------------------

   procedure For_Key_Action
     (Key     : HKEY;
      Sub_Key : String;
      Quit    : in out Boolean)
   is
      procedure For_Value_Action
        (Index   : Positive;
         Sub_Key : String;
         Value   : String;
         Quit    : in out Boolean);
      --  Iterator
      procedure Loop_Key_Values is new For_Every_Key_Value (For_Value_Action);

      Sub_Hkey : HKEY;

      ----------------------
      -- For_Value_Action --
      ----------------------

      procedure For_Value_Action
        (Index   : Positive;
         Sub_Key : String;
         Value   : String;
         Quit    : in out Boolean) is
      begin
         if Sub_Key = "ROOT"
           and then Is_Regular_File (Value & "\bin\gprconfig.exe")
         then
            Gprconfig_Root := new String'(Value & "\");
            Quit := True;
         end if;
      end For_Value_Action;

   begin
      Sub_Hkey := Open_Key (Key, Sub_Key);
      Loop_Key_Values (Sub_Hkey);
      Close_Key (Sub_Hkey);
   end For_Key_Action;

   -------------------------
   -- Find_Gprconfig_Root --
   -------------------------

   function Find_Gprconfig_Root return String is
      Gprconfig_Cmd  : String_Access;
      Key            : HKEY;
   begin
      Gprconfig_Cmd := Locate_Exec_On_Path ("gprconfig");
      if Gprconfig_Cmd /= null then
         for Last in reverse Gprconfig_Cmd'Range loop
            if Gprconfig_Cmd (Last) = '\' then
               Gprconfig_Root := new String'
                 (Normalize_Pathname
                    (Gprconfig_Cmd (Gprconfig_Cmd'First .. Last) & "..\") &
                  "\");
               exit;
            end if;
         end loop;

         Free (Gprconfig_Cmd);
      else
         Key := Open_Key
           (HKEY_LOCAL_MACHINE,
            "Software\Ada Core Technologies");
         Loop_Keys (Key, True);
         Close_Key (Key);
      end if;

      if Gprconfig_Root = null then
         return "";
      else
         declare
            The_Str : constant String := Gprconfig_Root.all;
         begin
            Free (Gprconfig_Root);
            return The_Str;
         end;
      end if;
   end Find_Gprconfig_Root;

end Gprconfig_Finder;
