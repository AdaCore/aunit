package AUnit.Test_Info is

   type Tested_Location is record
      Tested_File   : Message_String;
      Tested_Line   : Natural;
      Tested_Column : Natural;
      Tested_Name : Message_String;
   end record;
   --  Description of the sloc of a test.

   type Tested_Location_Access is access all Tested_Location;

   type Test_Suffix;
   type Test_Suffix_Access is access Test_Suffix;

   type Test_Suffix is record
      Suffix_Text       : Message_String;
      Suffix_Location   : Tested_Location_Access;
      Additional_Suffix : Test_Suffix_Access;
   end record;
end AUnit.Test_Info;
