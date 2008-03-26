package AUnit.Memory.Utils is

   function Message_Alloc (Length : Natural) return Message_String;

   generic
      type Object (<>) is limited private;
      type Name is access Object;
   function Gen_Alloc return Name;

end AUnit.Memory.Utils;
