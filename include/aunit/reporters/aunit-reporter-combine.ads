with Ada.Containers.Vectors;

package AUnit.Reporter.Combine is

   type Combined_Reporter is new Reporter with private;

   overriding
   procedure Report
     (Engine  : Combined_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options);

   procedure Add_Reporter (C : in out Combined_Reporter; R : access constant Reporter'Class);

private

   type Reporter_Access is access constant Reporter'Class;

   package Reporter_Vecs is new Ada.Containers.Vectors
       (Element_Type => Reporter_Access,
        Index_Type => Positive);

   type Combined_Reporter is new Reporter with record
      Reporters : Reporter_Vecs.Vector;
   end record;
end AUnit.Reporter.Combine;
