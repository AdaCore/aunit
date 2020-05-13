with Ada.Finalization;

package AUnit.Reporter.TRX is

   type Reporter_Generator is new Ada.Finalization.Controlled with null record;

   function Get_Reporter (Limited_Reporter : Reporter_Generator) return Reporter'Class;

private

   type TRX_Reporter is new AUnit.Reporter.Reporter with null record;

   overriding
   procedure Report
     (Engine  : TRX_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options);

   overriding
   procedure Initialize(Object: in out Reporter_Generator);

   overriding
   procedure Finalize(Object: in out Reporter_Generator);

end AUnit.Reporter.TRX;
