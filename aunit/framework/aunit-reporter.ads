with AUnit.Test_Results; use AUnit.Test_Results;

package AUnit.Reporter is

   type Reporter is abstract tagged null record;

   procedure Report
     (Engine : Reporter;
      R      : in out Result) is abstract;

end AUnit.Reporter;
