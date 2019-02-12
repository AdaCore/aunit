with Ada.Text_IO;

--  jenkins-junit.xsd compatible reporter to file.
package AUnit.Reporter.JUnit is
   
   type JUnit_Reporter is new Reporter with
      record
         File : Ada.Text_IO.File_Access := Ada.Text_IO.Standard_Output;
      end record;
   
   procedure Report (Engine  : JUnit_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options);

end AUnit.Reporter.JUnit;
