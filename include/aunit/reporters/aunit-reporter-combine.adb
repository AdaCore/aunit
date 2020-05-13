
package body AUnit.Reporter.Combine is

   procedure Report
     (Engine  : Combined_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options) is

      procedure Run_Reporter(Position : Reporter_Vecs.Cursor) is
         Element : constant Reporter_Access := Reporter_Vecs.Element (Position);
      begin
         AUnit.Reporter.Report (Element.all, R, Options);
      end Run_Reporter;
   begin
      Reporter_Vecs.Iterate (Engine.Reporters, Run_Reporter'Access);
   end Report;

   procedure Add_Reporter (C : in out Combined_Reporter; R : access constant Reporter'Class) is
   begin
      C.Reporters.Append(R);
   end Add_Reporter;

end AUnit.Reporter.Combine;
