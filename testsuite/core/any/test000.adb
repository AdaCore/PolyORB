with PolyORB.Any;
with PolyORB.Initialization;
with PolyORB.Types;
with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Test000 is

   use PolyORB.Any;
   use PolyORB.Utils.Report;
   use PolyORB.Types;

   procedure Simple_Test;

   -----------------
   -- Simple_Test --
   -----------------

   procedure Simple_Test
   is
      A : Any;

      Initial_Value : constant PolyORB.Types.Short
        := PolyORB.Types.Short (2);

      Value : PolyORB.Types.Short;
   begin
      A := To_Any (PolyORB.Types.Short (2));
      Value := From_Any (A);
      Output ("Any: Short", Value = Initial_Value);
   end Simple_Test;

begin
   PolyORB.Initialization.Initialize_World;
   Simple_Test;
   End_Report;
end Test000;
