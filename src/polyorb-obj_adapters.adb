package body PolyORB.Obj_Adapters is

   -------------
   -- Set_ORB --
   -------------

   procedure Set_ORB
     (OA      : access Obj_Adapter;
      The_ORB :        PolyORB.Components.Component_Access)
   is
   begin
      OA.ORB := The_ORB;
   end Set_ORB;

end PolyORB.Obj_Adapters;
