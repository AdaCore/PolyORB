with PolyORB.Servants;
with PolyORB.Components;
with PolyORB.Types;

package Test_Servant is

   type My_Servant is new PolyORB.Servants.Servant with
     record
        Nb   : Integer;
        Name : PolyORB.Types.String;
     end record;
   type My_Servant_Access is access all My_Servant;

   function Execute_Servant
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)

     return PolyORB.Components.Message'Class;

   function "=" (Left, Right : My_Servant)
     return Standard.Boolean;

end Test_Servant;
