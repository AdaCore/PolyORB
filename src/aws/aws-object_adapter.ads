with PolyORB.Errors;
with PolyORB.POA_Types;
--  with PolyORB.POA;

package AWS.Object_Adapter is

   use PolyORB.POA_Types;

   type AWS_AdapterActivator is new PolyORB.POA_Types.AdapterActivator with
     null record;

   procedure Unknown_Adapter
     (Self   : access AWS_AdapterActivator;
      Parent : access Obj_Adapter'Class;
      Name   : in     String;
      Result :    out Boolean;
      Error  : in out PolyORB.Errors.Error_Container);
   --  Called by the POA when no appropriate child POA can be found

end AWS.Object_Adapter;
