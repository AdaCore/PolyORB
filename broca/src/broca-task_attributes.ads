with PortableServer;

pragma Elaborate_All (PortableServer);

package Broca.Task_Attributes is

   function Current_Object return PortableServer.ObjectId;
   pragma Inline (Current_Object);

   function Current_POA return PortableServer.POA_Forward.Ref;
   pragma Inline (Current_POA);

   procedure Set_Current_Object (Val : PortableServer.ObjectId);
   pragma Inline (Set_Current_Object);

   procedure Set_Current_POA (Val : PortableServer.POA_Forward.Ref);
   pragma Inline (Set_Current_POA);

end Broca.Task_Attributes;
