with PolyORB;

package body MOMA.Connection_Factories.Topics is

   ------------
   -- Create --
   ------------

   procedure Create (Self     : out Connection_Factory_Topic;
                     Remote   : PolyORB.References.Ref)
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   end Create;

   -----------------------
   -- Create_Connection --
   -----------------------

   function Create_Connection (Self   : Connection_Factory_Topic)
                    return MOMA.Connections.Connection'Class
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Connection (Self);
      pragma Warnings (On);
   end Create_Connection;


   function Create_Connection (Self      : Connection_Factory_Topic;
                    Username  : String;
                    Password  : String)
                    return MOMA.Connections.Connection'Class
   is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Connection (Self, Username, Password);
      pragma Warnings (On);
   end Create_Connection;

end MOMA.Connection_Factories.Topics;

