----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the Vehicle object                                         ----
----                                                                    ----
----                package Vehicle_impl                                ----
----                                                                    ----
----                authors : Sebastien Ponce                           ----
----                                                                    ----
----------------------------------------------------------------------------


package Vehicle.Impl is

   --------------------------------------------------
   ----                spec                      ----
   --------------------------------------------------

   type Object is new Corba.Implementation_Defined.Object with private;

   procedure Set_Mark (Self: in Ref ;
                       To: in Corba.String) ;

   function Get_Mark (Self: in Ref) return Corba.String ;

   procedure Drive (Self: in Ref;
                    Vehicle_Mark: in Corba.String) ;


   --------------------------------------------------
   ----              not in  spec                ----
   --------------------------------------------------

   procedure Init (Self : in out Object; K : in OmniORB.ObjectKey);
   -- initializer

   function Dispatch (Self : in Object;
                      Orls : in out Giop_S;
                      Orl_Op : in Corba.String;
                      Orl_Response_Expected : Corba.Boolean)
                      return Corba.Boolean;
   -- called by the ORB's dispatch function in omniObject.ads
   -- calls the function whose name is Orl_OP
   -- returns true on success, false on failure


private

   type Object is new Corba.Object.Object with record
      Mark: Corba.String ;
   end record ;

End Echo.Impl ;



