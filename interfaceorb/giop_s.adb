-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----             package body giop_s                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/22/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;

package body Giop_S is

   -- Request_Received
   -------------------
   procedure Request_Received (Self : in Object) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Giop_S.Request_Received") ;
   end ;


   -- Initialize_Reply
   -------------------
   procedure Initialize_Reply (Self : in Object ;
                              Status : in GIOP.ReplyStatusType ;
                              MsgSize : Integer ) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Giop_S.Initialize_Reply") ;
   end ;


   -- Reply_Completed
   ------------------
   procedure Reply_Completed (Self : in Object) is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Giop_S.Reply_Completed") ;
   end ;


end Giop_S ;

