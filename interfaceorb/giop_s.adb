-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_Giop_s declared in Ada_Giop_s.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It does not include an Init function since it is          ----
----   useless for AdaBroker. (AdaBroker never creates a Giop_s    ----
----   object)                                                     ----
----                                                               ----
----                  package body Giop_S                          ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;
with Ada.Exceptions ;

package body Giop_S is

   -- Request_Received
   -------------------
   procedure Request_Received (Self : in Object'Class ;
                               Skip : in Boolean) is
      C_Skip : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_Skip := Sys_Dep.Boolean_Ada_To_C (Skip) ;
      -- ... and calls the C procedure
      C_Request_Received (Self, C_Skip) ;
   end;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- Ada_To_C_Int
   ---------------
   function Ada_To_C_Int is
     new Ada.Unchecked_Conversion (Integer,
                                   Interfaces.C.Int) ;
   -- needed to change ada type Integer
   -- into C type Interfaces.C.Int


   -- Initialize_Reply
   -------------------
   procedure Initialize_Reply (Self : in Object'Class ;
                              Status : in Giop.Reply_Status_Type ;
                              MsgSize : in Corba.Unsigned_Long ) is
      C_Status : Interfaces.C.Int ;
      C_MsgSize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments into a C type ...
      C_Status := Ada_To_C_Int (Giop.Reply_Status_Type'Pos(Status)) ;
      C_MsgSize := Ada_To_C_Unsigned_Long (MsgSize) ;
      -- ... and calls the C procedure
      C_Initialize_Reply (Self, C_Status, C_MsgSize) ;
   end;

   -- C_Raise_Ada_Exception
   ------------------------
   procedure C_Raise_Ada_Exception (Self : in Object'Class ;
                                    Msg : in Interfaces.C.Strings.Chars_Ptr) is
      Ada_Msg : String := Interfaces.C.Strings.Value (Msg) ;
   begin
      -- argument already tranformed in a Ada type ...
      -- ... so calls the Ada procedure
      Raise_Ada_Exception (Self,Ada_Msg) ;
   end ;


   -- Raise_Ada_Exception
   ----------------------
   procedure Raise_Ada_Exception (Self : in Object'Class ;
                                  Msg : in String) is
   begin
      Ada.Exceptions.Raise_Exception (Corba.No_Initialisation_Error'Identity,Msg) ;
   end ;


end Giop_S ;






