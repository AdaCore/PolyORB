
--  This package is wrapped around the C++ class Ada_Giop_s declared in
--  Ada_Giop_s.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It does
--  not include an Init function since it is useless for
--  AdaBroker. (AdaBroker never creates a Giop_s object)

package body GIOP_S is

   -------------------------
   -- C_Reply_Header_Size --
   -------------------------

   function C_Reply_Header_Size return Interfaces.C.unsigned_long;
   pragma Import (CPP, C_Reply_Header_Size, "ReplyHeaderSize__10Ada_Giop_s");

   -----------------------
   -- Reply_Header_Size --
   -----------------------

   function Reply_Header_Size return CORBA.Unsigned_Long is
      C_Result : Interfaces.C.unsigned_long;
   begin
      --  Call the C function ...
      C_Result := C_Reply_Header_Size;

      --  Transform the result into an Ada type
      return CORBA.Unsigned_Long (C_Result);
   end Reply_Header_Size;

   ------------------------
   -- C_Request_Received --
   ------------------------

   procedure C_Request_Received
     (Self : in out Object'Class;
      Skip : in Sys_Dep.C_Boolean);
   pragma Import (CPP, C_Request_Received, "RequestReceived__10Ada_Giop_sb");
   --  Wrapper around Ada_Giop_s procedure RequestReceived (see
   --  Ada_Giop_s.hh) called by the Ada equivalent : Request_Received

   ----------------------
   -- Request_Received --
   ----------------------
   procedure Request_Received
     (Self : in out Object'Class;
      Skip : in Boolean := False)
   is
      C_Skip : Sys_Dep.C_Boolean;
   begin
      --  Transform the arguments into a C type ...
      C_Skip := Sys_Dep.Boolean_Ada_To_C (Skip);

      --  Call the C procedure
      C_Request_Received (Self, C_Skip);
   end Request_Received;

   ------------------------
   -- C_Initialise_Reply --
   ------------------------

   procedure C_Initialize_Reply
     (Self    : in out Object'Class;
      Status  : in Interfaces.C.int;
      MsgSize : in Interfaces.C.unsigned_long);

   pragma Import (CPP, C_Initialize_Reply, "InitialiseReply__10Ada_Giop_siUi");
   --  Wrapper around Ada_Giop_s procedure InitialiseReply (see
   --  Ada_Giop_s.hh) called by the Ada equivalent : Initialise_Reply

   ----------------------
   -- Initialize_Reply --
   ----------------------

   procedure Initialize_Reply
     (Self    : in out Object'Class;
      Status  : in GIOP.Reply_Status_Type;
      MsgSize : in CORBA.Unsigned_Long)
   is
      C_Status  : Interfaces.C.int;
      C_MsgSize : Interfaces.C.unsigned_long;
   begin
      --  Transform the arguments into a C type ...
      C_Status  := GIOP.Reply_Status_Type_To_C_Int (Status);
      C_MsgSize := Interfaces.C.unsigned_long (MsgSize);

      --  Call the C procedure
      C_Initialize_Reply (Self, C_Status, C_MsgSize);
   end Initialize_Reply;

end GIOP_S;






