------------------------------------------------------------------------------
--                                                                          --
--                        ADABROKER COMPONENTS                              --
--                                                                          --
--                     A D A B R O K E R . G I O P _ S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package is wrapped around the C++ class Ada_Giop_s declared in
--  Ada_Giop_s.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It does
--  not include an Init function since it is useless for
--  AdaBroker. (AdaBroker never creates a Giop_s object)

package body AdaBroker.GIOP_S is

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
      Skip : in Sysdep.Bool);
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
      C_Skip : Sysdep.Bool;
   begin
      --  Transform the arguments into a C type ...
      C_Skip := Sysdep.To_Bool (Skip);

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

end AdaBroker.GIOP_S;
