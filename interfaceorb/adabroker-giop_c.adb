------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     A D A B R O K E R . G I O P _ C                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
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

--  This package is wrapped around the C++ class Ada_GIOP_s declared in
--  Ada_GIOP_s.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It does
--  not include an Init function since it is useless for
--  AdaBroker. (AdaBroker never creates a GIOP_s object)

with Ada.Strings.Unbounded;

with AdaBroker; use AdaBroker;
with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

with CORBA;

package body AdaBroker.GIOP_C is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("giop_c");
   procedure O is new AdaBroker.Debug.Output (Flag);

   use type CORBA.Unsigned_Long;

   ------------
   -- C_Init --
   ------------

   procedure C_Init
     (Self : in out Object'Class;
      R    : in System.Address);

   pragma Import (CPP, C_Init, "Init__10Ada_Giop_cP4Rope");
   --  Wrapper around Ada_Giop_c procedure Init (see Ada_Giop_c.hh) called
   --  by the Ada equivalent : Init

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : in out Object'Class;
      R    : in Rope.Object)
   is
   begin
      pragma Debug (O ("--- GIOP_C.Init ---"));
      pragma Debug
        (O ("init_Ok = " &
            Boolean'Image (Sysdep.To_Boolean (Self.Init_Ok))));
      --  Just calls the C procedure
      C_Init (Self, System.Address (R));
   end Init;

   --------------------------
   -- C_Initialize_Request --
   --------------------------

   procedure C_Initialize_Request
     (Self       : in Object'Class;
      Objkey     : in Key.Object;
      Objkeysize : in Interfaces.C.unsigned_long;
      Opname     : in Interfaces.C.Strings.chars_ptr;
      Opnamesize : in Interfaces.C.unsigned_long;
      MsgSize    : in Interfaces.C.unsigned_long;
      Oneway     : in Sysdep.Bool);
   pragma Import
     (CPP, C_Initialize_Request,
      "InitialiseRequest__10Ada_Giop_cPCvUiPCcUiUib");
   --  Wrapper around Ada_Giop_c procedure Initialize_Request (see
   --  Ada_Giop_c.hh) called by the Ada equivalent : Initialize_Request

   ------------------------
   -- Initialize_Request --
   ------------------------

   procedure Initialize_Request
     (Self       : in Object'Class;
      Objkey     : in Key.Object;
      Objkeysize : in CORBA.Unsigned_Long;
      Opname     : in CORBA.String;
      MsgSize    : in CORBA.Unsigned_Long;
      Oneway     : in CORBA.Boolean)
   is

      use Ada.Strings.Unbounded;

      C_Objkeysize : Interfaces.C.unsigned_long;
      Ada_Opname   : String := To_String (Unbounded_String (Opname));
      C_Opname     : Interfaces.C.Strings.chars_ptr;
      C_OpnameSize : Interfaces.C.unsigned_long;
      C_MsgSize    : Interfaces.C.unsigned_long;
      C_Oneway     : Sysdep.Bool;
   begin
      --  Transform the arguments into a C type ...
      C_Objkeysize := Interfaces.C.unsigned_long (Objkeysize);
      C_Opname     := Interfaces.C.Strings.New_String (Ada_Opname);

      --  Desallocation in a few lines
      C_OpnameSize := Interfaces.C.unsigned_long
        (Ada_Opname'Length + CORBA.Unsigned_Long (1));
      C_MsgSize    := Interfaces.C.unsigned_long (MsgSize);
      C_Oneway     := Sysdep.To_Bool (Oneway);

      --  Call the C procedure
      C_Initialize_Request
        (Self, Objkey, C_Objkeysize, C_Opname,
         C_OpnameSize, C_MsgSize, C_Oneway);

      --  Desallocation of C_Opname
      Interfaces.C.Strings.Free (C_Opname);
   end Initialize_Request;

   ---------------------
   -- C_Receive_Reply --
   ---------------------

   procedure C_Receive_Reply
     (Self   : in out Object'Class;
      Result : out Interfaces.C.int);

   pragma Import
     (CPP, C_Receive_Reply,
      "ReceiveReply__10Ada_Giop_cRQ24GIOP15ReplyStatusType");
   --  Wrapper around Ada_Giop_c procedure Receive_Reply (see
   --  Ada_Giop_c.hh) called by the Ada equivalent : Receive_Reply

   -------------------
   -- Receive_Reply --
   -------------------

   procedure Receive_Reply
     (Self   : in out Object'Class;
      Result : out GIOP.Reply_Status_Type)
   is
      C_Result : Interfaces.C.int;
   begin
      --  Call the C function ...
      pragma Debug (O ("GIOP_c.Receive_Reply : call of the C_function"));

      C_Receive_Reply (Self, C_Result);

      pragma Debug (O ("GIOP_c.Receive_Reply : C_function successfull"));
      --  Transform the result into an Ada type

      Result := GIOP.C_Int_To_Reply_Status_Type (C_Result);
   end Receive_Reply;

   -------------------------
   -- C_Request_Completed --
   -------------------------

   procedure C_Request_Completed
     (Self     : in Object'Class;
      Skip_Msg : in Sysdep.Bool);

   pragma Import (CPP, C_Request_Completed, "RequestCompleted__10Ada_Giop_cb");
   --  Wrapper around Ada_Giop_c procedure Request_Completed (see
   --  Ada_Giop_c.hh) called by the Ada equivalent : Request_Completed

   -----------------------
   -- Request_Completed --
   -----------------------

   procedure Request_Completed
     (Self     : in Object'Class;
      Skip_Msg : in CORBA.Boolean := False)
   is
      C_Skip_Msg : Sysdep.Bool;
   begin
      --  Transform the arguments into a C type ...
      C_Skip_Msg := Sysdep.To_Bool (Skip_Msg);

      --  Call the C procedure
      C_Request_Completed (Self, C_Skip_Msg);
   end Request_Completed;

   ---------------------------
   -- C_Request_Header_Size --
   ---------------------------

   function C_Request_Header_Size
     (Self       : in Object'Class;
      Objkeysize : in Interfaces.C.unsigned_long;
      Opnamesize : in Interfaces.C.unsigned_long)
      return Interfaces.C.unsigned_long;

   pragma Import
     (CPP, C_Request_Header_Size, "RequestHeaderSize__10Ada_Giop_cUiUi");
   --  Wrapper around Ada_Giop_c procedure Request_Header_Size (see
   --  Ada_Giop_c.hh) called by the Ada equivalent : Request_Header_Size

   -------------------------
   -- Request_Header_Size --
   -------------------------

   function Request_Header_Size
     (Self       : in Object'Class;
      Objkeysize : in CORBA.Unsigned_Long;
      Opnamesize : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      C_Objkeysize : Interfaces.C.unsigned_long;
      C_Opnamesize : Interfaces.C.unsigned_long;
      C_Result     : Interfaces.C.unsigned_long;
   begin
      --  Transform the arguments into a C type ...
      C_Objkeysize := Interfaces.C.unsigned_long (Objkeysize);
      C_Opnamesize := Interfaces.C.unsigned_long (Opnamesize + 1);

      --  The "+1" is because the length of a C string is a character
      --  longer than an Ada one, with the \O at the end ... calls the C
      --  procedure ...
      C_Result := C_Request_Header_Size (Self, C_Objkeysize, C_Opnamesize);

      --  Transform the result into an Ada type
      return CORBA.Unsigned_Long (C_Result);
   end Request_Header_Size;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self : in out Controlled_Wrapper) is
   begin
      Free (Self.Real);
   end Finalize;

end AdaBroker.GIOP_C;

