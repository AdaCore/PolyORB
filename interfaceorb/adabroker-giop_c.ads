------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     A D A B R O K E R . G I O P _ C                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

--  This package is wrapped around the C++ class Ada_GIOP_c declared in
--  Ada_GIOP_c.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It
--  includes the definition of the function RequestHeaderSize declared in
--  giopDriver.h but not present in Ada_GIOP_c since it is a static
--  function.

with Ada.Finalization;

with Interfaces.C;
with Interfaces.CPP;
with Interfaces.C.Strings;

with System;

with CORBA;

with AdaBroker.GIOP;
with AdaBroker.Key;
with AdaBroker.Sysdep;
with AdaBroker.Rope;
with AdaBroker.NetBufferedStream;

package AdaBroker.GIOP_C is

   type Object is new NetBufferedStream.Object with record
      Table1 : Interfaces.CPP.Vtable_Ptr;
   end record;
   --  Table1 (Ada) : needed to interface C++ and Ada

   pragma Cpp_Class (Object);
   pragma Cpp_Vtable (Object, Table1, 1);
   --  This type is both a C and an Ada class it is wrapped around
   --  Ada_GIOP_c (see Ada_GIOP_c.hh)


   type Controlled_Wrapper is
     new Ada.Finalization.Limited_Controlled with record
        Real : Object;
     end record;


   procedure Init
     (Self : in out Object'Class;
      R    : in Rope.Object);
   --  Ada constructor of the class.  This function must be called after
   --  each declaration of an Object object. If it is not, you can not use
   --  the object.

   procedure Free (Self : in out Object'Class);
   pragma Import (CPP, Free, "Free__10Ada_Giop_c");
   --  Deletes the underlying C pointer

   procedure Initialize_Request
     (Self       : in Object'Class;
      Objkey     : in Key.Object;
      Objkeysize : in CORBA.Unsigned_Long;
      Opname     : in CORBA.String;
      MsgSize    : in CORBA.Unsigned_Long;
      Oneway     : in CORBA.Boolean);
   --  Initialisation of a request (see giopDriver.h L150 for more details)


   procedure Receive_Reply
     (Self   : in out Object'Class;
      Result : out GIOP.Reply_Status_Type);
   --  Called to inform the ORD that the reply was received (see
   --  giopDriver.h L150 for more details)


   procedure Request_Completed
     (Self     : in Object'Class;
      Skip_Msg : in CORBA.Boolean := False);
   --  Called to inform the ORB that the request was completed (see
   --  giopDriver.h L150 for more details)


   function Request_Header_Size
     (Self       : in Object'Class;
      Objkeysize : in CORBA.Unsigned_Long;
      Opnamesize : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Returns the header size. This includes the size of the GIOP message
   --  header and the Request message header.


private

   procedure Finalize (Self : in out Controlled_Wrapper);
   --  Calls free on the underlying object

   function Constructor1 return Object'Class;
   pragma Cpp_Constructor (Constructor1);
   pragma Import (CPP, Constructor1, "__10Ada_Giop_c");
   --  Default constructor of the C class.  Actually, this constructor does
   --  nothing and you must call Init to init properly an object.

end AdaBroker.GIOP_C;
