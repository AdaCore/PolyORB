------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     A D A B R O K E R . G I O P _ S                      --
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

--  This package is wrapped around the C++ class Ada_Giop_s declared in
--  Ada_Giop_s.hh.  It provides the same functions as this package plus the
--  Ada version of thouse where arguments types are to be change.  It does
--  not include an Init function since it is useless for
--  AdaBroker. (AdaBroker never creates a Giop_s object)

with Interfaces.C;
with Interfaces.CPP;

with CORBA;

with AdaBroker; use AdaBroker;
with AdaBroker.GIOP;
with AdaBroker.Sysdep;
with AdaBroker.NetBufferedStream;

package AdaBroker.GIOP_S is

   type Object is new NetBufferedStream.Object with record
      Table1 : Interfaces.CPP.Vtable_Ptr;
   end record;
   --  Table1 (Ada) : needed to interface C++ and Ada

   pragma Cpp_Class  (Object);
   pragma Cpp_Vtable (Object, Table1, 1);
   --  This type is both a C and an Ada class it is wrapped around
   --  Ada_Giop_s (see Ada_Giop_s.hh)


   type Object_Ptr is access all Object;
   --  Type pointer on type Object


   function Reply_Header_Size return CORBA.Unsigned_Long;
   --  Compute the size of the header for a reply


   procedure Request_Received
     (Self : in out Object'Class;
      Skip : in Boolean := False);
   --  Informs the ORB that the request was received (see giopDriver.h L150
   --  for more details)


   procedure Initialize_Reply
     (Self    : in out Object'Class;
      Status  : in GIOP.Reply_Status_Type;
      MsgSize : in CORBA.Unsigned_Long);
   --  Initialisation of a reply (see giopDriver.h L150 for more details)


   procedure Reply_Completed (Self : in out Object'Class);
   pragma Import (CPP, Reply_Completed, "ReplyCompleted__10Ada_Giop_s");
   --  Wrapper around Ada_Giop_s procedure InitialiseReply (see
   --  Ada_Giop_s.hh) it is both a C and an Ada procedure.  it informs the
   --  ORB that the reply was completed (see giopDriver.h L150 for more
   --  details)

private

   function Constructor return Object'Class;
   pragma Cpp_Constructor (Constructor);
   pragma Import (CPP, Constructor, "__10Ada_Giop_s");
   --  Default constructor of the C class. Actually, this constructor does
   --  nothing.

end AdaBroker.GIOP_S;
