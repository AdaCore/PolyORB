------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.74 $
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

with Ada.Finalization;

with AdaBroker;
with AdaBroker.OmniORB;
with CORBA.Context;
with CORBA.NVList;
with CORBA.Request;


package CORBA.Object is

   type Ref is tagged private;

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean renames Is_Nil;

   procedure Release (Self : in out Ref);

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean;
   --  Return True if Self type is Logical_Type_Id (a Repository_Id)
   --  or one of its descendants.

   function Non_Existent
     (Self : in Ref)
      return CORBA.Boolean;
   --  Return True when the implementation referenced by this proxy
   --  object does not exist.

   function Is_Equivalent
     (Self  : in Ref;
      Other : in Ref)
      return CORBA.Boolean;
   --  Return True when both objects point to the same distant
   --  implementation.

   function Hash
     (Self    : in Ref;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Return a hash value for object. Not implemented yet.

   function Get_Implementation
     (Self : in Ref'Class)
      return AdaBroker.OmniORB.OmniObject_Ptr;

   Nil_Ref : constant Ref;


   procedure Set_Implementation
     (Self : in out Ref'Class;
      Imp  : in     AdaBroker.OmniORB.OmniObject_Ptr);
   --  used as a work around to build a Ref from an OmnoObject_Ptr
   --  in AdaBroker.OmniProxyCallWrapperDyn_Invoke


   -----------------------------
   --  DII related functions  --
   -----------------------------

   function To_Any (From : in CORBA.Object.Ref) return CORBA.Any;

   function From_Any (From : in CORBA.Any) return CORBA.Object.Ref;


   --  the 2 following functions will be implemented later

   --   function Get_Implementation
   --  deprecated,  will be removed in CORBA 2.2
   --     (Self : in Ref)
   --      return CORBA.ImplementationDef.Ref;

   --   function Get_Interface
   --     (Self : in Ref)
   --      return CORBA.InterfaceDef.Ref;


   procedure Create_Request
   --  the only way for a client to build dynamically a request
   --  he may use Add_Arg method (corba-request) after that
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Object;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Object;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags;
      Returns   :    out Status);


private

   type Ref is new Ada.Finalization.Controlled with record
      OmniObj : AdaBroker.OmniORB.OmniObject_Ptr := null;
   end record;

   Repository_Id : CORBA.String
     := CORBA.To_CORBA_String ("IDL:omg.org/CORBA/Object:1.0");

   procedure Initialize (Self : in out Ref);
   --  Set OmniObject to null.

   procedure Adjust (Self : in out Ref);
   --  Duplicate underlying OmniObject

   procedure Finalize (Self : in out Ref);
   --  Release underlying OmniObject

   Nil_Ref : constant Ref
     := (Ada.Finalization.Controlled with OmniObj => null);

   -----------
   --  DII  --
   -----------

   type Content_Object_Ref is new Content with
      record
         Value : CORBA.Object.Ref;
      end record;
   type Content_Object_Ref_Ptr is access all Content_Object_Ref;

end CORBA.Object;
