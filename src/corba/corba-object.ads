------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.References;

with CORBA.AbstractBase;
with CORBA.Context;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.NVList;
with CORBA.Request;

package CORBA.Object is

   type Ref is new CORBA.AbstractBase.Ref with private;

   function Get_Interface (Self : Ref) return CORBA.Object.Ref'Class;

   overriding function Is_Nil  (Self : Ref) return CORBA.Boolean;
   overriding function Is_Null (Self : Ref) return CORBA.Boolean
                renames Is_Nil;

   procedure Duplicate (Self : in out Ref);

   overriding procedure Release (Self : in out Ref);

   function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean;

   function Non_Existent (Self : Ref) return CORBA.Boolean;

   function Is_Equivalent
     (Self         : Ref;
      Other_Object : Ref'Class) return Boolean;

   procedure Create_Request
     (Self      : Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags);
   --  Implementation Note: the CORBA specifications define one
   --  possible value for Req_Flags: CORBA::OUT_LIST_MEMORY, which is
   --  currently not supported. The only possible value for
   --  Req_Flags is 0, all other values will be ignored for now.

   procedure Create_Request
     (Self      : Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : ExceptionList.Ref;
      Ctxt_List : ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags);
   --  Implementation Notes:
   --  #1: see above
   --
   --  #2: this procedure implements the recommendation detailed in
   --  the OMG issue #3706, that add new primitives to
   --  CORBA::Object. It adds the Exc_List and Ctxt_List parameters,
   --  to provide more control on the request created.

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

   --  Implementation Note: The following policy management related
   --  Object operations were moved into child package
   --  CORBA.Object.Policies to avoid circular dependency.
   --
   --     function Get_Policy
   --       (Self        : Ref;
   --        Policy_Type : PolicyType)
   --       return CORBA.Policy.Ref;

   --     function Get_Domain_Managers
   --       (Self : Ref)
   --       return CORBA.DomainManager.DomainManagerList;

   --     procedure Set_Policy_Overrides
   --       (Self     : Ref;
   --        Policies : CORBA.Policy.PolicyList;
   --        Set_Add  : SetOverrideType);

   --     function Get_Client_Policy
   --       (Self     : Ref;
   --        The_Type : PolicyType)
   --       return CORBA.Policy.Ref;

   --     function Get_Policy_Overrides
   --       (Self  : Ref;
   --        Types : CORBA.Policy.PolicyTypeSeq)
   --       return CORBA.Policy.PolicyList;

   --     procedure Validate_Connection
   --       (Self                  : Ref;
   --        Inconsistent_Policies :    out CORBA.Policy.PolicyList;
   --        Result                :    out Boolean);

   function TC_Object return CORBA.TypeCode.Object;

   function  Object_To_String
     (Obj : CORBA.Object.Ref'Class) return CORBA.String;

   package Internals is

      --  Implementation Note: This package defines internal subprograms
      --  specific to PolyORB. You must not use them.

      function To_PolyORB_Ref (R : Ref) return PolyORB.References.Ref;
      function To_CORBA_Ref (R : PolyORB.References.Ref) return Ref;
      --  Conversion functions between CORBA and neutral references.

   end Internals;

private

   type Ref is new CORBA.AbstractBase.Ref with null record;

   pragma Inline (Object_To_String);

end CORBA.Object;
