------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.OPERATIONDEF.IMPL                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;

package CORBA.Repository_Root.OperationDef.Impl is

   type Object is
     new CORBA.Repository_Root.Contained.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Result_Def : CORBA.Repository_Root.IDLType.Ref;
                   Params : CORBA.Repository_Root.ParDescriptionSeq;
                   Mode : CORBA.Repository_Root.OperationMode;
                   Contexts : CORBA.Repository_Root.ContextIdSeq;
                   Exceptions : CORBA.Repository_Root.ExceptionDefSeq);

   function get_result
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_result_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_result_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref);

   function get_params
     (Self : access Object)
     return CORBA.Repository_Root.ParDescriptionSeq;

   procedure set_params
     (Self : access Object;
      To : CORBA.Repository_Root.ParDescriptionSeq);

   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.OperationMode;

   procedure set_mode
     (Self : access Object;
      To : CORBA.Repository_Root.OperationMode);

   function get_contexts
     (Self : access Object)
     return CORBA.Repository_Root.ContextIdSeq;

   procedure set_contexts
     (Self : access Object;
      To : CORBA.Repository_Root.ContextIdSeq);

   function get_exceptions
     (Self : access Object)
     return CORBA.Repository_Root.ExceptionDefSeq;

   procedure set_exceptions
     (Self : access Object;
      To : CORBA.Repository_Root.ExceptionDefSeq);

   --  override this from contained
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

private

   type Object is new CORBA.Repository_Root.Contained.Impl.Object with record
      --  the Result is the type of the result_def
      Result_Def : CORBA.Repository_Root.IDLType.Ref;
      Params : CORBA.Repository_Root.ParDescriptionSeq;
      Mode : CORBA.Repository_Root.OperationMode;
      Contexts : CORBA.Repository_Root.ContextIdSeq;
      Exceptions : CORBA.Repository_Root.ExceptionDefSeq;
   end record;

end CORBA.Repository_Root.OperationDef.Impl;
