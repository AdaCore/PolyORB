------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                 C O R B A . O B J E C T . O M N I O R B                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $
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

with Interfaces.C;

with CORBA.Object;

with AdaBroker.OmniORB;
with AdaBroker.MemBufferedStream;
with AdaBroker.NetBufferedStream;
with AdaBroker.GIOP_S;

package CORBA.Object.OmniORB is

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String;
   --  Return the IOR corresponding to Obj (see CORBA.ORB).

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Return a Ref'Class out of an IOR (see CORBA.ORB).

   type Dispatch_Procedure is access
     procedure (Self                  : AdaBroker.OmniORB.ImplObject_Ptr;
                Orls                  : in out AdaBroker.GIOP_S.Object;
                Orl_Op                : in Standard.String;
                Orl_Response_Expected : in CORBA.Boolean;
                Success               : out CORBA.Boolean);
   --  This type is made to handle dispatching calls from the ORB.

   function Resolve_Initial_References
     (Identifier : in CORBA.String)
     return CORBA.Object.Ref;

   ----------------------------
   -- Marshalling Operations --
   ----------------------------

   function Align_Size
     (Obj            : in Ref'Class;
      Initial_Offset : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Compute size needed to marshall Obj.

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out AdaBroker.NetBufferedStream.Object'Class);
   --  Marshall Obj into stream S.

   procedure Marshall
     (Obj : in Ref'Class;
      S   : in out AdaBroker.MemBufferedStream.Object'Class);
   --  Marshall Obj into stream S.

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out AdaBroker.NetBufferedStream.Object'Class);
   --  Unmarshall Obj from stream S.

   procedure Unmarshall
     (Obj : out Ref'Class;
      S   : in out AdaBroker.MemBufferedStream.Object'Class);
   --  Unmarshall Obj from stream S.


   ----------------------------------
   -- Interface Dynamic Conversion --
   ----------------------------------

   --  The CORBA.Object.Ref hierarchy is not equivalent to the CORBA
   --  interface hierarchy. So, each interface registers during its
   --  elaboration its repository id and its nil ref. These interfaces
   --  are registered using an index. Several operations allow to
   --  retrieve a repository, a nil ref and an index. Functions Is_A
   --  are dispatching operations. The nil ref will allow to invoke
   --  the overloaded operations Is_A corresponding to the derived
   --  Ref.
   --
   --  The operations are not thread-safe

   type Ref_Ptr is access all Ref'Class;

   procedure Register
     (The_Rep  : in CORBA.String;
      The_Ref  : in Ref'Class;
      Dispatch : in Dispatch_Procedure);
   --  Register a repository id, a nil ref and a dispatch procedure.

   --  Interfaces are registered in a table and we associate an index to
   --  each interface.

   function Rep_To_Id  (Self : CORBA.String) return Interfaces.C.int;
   --  Find interface index using the repository_id.

   function Ref_To_Id  (Self : Ref'Class) return Interfaces.C.int;
   --  Find interface index using the ref. If this ref is nil, then
   --  use tag to find interface index.

   function Tag_To_Id  (Self : Ref'Class) return Interfaces.C.int;
   --  Find interface index using the ref tag.

   function Id_To_Ref      (Self : Interfaces.C.int) return Ref_Ptr;
   function Id_To_Rep      (Self : Interfaces.C.int) return CORBA.String;
   function Id_To_Dispatch (Self : Interfaces.C.int) return Dispatch_Procedure;
   --  Return Ref, Rep or Dispatch of interface of index Self.

   function To_Ref
     (Self   : in AdaBroker.OmniORB.ImplObject'Class;
      RepoID : in CORBA.String)
     return CORBA.Object.Ref;

end CORBA.Object.OmniORB;
