with Interfaces.C;

with CORBA.Object;

with AdaBroker.OmniORB;
with AdaBroker.MemBufferedStream;
with AdaBroker.NetBufferedStream;

package CORBA.Object.OmniORB is

   function Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
      return CORBA.String;
   --  Return the IOR corresponding to Obj (see CORBA.ORB).

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Return a Ref'Class out of an IOR (see CORBA.ORB).

   --  function Resolve_Initial_References
   --    (Identifier : in CORBA.String)
   --    return CORBA.Object.Ref;

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
     (The_Rep : in CORBA.String;
      The_Ref : in Ref'Class);
   --  Register a repository id and a nil ref.

   --  Interfaces are registered in a table and we associate an index to
   --  each interface.

   function Rep_To_Id  (Self : CORBA.String) return Interfaces.C.int;
   --  Find interface index using the repository_id.

   function Ref_To_Id  (Self : Ref'Class) return Interfaces.C.int;
   --  Find interface index using the ref. If this ref is nil, then
   --  use tag to find interface index.

   function Tag_To_Id  (Self : Ref'Class) return Interfaces.C.int;
   --  Find interface index using the ref tag.

   function Id_To_Ref  (Self : Interfaces.C.int) return Ref_Ptr;
   function Id_To_Rep  (Self : Interfaces.C.int) return CORBA.String;
   --  Return Ref or Rep of interface of index Self.

   function To_Ref
     (Self   : in AdaBroker.OmniORB.ImplObject'Class;
      RepoID : in CORBA.String)
     return CORBA.Object.Ref;

end CORBA.Object.OmniORB;
