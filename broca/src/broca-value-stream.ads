------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   B R O C A . V A L U E . S T R E A M                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with CORBA;  use CORBA;
with CORBA.AbstractBase;
with CORBA.Value;
with CORBA.Impl;

with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);
with Sequences.Unbounded.Search;
pragma Elaborate_All (Sequences.Unbounded.Search);

with Broca.Buffers;  use Broca.Buffers;

with Broca.Operation_Store;
pragma Elaborate_All (Broca.Operation_Store);

with Broca.Value.Operation_Store;
pragma Elaborate_All (Broca.Value.Operation_Store);

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Broca.Value.Stream is

   --  Definitions to handle indirections in the GIOP stream  --
   type Indirected_Type is (Codebase_Url,
                            Repository_Id,
                            Repository_Id_List,
                            Value_Ref);

   --  a sequence of repository Ids
   package RepositoryId_Sequence is
     new Sequences.Unbounded (CORBA.RepositoryId);
   package RS renames RepositoryId_Sequence;

   type RS_Ptr is access RS.Sequence;
   procedure Free is new Ada.Unchecked_Deallocation
     (RS.Sequence, RS_Ptr);

   type RepoId_Ptr is access CORBA.RepositoryId;
   procedure Free is new Ada.Unchecked_Deallocation
     (CORBA.RepositoryId, RepoId_Ptr);

   type Indirection_Element is new Ada.Finalization.Controlled
     with record
        Offset : CORBA.Long := 0;
        Type_Id : Indirected_Type;
        RepoId : RepoId_Ptr := null;
        Ref : CORBA.AbstractBase.Ref := CORBA.AbstractBase.Nil_Ref;
        RepoId_List : RS_Ptr := null;
     end record;
   --  This type has to be constrained to be stored in a sequence.
   --  This is why it is controlled and not discriminated.
   --  Valuetypes are stored as abstractbase.Ref : They have to
   --  be stored as CORBA.Impl.Object_Ptr, but storing them
   --  as abstractbase.Ref keeps a sane reference counting

   procedure Finalize (El : in out Indirection_Element);

   function Match (Item : in Indirection_Element;
                   Needle : in CORBA.AbstractBase.Ref) return Standard.Boolean;
   --  A match is found when the Object_Ptr is the same, ie when
   --  the two refs reference the same object.

   function Match (Item : in Indirection_Element;
                   Needle : in CORBA.Long) return Standard.Boolean;

   package Indirection_Seq is new Sequences.Unbounded (Indirection_Element);
   package ISeq renames Indirection_Seq;

   package ISS_Ptr is new ISeq.Search (CORBA.AbstractBase.Ref, Match);
   package ISS_Offset is new ISeq.Search (CORBA.Long, Match);
   --  Store for Marshall operation
   --------------------------------
   type Marshall_Type is access
     procedure (Buffer : access Buffer_Type;
                Val    : in CORBA.Impl.Object_Ptr;
                Already_Marshalled : in out ISeq.Sequence;
                Formal : in CORBA.RepositoryId;
                Only_Members : in Boolean;
                Nesting_Depth : in CORBA.Long);

   package Marshall_Store is
      new Broca.Value.Operation_Store (Marshall_Type);
   --  registered with Impl.Object'tag

   --  Store for Unmarshall operation
   ----------------------------------
   type Unmarshall_Fields_Type is access procedure
     (Buffer : access Buffer_Type;
      Result : in out CORBA.Value.Base'Class;
      Already_Unmarshalled : in out ISeq.Sequence;
      With_Chunking : in Boolean;
      Nesting_Depth : in CORBA.Long;
      Closing_Tag_Read : out CORBA.Long);

   package Unmarshall_Fields_Store is
     new Broca.Operation_Store (Unmarshall_Fields_Type);
   --  registered with ref tag

   --  Utility subprograms
   -----------------------
   procedure Marshall_Indirection
     (Buffer : access Buffer_Type;
      Already_Marshalled : in out ISeq.Sequence;
      Val : in CORBA.Value.Base'Class;
      Success : out Boolean);
   --  Test if this object was previously marshalled in the buffer.
   --  If true, then marshall an indirection to it and return true.
   --  Otherwise, add it to the Already_Marshalled sequence and return false.

   Indirection_Tag : constant CORBA.Long
     := CORBA.Long (-1);

   Null_Tag : constant CORBA.Long
     := CORBA.Long (0);

   Default_Value_Tag : constant CORBA.Long
     := CORBA.Long (16#7FFFFF00#);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class;
      Formal : in CORBA.RepositoryId;
      Already_Marshalled : in out ISeq.Sequence;
      Nesting_Depth : in CORBA.Long);


   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Formal : in CORBA.RepositoryId;
      Result   : in out CORBA.Value.Base'Class);


   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Formal : in CORBA.RepositoryId;
      Result : in out CORBA.Value.Base'Class;
      Already_Unmarshalled : in out ISeq.Sequence;
      With_Chunking : in Boolean;
      Nesting_Depth : in CORBA.Long;
      Closing_Tag_Read : out CORBA.Long);

   procedure Append
     (Buffer : access Buffer_Type;
      Already_Unmarshalled : in out ISeq.Sequence;
      Data : in CORBA.Value.Base'Class);
   --  Appends a freshly unmarshalled object to the
   --  Already_Unmarshalled list

end Broca.Value.Stream;
