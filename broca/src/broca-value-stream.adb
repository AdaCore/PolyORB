------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--               B R O C A . V A L U E . V A L U E _ S T R E A M            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA.Value;
with CORBA.Impl;

with Broca.Exceptions;

with Broca.Opaque;
with Broca.Buffers;  use Broca.Buffers;

with Broca.CDR; use Broca.CDR;

package body Broca.Value.Stream is

   function CDR_Position (Buffer : access Buffer_Type)
                          return CORBA.Long;
   --  return the current CDR position of the buffer
   --  in the marshalling stream as a CORBA.Long. Raises
   --  CORBA::Marshall if it is too big to fit in a CORBA.Long.


   --------------------------------------
   --  end of subprogram declarations  --
   --------------------------------------


   -------------
   --  Match  --
   -------------
   function Match
     (Item : in Indirection_Element;
      Needle : in CORBA.AbstractBase.Ref)
      return Standard.Boolean is
      use CORBA.Impl;
   begin
      return
        (Item.Type_Id = Value_Ref
         and then
         CORBA.AbstractBase.Object_Of (Item.Ref)
         = CORBA.AbstractBase.Object_Of (Needle));
   end Match;


   --------------------
   --  CDR_Position  --
   --------------------
   function CDR_Position (Buffer : access Buffer_Type)
                          return CORBA.Long is
      Index : constant Broca.Opaque.Index_Type
        := Broca.Buffers.CDR_Position (Buffer);
      use Broca.Opaque;
   begin
      if Index >= 2**31 then
         Broca.Exceptions.Raise_Marshal;
      else
         return CORBA.Long (Index);
      end if;
   end CDR_Position;

   ---------------------------
   --  Marshall_Indirection --
   ---------------------------
   procedure Marshall_Indirection
     (Buffer : access Buffer_Type;
      Already_Marshalled : in out ISeq.Sequence;
      Val : in CORBA.Value.Base'Class;
      Success : out CORBA.Boolean) is
      Index : constant Natural
        := ISS_Ptr.Index (Already_Marshalled,
                          CORBA.AbstractBase.Ref (Val));
      Element : Indirection_Element;
   begin
      if Index = 0 then
         --  This is the first time this object is marshalled
         Element.Offset := CDR_Position (Buffer);
         Element.Type_Id := Value_Ref;
         Element.Ref := CORBA.AbstractBase.Ref (Val);
         ISeq.Append (Already_Marshalled, Element);
         Success := CORBA.Boolean (False);

      else
         --  This is *not* the first time this object is marshalled
         Marshall (Buffer, Indirection_Tag);
         Element := ISeq.Element_Of (Already_Marshalled, Index);
         Marshall (Buffer,
                   Element.Offset - CDR_Position (Buffer));
         Success := CORBA.Boolean (True);
      end if;
   end Marshall_Indirection;


   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (El : in out Indirection_Element) is
   begin
      case El.Type_Id is
         when Codebase_Url =>
            null;

         when Value_Ref =>
            null;

         when Repository_Id =>
            Free (El.RepoId);

         when Repository_Id_List =>
            Free (El.RepoId_List);
      end case;
   end Finalize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class) is
      Already_Marshalled : ISeq.Sequence
        := ISeq.Null_Sequence;
   begin
      Marshall
        (Buffer,
         Data,
         Already_Marshalled);
   end Marshall;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Value.Base'Class;
      Already_Marshalled : in out ISeq.Sequence) is
   begin
      if CORBA.Value.Is_Nil (Data) then
         Marshall (Buffer, Null_Tag);
      else
         declare
            Success : Boolean;
         begin
            Marshall_Indirection
              (Buffer,
               Already_Marshalled,
               Data,
               Success);
            if not Success then
               declare
                  Operation : Marshall_Type;
               begin
                  Operation := Marshall_Store.Get_Operation
                    (CORBA.Value.Object_Of (Data).all'Tag);
                  Operation (Buffer,
                             CORBA.Value.Object_Of (Data),
                             Already_Marshalled);
               end;
            end if;
         end;
      end if;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data   : out CORBA.Value.Base'Class) is
      Already_Marshalled : ISeq.Sequence
        := ISeq.Null_Sequence;
   begin
      Unmarshall
        (Buffer,
         Data,
         Already_Marshalled);
   end Unmarshall;

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data : out CORBA.Value.Base'Class;
      Already_Unmarshalled : in out ISeq.Sequence) is
      Value_Tag : constant CORBA.Long
        := Unmarshall (Buffer);
      use CORBA;
      Null_Ptr : CORBA.Impl.Object_Ptr := null;
   begin
      if Value_Tag = Null_Tag then
         CORBA.Value.Set (CORBA.Value.Base (Data), Null_Ptr);
      elsif Value_Tag = Indirection_Tag then
         --  handle indirection NIY
         raise Program_Error;
      else
         --  FIXME suppose tag is OK with no type info
         declare
            Operation : Unmarshall_Fields_Type
              := Unmarshall_Fields_Store.Get_Operation (Data'Tag);
            --  should be repositoryId
         begin
            --  initialize the value in case it was not done
            CORBA.Value.Set (CORBA.Value.Base (Data), Null_Ptr);
            Operation (Buffer,
                       Data,
                       Already_Unmarshalled);
         end;
      end if;
   end Unmarshall;

end Broca.Value.Stream;





