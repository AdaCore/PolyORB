-----------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.65 $
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

--  This package corresponds to the CORBA 2.0 specification. It contains
--  the definition of type CORBA.Object.Ref, which is the base class of all
--  proxy objects

with Ada.Exceptions;

with AdaBroker.OmniORB;        use AdaBroker.OmniORB;
with AdaBroker.OmniRopeAndKey; use AdaBroker.OmniRopeAndKey;

with AdaBroker.Debug;
pragma Elaborate_All (AdaBroker.Debug);

with CORBA;
pragma Elaborate_All (CORBA);

package body CORBA.Object is

   Flag : constant Natural := AdaBroker.Debug.Is_Active ("corba.object");
   procedure O is new AdaBroker.Debug.Output (Flag);

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : in Ref) return Boolean is
   begin
      return  Self.OmniObj = null;
   end Is_Nil;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Finalize (Self);
   end Release;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean is
   begin
      return Repository_Id = Logical_Type_Id;
   end Is_A;

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : in Ref) return CORBA.Boolean is
   begin
      if Is_Nil (Self) then
         return True;
      end if;
      return Non_Existent (Self.OmniObj.all);
   end Non_Existent;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self  : in Ref;
      Other : in Ref)
      return CORBA.Boolean
   is
      W1, W2 : Controlled_Wrapper;
      S1, S2 : CORBA.Boolean;
   begin
      --  This is a copy from corbaObject.cc L160. Refs are proxy objects.

      if Self.OmniObj = null then
         return Other.OmniObj = null;
      elsif Other.OmniObj = null then
         return False;
      end if;

      Get_Rope_And_Key (Self.OmniObj.all, W1.Real, S1);
      Get_Rope_And_Key (Other.OmniObj.all, W2.Real, S2);

      return W1.Real = W2.Real;
   end Is_Equivalent;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : in Ref;
      Maximum : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      if Is_Nil (Self) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "cannot call hash on a nil reference");
      end if;

      return Hash (Self.OmniObj.all, Maximum);
   end Hash;

   ------------------------
   -- Get_Implementation --
   ------------------------

   function Get_Implementation
     (Self : in Ref'Class)
      return OmniObject_Ptr
   is
   begin
      return Self.OmniObj;
   end Get_Implementation;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Ref) is
   begin
      pragma Debug (O ("initialize : enter"));

      Self.OmniObj := null;

      pragma Debug (O ("initialize : leave"));
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Ref) is
   begin
      pragma Debug (O ("adjust : enter"));

      if Self.OmniObj /= null then
         Self.OmniObj := Duplicate_OmniObject (Self.OmniObj);
      end if;

      pragma Debug (O ("adjust : leave"));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Ref) is
   begin
      pragma Debug (O ("finalize : enter"));

      if Self.OmniObj /= null then
         Destruct_OmniObject (Self.OmniObj);
         Self.OmniObj := null;
      end if;

      pragma Debug (O ("finalize : leave"));
   end Finalize;


   -----------------------------
   --  DII related functions  --
   -----------------------------

   function To_Any (From : in Ref)
                    return CORBA.Any is
      The_Any : CORBA.Any;
      Tco : CORBA.TypeCode.Object;
   begin
      CORBA.TypeCode.Set (Tco, Tk_Objref);
      The_Any := (new C_Object_Ref' (Value => From), Tco);
      return The_Any;
   end To_Any;

   function From_Any (From : in Any)
                      return Ref is
      Tmp : C_Object_Ref_Ptr;
   begin
      if (TypeCode.Kind (From.The_Type) /= Tk_Objref) then
         raise CORBA.Bad_Typecode;
      end if;
      Tmp := C_Object_Ref_Ptr (From.The_Value);
      return Tmp.Value;
   end From_Any;

   procedure Create_Request (Self      : in     Ref;
                             Ctx       : in     CORBA.Context.Object;
                             Operation : in     CORBA.Identifier;
                             Arg_List  : in     CORBA.NVList.Object;
                             Result    : access CORBA.NamedValue;
                             Request   :    out CORBA.Request.Object;
                             Req_Flags : in     CORBA.Flags) is
      --  to be fixed
   begin
      CORBA.Request.Set (Request,
                         Ctx,
                         Operation,
                         Arg_List,
                         Result,
                         Req_Flags);
      --  Self is not used now ???
   end Create_Request;



end CORBA.Object;
