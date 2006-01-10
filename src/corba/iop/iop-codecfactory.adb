------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     I O P . C O D E C F A C T O R Y                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with CORBA.Impl;
with PolyORB.CORBA_P.Initial_References;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Utils.Strings.Lists;

with IOP.CodecFactory.Impl;

package body IOP.CodecFactory is

   function Create return CORBA.Object.Ref;

   procedure Deferred_Initialization;

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref is
      Result  : Local_Ref;
      Current : constant CORBA.Impl.Object_Ptr := new Impl.Object;

   begin
      Set (Result, Current);

      return CORBA.Object.Ref (Result);
   end Create;

   ------------------
   -- Create_Codec --
   ------------------

   function Create_Codec
     (Self : Local_Ref;
      Enc  : IOP.Encoding)
     return IOP.Codec.Local_Ref
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Create_Codec (Impl.Object_Ptr (Entity_Of (Self)), Enc);
   end Create_Codec;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PolyORB.CORBA_P.Initial_References.Register_Initial_Reference
        ("CodecFactory", Create'Access);
   end Deferred_Initialization;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out UnknownEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"ior.codecfactory",
          Conflicts => Empty,
          Depends   => +"corba.initial_references",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end IOP.CodecFactory;
