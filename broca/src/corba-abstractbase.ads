------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   C O R B A . A B S T R A C T B A S E                    --
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

with Broca.Refs;
with CORBA.Impl;

package CORBA.AbstractBase is

   pragma Elaborate_Body;

   type Ref is new Broca.Refs.Ref with private;

   procedure Set
     (The_Ref : in out Ref;
      The_Object : CORBA.Impl.Object_Ptr);

   function Object_Of (The_Ref : Ref) return CORBA.Impl.Object_Ptr;

   function Get (The_Ref : Ref) return CORBA.Impl.Object_Ptr
     renames Object_Of;

   --  The following primitive operations are inherited
   --  from Broca.Refs.Ref.

   --    procedure Set
   --      (The_Ref : in out Ref;
   --       The_Entity : Ref_Ptr);

   --    procedure Unref (The_Ref : in out Ref)
   --      renames Finalize;

   --    function Is_Nil (The_Ref : Ref) return Boolean;
   --    function Is_Null (The_Ref : Ref) return Boolean
   --      renames Is_Nil;

   --    procedure Duplicate (The_Ref : in out Ref)
   --      renames Adjust;

   --    procedure Release (The_Ref : in out Ref);

   Nil_Ref : constant Ref;

private

   type Ref is new Broca.Refs.Ref with null record;
   Nil_Ref : constant Ref
     := (Broca.Refs.Nil_Ref with null record);

end CORBA.AbstractBase;
