------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            C O S N A M I N G . B I N D I N G I T E R A T O R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package CosNaming.BindingIterator is

   type Ref is new CORBA.Object.Ref with null record;

   procedure next_one
     (Self : in Ref;
      b : out CosNaming.Binding;
      Returns : out CORBA.Boolean);

   next_one_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingIterator/next_one:1.0";

   procedure next_n
     (Self : in Ref;
      how_many : in CORBA.Unsigned_Long;
      bl : out CosNaming.BindingList;
      Returns : out CORBA.Boolean);

   next_n_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingIterator/next_n:1.0";

   procedure destroy
     (Self : in Ref);

   destroy_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingIterator/destroy:1.0";

   Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingIterator:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

   package Convert_Forward is
     new CosNaming.BindingIterator_Forward.Convert (Ref);
private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end CosNaming.BindingIterator;
