------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    M O M A . D E S T I N A T I O N S                     --
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

--  Definition of the 'destination' object.

--  $Id$

with MOMA.Types;
with PolyORB.References;

package MOMA.Destinations is

   type Destination is tagged private;
   --  A destination is a structure embedding a reference to an object
   --  (message pool or message router) to which message must be sent.

   function Get_Name (Self : Destination) return MOMA.Types.String;
   pragma Inline (Get_Name);

   procedure Set_Name (Self : in out Destination;
                       Name : MOMA.Types.String);
   pragma Inline (Set_Name);

   function Get_Ref (Self : Destination) return PolyORB.References.Ref;
   pragma Inline (Get_Ref);

   procedure Set_Ref (Self : in out Destination;
                      Ref  : PolyORB.References.Ref);
   pragma Inline (Set_Ref);

   procedure Delete;
   --  XXX really useful in this context ?

private
   type Destination is tagged record
      Name : MOMA.Types.String;

      Ref  : PolyORB.References.Ref;
      --  Reference to the actual destination object.
   end record;
end MOMA.Destinations;
