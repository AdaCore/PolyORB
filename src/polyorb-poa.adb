------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . P O A                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Abstract interface for the POA.

--  $Id$

with PolyORB.Utils;

package body PolyORB.POA is

   use PolyORB.Types;
   use PolyORB.Utils;

   function Oid_To_Rel_URI
     (OA : access Obj_Adapter;
      Id : Object_Id)
     return Types.String
   is
      U_Oid : Unmarshalled_Oid_Access := Oid_To_U_Oid (Id);
      URI : Types.String := To_PolyORB_String ("/");
   begin
      if Length (U_Oid.Creator) /= 0 then
         URI := URI & U_Oid.Creator & To_PolyORB_String ("/");
      end if;
      URI := URI & URI_Encode (To_Standard_String (U_Oid.Id));
      --  XXX Here we make the assumption that Id needs to be
      --  URI-escaped, and Creator needs not, but there is
      --  no reason to. What should actually be done is that
      --  Creator should be a list, and each of its components
      --  should be separately URLencoded.

      if U_Oid.Persistency_Flag /= 0 then
         URI := URI & ";" & Trimmed_Image
           (Integer (U_Oid.Persistency_Flag));
      end if;
      Free (U_Oid);
      return URI;
   end Oid_To_Rel_URI;

   function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI : Types.String)
     return Object_Id is
   begin
      raise PolyORB.Not_Implemented;
      return Rel_URI_To_Oid (OA, URI);
   end Rel_URI_To_Oid;


end PolyORB.POA;
