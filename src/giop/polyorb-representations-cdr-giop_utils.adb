------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REPRESENTATIONS.CDR.GIOP_UTILS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Log;

package body PolyORB.Representations.CDR.GIOP_Utils is

   use PolyORB.Log;

   package L is
     new PolyORB.Log.Facility_Log ("polyorb.representations.cdr.giop_utils");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : in     CDR_Representation'Class;
      Data           : in     PolyORB.Any.NamedValue)
   is
      use PolyORB.Exceptions;

      Error : Exceptions.Error_Container;

   begin
      pragma Debug (O ("Marshall (NamedValue) : enter"));
      Marshall_From_Any (Representation, Buffer, Data.Argument, Error);

      if Found (Error) then
         Catch (Error);
         raise Program_Error;
         --  XXX We cannot silentely ignore any error. For now, we
         --  raise this exception. To be investigated.
      end if;

      pragma Debug (O ("Marshall (NamedValue) : end"));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : in     CDR_Representation'Class)
      return PolyORB.Any.NamedValue
   is
      use PolyORB.Exceptions;

      NV  :  PolyORB.Any.NamedValue;
      pragma Warnings (Off, NV);
      --  Default initialization

      Error : Exceptions.Error_Container;

   begin
      pragma Debug (O ("Unmarshall (NamedValue) : enter"));
      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      Unmarshall_To_Any (Representation, Buffer, NV.Argument, Error);

      if Found (Error) then
         Catch (Error);
         raise Program_Error;
         --  XXX We cannot silentely ignore any error. For now, we
         --  raise this exception. To be investigated.
      end if;

      pragma Debug (O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (NV.Argument))));
      pragma Debug (O ("Unmarshall (NamedValue) : end"));
      return NV;
   end Unmarshall;

end PolyORB.Representations.CDR.GIOP_Utils;
