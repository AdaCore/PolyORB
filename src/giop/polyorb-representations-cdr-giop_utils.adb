------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REPRESENTATIONS.CDR.GIOP_UTILS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;

package body PolyORB.Representations.CDR.GIOP_Utils is

   use PolyORB.Log;

   package L is
     new PolyORB.Log.Facility_Log ("polyorb.representations.cdr.giop_utils");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class;
      Data           : PolyORB.Any.NamedValue;
      Error          : in out Errors.Error_Container)
   is
      use PolyORB.Any;
   begin
      pragma Debug (C, O ("Marshall (NamedValue) : enter"));
      Marshall_From_Any
        (Representation,
         Buffer,
         Get_Container (Data.Argument).all,
         Error);
      pragma Debug (C, O ("Marshall (NamedValue) : end"));
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer         : access Buffers.Buffer_Type;
      Representation : access CDR_Representation'Class;
      Data           : in out PolyORB.Any.NamedValue;
      Error          : in out Errors.Error_Container)
   is
      use PolyORB.Any;
   begin
      pragma Debug (C, O ("Unmarshall (NamedValue) : enter"));

      Unmarshall_To_Any
        (Representation,
         Buffer,
         Get_Container (Data.Argument).all,
         Error);

      pragma Debug (C, O ("Unmarshall (NamedValue) : is_empty := "
                       & Boolean'Image (PolyORB.Any.Is_Empty
                                        (Data.Argument))));
      pragma Debug (C, O ("Unmarshall (NamedValue) : end"));
   end Unmarshall;

end PolyORB.Representations.CDR.GIOP_Utils;
