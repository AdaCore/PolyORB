------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . G I O P _ P . S E R V I C E _ C O N T E X T S       --
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

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;

package body PolyORB.GIOP_P.Service_Contexts is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.service_contexts");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------------------
   -- Marshall_Service_Context_List --
   -----------------------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    : PRSC.QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use PolyORB.QoS.Service_Contexts.Service_Context_Lists;

      Iter : Iterator;
   begin
      if SCP = null then
         Marshall (Buffer, Types.Unsigned_Long'(0));
         return;
      end if;

      pragma Debug (O ("Marshall_Service_Context_List: enter, length="
                       & Integer'Image (Length (SCP.Service_Contexts))));

      Iter := First (SCP.Service_Contexts);

      Marshall (Buffer, Types.Unsigned_Long (Length (SCP.Service_Contexts)));

      while not Last (Iter) loop
         Marshall (Buffer, Unsigned_Long (Value (Iter).Context_Id));
         Marshall (Buffer, Value (Iter).Context_Data.all);
         Next (Iter);
      end loop;

      pragma Debug (O ("Marshall_Service_Context_List: leave"));
   end Marshall_Service_Context_List;

   -------------------------------------
   -- Unmarshall_Service_Context_List --
   -------------------------------------

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    :    out PRSC.QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use Service_Context_Lists;

      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);

   begin
      pragma Debug (O ("Unmarshall_Service_Context_List: enter, length ="
                       & PolyORB.Types.Unsigned_Long'Image (Length)));

      if Length = 0 then
         SCP := null;
         return;
      end if;

      SCP := new QoS_GIOP_Service_Contexts_Parameter;

      for J in 1 .. Length loop
         Append
           (SCP.Service_Contexts,
            (Types.Unsigned_Long'(Unmarshall (Buffer)),
             new Encapsulation'(Unmarshall (Buffer))));
      end loop;

      pragma Debug (O ("Unmarshall_Service_Context_List: leave"));
   end Unmarshall_Service_Context_List;

end PolyORB.GIOP_P.Service_Contexts;
