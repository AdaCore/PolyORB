------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ P R O D U C E R S                --
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

with Ada.Calendar; use Ada.Calendar;
with Temp; use Temp;
with MOMA.Destinations;
with PolyORB.References;

package MOMA.Message_Producers is

   type Message_Producer is abstract tagged private;

   procedure Close;

   function Get_Persistent (Self : Message_Producer) return Boolean;

   procedure Set_Persistent (Self : in out Message_Producer;
                             Persistent : Boolean);

   function Get_Priority (Self : Message_Producer) return Priority;

   procedure Set_Priority (Self : in out Message_Producer;
                           Value : Priority);

   function Get_Time_To_Live (Self : Message_Producer) return Time;

   procedure Set_Time_To_Live (Self : in out Message_Producer;
                               TTL : Time);

   function Get_Ref (Self : Message_Producer) return PolyORB.References.Ref;

   procedure Set_Ref (Self : in out Message_Producer;
                      Ref  : PolyORB.References.Ref);

   function Get_Destination (Self : Message_Producer)
                             return MOMA.Destinations.Destination;

   procedure Set_Destination (Self : in out Message_Producer'Class;
                              Dest : MOMA.Destinations.Destination);

private
   type Message_Producer is abstract tagged record
      Priority_Level : Priority;
      Persistent     : Boolean;
      TTL            : Time;
      Destination    : MOMA.Destinations.Destination;
      Ref            : PolyORB.References.Ref;
   end record;

end MOMA.Message_Producers;
