------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . M E S S A G E S                         --
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

with MOMA.Destinations;
with Ada.Calendar; use Ada.Calendar;
with Temp; use Temp;

package MOMA.Messages is

   type Message is tagged private;

   procedure Acknowledge;

   procedure Clear_Body;

   function Get_Property (Name : String) return Property_Type;

   function Get_Correlation_Id return String;

   function Get_Persistent return Boolean;

   function Get_Destination
     return MOMA.Destinations.Destination is abstract;

   function Get_Expiration return Time;

   function Get_Message_Id return String;

   function Get_Priority return Priority;

   function Get_Redelivered return Boolean;

   function Get_Reply_To
     return MOMA.Destinations.Destination is abstract;

   function Get_Timestamp return Time;

   function Get_Type return String;

   --  ??? return
   function Get_Property_Names
     return Integer;

   function Property_Exists (Name : String) return Boolean;

   procedure Set_Property (Name : String; Value : Property_Type);

   procedure Set_Correlation_Id (Correlation_Id : String);

   procedure Set_Persistent (Is_Persistent : Boolean);

   procedure Set_Destination (Destination : MOMA.Destinations.Destination);

   procedure Set_Expiration (Expiration : Time);

   procedure Set_Message_Id (Id : String);

   procedure Set_Priority (Value : Priority);

   procedure Set_Redelivered (Redelivered : Boolean);

   procedure Set_Reply_To (Reply_To : MOMA.Destinations.Destination);

   procedure Set_Timestamp (Timestamp : Time);

   -- Abstract Functions and Procedures --

   ----------------------------------
   --  Abstract Set_Type Procedure --
   ----------------------------------
   procedure Set_Type (Message_Type : String) is abstract;

private
   type Message is tagged null record;

end MOMA.Messages;
