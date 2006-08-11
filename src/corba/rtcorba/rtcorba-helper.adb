------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       R T C O R B A . H E L P E R                        --
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

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);

package body RTCORBA.Helper is

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return RTCORBA.Priority
   is
      Result : constant CORBA.Short := CORBA.From_Any (Item);

   begin
      return RTCORBA.Priority (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RTCORBA.Priority)
     return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_Priority);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
      Name : CORBA.String := CORBA.To_CORBA_String ("Priority");
      Id : CORBA.String := CORBA.To_CORBA_String ("IDL:RTCORBA/Priority:1.0");
   begin
      CORBA.TypeCode.Internals.Add_Parameter
        (TC_Priority,
         CORBA.To_Any (Name));

      CORBA.TypeCode.Internals.Add_Parameter (TC_Priority, CORBA.To_Any (Id));

      CORBA.TypeCode.Internals.Add_Parameter
        (TC_Priority,
         CORBA.To_Any (CORBA.TC_Short));
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"RTCORBA.Helper",
       Conflicts => Empty,
       Depends   => +"any",
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end RTCORBA.Helper;
