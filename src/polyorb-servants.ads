------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . S E R V A N T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  Root type for concrete object implementations (servants).

--  $Id$

with PolyORB.Annotations;
with PolyORB.Components;

package PolyORB.Servants is

   -------------
   -- Servant --
   -------------

   --  A Servant is a Component that supports the messages
   --  defined in PolyORB.Servants.Interface. This type may
   --  be further derived by personality-specific units.

   type Servant is abstract new PolyORB.Components.Component with private;

   type Servant_Access is access all Servant'Class;

   function Handle_Message
     (S   : access Servant;
      Msg :        Components.Message'Class)
      return Components.Message'Class;

   function Execute_Servant
     (S   : access Servant;
      Msg :        Components.Message'Class)
     return Components.Message'Class
      is abstract;

   function Notepad_Of
     (S : Servant_Access)
     return PolyORB.Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  Return Notepad associated to a servant

   --------------
   -- Executor --
   --------------

   --  An Executor is responsible for setting the condition under
   --  which a Servant object executes a request.

   type Executor is abstract tagged limited private;
   type Executor_Access is access all Executor'Class;

   function Handle_Request_Execution
     (Self      : access Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class
      is abstract;

   procedure Set_Executor
     (S    : access Servant;
      Exec :        Executor_Access);
   pragma Inline (Set_Executor);

private

   type Executor is abstract tagged limited null record;

   type Servant is abstract new PolyORB.Components.Component with record
      Exec : Executor_Access;
      Notepad : aliased PolyORB.Annotations.Notepad;
   end record;
end PolyORB.Servants;
