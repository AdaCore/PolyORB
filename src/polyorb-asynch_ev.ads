------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . A S Y N C H _ E V                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  Abstract data type for an asynchrous event source

with PolyORB.Annotations;
with PolyORB.Jobs;

package PolyORB.Asynch_Ev is

   pragma Elaborate_Body;

   --  Some environment components can produce events in an
   --  asynchronous fashion, i.e. independently of middleware actions
   --  currently in progress. A typical example of such components is
   --  a connection from the outside world.

   --  Such components are represented within PolyORB as Asynch_Ev_Source
   --  objects. These objects are registered in collections called
   --  Asynch_Ev_Monitors.

   --  Monitors provide an interface for the middleware to check whether
   --  events have occured on any of their member Asynch_Ev_Sources.

   type Asynch_Ev_Monitor is abstract tagged limited private;

   function Has_Sources (AEM : Asynch_Ev_Monitor) return Boolean is abstract;
   --  Return True iff AEM has sources to monitor

   type Asynch_Ev_Monitor_Access is
     access all Asynch_Ev_Monitor'Class;

   type AEM_Factory is access function
     return Asynch_Ev_Monitor_Access;
   --  A function that allocates an instance of a concrete AEM type

   type Asynch_Ev_Source is abstract tagged limited private;

   type Asynch_Ev_Source_Access is
     access all Asynch_Ev_Source'Class;

   function Notepad_Of
     (AES : Asynch_Ev_Source_Access)
     return Annotations.Notepad_Access;
   pragma Inline (Notepad_Of);
   --  An Asynch_Ev_Source is an annotable object (cf. PolyORB.Annotations),
   --  so clients can associate it with any information that is necessary
   --  to process events that occur on it.
   --  This functions returns an access to AES' Notepad attribute.

   function AEM_Factory_Of
     (AES : Asynch_Ev_Source)
     return AEM_Factory
      is abstract;
   pragma Inline (AEM_Factory_Of);
   --  Return a factory capable of creating an AEM that can monitor AES

   procedure Create (AEM : out Asynch_Ev_Monitor) is abstract;
   --  Initialize AEM

   procedure Destroy (AEM : in out Asynch_Ev_Monitor) is abstract;
   --  Finalize AEM

   function AEM_Of (AES : Asynch_Ev_Source) return Asynch_Ev_Monitor_Access;

   procedure Register_Source
     (AEM     : access Asynch_Ev_Monitor;
      AES     :        Asynch_Ev_Source_Access;
      Success :    out Boolean)
      is abstract;
   --  Try to register AES for monitoring by AEM.
   --  On exit, Success is True iff AEM accepts AES for monitoring.

   procedure Unregister_Source
     (AEM     : in out Asynch_Ev_Monitor;
      AES     : Asynch_Ev_Source_Access;
      Success : out Boolean)
      is abstract;
   --  Remove AES from the set of sources monitored by AEM. On exit,
   --  Success is True iff AES was previously registered with this AEM.

   function Unregister_Source (AES : Asynch_Ev_Source_Access) return Boolean;
   --  Remove AES from any AEM that it is currently in. Returns True if
   --  AES actually has been unregistered from an AEM, False otherwise.

   procedure Destroy (AES : in out Asynch_Ev_Source_Access);
   --  Destroy AES

   type AES_Array is array (Integer range <>)
     of Asynch_Ev_Source_Access;

   function Check_Sources
     (AEM     : access Asynch_Ev_Monitor;
      Timeout :        Duration)
     return AES_Array
      is abstract;
   --  Wait for events on sources monitored by AEM.
   --  Return when one event source in AEM has had an event.
   --  If no event happened within Timeout, an empty array is returned.
   --  Otherwise, the returned array contains those sources on which
   --  events have occurred, and these sources are removed from AEM.
   --  Note that a Timeout of 0.0 returns immediatly.
   --  A Timeout of PolyORB.Constants.Forever means to not return
   --  until an event occurs.

   procedure Abort_Check_Sources (AEM : Asynch_Ev_Monitor) is abstract;
   --  Send an persistent abort signal to AEM. This signal aborts any
   --  task currently executing Check_Sources on AEM, or will abort
   --  next call to Check_Sources.

   -------------------------------------
   -- Reactor for asynchronous events --
   -------------------------------------

   --  The middleware core implements the Reactor pattern to handle
   --  event occurring on asynchronous event sources.  An event
   --  handler is associated with each asynchronous event source. The
   --  handling of an event constitutes a Job that can be performed by
   --  an ORB task.

   type AES_Event_Handler is abstract new PolyORB.Jobs.Job with record
      AES : Asynch_Ev_Source_Access;
   end record;
   type AES_Event_Handler_Access is access all AES_Event_Handler'Class;

   procedure Handle_Event
     (H : access AES_Event_Handler)
      is abstract;
   --  Handle an event that has occurred on this asynchronous event
   --  source. If AES is null on exit, then the asynchronous event
   --  source has been destroyed, and the handler must be deallocated.

   --  In this implementation of the Reactor pattern, the association
   --  between an event source and its event handler is made using an
   --  Annotation on the event source.

   procedure Run (AEH : access  AES_Event_Handler);
   --  Call Handle_Event

   type AES_Note is new Annotations.Note with record
      Handler : AES_Event_Handler_Access;
   end record;

private

   type Asynch_Ev_Source is abstract tagged limited record
      Monitor : Asynch_Ev_Monitor_Access;
      --  The AEM with which this source was registered. A concrete
      --  implementation of Register_Source returning with Success =
      --  True must set this member of its AES argument to the value
      --  of its AEM argument.

      Notes   : aliased Annotations.Notepad;
   end record;

   type Asynch_Ev_Monitor is abstract tagged limited null record;

end PolyORB.Asynch_Ev;
