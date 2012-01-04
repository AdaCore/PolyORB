------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . T E R M _ M A N A G E R _ I N F O         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

--  This package is in charge of processing DSA_TM_Info service contexts.
--  DSA_TM_Info service contexts are used by clients to pass to the servers a
--  reference to their termination manager. When a request is received on the
--  server side, the function Extract_TM_Info is called. This function extracts
--  the reference from the QoS parameter and stores it in the requestor Binding
--  Object notepad. Later on, the termination manager will retrieve this
--  reference to reach the client's termination manager.

with PolyORB.Annotations;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Tasking.Mutexes;

package PolyORB.QoS.Term_Manager_Info is

   pragma Elaborate_Body;

   type QoS_DSA_TM_Info_Parameter is new QoS_Parameter (DSA_TM_Info) with
   record
         TM_Ref : References.Ref;
   end record;
   --  The QoS parameter type associated with TM_Info service contexts

   type QoS_DSA_TM_Info_Parameter_Access is
     access all QoS_DSA_TM_Info_Parameter;

   type BO_Note is new Annotations.Note with record
      TM_Ref : References.Ref;
      --  TM_Ref is a reference to the Termination Manager of the node the BO
      --  containing this type of note links to.

   end record;
   --  This type of note is used to store the Reference extracted from the
   --  service context into a Binding Object.

   Default_BO_Note : constant BO_Note
     := (PolyORB.Annotations.Note with TM_Ref => References.Nil_Ref);

   procedure Extract_TM_Info (R : access PolyORB.Requests.Request);
   --  Extracts the Transaction Manager Info from request R

   procedure Enter_BO_Note_Lock;
   --  Take the lock ensuring integrity of the BO_Notes

   procedure Leave_BO_Note_Lock;
   --  Release the lock ensuring integrity of the BO_Notes

private
   Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
   --  The lock ensuring integrity of BO_Notes

   procedure Initialize;
end PolyORB.QoS.Term_Manager_Info;
