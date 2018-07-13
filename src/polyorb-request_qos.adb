------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . R E Q U E S T _ Q O S                   --
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

pragma Ada_2012;

with PolyORB.Annotations;
with PolyORB.Log;

package body PolyORB.Request_QoS is

   use PolyORB.Annotations;
   use PolyORB.Log;
   use PolyORB.QoS;

   package L is new PolyORB.Log.Facility_Log ("polyorb.request_qos");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Call_Back_Array : array (QoS_Kind'Range) of Fetch_QoS_CB;

   type QoS_Note is new Note with record
      Request_QoS : QoS_Parameters;
      Reply_QoS   : QoS_Parameters;
   end record;

   overriding procedure Destroy (N : in out QoS_Note);

   Empty : constant QoS_Parameters := (others => null);

   Default_Note : constant QoS_Note := QoS_Note'(Note with Empty, Empty);

   -------------------
   -- Add_Reply_QoS --
   -------------------

   procedure Add_Reply_QoS
     (Req  : in out Requests.Request;
      Kind : QoS_Kind;
      QoS  : QoS_Parameter_Access)
   is
      Note : QoS_Note;

   begin
      pragma Assert (QoS = null or else QoS.Kind = Kind);

      Get_Note (Req.Notepad, Note, Default_Note);

      Release (Note.Reply_QoS (Kind));
      Note.Reply_QoS (Kind) := QoS;

      Set_Note (Req.Notepad, Note);
   end Add_Reply_QoS;

   ---------------------
   -- Add_Request_QoS --
   ---------------------

   procedure Add_Request_QoS
     (Req  : in out Requests.Request;
      Kind : QoS_Kind;
      QoS  : QoS_Parameter_Access)
   is
      Note : QoS_Note;

   begin
      pragma Assert (QoS = null or else QoS.Kind = Kind);

      Get_Note (Req.Notepad, Note, Default_Note);

      Release (Note.Request_QoS (Kind));
      Note.Request_QoS (Kind) := QoS;

      Set_Note (Req.Notepad, Note);
   end Add_Request_QoS;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (N : in out QoS_Note) is
   begin
      for J in QoS_Kind loop
         Release (N.Request_QoS (J));
         Release (N.Reply_QoS (J));
      end loop;
   end Destroy;

   -----------------------------
   -- Extract_Reply_Parameter --
   -----------------------------

   function Extract_Reply_Parameter
     (Kind : QoS_Kind;
      Req  : Requests.Request) return QoS_Parameter_Access
   is
      Note : QoS_Note;

   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      return Note.Reply_QoS (Kind);
   end Extract_Reply_Parameter;

   -------------------------------
   -- Extract_Request_Parameter --
   -------------------------------

   function Extract_Request_Parameter
     (Kind : QoS_Kind;
      Req  : Requests.Request) return QoS_Parameter_Access
   is
      Note : QoS_Note;

   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      return Note.Request_QoS (Kind);
   end Extract_Request_Parameter;

   ---------------
   -- Fetch_QoS --
   ---------------

   function Fetch_QoS
     (Ref : PolyORB.References.Ref) return QoS_Parameters
   is
      Result      : QoS_Parameters;
      A_Parameter : QoS_Parameter_Access;

   begin
      pragma Debug (C, O ("Fetch_QoS: enter"));

      for J in Call_Back_Array'Range loop
         if Call_Back_Array (J) /= null then
            pragma Debug (C, O ("Fetching QoS parameters for "
                             & QoS_Kind'Image (J)));

            A_Parameter := Call_Back_Array (J) (Ref);
            if A_Parameter /= null then
               pragma Assert (J = A_Parameter.Kind);
               Result (A_Parameter.Kind) := A_Parameter;
            end if;
         end if;
      end loop;

      pragma Debug (C, O ("Fetch_QoS: leave"));
      return Result;
   end Fetch_QoS;

   -------------------
   -- Get_Reply_QoS --
   -------------------

   function Get_Reply_QoS
     (Req : Requests.Request) return QoS_Parameters
   is
      Note : QoS_Note;
   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      return Note.Reply_QoS;
   end Get_Reply_QoS;

   ---------------------
   -- Get_Request_QoS --
   ---------------------

   function Get_Request_QoS
     (Req : Requests.Request) return QoS_Parameters
   is
      Note : QoS_Note;
   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      return Note.Request_QoS;
   end Get_Request_QoS;

   --------------
   -- Register --
   --------------

   procedure Register (Kind : QoS_Kind; CB : Fetch_QoS_CB) is
   begin
      pragma Debug (C, O ("Registering call back for "
                       & QoS_Kind'Image (Kind)));

      pragma Assert (Call_Back_Array (Kind) = null);
      Call_Back_Array (Kind) := CB;
   end Register;

   -------------------
   -- Set_Reply_QoS --
   -------------------

   procedure Set_Reply_QoS
     (Req : in out Requests.Request;
      QoS : QoS_Parameters)
   is
      Note : QoS_Note;

   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      Note.Reply_QoS := QoS;
      Set_Note (Req.Notepad, Note);
   end Set_Reply_QoS;

   ---------------------
   -- Set_Request_QoS --
   ---------------------

   procedure Set_Request_QoS
     (Req : in out Requests.Request;
      QoS : QoS_Parameters)
   is
      Note : QoS_Note;

   begin
      Get_Note (Req.Notepad, Note, Default_Note);
      Note.Request_QoS := QoS;
      Set_Note (Req.Notepad, Note);
   end Set_Request_QoS;

end PolyORB.Request_QoS;
