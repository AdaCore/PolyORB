-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Giop_s                          ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_Giop_s declared in Ada_Giop_s.hh.                       ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It does not include an Init function since it is          ----
----   useless for AdaBroker. (AdaBroker never creates a Giop_s    ----
----   object)                                                     ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Unchecked_Conversion ;
with Ada.Exceptions ;

package body Giop_S is

   -- C_To_Ada_Unsigned_Long
   -------------------------
   function C_To_Ada_Unsigned_Long is
     new Ada.Unchecked_Conversion (Interfaces.C.Unsigned_Long,
                                   Corba.Unsigned_Long) ;
   -- needed to change C type Interfaces.C.Unsigned_Long
   -- into Ada type Corba.Unsigned_Long


   -- C_Reply_Header_Size
   ----------------------
   function C_Reply_Header_Size (Self : in Object'Class)
                                 return Interfaces.C.Unsigned_Long ;
   pragma Import (CPP,C_Reply_Header_Size,"ReplyHeaderSize__6GIOP_S") ;

   -- Reply_Header_Size
   --------------------
   function Reply_Header_Size (Self : in Object'Class)
                               return Corba.Unsigned_Long is
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- calls the C function ...
      C_Result := C_Reply_Header_Size (Self) ;
      -- ... and transforms the result into an Ada type
      return C_To_ada_Unsigned_Long (C_Result) ;
   end ;


   -- C_Request_Received
   ---------------------
   procedure C_Request_Received (Self : in Object'Class ;
                                 Skip : in Sys_Dep.C_Boolean) ;
   pragma Import (C,C_Request_Received,"RequestReceived__10Ada_Giop_sb") ;
   -- wrapper around  Ada_Giop_s procedure RequestReceived
   -- (see Ada_Giop_s.hh)
   -- called by the Ada equivalent : Request_Received


   -- Request_Received
   -------------------
   procedure Request_Received (Self : in Object'Class ;
                               Skip : in Boolean) is
      C_Skip : Sys_Dep.C_Boolean ;
   begin
      -- transforms the arguments into a C type ...
      C_Skip := Sys_Dep.Boolean_Ada_To_C (Skip) ;
      -- ... and calls the C procedure
      C_Request_Received (Self, C_Skip) ;
   end;


   -- Ada_To_C_Unsigned_Long
   -------------------------
   function Ada_To_C_Unsigned_Long is
     new Ada.Unchecked_Conversion (Corba.Unsigned_Long,
                                   Interfaces.C.Unsigned_Long) ;
   -- needed to change ada type Corba.Unsigned_Long
   -- into C type Interfaces.C.Unsigned_Long


   -- C_Initialise_Reply
   ---------------------
   procedure C_Initialize_Reply (Self : in Object'Class ;
                                 Status : in Interfaces.C.int ;
                                 MsgSize : in Interfaces.C.Unsigned_Long);
   pragma Import (C,C_Initialize_Reply,"InitialiseReply__10Ada_Giop_siUi") ;
   -- wrapper around  Ada_Giop_s procedure InitialiseReply
   -- (see Ada_Giop_s.hh)
   -- called by the Ada equivalent : Initialise_Reply


   -- Initialize_Reply
   -------------------
   procedure Initialize_Reply (Self : in Object'Class ;
                              Status : in Giop.Reply_Status_Type ;
                              MsgSize : in Corba.Unsigned_Long ) is
      C_Status : Interfaces.C.Int ;
      C_MsgSize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments into a C type ...
      C_Status := Giop.Reply_Status_Type_To_C_Int(Status) ;
      C_MsgSize := Ada_To_C_Unsigned_Long (MsgSize) ;
      -- ... and calls the C procedure
      C_Initialize_Reply (Self, C_Status, C_MsgSize) ;
   end;

end Giop_S ;






