-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Corba                           ----
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
----     This package deffines all general types and associated    ----
----   functions used in AdaBroker.                                ----
----     The first part is the definition of corba types out of    ----
----   Ada ones. Pointers on these types are also defined as well  ----
----   as the associated free functions.                           ----
----     Then, the corba exception type is defined and all corba   ----
----   system exceptions.                                          ----
----     At last, some OmniOrb or AdaBroker specific exceptions    ----
----   and functions are defined.                                  ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Corba.Exceptions ;

package body Corba is

   -----------------------------------------------------------
   ----           Exceptions in spec                       ---
   -----------------------------------------------------------

   -- GetMembers
   -------------
   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To : out Ex_Body) is
   begin
      -- calls the correponding procedure in Corba.Exception
      Corba.Exceptions.Get_Members (From,To) ;
   end ;


   -- Raise_Corba_Exception
   ------------------------
   procedure Raise_Corba_Exception(Excp : in Ada.Exceptions.Exception_Id ;
                                   Excp_Memb: in Idl_Exception_Members'Class) is
   begin
      -- calls the correponding procedure in Corba.Exception
      Corba.Exceptions.Raise_Corba_Exception(Excp, Excp_Memb) ;
   end ;


   -----------------------------------------------------------
   ----        not in spec, AdaBroker specific             ---
   -----------------------------------------------------------

   -- To_Corba_String
   ------------------
    function To_Corba_String(S: in Standard.String) return Corba.String is
    begin
       return Corba.String(Ada.Strings.Unbounded.To_Unbounded_String(S)) ;
    end ;


    -- To_Standard_String
    ---------------------
    function To_Standard_String(S: in Corba.String) return Standard.String is
    begin
       return Ada.Strings.Unbounded.To_String(Ada.Strings.Unbounded.Unbounded_String(S)) ;
    end;


    -- Length
    ---------
    function Length(Str : in Corba.String) return Corba.Unsigned_Long is
    begin
       return Corba.Unsigned_Long(Ada.Strings.Unbounded.
                                  Length(Ada.Strings.Unbounded.Unbounded_String(Str))) ;
    end ;

end Corba ;
