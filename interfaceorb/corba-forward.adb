-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                  package Corba.Forward                        ----
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
----   This package implements the corba specification,            ----
----   to cope with two packages that need one another.            ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

with Ada.Exceptions ;

with Adabroker_Debug ; use Adabroker_Debug ;

package body Corba.Forward is


   -- To_Ref
   ---------
   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref is
      Dummy_Result : Ref ;
   begin
      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Corba.Forward.To_Ref is illegal on a forwarded type"
                                     & "use From_Forward first to convert it into a non forwarded type") ;
      return Dummy_Result ;
   end ;


   -- Get_Nil_Ref
   --------------
   function Get_Nil_Ref(Self : in Ref) return Ref is
      Dummy_Result : Ref ;
   begin
      Ada.Exceptions.Raise_Exception(Constraint_Error'Identity,
                                     "Corba.Forward.Get_Nil_Ref is illegal on a forwarded type"
                                     & "use From_Forward first to convert it into a non forwarded type") ;
      return Dummy_Result ;
   end ;



   --------------------------------------------------------
   ----             package Convert                    ----
   --------------------------------------------------------
   package body Convert is

      -- From_Forward
      ---------------
      function From_Forward(The_Forward : in Ref)
                            return Ref_Type is
      begin
         return To_Ref(The_Forward) ;
      end ;

      -- To_Forward
      -------------
      function To_Forward(The_Ref : in Ref_Type)
                          return Ref is
         Intermediate_Result : Corba.Object.Ref ;
         Toto : Corba.Object.Ref ;
         Result : Ref ;
      begin
         pragma Debug(Output(Forward,"*** Corba.Forward.To_Forward : start")) ;

         if Is_Nil(Result) then
            Output(True, "*********** Is nil") ;
         else
            Output(True, "************ Is not nil") ;
         end if ;

         Result := ( Toto with null record);
         Output(True,"TOTOTOTOOTOTOTOTOOTOTOOTOTTOOTOTOOTOTOTOOTOTOOTOTOTO") ;







         Intermediate_Result :=  Corba.Object.Ref(The_Ref) ;
         pragma Debug(Output(Forward, "*** Corba.Forward.To_Forward : object cast to Corba.Object.Ref")) ;
         Result := ( Toto with null record);
         Output(True, "*** coucoucoucoucoucouocuocuocuoucoucucoucoucouocuocuouc") ;
         Result := (Intermediate_Result with null record) ;
         pragma Debug(Output(Forward,"*** Corba.Forward.To_Forward : Got the object")) ;
         Output(True, "**** " & To_Standard_String(Get_Repository_Id(The_Ref))) ;
         return Result ;
      end ;


   end Convert ;


end Corba.Forward ;

