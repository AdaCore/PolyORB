-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                 package Omniobjectmanager                     ----
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
----     This package is wrapped around the C class                ----
----   omniObjectManager declared in omniInternal.h.               ----
----     It provides the sames functions as the C class (ie        ----
----   one function : NilObjectManager).                           ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.CPP ;
with System ;

package OmniObjectManager is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- this type is both a C and an Ada class
   -- it is wrapped around OmniObjectManager
   -- (see omniInternal.h)


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   function Nil_Object_Manager return Object ;
   pragma CPP_Virtual(Nil_Object_Manager) ;
   -- Returns a Null Object Manager


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__17omniObjectManager");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing

end OmniObjectManager ;

