///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_Rope                                ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////    This class is both a C and an Ada Class(see omniObject.ads)////
////    It is wrapped around Rope in order to avoid the presence   ////
////    of non default constructors.                               ////
////    So, it provides the same functions Rope except that cons-  ////
////    tructors are replaced by Init functions.                   ////
////    It has also a pointer on the underlying Rope Object        ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include "Ada_Rope.hh"

Ada_Rope::Ada_Rope ()
{
  Init_Ok = false;
  C_Rope = 0;
};
// default constructor


