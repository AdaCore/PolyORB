///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_Corba_Orb                           ////
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
////    This file contains wrapper functions around functions      ////
////   defined in IOP.h They are here to handle C++ exceptions     ////
////   which could make the Ada program halt.                      ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#include <omniORB2/IOP.h>

void marshall (IOP::TaggedProfileList* t, NetBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* t, NetBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


void marshall (IOP::TaggedProfileList* t, MemBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* t, MemBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


size_t NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset);
// wrapper around IOP::TaggedProfile function NP_alignedSize
// (see IOP.h)


size_t length (IOP::TaggedProfileList* t);
// wrapper around IOP::TaggedProfile function length
// (see IOP.h)

