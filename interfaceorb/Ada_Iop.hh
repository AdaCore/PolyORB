#include "Ada_netBufferedStream.hh"
#include "Ada_memBufferedStream.hh"

void marshall (IOP::TaggedProfileList* t, Ada_netBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* &t, Ada_netBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


void marshall (IOP::TaggedProfileList* t, Ada_memBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* &t, Ada_memBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


size_t NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset);
// wrapper around IOP::TaggedProfile function NP_alignedSize
// (see IOP.h)


size_t length (IOP::TaggedProfileList* t);
// wrapper around IOP::TaggedProfile function length
// (see IOP.h)

