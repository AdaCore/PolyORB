------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--               G L A D E . N A M I N G . I N T E R F A C E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with GLADE.Objects;

package GLADE.Naming.Interface is

   --  This package is an implementation of a service close to the Naming
   --  Service of CORBA using the Distributed Systems Annex of Ada95.

   --  The naming resolution process is a little bit different from what is
   --  specified in CORBA Naming Service. For instance, (< c1 ; ... ; cn >)
   --  is resolved as (< c1 >) -> (< c2 ; ... ; cn >) and not as
   --  (< c1 ; ... ; cn-1 >) -> (< cn >). This prevents to many "narrowing"
   --  operations. In fact, we tried to prevent casting the returned object
   --  from Object to a more specialized type. Note also that "narrowing"
   --  operations are not entirely implemented in glade-3.11. We tried also
   --  to keep the implementation of sequences as simple as possible.

   --  A name-to-object association is called a name binding. A name binding
   --  is always defined relative to a naming context. A naming context is an
   --  object that contains a set of name bindings in which each name is
   --  unique. Different names can be  bound to an  object in the same or
   --  different contexts at the same  time. There is no requirement,
   --  however, that all objects must be named.
   --
   --  To resolve a name is to determine the object associated with the name
   --  in a given  context. To bind a name is to create a name binding in a
   --  given context. A name is  always resolved relative to a context; there
   --  are no  absolute names. Because a context is like any other object,
   --  it can also be bound to a name in a naming context. Binding contexts
   --  in other contexts creates a  naming graph; a directed graph with nodes
   --  and labeled edges where the nodes are contexts. A naming  graph
   --  allows  more complex name s to reference a n object. Given a context
   --  in a naming graph, a sequence of names can reference an object. This
   --  sequence of names (called a  compound name) defines a path in the
   --  naming graph to navigate the resolution process.

   pragma Remote_Types;

   ----------------------
   -- Binding_Iterator --
   ----------------------

   type Binding_Iterator is abstract tagged limited private;
   type Binding_Iterator_Ref is access all Binding_Iterator'Class;

   --------------------
   -- Naming_Context --
   --------------------

   type Naming_Context is abstract new Objects.Object with private;
   type Naming_Context_Ref is access all Naming_Context'Class;

   --------------------------------
   -- Naming_Context Subprograms --
   --------------------------------

   procedure Bind
     (Ctx : access Naming_Context;
      N   : in Name;
      Obj : in Objects.Object_Ref)
     is abstract;
   --  Creates a binding of a name and  an object in the naming context.
   --  Naming  contexts that are bound using  bind do not participate
   --  in name resolution when  compound names are passed to be resolved.
   --
   --  A bind operation that is passed a compound name is defined as follows:
   --  ctx->bind(< c1 ; c2 ; ... ; cn >, obj) <=>
   --  (ctx->resolve(< c1 >))->bind(< c2 ; ... ; cn >, obj)

   procedure Rebind
     (Ctx : access Naming_Context;
      N   : in Name;
      Obj : in Objects.Object_Ref)
     is abstract;
   --  Creates a binding of a name and an object in the  naming context even
   --  if the  name is already bound in the context. Naming contexts that are
   --  bound using  rebind do not participate in name resolution when
   --  compound names are  passed to be resolved.

   procedure Bind_Context
     (Ctx : access Naming_Context;
      N   : in Name;
      NC  : in Naming_Context_Ref)
     is abstract;
   --  Names an object that is a naming context. Naming contexts that are
   --  bound using  bind_context() participate in name resolution when
   --  compound names are  passed to be resolved.
   --
   --  A bind_context operation that is passed a compound name is defined as
   --  follows:
   --
   --  ctx->bind_context(< c1 ; c2 ;  ... ; cn >, nc)  <=>
   --  (ctx->resolve(< c1 >))->bind_context(< c2 ;  ... ; cn-1; cn >, nc )

   procedure Rebind_Context
     (Ctx : access Naming_Context;
      N   : in Name;
      NC  : in Naming_Context_Ref)
     is abstract;
   --  Creates a binding of a name and a naming context in the  naming
   --  context even if the name is already bound in the context. Naming
   --  contexts that are bound using rebind_context() participate in name
   --  resolution when compound names are passed to be resolved.

   --  The resolve operation is the process of retrieving an object bound
   --  to a name in a  given context. The given name must exactly match the
   --  bound name. The naming service does not return the type of the object.

   function Resolve
     (Ctx : access Naming_Context;
      N   : in Name)
      return Objects.Object_Ref
     is abstract;
   --  Names can have multiple components; therefore, name resolution can
   --  traverse multiple  contexts. A compound resolve is defined as follows:
   --
   --  ctx->resolve(< c1 ; c2 ;  ... ; cn >)  <=>
   --  ctx->resolve(< c1 >)->resolve(< c2 ;  ... ; cn >)

   function Resolve
     (Ctx : access Naming_Context;
      N   : in Name)
      return Naming_Context_Ref
     is abstract;
   --  Names can have multiple components; therefore, name resolution can
   --  traverse multiple contexts. A compound resolve is defined as follows:
   --
   --  ctx->resolve(< c1 ; c2 ;  ... ; cn >)  <=>
   --  ctx->resolve(< c1 >)->resolve(< c2 ;  ... ; cn >)

   procedure Unbind
     (Ctx : access Naming_Context;
      N   : in Name)
     is abstract;
   --  The unbind operation removes a name binding from a context. The
   --  definition of the  unbind operation is:
   --
   --  A unbind operation that is passed a compound name is defined as
   --  follows:
   --
   --  ctx->unbind(< c1 ; c2 ; ... ; cn >)  <=>
   --  (ctx->resolve(< c1 >))->unbind(< c2 ; ... ; cn >)

   function New_Context
     (Ctx : access Naming_Context)
      return Naming_Context_Ref
     is abstract;
   --  This operation returns a naming context implemented by the same
   --  naming server as the context on which the operation  was invoked.
   --  The new context is not bound to any name.

   procedure Bind_New_Context
     (Ctx : access Naming_Context;
      N   : in Name)
     is abstract;
   --  This operation creates a new context and binds it to the name
   --  supplied as an argument. The newly-created context is implemented
   --  by the same naming server as the context in which it was bound
   --  (that is, the naming server that implements the context denoted by
   --  the name argument excluding the last component).
   --  A bind_new_context that is passed a  compound name is defined as
   --  follows:
   --
   --  ctx->bind_new_context(< c1 ; c2 ;  ... ; cn >)  <=>
   --  (ctx->resolve(< c1 >))->bind_new_context(< c2 ; ... ; cn >)

   procedure Destroy
     (Ctx : access Naming_Context)
     is abstract;
   --  If the naming context contains bindings, the Not_Empty exception is
   --  raised.

   ----------------------------------
   -- Binding_Iterator Subprograms --
   ----------------------------------

   procedure List
     (Ctx      : access Naming_Context;
      How_Many : in  Natural;
      BL       : out Binding_List;
      BI       : out Binding_Iterator_Ref)
     is abstract;
   --  The list operation allows a client to iterate through a set of
   --  bindings in a naming context.
   --  The list operation returns at most the requested number of bindings in
   --  Binding_List bl.
   --  * If the naming context contains additional bindings, the List
   --  operation returns a Binding_Iterator with the additional bindings.
   --  * If the naming context  does not contain additional bindings, the
   --  binding iterator is a null object reference.

   procedure Next_One
     (BI   : access Binding_Iterator;
      B    : out Binding;
      Done : out Boolean)
     is abstract;
   --  This operation returns the next binding. If there are no more bindings,
   --  false is returned.

   procedure Next_N
     (BI       : access Binding_Iterator;
      How_Many : in Natural;
      BL       : out Binding_List;
      Done     : out Boolean)
     is abstract;
   --  This operation returns at most the requested number of bindings.
   --  If there are no more bindings, false is returned.

   procedure Destroy
     (BI : access Binding_Iterator)
     is abstract;
   --  This operation destroys the iterator.

private

   type Naming_Context is abstract new Objects.Object with null record;
   type Binding_Iterator is abstract tagged limited null record;

end GLADE.Naming.Interface;
