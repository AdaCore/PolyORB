------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . R E Q U E S T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package body CORBA.Request is

   -------------
   -- Add_Arg --
   -------------

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue)
   is
   begin
      null;
   end Add_Arg;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags)
   is
   begin
      null;
   end Invoke;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Object)
   is
   begin
      null;
   end Delete;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags)
   is
   begin
      null;
   end Send;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Self           : in out Object;
      Response_Flags : in     Flags)
   is
   begin
      null;
   end Get_Response;

   ----------------------------------------
   --  implementation defined functions  --
   ----------------------------------------

   -----------
   --  set  --
   -----------

   procedure Set
     (Self       :    out CORBA.Request.Object;
      Ctx        : in     CORBA.Context.Object;
      Operation  : in     CORBA.Identifier;
      Arg_List   : in     CORBA.NVList.Object;
      Result     : access CORBA.NamedValue;
      Req_Flags  : in     CORBA.Flags) is
   begin
      Self := (null, --  contexts used not implemented at all for now
               Operation,
               Arg_List,
               Ptr (Result),
               Req_Flags);
   end Set;



end CORBA.Request;
