--
--  $Id$
--

with Ada.Unchecked_Deallocation;

package body System.Garlic.Utils is

   use Ada.Streams;

   Node_Size : constant Stream_Element_Count := 4096;

   type Node;
   type Node_Ptr is access all Node;

   type Node is record
      Content : Stream_Element_Array (1 .. Node_Size);
      Last    : Stream_Element_Offset;
      Next    : Node_Ptr;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   -------------
   -- Barrier --
   -------------

   protected body Barrier is

      ------------
      -- Signal --
      ------------

      procedure Signal (How_Many : Positive := 1) is
      begin
         if not Perm then
            Free := Free + How_Many;
         end if;
      end Signal;

      ----------------
      -- Signal_All --
      ----------------

      procedure Signal_All (Permanent : Boolean) is
      begin
         if not Perm then
            if Permanent then
               Perm := True;
            else
               Free := Free + Wait'Count;
            end if;
         end if;
      end Signal_All;

      ----------
      -- Wait --
      ----------

      entry Wait when Perm or else Free > 0 is
      begin
         if not Perm then
            Free := Free - 1;
         end if;
      end Wait;

   end Barrier;

   ---------------------------
   -- To_Params_Stream_Type --
   ---------------------------

   procedure To_Params_Stream_Type
     (Content : Stream_Element_Array;
      Params  : access System.RPC.Params_Stream_Type)
   is
   begin
      System.RPC.Write (Params.all, Content);
   end To_Params_Stream_Type;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Params : access System.RPC.Params_Stream_Type)
      return Stream_Element_Array
   is
      First   : Node_Ptr := new Node;
      Current : Node_Ptr := First;
      Total   : Stream_Element_Count := 0;
   begin
      loop
         System.RPC.Read (Params.all, Current.Content, Current.Last);
         Total := Total + Current.Last;
         exit when Current.Last < Node_Size;
         Current.Next := new Node;
         Current := Current.Next;
      end loop;
      declare
         Result : Stream_Element_Array (1 .. Total);
         Index  : Stream_Element_Offset := 1;
      begin
         Current := First;
         while Current /= null loop
            Result (Index .. Index + Current.Last - 1) :=
              Current.Content (1 .. Current.Last);
            Index := Index + Current.Last;
            First := Current.Next;
            Free (Current);
            Current := First;
         end loop;
         return Result;
      end;
   end To_Stream_Element_Array;

end System.Garlic.Utils;
