--  $Id$

with Ada.Unchecked_Deallocation;
with Droopi.Soft_Links;

package body Droopi.Jobs is

   procedure Free is new Ada.Unchecked_Deallocation
     (Queue_Element, Queue_Element_Access);


   procedure Create (Q : out Job_Queue_Access) is
   begin
      Q := new Job_Queue;

      Q.First := null;
      Q.Last  := null;
   end Create;

   procedure Queue_Job
     (Q : access Job_Queue;
      J : Job_Access)
   is
      E : Queue_Element_Access
        := new Queue_Element'(Next => null, Job  => J);
   begin
      if Q.Last = null then
         pragma Assert (Q.First = null);
         Q.First := E;
         Q.Last  := E;
      else
         Q.Last.Next := E;
         Q.Last := E;
      end if;
   end Queue_Job;

   function Empty (Q : access Job_Queue) return Boolean is
   begin
      return Q.First = null;
   end Empty;

   function Fetch_Job (Q : access Job_Queue) return Job_Access is
      First  : Queue_Element_Access := Q.First;
      Result : Job_Access := null;
   begin
      if First /= null then
         Result := First.Job;
         Q.First := First.Next;
         if Q.First = null then
            Q.Last := null;
         end if;

         Free (First);
      end if;

      return Result;
   end Fetch_Job;

end Droopi.Jobs;
