--  $Id$

with Droopi.Soft_Links;

package body Droopi.ORB is
   
   use Droopi.Asynchronous_Events;
   use Droopi.Jobs;
   use Droopi.Soft_Links;
   
   procedure Create (O : out ORB_Access) is
   begin
      O := new ORB;
      
      Create (O.Job_Queue);
      Create (O.Idle_Tasks);
      
      O.Shutdown := False;
      O.Polling  := False;
   end Create;
   
   procedure Start (O : access ORB);
   
   procedure Start (O : access ORB) is
   begin
      --  Start accepting incoming connections.
      raise Not_Implemented;
   end Start;
   
   function Try_Perform_Work (Q : access Job_Queue) return Boolean;
   --  Perform one item of work from Q, if available.
   --  Precondition: This function must be called from within a
   --    critical section.
   --  Postcondition: if a job has been executed, then the critical
   --    section has been left, and True is returned.
   --    If no job was available, the critical section is not left,
   --    and False is returned.
   
   function Try_Perform_Work (Q : access Job_Queue) return Boolean is
   begin
      if not Empty (Q) then
	 declare
	    Job : Job_Access := Fetch_Job (Q);
	 begin
	    Leave_Critical_Section;
	    
	    pragma Assert (Job /= null);
	    Run (Job);
	    return True;
	 end;
      else
	 return False;
      end if;
   end Try_Perform_Work;
	 
   procedure Run
     (O              : access ORB;
      Exit_Condition : Exit_Condition_Access := null;
      Blocker        : AES_Access := null) is
   begin
      --  Ensure the ORB is accepting incoming connections
      --  (if this is a server...)
      raise Not_Implemented;
      
      loop
	 Enter_Critical_Section;
	 
	 if (Exit_Condition /= null and then Exit_Condition.all)
	   or else O.Shutdown then
	    Leave_Critical_Section;
	    exit;
	 end if;
	   
	 if Try_Perform_Work (O.Job_Queue) then
	    null;
	 elsif Blocker = null or else Polling (Blocker) then
	    
	    --  This task is going idle.
	    --  It should first ask for persmission to
	    --  do so from the tasking policy object.
	    
	    Leave_Critical_Section;
	    
	    Wait (O.Idle_Tasks);
	   
	 else
	    Set_Polling (Blocker, True);
	    Leave_Critical_Section;
	    
	    Poll (Blocker);
	    
	    Enter_Critical_Section;
	    Set_Polling (Blocker, False);
	    Handle_Events (Blocker);
	    Leave_Critical_Section;
	 end if;
      end loop;
   end Run;

   function Work_Pending (O : access ORB) return Boolean
   is
      Result : Boolean;
   begin
      Enter_Critical_Section;
      Result := not Empty (O.Job_Queue);
      Leave_Critical_Section;
      return Result;
   end Work_Pending;
   
   procedure Perform_Work (O : access ORB) is
   begin
      Enter_Critical_Section;
      if not Try_Perform_Work (O.Job_Queue) then
	 Leave_Critical_Section;
      end if;
   end Perform_Work;

   procedure Shutdown
     (O                   : access ORB;
      Wait_For_Completion : Boolean := True) is
   begin
      
      --  Stop accepting incoming connections.
      raise Not_Implemented;
      
      if Wait_For_Completion then
	 raise Not_Implemented;
      end if;
      
      Enter_Critical_Section;
      O.Shutdown := True;
      Leave_Critical_Section;
      
   end Shutdown;

end Droopi.ORB;
