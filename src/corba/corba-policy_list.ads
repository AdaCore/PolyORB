with CORBA.Policy_Types; use CORBA.Policy_Types;
with CORBA.Policy;       use CORBA.Policy;

package CORBA.Policy_List is

   type Policy_List is array (PolicyType
                             range PolicyType (THREAD_POLICY_ID) ..
                             PolicyType (REQUEST_PROCESSING_POLICY_ID))
     of Policy_Ptr;
   type Policy_List_Ptr is access Policy_List;

end CORBA.Policy_List;
