package Broca.Names is

   pragma Pure;

   function OMG_Prefix return String;
   function OMG_Version return String;
   function OMG_RepositoryId (Name : String) return String;

private

   pragma Inline (OMG_Prefix);
   pragma Inline (OMG_Version);
   pragma Inline (OMG_RepositoryId);

end Broca.Names;
