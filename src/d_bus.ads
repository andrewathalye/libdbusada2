package D_Bus is
private
   Protocol_Error : exception;

   Initialisation_Required : exception;
   --  Raised if a user attempts to create an object
   --  manually without using the provided constructor.
   --  Any objects this applies to are explicitly marked.
end D_Bus;
