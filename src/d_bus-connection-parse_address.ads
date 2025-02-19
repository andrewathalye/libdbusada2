pragma Ada_2012;
function D_Bus.Connection.Parse_Address
     (Mode : Mode_Type; Addr : Server_Address)
      return GNAT.Sockets.Socket_Set_Type;
   --  Parse a given address and return a set of populated sockets
   --  ready to authenticate. Any erroneous address entries will
   --  be ignored unless they are syntactically invalid.
   --
   --  May raise Transport_Error, Address_Error
