pragma Ada_2012;

package D_Bus.Errors is
   --  Standard D-Bus Errors. These are not defined in the specfiication,
   --  but sd-bus provides the following and we use some of them. A binding
   --  may choose to implement some, all, or none of these as Ada exceptions.
   pragma Pure (D_Bus.Errors);

   Failed                             : constant String;
   No_Memory                          : constant String;
   Service_Unknown                    : constant String;
   Name_Has_No_Owner                  : constant String;
   No_Reply                           : constant String;
   IO_Error                           : constant String;
   Bad_Address                        : constant String;
   Not_Supported                      : constant String;
   Limits_Exceeded                    : constant String;
   Access_Denied                      : constant String;
   Auth_Failed                        : constant String;
   No_Server                          : constant String;
   Timeout                            : constant String;
   No_Network                         : constant String;
   Address_In_Use                     : constant String;
   Disconnected                       : constant String;
   Invalid_Args                       : constant String;
   File_Not_Found                     : constant String;
   File_Exists                        : constant String;
   Unknown_Method                     : constant String;
   Unknown_Object                     : constant String;
   Unknown_Interface                  : constant String;
   Unknown_Property                   : constant String;
   Property_Read_Only                 : constant String;
   Unix_Process_Id_Unknown            : constant String;
   Invalid_Signature                  : constant String;
   Inconsistent_Message               : constant String;
   Timed_Out                          : constant String;
   Match_Rule_Not_Found               : constant String;
   Match_Rule_Invalid                 : constant String;
   Interactive_Authorization_Required : constant String;
   Invalid_File_Content               : constant String;
   SELinux_Security_Context_Unknown   : constant String;
   Object_Path_In_Use                 : constant String;
private
   Failed : constant String := "org.freedesktop.DBus.Error.Failed";
   No_Memory : constant String := "org.freedesktop.DBus.Error.NoMemory";
   Service_Unknown                    : constant String :=
     "org.freedesktop.DBus.Error.ServiceUnknown";
   Name_Has_No_Owner                  : constant String :=
     "org.freedesktop.DBus.Error.NameHasNoOwner";
   No_Reply : constant String := "org.freedesktop.DBus.Error.NoReply";
   IO_Error : constant String := "org.freedesktop.DBus.Error.IOError";
   Bad_Address : constant String := "org.freedesktop.DBus.Error.BadAddress";
   Not_Supported                      : constant String :=
     "org.freedesktop.DBus.Error.NotSupported";
   Limits_Exceeded                    : constant String :=
     "org.freedesktop.DBus.Error.LimitsExceeded";
   Access_Denied                      : constant String :=
     "org.freedesktop.DBus.Error.AccessDenied";
   Auth_Failed : constant String := "org.freedesktop.DBus.Error.AuthFailed";
   No_Server : constant String := "org.freedesktop.DBus.Error.NoServer";
   Timeout : constant String := "org.freedesktop.DBus.Error.Timeout";
   No_Network : constant String := "org.freedesktop.DBus.Error.NoNetwork";
   Address_In_Use                     : constant String :=
     "org.freedesktop.DBus.Error.AddressInUse";
   Disconnected : constant String := "org.freedesktop.DBus.Error.Disconnected";
   Invalid_Args : constant String := "org.freedesktop.DBus.Error.InvalidArgs";
   File_Not_Found                     : constant String :=
     "org.freedesktop.DBus.Error.FileNotFound";
   File_Exists : constant String := "org.freedesktop.DBus.Error.FileExists";
   Unknown_Method                     : constant String :=
     "org.freedesktop.DBus.Error.UnknownMethod";
   Unknown_Object                     : constant String :=
     "org.freedesktop.DBus.Error.UnknownObject";
   Unknown_Interface                  : constant String :=
     "org.freedesktop.DBus.Error.UnknownInterface";
   Unknown_Property                   : constant String :=
     "org.freedesktop.DBus.Error.UnknownProperty";
   Property_Read_Only                 : constant String :=
     "org.freedesktop.DBus.Error.PropertyReadOnly";
   Unix_Process_Id_Unknown            : constant String :=
     "org.freedesktop.DBus.Error.UnixProcessIdUnknown";
   Invalid_Signature                  : constant String :=
     "org.freedesktop.DBus.Error.InvalidSignature";
   Inconsistent_Message               : constant String :=
     "org.freedesktop.DBus.Error.InconsistentMessage";
   Timed_Out : constant String := "org.freedesktop.DBus.Error.TimedOut";
   Match_Rule_Not_Found               : constant String :=
     "org.freedesktop.DBus.Error.MatchRuleNotFound";
   Match_Rule_Invalid                 : constant String :=
     "org.freedesktop.DBus.Error.MatchRuleInvalid";
   Interactive_Authorization_Required : constant String :=
     "org.freedesktop.DBus.Error.InteractiveAuthorizationRequired";
   Invalid_File_Content               : constant String :=
     "org.freedesktop.DBus.Error.InvalidFileContent";
   SELinux_Security_Context_Unknown   : constant String :=
     "org.freedesktop.DBus.Error.SELinuxSecurityContextUnknown";
   Object_Path_In_Use                 : constant String :=
     "org.freedesktop.DBus.Error.ObjectPathInUse";
end D_Bus.Errors;
