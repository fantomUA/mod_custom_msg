mod_custom_msg
==============

Ejabberd mod_custom_msg

Installation
============
  * Move the mod_custom_msg directory into the root of the ejabberd-contrib folder
  * Navigate to the mod_custom_msg directory in the ejabberd-contrib folder and call ./build.sh
  * If successful the module has been compiled and output to ebin/mod_custom_msg.beam. Copy this file to your ejabberd system ebin folder folder (e.g. /usr/lib/ejabberd/ebin on Debian)

Config
============
CFG  
```{mod_custom_msg, []}```  
YAML  
```mod_custom_version: {}```  

Execute XML-RPC
============
send_like - Send notify about liked message   
```Struct(Integer res) send_like(Struct(String TOJID, String MSG_ID, String STATUS, String USERNAME, String TIMESTAMP,  String JID))```  
send_flag - Send notify about flagged message  
```Struct(Integer res) send_like(Struct(String TOJID, String MSG_ID, String STATUS, String USERNAME, String TIMESTAMP))```  
send_expire - Send notify about expired room  
```Struct(Integer res) send_like(Struct(String TOJID, String NAME  String JID, String STATUS, String TIMESTAMP))```  