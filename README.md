mod_custom_msg
==============

Ejabberd mod_custom_msg

Installation
============
  * Download mod_log_chat_mysql5 : git clone git@github.com:trepa/mod_custom_msg.git
  * Move the mod_custom_msg directory into the root of the ejabberd-modules folder
  * Navigate to the mod_custom_msg directory in the ejabberd-modules folder and call ./build.sh
  * If successful the module has been compiled and output to ebin/mod_custom_msg.beam. Copy this file to your ejabberd system ebin folder folder (e.g. /usr/lib/ejabberd/ebin on Debian)

Config
============
```{mod_custom_msg, []}```


Execute XML-RPC
============
send_like
```Struct(Integer res) send_like(Struct(String JID, String MSG_ID, String STATUS))```