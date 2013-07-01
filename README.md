erlang-seorsus
==============

Yet another wrapper around amqp_client.

See examples directory to get idea how to use.
-------------------------
The main idea is that you use seorsus as included application, running `seorsus_sup` in your supervision tree. You also have to provide connection specifications (where is your server to connect), channel specifications (how many channel you wish to create for the connection), and consumer specifications (the rules for create exchanges, queues make bindings and process name to which parsed messages to be sent).

Pull-requests are welcome
-------------------------
This is beta software, written in couple of hours. 
