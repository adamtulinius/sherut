sherut
======

State: Idea/proof-of-concept phase.

Sherut is (going to be) my take on how i want to host my web-applications.

Sherut is (going to be) split into two parts:
* a http proxy
* a webservice which will "host" the actual web applications.

Now you might ask "why make yet another http proxy?", to which i'll
answer this: Because i want the proxy to be able to request additional
workers to start up on machines running the sherut-webservice automatically,
and because i think it's interesting to work with.