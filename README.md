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


= Flow

Incometh is a request, r

r is received by warp, which will use the hostname and url (henceforth "site")
to determine the backend which will process the request. Since each site can
have multiple backends, the request will first be queued in the site-global
queue, and when a backend is ready the MVar (which warp originally added to
the queue) will be made to contain the backend to use.
Instead of passing the actual request through the queue, the queue is
only used as a stop-and-go sign. This makes the code more straightforward since
the queueing code is boiled down to a few calls from the processing code.