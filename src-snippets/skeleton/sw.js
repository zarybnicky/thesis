self.addEventListener("install", (function(jmId_0)
                                  {
                                    jmId_0.waitUntil(caches.open("precache").
                                    then((function(jmId_1)
                                          {
                                            return jmId_1.addAll([]);
                                          })));
                                  }));
self.addEventListener("fetch", (function(jmId_2)
                                {
                                  var jmId_3;
                                  jmId_3 = jmId_2.request;
                                  var jmId_4;
                                  jmId_4 = jmId_3.method;
                                  var jmId_5;
                                  jmId_5 = new URL(jmId_3.url);
                                  var jmId_6;
                                  jmId_6 = jmId_5.pathname.substr(1).
                                  split("\/");
                                  var jmId_7;
                                  jmId_7 = jmId_5.searchParams;
                                }));
self.addEventListener("push", (function(jmId_8)
                               {
                                 var jmId_9;
                                 jmId_9 = jmId_8.data.json();
                                 jmId_8.waitUntil(self.registration.
                                 showNotification(jmId_9.title,
                                 { 'body': jmId_9.body
                                 }));
                               }));
self.addEventListener("notificationclick", (function(jmId_10)
                                            {
                                              jmId_10.waitUntil(self.clients.
                                              matchAll().
                                              then((function(jmId_11)
                                                    {
                                                      if((jmId_11.length > 0))
                                                      {
                                                        return jmId_11[0].
                                                        focus();
                                                      };
                                                      return self.clients.
                                                      openWindow("http:\/\/localhost:3000");
                                                    })));
                                            }));