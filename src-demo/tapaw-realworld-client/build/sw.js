self.addEventListener("install", (function(jmId_0)
                                  {
                                    jmId_0.waitUntil(caches.open("precache").
                                    then((function(jmId_1)
                                          {
                                            return jmId_1.addAll(["\/",
                                            "\/sw.js", "\/all.js"]);
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
                                  if(((true && true) && !jmId_6[0]))
                                  {
                                    var jmId_8;
                                    jmId_8 = fetch(jmId_3).
                                    then((function(jmId_9)
                                          {
                                            return caches.open("precache").
                                            then((function(jmId_10)
                                                  {
                                                    return jmId_10.put(jmId_3,
                                                    jmId_9.clone());
                                                  })).
                                            then((function()
                                                  {
                                                    return jmId_9;
                                                  }));
                                          }), (function()
                                               {
                                                 return undefined;
                                               }));
                                    jmId_2.waitUntil(jmId_8);
                                    return jmId_2.respondWith(caches.
                                    open("precache").
                                    then((function(jmId_11)
                                          {
                                            return jmId_11.match(jmId_3);
                                          })).
                                    then((function(jmId_12)
                                          {
                                            return (jmId_12 || jmId_8);
                                          })));
                                  };
                                  if(((true && true)
                                  &&
                                  (("sw.js" == jmId_6[0]) && !jmId_6[1])))
                                  {
                                    var jmId_13;
                                    jmId_13 = fetch(jmId_3).
                                    then((function(jmId_14)
                                          {
                                            return caches.open("precache").
                                            then((function(jmId_15)
                                                  {
                                                    return jmId_15.put(jmId_3,
                                                    jmId_14.clone());
                                                  })).
                                            then((function()
                                                  {
                                                    return jmId_14;
                                                  }));
                                          }), (function()
                                               {
                                                 return undefined;
                                               }));
                                    jmId_2.waitUntil(jmId_13);
                                    return jmId_2.respondWith(caches.
                                    open("precache").
                                    then((function(jmId_16)
                                          {
                                            return jmId_16.match(jmId_3);
                                          })).
                                    then((function(jmId_17)
                                          {
                                            return (jmId_17 || jmId_13);
                                          })));
                                  };
                                  if(((true && true)
                                  &&
                                  (("all.js" == jmId_6[0]) && !jmId_6[1])))
                                  {
                                    var jmId_18;
                                    jmId_18 = fetch(jmId_3).
                                    then((function(jmId_19)
                                          {
                                            return caches.open("precache").
                                            then((function(jmId_20)
                                                  {
                                                    return jmId_20.put(jmId_3,
                                                    jmId_19.clone());
                                                  })).
                                            then((function()
                                                  {
                                                    return jmId_19;
                                                  }));
                                          }), (function()
                                               {
                                                 return undefined;
                                               }));
                                    jmId_2.waitUntil(jmId_18);
                                    return jmId_2.respondWith(caches.
                                    open("precache").
                                    then((function(jmId_21)
                                          {
                                            return jmId_21.match(jmId_3);
                                          })).
                                    then((function(jmId_22)
                                          {
                                            return (jmId_22 || jmId_18);
                                          })));
                                  };
                                  if(((true && true)
                                  &&
                                  /.*firebaseio.*/.test(jmId_3.url)))
                                  {
                                    return jmId_2.respondWith(caches.
                                    open("firebase").
                                    then((function(jmId_23)
                                          {
                                            return jmId_23.match(jmId_3).
                                            then((function(jmId_24)
                                                  {
                                                    return (jmId_24
                                                    ||
                                                    fetch(jmId_3).
                                                    then((function(jmId_25)
                                                          {
                                                            jmId_2.
                                                            waitUntil(jmId_23.
                                                            put(jmId_3, jmId_25.
                                                            clone()));
                                                            return jmId_25;
                                                          })));
                                                  }));
                                          })));
                                  };
                                }));
self.addEventListener("push", (function(jmId_26)
                               {
                                 var jmId_27;
                                 jmId_27 = jmId_26.data.json();
                                 jmId_26.waitUntil(self.registration.
                                 showNotification(jmId_27.title,
                                 { 'body': jmId_27.body
                                 }));
                               }));
self.addEventListener("notificationclick", (function(jmId_28)
                                            {
                                              jmId_28.waitUntil(self.clients.
                                              matchAll().
                                              then((function(jmId_29)
                                                    {
                                                      if((jmId_29.length > 0))
                                                      {
                                                        return jmId_29[0].
                                                        focus();
                                                      };
                                                      return self.clients.
                                                      openWindow("http:\/\/localhost:8000\/");
                                                    })));
                                            }));