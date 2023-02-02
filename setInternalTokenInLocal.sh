#!/usr/bin/env bash
curl -X PUT -H 'Content-Type: application/json' -d '[
                                                      {
                                                        "resourceType": "internal-auth",
                                                        "actions": [
                                                          "READ",
                                                          "WRITE",
                                                          "DELETE"
                                                        ]
                                                      },
                                                      {
                                                        "resourceType": "object-store",
                                                        "actions": [
                                                          "READ",
                                                          "WRITE",
                                                          "DELETE"
                                                        ]
                                                      }
                                                    ]' http://localhost:8470/test-only/resource-definitions

response=$(curl -X POST -H 'Content-Type: application/json' -d '{"principal": "gform",
                                "permissions": [
                                    {
                                        "resourceType": "object-store",
                                        "resourceLocation": "*",
                                        "actions": [
                                            "*"
                                        ]
                                    }
                                ]
                            }' http://localhost:8470/test-only/token -H ... 2>/dev/null)
token=$(echo $response | grep -o '"token":"[^"]*' | grep -o '[^"]*$')
sed -i -e 's/<INSERT-INTERNAL-AUTH>/'$token'/g' ./conf/application.conf