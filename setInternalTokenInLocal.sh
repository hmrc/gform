#!/usr/bin/env bash

echo 'Creating new token:'

curl -X POST -H 'Content-Type: application/json' -d '{
  "token": "<INSERT-VALUE>",
  "principal": "gform-dev-token",
  "permissions": [
    {
      "resourceType": "object-store-admin-frontend",
      "resourceLocation": "*",
      "actions": [
        "*"
      ]
    },
    {
      "resourceType": "object-store",
      "resourceLocation": "gform",
      "actions": [
        "*"
      ]
    },
    {
      "resourceType": "object-store",
      "resourceLocation": "submission-consolidator",
      "actions": [
        "*"
      ]
    },
    {
      "resourceType": "eeitt-admin-frontend",
      "resourceLocation": "*",
      "actions": [
        "*"
      ]
    }
  ]
}' http://localhost:8470/test-only/token -H ... 2>/dev/null

echo $'\nCreated token info:'

curl http://localhost:8470/test-only/token -H 'Authorization: <INSERT-VALUE>' ... 2>/dev/null

echo $'\nVisit http://localhost:8467/object-store-admin-frontend to see object store data'
