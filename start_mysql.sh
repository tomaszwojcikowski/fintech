#!/bin/bash

CN="fintech-mysql"

docker rm "/$CN" -f
docker run --name $CN \
-p 33061:3306 \
-e MYSQL_ROOT_PASSWORD=fintech \
-e MYSQL_PASSWORD=fintech \
-e MYSQL_USER=fintech \
-e MYSQL_DATABASE=fintech \
-v "$(pwd)/priv/:/docker-entrypoint-initdb.d/" \
--health-cmd='mysqladmin ping --silent' \
-d mysql
echo "Waiting for mysql container"
while [ docker inspect '$CN' 2>/dev/null ]; do printf "."; sleep 1; done
echo "Waiting for mysql health status"
while [ $(docker inspect --format "{{json .State.Health.Status }}" 'fintech-mysql') != "\"healthy\"" ]; do printf "."; sleep 1; done
echo ""
echo "mysql is ready"