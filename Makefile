.PHONY: mysql

mysql:
	docker run --name fintech-mysql \
	-p 33061:3306 \
	-e MYSQL_ROOT_PASSWORD=fintech \
	-e MYSQL_USER=fintech \
    -e MYSQL_DATABASE=fintech \
	-v "${CURDIR}/priv/:/docker-entrypoint-initdb.d/" \
	--health-cmd='mysqladmin ping --silent' \
	-d --rm mysql