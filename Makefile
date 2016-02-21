
ROOT_DIR					:= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

SERVER_SRC				:= $(shell find server -type f -not -path '*/\.*')
GLOBAL_STACK_ROOT := $(shell stack path --global-stack-root)
RUN_CMD						:= docker run -d \
												-m 512m\
												-v $(GLOBAL_STACK_ROOT):/root/.stack \
												-v $(ROOT_DIR)/server/.stack-work:/opt/server/.stack-work \
												nktimesheeting-build:latest \
												tail -f /dev/null

.PHONY: build
build: dist/server dist/migrations

dist/server: server/static $(SERVER_SRC)
	@echo "[INFO] Building the server on a cedar-like docker instance"
	@mkdir -p $(@D)
	@echo "[INFO] Bootstrapping image for build"; \
		docker build -t nktimesheeting-build:latest --file server/Dockerfile.build server; \
		echo "[INFO] Starting a build container"; \
		CONTAINER_ID=`${RUN_CMD}`; \
		echo "[INFO] Building binary on docker container: $$CONTAINER_ID"; \
		docker exec $$CONTAINER_ID stack setup --allow-different-user && \
		docker exec $$CONTAINER_ID stack install --allow-different-user && \
		echo "[INFO] Copying built binary to host (this machine)" && \
		docker cp $${CONTAINER_ID}:/root/.local/bin/server $@; \
		echo "[INFO] Tearing down the build container"; \
		docker stop $$CONTAINER_ID

dist/migrations: $(SERVER_SRC)
	cp -r server/migrations dist/migrations

server/static: dist/static
	@echo "[INFO] Copying web assets into static directory"
	cp -r dist/static $@

WEB_SRC := $(func web -type f -not -path '*/dist*' -not -path '*/node_modules')

dist/static: $(WEB_SRC)
	@echo "[INFO] Building web assets"
	cd web; npm run build
	cp -r web/dist $@
