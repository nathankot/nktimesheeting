
ROOT_DIR					:= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

SERVER_SRC				:= $(find server -type f -not -path '*/\.*')
GLOBAL_STACK_ROOT := $(shell stack path --global-stack-root)
RUN_CMD						:= docker run -d \
												-m 512m\
												-v $(GLOBAL_STACK_ROOT):/root/.stack \
												-v $(ROOT_DIR)/server/.stack-work:/opt/server/.stack-work \
												nktimesheeting-build:latest \
												tail -f /dev/null

dist/server: web/dist $(SERVER_SRC)
	@echo "[INFO] Copying web assets into static directory"
	cp -r web/dist server/static
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

WEB_SRC := $(func web -type f -not -path '*/dist*' -not -path '*/node_modules')

web/dist: $(WEB_SRC)
	@echo "[INFO] Building web assets"
	cd web; npm run build