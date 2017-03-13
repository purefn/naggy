VERSION=$(shell grep ^version webapp/naggy.cabal | awk '{print $$2;}')

SRCS=$(shell find webapp/src/ -name *.hs)

.PHONY: clean

# result: release.nix webapp/default.nix ui/default.nix $(SRCS)
result: release.nix webapp/default.nix $(SRCS)
	nix-build -A docker-container release.nix

docker: result
	docker load -i result
	docker tag naggy docker.atlassian.io/rwallace/naggy:$(VERSION)
	docker push docker.atlassian.io/rwallace/naggy:$(VERSION)

ddev: docker naggy.sd.yml
	DOCKER_TAG=$(VERSION) micros service:deploy naggy -f naggy.sd.yml -e ddev

clean:
	rm result
