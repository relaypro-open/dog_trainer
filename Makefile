HOSTNAME=github.com
NAMESPACE=relaypro-open
NAME=dog_trainer
BINARY=${NAME}
VERSION=v1.4.11
OS_ARCH=linux_amd64

default: install

build:
	rebar3 compile

release:
	rebar3 tar as public

github_release:
	git tag ${VERSION}
	git push --tags --force

e2e:
	./test/e2e/run.sh

delete_release:
	git tag -d ${VERSION}
	git push --delete origin ${VERSION}
