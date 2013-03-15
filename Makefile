TAGS:
	find . -name '*.hs' | xargs hasktags -e -o - > TAGS

AGENTS = $(HOME)/Library/LaunchAgents

tests:
	if ! ps ax | grep -q '[b]in/redis-server'; then			\
	    launchctl load -w $(AGENTS)/homebrew.mxcl.redis.plist ;	\
	fi
	if ! ps ax | grep -q '[m]ock_s3\.py'; then			\
	    python vendor/mock-s3/mock_s3.py ;				\
	fi
	perl -i -pe 's/^-- //;' gitlib-github/gitlib-github.cabal
	for i in cmdline github libgit2 s3 ; do				\
	    (echo gitlib-$$i ; cd gitlib-$$i ;				\
	     cabal clean > /dev/null ;					\
	     cabal configure --enable-tests > /dev/null ;		\
	     cabal build > /dev/null &&					\
	     echo === $$i === &&					\
	     GITHUB_OWNER=$(GITHUB_OWNER) GITHUB_TOKEN=$(GITHUB_TOKEN)	\
	     dist/build/smoke/smoke) ;					\
	done
	for i in test sample ; do					\
	    (echo gitlib-$$i ; cd gitlib-$$i ;				\
	     cabal clean > /dev/null ;					\
	     cabal configure --enable-tests > /dev/null ;		\
	     cabal build) ;						\
	done
