.PHONY: test
test: $(shell find src -name '*.sml') isoparta.cm $(shell find test -name '*.sml') test/tests.cm
	cd test; \
	SMLNJ_HOME=/usr/lib/smlnj ml-build -Cparser.succ-ml=true tests.cm Tests.main tests && \
	sml @SMLload=tests.amd64-linux; \
        cd ..

