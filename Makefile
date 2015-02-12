PROJECT=erldocs

DEPS=forms meta
dep_forms= git https://github.com/efcasado/forms master
dep_meta= git https://github.com/efcasado/meta master

TEST_DIR=tests

include erlang.mk
