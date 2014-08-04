PROJECT = ecoveralls
PROJECT_VERSION = 0.1

DEPS = jsx hackney
dep_jsx = git https://github.com/talentdeficit/jsx v2.0.4
dep_hackney = git https://github.com/benoitc/hackney 0.13.0

TEST_DEPS = nifoc_ct_helper
dep_nifoc_ct_helper = git https://github.com/nifoc/nifoc_ct_helper master

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

TEST_ERLC_OPTS ?= +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

PLT_APPS = asn1 crypto public_key ssl inets

CT_SUITES = eunit cover
CT_OPTS = -ct_hooks nifoc_ct_hook [] -cover ./test/cover.spec

include erlang.mk

define upload_coverage
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval ' \
		ok = ecoveralls:start(), \
		ok = ecoveralls:report("$(1)", [{service_name, <<"travis-ci">>}, {service_job_id, <<"$(TRAVIS_JOB_ID)">>}]), \
		init:stop()'
endef

coverage-report: $(shell find logs -type f -name \*.coverdata 2>/dev/null | tail -n1)
	$(if $(strip $?),$(call upload_coverage,$?))

.PHONY: coverage-report
