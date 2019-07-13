PROJECT = jo
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy cowlib ranch pgo backoff opencensus wts ctx jsx counters rfc3339 proper edown erl_img erlang_magic gen_smtp telemetry havoc pg_types

dep_cowboy = cp /var/www/erl_pkgs/cowboy
dep_cowlib = cp /var/www/erl_pkgs/cowlib
dep_ranch = cp /var/www/erl_pkgs/ranch
dep_cowboy_commit = master

dep_backoff = cp /var/www/erl_pkgs/backoff
dep_opencensus = cp /var/www/erl_pkgs/opencensus
dep_wts = cp /var/www/erl_pkgs/wts
dep_ctx = cp /var/www/erl_pkgs/ctx
dep_jsx = cp /var/www/erl_pkgs/jsx
dep_pgo = cp /var/www/erl_pkgs/pgo
dep_counters = cp /var/www/erl_pkgs/counters.erl
dep_edown = cp /var/www/erl_pkgs/edown 
dep_rfc3339 = cp /var/www/erl_pkgs/rfc3339
dep_proper = cp /var/www/erl_pkgs/proper
dep_telemetry = cp /var/www/erl_pkgs/telemetry
dep_havoc = cp /var/www/erl_pkgs/havoc
dep_pg_types = cp /var/www/erl_pkgs/pg_types

dep_crontab = cp /var/www/erl_pkgs/crontab
dep_stdlib2 = cp /var/www/erl_pkgs/stdlib2
dep_cronjobs = cp /var/www/erl_pkgs/cronjobs

dep_erl_img = cp /var/www/erl_pkgs/erl_img
dep_erlang_magic = cp /var/www/erl_pkgs/erlang_magic

dep_gen_smtp = cp /var/www/erl_pkgs/gen_smtp

BUILD_DEPS = reload_mk elvis_mk
DEP_PLUGINS = reload_mk elvis_mk
dep_reload_mk = cp /var/www/erl_pkgs/reload.mk
dep_elvis_mk = cp /var/www/erl_pkgs/elvis.mk

LOCAL_DEPS = inets ssl

#to disable dev mode and prevent symlinking. 
#RELX_OPTS = -d false


include erlang.mk
