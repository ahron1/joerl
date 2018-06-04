PROJECT = jo
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy cowlib ranch pgo backoff opencensus wts ctx jsx counters rfc3339 crontab stdlib2 cronjobs edown proper 
dep_cowboy = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/cowboy
dep_cowlib = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/cowlib
dep_ranch = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/ranch
dep_cowboy_commit = master

dep_pgo = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/pgo
dep_backoff = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/backoff
dep_opencensus = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/opencensus
dep_wts = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/wts
dep_ctx = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/ctx
dep_jsx = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/jsx
dep_counters = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/counters.erl
dep_edown = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/edown 
dep_rfc3339 = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/rfc3339
dep_proper = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/proper

dep_crontab = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/crontab
dep_stdlib2 = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/stdlib2
dep_cronjobs = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/cronjobs

#dep_gen_smtp = cp /usr/home/yc/vm_shared_dir/cowboy_servers/erl_pkgs/gen_smtp

BUILD_DEPS = reload_mk elvis_mk
DEP_PLUGINS = reload_mk elvis_mk
dep_reload_mk = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/reload.mk
dep_elvis_mk = cp ~/vm_shared_dir/cowboy_servers/erl_pkgs/elvis.mk

#LOCAL_DEPS = inets ssl

#to disable dev mode and prevent symlinking. 
#RELX_OPTS = -d false


include erlang.mk
