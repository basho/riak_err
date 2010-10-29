%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
{application, riak_err,
 [
  {description, "Custom error handler"},
  {vsn, "0.1.0"},
  {modules, [
             riak_err_app
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl
                 ]},
  {registered, []},
  {mod, {riak_err_app, []}},
  {env, [
        ]}
 ]}.
