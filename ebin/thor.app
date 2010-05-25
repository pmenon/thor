{application, thor,
    [{description, "THOR owns you!"},
    {vsn, "1.0"},
    {modules, [ thor_sup,
                thor,
                thor_server,
                thor_socket ]},
    {registered, [ thor_sup ]},
	{applications, [kernel, stdlib]},
	{mod, {thor, [ "/Users/menonp/development/thor/conf/thor.conf" ]}},
	{env, [{conf, "/conf/thor.conf"}]}
 ]}.
