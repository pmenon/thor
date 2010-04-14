{application, thor,
    [{description, "THOR owns you!"},
    {vsn, "1.0"},
    {modules, [ thor_sup,
                thor,
                thor_server,
                thor_socket ]},
    {registered, [ thor_sup ]},
	{applications, [kernel, stdlib]},
	{mod, {thor, []}},
	{env, [{conf, "/conf/storage.properties"}]}
 ]}.
