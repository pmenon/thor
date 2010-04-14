-record(server_conf, { name,               %% The name that is assigned to this server
                       port,               %% The port this server is listening on
                       callback            %% The callback module that is run on a new request
                     }).

-record(req, {socket,
              connection = keep_alive,      %% keep_alive or close
              content_length,               %% integer  
              vsn,                          %% {Major, Minor}
              method,                       %% 'GET' or 'POST'
              uri,                          %% Truncated URI '/index.html'
              args="",                      %% Part of the URI after the '?'
              headers,                      %% {Key, Value} HTTP headers
              body = <<>>}).                %% Content body
