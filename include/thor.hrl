-record(server_conf, { name,               %% The name that is assigned to this server
                       port,               %% The port this server is listening on
                       callback            %% The callback module that is run on a new request
                     }).

-record(conn, {sock,                       %% the socket 
               port,                       %% the port 
               peer_addr,                  %% the address of the peer connected to this socket
               peer_port}).                %% the port of the peer connected to this socket

-record(req, {connection = keep_alive,      %% keep_alive or close
              content_length,               %% integer  
              vsn,                          %% {Major, Minor}
              method,                       %% 'GE:bT' or 'POST'
              uri,                          %% Truncated URI '/index.html'
              args="",                      %% Part of the URI after the '?'
              headers,                      %% {Key, Value} HTTP headers
              body = <<>>}).                %% Content body

%% Single message format
-record(message, {from,        %% Who sent the message
                  msg_txt,     %% The contents of the message
                  timestamp,   %% The timestamp of the message
                  seq_no }).   %% The sequence number of the message


