-define(DEFAULT_LOGGER, default_logger).
-define(LOG_INFO(M,A), thor_log:info(?DEFAULT_LOGGER, atom_to_list(?MODULE) ++ ": " ++ M, A)).
-define(LOG_DEBUG(M,A), thor_log:debug(?DEFAULT_LOGGER, atom_to_list(?MODULE) ++ ": " ++ M, A)).
-define(LOG_ERROR(M,A), thor_log:error(?DEFAULT_LOGGER, atom_to_list(?MODULE) ++ ": " ++ M, A)).
-define(LOG_WARN(M,A), thor_log:warn(?DEFAULT_LOGGER, atom_to_list(?MODULE) ++ ": " ++ M, A)).

-record(log, {level,                     %% The logging level
              pid,                       %% Caller Pid
              msg,                       %% The log message
              args,                      %% Arguments
              time,                      %% The logging date
              millis }).                 %% The milliseconds


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
              method,                       %% 'GET' or 'POST'
              uri,                          %% Truncated URI '/index.html'
              args="",                      %% Part of the URI after the '?'
              headers,                      %% {Key, Value} HTTP headers
              body = <<>>}).                %% Content body

%% Single message format
-record(message, {from,        %% Who sent the message
                  msg_txt,     %% The contents of the message
                  timestamp,   %% The timestamp of the message
                  seq_no }).   %% The sequence number of the message

