%%% Name: gossip_user_interface.hrl
%%% Project: Gossip System
%%%	Author: Hang Gao
%%% Description: This is where the user interfaces should be. 

-module(gossip_user_interface).
-include("gossip_config.hrl").
-include("gossip_message.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%%% ---- find_longest_word ----
%%% Description: find the longest word in file F.
%%% Requirement:
%%%			1. send request to the userver process of target node
%%%			2. starts receive for message
%%%			3. return the received message
find_longest_word(DS)->
	done.

%% ====================================================================
%% Internal functions
%% ====================================================================


