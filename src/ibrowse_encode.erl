-module(ibrowse_encode).
-compile(export_all).

url_encode(list_to_string,List)->
    Body_List=lists:map(fun(T)  when is_tuple(T)->
				%% Body=tuple_to_list(T), 
				%% io:format("body ~w~n",[Body]),
				{K,V}=T,
				Body_String=lists:concat([K,'=',V]),
				%% io:format("body_string~w~n",[Body_String]),
				Body_String
			end,List),
    String=string:join(Body_List,"&"),
    String.
			    
		      

