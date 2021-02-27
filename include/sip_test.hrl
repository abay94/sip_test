-define(LOGSTART, (?MODULE_STRING ++ ":" ++ erlang:integer_to_list(?LINE) ++ " ")).

-define(LOGERROR(Text), lager:error(?LOGSTART ++ Text)).
-define(LOGERROR(Text,Params), lager:error(?LOGSTART ++ Text, Params)).
-define(LOGWARNING(Text), lager:warning(?LOGSTART ++ Text)).
-define(LOGWARNING(Text,Params), lager:warning(?LOGSTART ++ Text, Params)).
-define(LOGINFO(Text), lager:info(?LOGSTART ++ Text)).
-define(LOGINFO(Text,Params), lager:info(?LOGSTART ++ Text, Params)).