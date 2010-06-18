%%% Copyright (C) 2009 Enrique Marcote, Miguel Rodriguez
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% o Redistributions of source code must retain the above copyright notice,
%%%   this list of conditions and the following disclaimer.
%%%
%%% o Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% o Neither the name of ERLANG TRAINING AND CONSULTING nor the names of its
%%%   contributors may be used to endorse or promote products derived from this
%%%   software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
-module(cl_calendar).

%%% EXTERNAL EXPORTS
-export([day/0,
         day/1,
         day/3,
         day_of_current_week_to_date/1,
         day_of_next_week_to_date/1,
         format_day/1,
         format_month/1,
         format_rfc2109/1,
         format_rfc2109/2,
         local_time/0,
         parse/1,
         time_difference/2,
         tstamp/0,
         tstamp/1,
         tstamp_to_local_time/1,
         valid_time/1,
         valid_time/3,
         week/0,
         week/1,
         week/3]).

%%% MACROS
-define(JANUARY_1ST_1970, 62167219200).
-define(SEPARATORS, [$/, $-, $_, $:, $ ]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
day() ->
    day(date()).

day({Year, Month, Day}) ->
    day(Year, Month, Day).

day(Year, Month, Day) ->
    January1st = calendar:date_to_gregorian_days(Year, 1, 1),
    calendar:date_to_gregorian_days(Year, Month, Day) - January1st + 1.


day_of_current_week_to_date(DayNumber) ->
    CurrentDayNumber = calendar:date_to_gregorian_days(date()),
    CurrentDayOfWeek = calendar:day_of_the_week(date()),
    calendar:gregorian_days_to_date(
      CurrentDayNumber + DayNumber - CurrentDayOfWeek).


day_of_next_week_to_date(DayNumber) ->
    CurrentDayNumber = calendar:date_to_gregorian_days(date()),
    CurrentDayOfWeek = calendar:day_of_the_week(date()),
    calendar:gregorian_days_to_date(
      CurrentDayNumber + DayNumber + 7 - CurrentDayOfWeek).


format_day(1) ->
    "Mon";
format_day(2) ->
    "Tue";
format_day(3) ->
    "Wed";
format_day(4) ->
    "Thu";
format_day(5) ->
    "Fri";
format_day(6) ->
    "Sat";
format_day(7) ->
    "Sun".


format_month(1) ->
    "Jan";
format_month(2) ->
    "Feb";
format_month(3) ->
    "Mar";
format_month(4) ->
    "Apr";
format_month(5) ->
    "May";
format_month(6) ->
    "Jun";
format_month(7) ->
    "Jul";
format_month(8) ->
    "Aug";
format_month(9) ->
    "Sep";
format_month(10) ->
    "Oct";
format_month(11) ->
    "Nov";
format_month(12) ->
    "Dec".


format_rfc2109(DateTime) ->
    format_rfc2109(DateTime, "GMT").

format_rfc2109({{Year, Month, Day}, {Hour, Min, Sec}}, Timezone) ->
    % Estrange but much faster than using io_lib
    FDay = format_day(calendar:day_of_the_week({Year, Month, Day})),
    FMonth = format_month(Month),
    Time = [pad(Hour), ":", pad(Min), ":", pad(Sec), " ", Timezone],
    lists:concat([FDay, ", ", pad(Day), "-", FMonth, "-", Year, " " | Time]).


local_time() ->
    % Estrange but much faster than using io_lib
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    LocalTime = Sec + (Min * 100) + (Hour * 10000) + (Day * 1000000) +
        (Month * 100000000) + (Year * 10000000000),
    integer_to_list(LocalTime).


parse([H1, H2, _, M1, M2, _, S1, S2]) ->
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([M1, M2]),
    Sec = list_to_integer([S1, S2]),
    true = valid_time(Hour, Min, Sec),
    {Hour, Min, Sec};
parse([Y1, Y2, Y3, Y4, _, M1, M2, _, D1, D2]) ->
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day = list_to_integer([D1, D2]),
    true = calendar:valid_date(Year, Month, Day),
    {Year, Month, Day};
parse([Y1, Y2, Y3, Y4, S1, M1, M2, S2, D1, D2, _ | Time]) ->
    {parse([Y1, Y2, Y3, Y4, S1, M1, M2, S2, D1, D2]), parse(Time)}.


time_difference(Time1, Time2) ->
    calendar:time_to_seconds(Time1) - calendar:time_to_seconds(Time2).


tstamp() ->
    tstamp(erlang:localtime()).

tstamp(Time) ->
    calendar:datetime_to_gregorian_seconds(Time) - ?JANUARY_1ST_1970.


tstamp_to_local_time(Tstamp) ->
    calendar:gregorian_seconds_to_datetime(Tstamp + ?JANUARY_1ST_1970).


valid_time({H, M, S}) ->
    valid_time(H, M, S).

valid_time(H, M, S) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
    true;
valid_time(_, _, _) ->
    false.


week() ->
    week(date()).

week({Year, Month, Day}) ->
    week(Year, Month, Day).

week(Year, Month, Day) ->
    DaysUntil1stSunday = 7 - calendar:day_of_the_week(Year, 1, 1) + 1,
    DaysSince1stSunday = day(Year, Month, Day) - DaysUntil1stSunday,
    if
        (DaysSince1stSunday rem 7) > 0 ->
            (DaysSince1stSunday div 7) + 1;
        true ->
            (DaysSince1stSunday div 7)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
pad(N) when N > 9 ->
    N;
pad(N) ->
    [$0 | integer_to_list(N)].
