# Erwatch

**Erwatch** is an Erlang/OTP application for tracking changes in
a file system. It doesn't have any dependency other than a recent version of
Erlang/OTP (17+ *should* be OK) and optionally [rebar3](http://www.rebar3.org/).
It is only tested on Linux/OSX, but only Erlang/OTP standard library functions
are used, so it should work on any platform where Erlang/OTP runs.

## Install

**Erwatch** uses [rebar3](http://www.rebar3.org/) to build and tests and
it is available on [hex.pm](https://hex.pm/). Just include the following
in your `rebar.config`:

```erlang
{deps[erwatch]}.
```

Since **Erwatch** is an OTP application, it is required to be started before
using. You can do that by including `erwatch` in your `*.app.src` file, like:

```erlang
...
  {applications,
   [kernel,
    stdlib,
    erwatch
   ]},
...
```

**rebar3** has a nice way of starting apps in the shell, you can try:

    $ rebar3 shell --apps rewatch


## Build

    $ rebar3 compile

## Usage

> Warning!
> Erwatch (*currently*) uses polling to determine file system changes.
> Setting the poll interval too frequent (lower) is not recommended
> for wildcards which may return a large number of paths.

**Erwatch** supports both the synchronous/on demand and asynchronous/message based way
of tracking changes.

Use `erwatch:new/0` to create a watch with default options, or `erwatch:new/1`
to create a watch with specified options:

```erlang
% on demand watch:
{ok, Watch} = erwatch:new(),
% a watch that fires every 3000 milliseconds, and sends a message on changes
{ok, AnotherWatch} = erwatch:new([{interval, 3000}]).
```

You can add any wildcard to the watch. See
[filelib:wildcard/1](http://erlang.org/doc/man/filelib.html#wildcard-1)
documentation for available patterns. Note that, the given wildcard is
relative to the current working directory.

```erlang
erwatch:add_wildcard("/tmp/somedir/**/src/*.erl", Watch).
```

Whether you use *synchronous* or *asynchronous* watches, the change sets are
in the form of list of `{Action, Path}` pairs, where `Action` is one of
`added`, `updated` or `deleted`. Note that, the very first change set will
contain all resolved paths in the given wildcards.

You can change the poll interval and switch between synchronous
and asynchronous modes using `erwatch:set_interval/2`. An interval of
`0` will make the watch synchronous and `> 0` will make it asynchronous.

```erlang
erwatch:set_interval(1000, Watch).
```

It is possible to pause an asynchronous watch using `erwatch:pause/1` and
resume it using `erwatch:resume/1`.

```erlang
erwatch:pause(Watch),
% later...
erwatch:resume(Watch).
```
When the time comes, you can kill a watch with `erwatch:remove/1`:

```erlang
erwatch:remove(Watch).
```

### Synchronous / On Demand Usage

Create a watch:

```erlang
{ok, Watch} = erwatch:new().
```
Add one or more wildcards to the watch:

```erlang
erwatch:add_wildcard("/tmp/foo1/*", Watch),
erwatch:add_wildcard("/tmp/bar2", Watch),
```

Assuming `/tmp/foo1` is a directory and `/tmp/bar2` is a file,
create, modify, delete directories, files in `/tmp/foo1` and/or
create, modify, delete `/tmp/bar2`.

Retrieve changes:

```erlang
erwatch:get_changes(Watch).
```

Returns e.g.:

```erlang
[{added,"/tmp/foo1/myfile"},
 {updated,"/tmp/foo1/yourfile"},
 {updated,"/tmp/foo1/x"},
 {deleted,"/tmp/bar2"}]
```

### Asynchronous / Message Based Usage

In this mode, **Erwatch** will send `erwatch@changes` messages to the
parent process (*currently the process which created the watch*) on
file system changes.

Create a watch with an interval:

```erlang
{ok, Watch} = erwatch:new([{interval, 1000}]).
```
Add one or more wildcards to the watch:

```erlang
erwatch:add_wildcard("/tmp/foo1/*", Watch),
erwatch:add_wildcard("/tmp/bar2", Watch),
```

Assuming `/tmp/foo1` is a directory and `/tmp/bar2` is a file,
create, modify, delete directories, files in `/tmp/foo1` and/or
create, modify, delete `/tmp/bar2`.

Receive changes:

```erlang
loop(Watch) ->
    receive
        {erwatch@changes, Watch, ChangeSet} ->
            % do something with `ChangeSet`
            loop(Watch)
        _ ->
            % received some other message...
            loop(Watch)
    end.
```
The `Changes` value might be:

```erlang
[{added,"/tmp/foo1/myfile"},
 {updated,"/tmp/foo1/yourfile"},
 {updated,"/tmp/foo1/x"},
 {deleted,"/tmp/bar2"}]
```

## Examples

See [examples/erwatch.escript](https://github.com/yuce/erwatch/blob/master/examples/watch.escript)
for a simple file watcher.

## License

```
Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* The names of its contributors may not be used to endorse or promote
  products derived from this software without specific prior written
  permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```