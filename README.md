# Erwatch

**Erwatch** is an Erlang/OTP application for tracking changes in
a file system. It can be used with Erlang/OTP (17+ *should* be OK) and
optionally [rebar3](http://www.rebar3.org/).
It is only tested on Linux/OSX, but only Erlang/OTP standard library functions
are used, so it should work on any platform where Erlang/OTP runs.

## News

* **2016-03-08**: Major change in **Erwatch** API.
    * Removed `erwatch:add_wildcard/2` function. You have to specify the wildcards to `erwatch:new/1` or `erwatch:new/2` instead.
    * The initial changesets are not returned. Use `filelib:wildcard/1` or `filelib:wildcard/2` instead.

## Install

**Erwatch** uses [rebar3](http://www.rebar3.org/) to build and tests and
it is available on [hex.pm](https://hex.pm/). Just include the following
in your `rebar.config`:

```erlang
{deps, [erwatch]}.
```

## Build

    $ rebar3 compile

## Usage

> Warning!
> Erwatch (*currently*) uses polling to determine file system changes.
> Setting the poll interval too frequent (lower) is not recommended
> for wildcards which may return a large number of paths.

Since **Erwatch** is an OTP application, it must be started before
using. You can do that by including `erwatch` in your `*.app.src` or `*.app` file, like:

```erlang
...
  {applications,
   [kernel,
    stdlib,
    erwatch
   ]},
...
```

Or, start it manually:

```erlang
ok = application:start(erwatch).
```

**rebar3** has a nice way of starting apps in the shell, you can try:

    $ rebar3 shell --apps rewatch


**Erwatch** supports both the synchronous/on demand and asynchronous/message based way
of tracking changes.

Use `erwatch:new/1` to create a watch with default options, or `erwatch:new/2`
to create a watch with specified options. The list of wildcards is mandatory for both:

```erlang
% on demand watch:
{ok, Watch} = erwatch:new(["/tmp/somedir/**/src/*.erl"]),
% a watch that fires every 3000 milliseconds, and sends a message on changes
{ok, AnotherWatch} = erwatch:new(["/tmp/somedir/**/src/*.erl"],
                                 [{interval, 3000}]).
```

See [filelib:wildcard/1](http://erlang.org/doc/man/filelib.html#wildcard-1)
documentation for available wildcard patterns. Note that, the given wildcard is
relative to the current working directory.

Whether you use *synchronous* or *asynchronous* watches, the change sets are
in the form of list of `{Action, Path}` pairs, where `Action` is one of
`added`, `updated` or `deleted`.

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
{ok, Watch} = erwatch:new(["/tmp/foo1/*", "/tmp/bar2"]).
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
{ok, Watch} = erwatch:new(["/tmp/foo1/*", "/tmp/bar2"],
                          [{interval, 1000}]).
```

Assuming `/tmp/foo1` is a directory and `/tmp/bar2` is a file,
create, modify, delete directories, files in `/tmp/foo1` and/or
create, modify, delete `/tmp/bar2`.

Receive changes:

```erlang
loop() ->
    receive
        {erwatch@changes, Watch, ChangeSet} ->
            % do something with `ChangeSet`
            loop()
        _ ->
            % received some other message...
            loop()
    end.
```
The `ChangeSet` value might be:

```erlang
[{added,"/tmp/foo1/myfile"},
 {updated,"/tmp/foo1/yourfile"},
 {updated,"/tmp/foo1/x"},
 {deleted,"/tmp/bar2"}]
```

## Examples

See [examples/watch.escript](https://github.com/yuce/erwatch/blob/master/examples/watch.escript)
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