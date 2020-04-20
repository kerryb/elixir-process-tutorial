# Processes

## Messages and mailboxes

Sending messages to yourself:

```
$ iex
iex(1)> send self, :foo
:foo
iex(2)> send self, :bar
:bar
iex(3)> receive do
...(3)> :foo -> IO.puts "got foo"
...(3)> :bar -> IO.puts "got bar"
...(3)> end
got foo
:ok
iex(4)> receive do
...(4)> :foo -> IO.puts "got foo"
...(4)> :bar -> IO.puts "got bar"
...(4)> end
got bar
:ok
iex(5)> receive do
...(5)> :foo -> IO.puts "got foo"
...(5)> :bar -> IO.puts "got bar"
...(5)> end
```

Receive will wait indefinitely, so you’ll have to kill iex (or at least the
current process). You can set a timeout:

```
$ iex
iex(1)> receive do
...(1)> :foo -> IO.puts "Got foo"
...(1)> after
...(1)> 1000 -> IO.puts "Timeout"
...(1)> end
Timeout
:ok
```

## Spawning processes

```
$ iex
iex(1)> self()
#PID<0.117.0>
iex(2)> spawn(fn -> IO.puts "Hello from #{inspect self()}" end)
Hello from #PID<0.122.0>
#PID<0.122.0>
```

Processes are asynchronoous:

```
$ iex
iex(1)> spawn(fn -> Process.sleep(5000); IO.puts "Hello from #{inspect self()}" end)
#PID<0.129.0>
iex(2)> 2 + 2
4
Hello from #PID<0.129.0>
```

If a standalone process dies, it doesn’t take down the caller:

```
$ iex
iex(1)> spawn(fn -> raise "crash!" end)

10:18:09.711 [error] Process #PID<0.132.0> raised an exception
** (RuntimeError) crash!
    (stdlib 3.11.2) erl_eval.erl:678: :erl_eval.do_apply/6
#PID<0.132.0>
```

But if we use spawn_link instead, it does:

```
$ iex
iex(1)> spawn_link(fn -> raise "crash!" end)
** (EXIT from #PID<0.117.0>) shell process exited with reason: an exception was raised:
    ** (RuntimeError) crash!
        (stdlib 3.11.2) erl_eval.erl:678: :erl_eval.do_apply/6

10:18:40.261 [error] Process #PID<0.134.0> raised an exception
** (RuntimeError) crash!
    (stdlib 3.11.2) erl_eval.erl:678: :erl_eval.do_apply/6
```

That doesn’t seem very helpful, but the caller can trap the exit signal and
restart the process. This is the basis of supervisors, which we may see later.

## Receive loops and state

```
mix new processes
cd processes
```

Create a `Counter` module (see `lib/counter.ex`)

```
$ iex -S mix
iex(1)> pid = spawn(&Counter.start/0)
#PID<0.154.0>
iex(2)> send pid, :inc
1
:inc
iex(3)> send pid, :inc
2
:inc
```

Of course it would be more useful if we could get a return value from the
counter. We need to send our own pid, so the counter can reply (see
`lib/counter2.ex`).

```
$ iex -S mix
iex(1)> self = self()
#PID<0.152.0>
iex(1)> pid = spawn(fn -> Counter2.start(self) end)
#PID<0.166.0>
iex(2)> send pid, :inc
:inc
iex(3)> receive do
...(3)> {:count, count} -> IO.puts "Count is #{count}"
...(3)> end
Count is 1
:ok
```

Note that we had to save the value of `self()` – if we’d just called `self()`
in the function we sent to spawn, it would have been called in the counter
process instead of iex, and the replies would have been sent to the wrong
place.

This is all very fiddly, but we can wrap it in a function in the counter module
(see `lib/counter3.ex`).

```
$ iex -S mix
iex(1)> pid = Counter3.start
#PID<0.154.0>
iex(2)> Counter3.inc(pid)
1
iex(3)> Counter3.inc(pid)
2
```

## Agents

Because processes sending messages to another is such an important part of the
architecture of Erlang/Elixir apps, all the common patterns for starting,
stopping, monitoring, upgrading and communicating with them were collected into
something called a “generic server” (genserver). In Erlang, all `gen_server`
modules had to implement eight callbacks. Elixir’s `GenServer` uses macros to
automatically provide default implementations, so you only need to define the
ones where you want to add behaviour.

Elixir also provides two simplified specialisations of `GenServer`: `Agent` for
maintaining state, and `Task` for running background jobs. Let’s rewrite our
counter as an `Agent` (see `lib/counter_agent.ex`).

```
$ iex -S mix
iex(1)> {:ok, pid} = CounterAgent.start_link()
{:ok, #PID<0.190.0>}
iex(2)> CounterAgent.inc(pid)
1
iex(3)> CounterAgent.inc(pid)
2
```

If we know we’re only going to have one counter, we can name the server and
avoid having to provide its pid. Convention is to use the module name, to
ensure uniqueness (see `lib/counter_agent2.ex`.

```
$ iex -S mix
iex(1)> CounterAgent2.start_link()
{:ok, #PID<0.215.0>}
iex(1)> CounterAgent2.inc()
1
iex(2)> CounterAgent2.inc()
2
```
