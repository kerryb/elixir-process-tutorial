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

## Supervision trees

If we want our counter to start automatically as part of the application, we
can add it to the supervision tree. We didn’t create the app with supervision
enabled, but we can re-run `mix new` with the `--sup` flag to add the required
files and configuration:

```
$ cd ..
$ mix new --sup processes
The directory "processes" already exists. Are you sure you want to continue? [Yn] y
* creating README.md
README.md already exists, overwrite? [Yn] n
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
mix.exs already exists, overwrite? [Yn] y
* creating lib
* creating lib/processes.ex
* creating lib/processes/application.ex
* creating test
* creating test/test_helper.exs
* creating test/processes_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd processes
    mix test

Run "mix help" for more commands.
$ cd -
```

Now we can add `CounterAgent2` to the supervision tree in
`lib/processes/application.ex`, and it’ll be started for us:

```
$ iex -S mix
iex(1)> CounterAgent2.inc()
1
iex(2)> CounterAgent2.inc()
2
```

If we kill the counter, it’s automatically restarted for us (note the new pid):

```
$ iex -S mix
iex(1)> pid = Process.whereis(CounterAgent2)
#PID<0.161.0>
iex(2)> Process.exit(pid, :kill)
true
iex(3)> pid = Process.whereis(CounterAgent2)
#PID<0.173.0>
iex(4)> CounterAgent2.inc()
1
```

The counter has restarted from 1, which isn’t ideal. In practice, you’d want to
have separate processes for storing the value (simple and should never crash)
and performing calculations (might crash, but doesn’t need to maintain its own
state).

## Tasks

The `SlowOperation` module (`lib/slow_operation.ex`) contains a `run/0`
function, which simulates a slow operation (eg interacting with an external
service). It calculates a random number between 1 and 10, sleeps for that
number of seconds, then prints out and returns the number. If we need to call
it multiple times sequentially, it’ll take a long time.

We can use Erlang’s `timer.tc/1` to measure how long the operation takes (in
microseconds):

```
$ iex -S mix
iex(1)> :timer.tc(fn -> (1..5) |> Enum.map(fn _ -> SlowOperation.run end) end)
3
3
10
2
4
{22005144, [3, 3, 10, 2, 4]}
```

Now we’ll use `Task.async/2` to spawn each operation in a separate process, and
`Task.await/1` to gather the results:

```
$ iex -S mix
iex(1)> tasks = Enum.map(1..5, fn _ -> Task.async(SlowOperation, :run, []) end)
[
  %Task{
    owner: #PID<0.146.0>,
    pid: #PID<0.162.0>,
    ref: #Reference<0.1449039036.2818310148.107275>
  },
  %Task{
    owner: #PID<0.146.0>,
    pid: #PID<0.163.0>,
    ref: #Reference<0.1449039036.2818310148.107276>
  },
  %Task{
    owner: #PID<0.146.0>,
    pid: #PID<0.164.0>,
    ref: #Reference<0.1449039036.2818310148.107277>
  },
  %Task{
    owner: #PID<0.146.0>,
    pid: #PID<0.165.0>,
    ref: #Reference<0.1449039036.2818310148.107278>
  },
  %Task{
    owner: #PID<0.146.0>,
    pid: #PID<0.166.0>,
    ref: #Reference<0.1449039036.2818310148.107279>
  }
]
1
2
4
5
6
iex(2)> Enum.map(tasks, &Task.await/1)
[1, 2, 6, 5, 4]
```

We can bundle that all together into a pipeline (supplying a timeout to `Task.await/2` – the default is 5 seconds):

```
$ iex -S mix
iex(1)> 1..5 |> Enum.map(fn _ -> Task.async(SlowOperation, :run, []) end) |> Enum.map(&Task.await(&1, 10_000))
2
2
3
4
6
[2, 3, 6, 2, 4]
```

Let’s see how long it takes to run it in parallel 10,000 times:

```
$ iex -S mix
iex(1)> :timer.tc(fn -> 1..10_000 |> Enum.map(fn _ -> Task.async(SlowOperation, :run, []) end) |> Enum.map(&Task.await(&1, 10_000)) end)
1
1
1
[... etc ...]
10
10
10
{12094767,
 [10, 1, 6, 10, 3, 4, 3, 7, 5, 4, 9, 5, 5, 1, 6, 2, 3, 2, 8, 5, 2, 5, 6, 8, 4,
  7, 2, 8, 4, 9, 8, 3, 8, 3, 10, 2, 6, 7, 9, 1, 10, 5, 4, 9, 3, 2, 7, 2, ...]}
```
