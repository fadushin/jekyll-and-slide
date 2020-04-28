---
title: AtomVM
description: A tiny Erlang/Elixir VM for microcontrollers
author: Fred Dushin
#year: 2020
theme: solarized
---

## Preface

<span class="fragment">`` `whoami` ``</span>

<span class="fragment">"Typical" AtomVM user</span>

<span class="fragment">Footnotes, caveats, get out of jail free card</span>

---

## How tiny?

~~

### Pretty tiny...

<img width="350" data-src="../../assets/images/esp32-pico.jpeg" alt="ESP32 Pico"/>

~~

### The chip

<img width="300" data-src="../../assets/images/esp32-chip.jpeg" alt="ESP32 Chipset"/>

~~

* Xtensa® 32-bit LX6 microprocessor
    * <div class="fragment highlight-blue">Dual core + ULP processor</div>
    * 160Mhz
    * <div class="fragment highlight-red">520KB RAM</div>
    * 4MB Flash (typical)
* <div class="fragment highlight-blue">Built-in 802.11 b/g/n wifi</div>
* Bluetooth 4.2
* 34 GPIO pins
* Power
    * 3.3v
    * 20-25 (mA)
    * <div class="fragment highlight-blue">5uA (deep sleep)</div>

~~

### System on a chip

<img width="350" data-src="../../assets/images/esp32-soc.jpeg" alt="ESP32 Soc" style="transform: rotate(270deg); -webkit-transform: rotate(270deg);" /> $2.50 Ali Express

~~

### ESP32 WROOM Adapter

<img width="400" data-src="../../assets/images/esp32-adapter.jpeg" alt="ESP32 Adapter"  />

~~

### ESP32 devboard

<img width="350" data-src="../../assets/images/esp32-devboard.jpg" alt="ESP32 Devboard"  />

~~

### ESP Development/Deployment

<img width="600" data-src="../../assets/images/esp32-scenarios.png" alt="ESP32 Scenarios"  />

---

## Platform

~~

### FreeRTOS

* Real-time OS
* Tasks
* Queues
* Interrupts

~~

###  Espressif IDF SDK

* SMP support for RTOS
* I/O interfaces
    * UART, GPIO, LEDC, I2C, I2S, etc
* Networking
    * WIFI (STA, AP, or STA+AP)
    * Bluetooth
    * UDP, TCP/IP (lwip)
* Storage
    * mmap
    * non-volatile storage
    * file systems (vFAT)

~~

### Tooling

* Xtensa C cross-compiler/linker
* GNU `make` or `cmake`
* `esptool.py` (flashing over USB/serial)

~~

### Build/Flash process (typical)

<img width="600" data-src="../../assets/images/esp32-build.png" alt="ESP32 Build"  />

~~

### ESP 32 Flash Layout

<img width="600" data-src="../../assets/images/esp32-flash.png" alt="ESP32 Flash Layout"  />


----

## AtomVM

~~

* Ground-up BEAM implementation
* Written in C (mostly)
* Limited (but growing) libraries in Erlang
* Supported on
    * UNIX-like (Linux, MacOS, FreeBSD)
    * STM32
    * ESP32
* Runs unmodified BEAM files emitted from Erlang/Elixir compiler*

\* With limitations

~~

### Language Features

* BEAM support
    * function calls, assignment, pattern matching
    * arithmentic/logical bifs
    * anonymous functions
* Data types
    * lists, tuples, integers, binaries, floats*
* try/catch
* Processes
    * `spawn`, `!`, `receive`
* Bit syntax (pack, match)

\* Not supported on all platforms

~~

### What's NOT there

* Bignums
* Maps
* Links (monitors, supervisors, etc)
* stacktrace
* Reference counted binaries
* SMP support
* Hot swapping
* disterl

~~

### Libraries

* estdlib
    * `erlang`, `timer`, `lists`, `proplists`
    * `gen_server`, `gen_statem`
    * `gen_tcp`, `gen_udp`, `inet`
    * `io`, `io_lib`
* eavmlib
    * `network_fsm`, `http_server`
    * `atomvm`, `esp`, `nvs`
    * `gpio`, `ledc_pwm`, `dht`

~~

### ESP Partition Table

| Use      | Type  |     Size |
|-----------------------------|
| nvs      |  data |      24k |
| phy_init |  data |       4k |
| atomvm   |   app |       1M |
| main.app |  data |       1M |

~~

### ESP Partitions

<img width="600" data-src="../../assets/images/esp32-partitions.png" alt="ESP32 Partitions"  />

~~

### PackBEAM

```sh
shell$ erlc foo.erl
shell$ erlc bar.erl
shell$ PackBEAM \
    -a foo.avm foo.beam bar.beam \
    .../eavmlib.avm \
    .../estdlib.avm
```

Generates an AVM file containing BEAM files from command line args

~~

### Flash

```sh
shell$ esptool.py \
    --chip esp32 \
    --port /dev/ttyusb0 \
    write_flash \
    0x110000 \
    foo.avm
```

Flash the AVM file to the start of the `main.app` partition (address 0x110000)

~~

### ESP Main partition

AVM file written to `main.app` partition; contains beam files in sequence.

<img width="600" data-src="../../assets/images/esp32-beams.png" alt="ESP32 Main Partition"  />

~~

### ESP Modules

<img width="600" data-src="../../assets/images/esp32-modules.png" alt="ESP32 Modules"  />

---

## AtomVM Programs

~~

### Obligatory Hello World!

```erlang
-module(hello).

-export([start/0]).

start() ->
    io:format("Hello World!~n").
```

All AVM programs have a `start` entrypoint.

~~

### Blinky

```erlang
-module (blinky).
-export([start/0]).

start() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    loop(GPIO, 1).

loop(GPIO, Val) ->
    gpio:set_level(GPIO, 2, Val),
    timer:sleep(1000),
    loop(GPIO, 1 - Val).
```

The "Hello World" of IoT!

~~

### Procs

```erlang
-module(pingpong).
-export([start/0]).

start() ->
    Pong = spawn(fun() -> pong() end),
    ping(Pong).

ping(Pong) ->
    Pong ! {ping, self()},
    receive
        pong -> ping(Pong)
    end.

pong() ->
    receive
        {ping, Pid} ->
	    timer:sleep(1000),
	    Pid ! pong
	    pong()
    end.
```

~~

### Non-volatile Storage

```erlang
-module(esp_nvs).
-export([start/0]).

-record(state, {count = 0}).

start() ->
    Bin = esp:nvs_get_binary(?MODULE, starts),
    NewState = case Bin of
        undefined ->
            #state{};
        _ ->
            case erlang:binary_to_term(Bin) of
                #state{count = Count} = OldState ->
                    OldState#state{count = Count + 1};
                _ ->
                    erlang:display({error, bad_value}),
                    #state{}
            end
    end,
    io:format("This device has rebooted ~p times.~n", [NewState#state.count]),
    io:format("Reset device to increment.~n"),
    esp:nvs_set_binary(?MODULE, starts, erlang:term_to_binary(NewState)).
```

~~

### DHT Example

```erlang
-module (dht_example).

-export([start/0]).

start() ->
    {ok, DHT11} = dht:start(21, dht11),
    loop(DHT11).

loop(DHT11) ->
    take_measurement(DHT11),
    timer:sleep(30000),
    loop(DHT11).

take_measurement(DHT) ->
    case dht:measure(DHT) of
        {ok, Measurement} ->
            {Temp, TempFractional, Hum, HumFractional} = Measurement,
            io:format(
                "Temperature: ~p.~pC  Humidity: ~p.~p%~n", [
                    Temp, TempFractional, Hum, HumFractional]
            );
        Error ->
            io:format("Error taking measurement on ~p: ~p~n", [Device, Error])
    end.
```

~~

### Network FSM

```erlang
-module(wait_for_wifi).

-export([start/0]).

start() ->
    case network_fsm:wait_for_sta(30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "IP address: ~p Netmask: ~p Gateway: ~p~n",
                [Address, Netmask, Gateway]
            ),
            do_something();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

do_something() ->
    ...
```

---

## Demo

~~

```sh
shell$ export ESP_ADDRESS=192.168.211.53
shell$ curl -s http://${ESP_ADDRESS}:8080/system/info | python -m json.tool
{
    "atom_count": 330,
    "heap_free": 200932,
    "platform": "esp32",
    "port_count": 4,
    "process_count": 8,
    "word_size": 4
}
```

~~

```sh
shell$ curl -s http://${ESP_ADDRESS}:8080/processes | python -m json.tool
[
    "<0.1.0>",
    "<0.2.0>",
    "<0.3.0>",
    "<0.4.0>",
    "<0.5.0>",
    "<0.10.0>",
    "<0.11.0>",
    "<0.12.0>"
]
```

~~

```sh
shell$ for i in {1..8}; do echo "$(curl -s http://${ESP_ADDRESS}:8080/processes/${i}/info)
"; done
{"pid":"<0.1.0>","heap_size":287,"stack_size":4,"message_queue_len":0,"memory":6488}
{"pid":"<0.2.0>","heap_size":107,"stack_size":6,"message_queue_len":0,"memory":796}
{"pid":"<0.3.0>","heap_size":37,"stack_size":0,"message_queue_len":0,"memory":440}
{"pid":"<0.4.0>","heap_size":18,"stack_size":0,"message_queue_len":0,"memory":360}
{"pid":"<0.5.0>","heap_size":36,"stack_size":0,"message_queue_len":0,"memory":1368}
{"pid":"<0.22.0>","heap_size":262,"stack_size":11,"message_queue_len":0,"memory":1336}
{"pid":"<0.25.0>","heap_size":11,"stack_size":0,"message_queue_len":0,"memory":432}
{"pid":"<0.28.0>","heap_size":18,"stack_size":8,"message_queue_len":0,"memory":420}
```

---

### TODOs

1. stability/release/tooling
1. maps
1. peripherals, peripherals, peripherals
1. AP, STA+AP modes
1. links
1. disterl
1. SMP
1. deep sleep

---

### Thank you!

* [AtomVM on Github](https://github.com/bettio/AtomVM/)
* [Running AtomVM on MacOS and an ESP32](http://blog.dushin.net/2018/11/running-atomvm-on-macos-and-an-esp32/)
* [Why Erlang is Relevant to the Internet of Things](http://blog.dushin.net/2018/11/why-erlang-is-relevant-to-iot/)
