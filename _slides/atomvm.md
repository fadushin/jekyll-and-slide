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

~~

### Process Memory

<img width="600" data-src="../../assets/images/esp32-memory.png" alt="ESP32 Memory"  />

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
        pong ->
            erlang:display(pong),
            ping(Pong)
    end.

pong() ->
    receive
        {ping, Pid} ->
            erlang:display(ping),
            timer:sleep(1000),
            Pid ! pong,
            pong()
    end.
```

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
    io:format("Set pin 2 to ~p~n", [Val]),
    timer:sleep(1000),
    loop(GPIO, 1 - Val).
```

The "Hello World" of IoT!

~~

### DHT

```erlang
-module (dht_demo).

-export([start/0]).

start() ->
    {ok, DHT11} = dht:start(21, dht11),
    loop(DHT11).

loop(DHT11) ->
    take_measurement(DHT11),
    timer:sleep(10000),
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
            io:format("Error taking measurement: ~p~n", [Error])
    end.
```

~~

### WIFI (STA mode)

```erlang
-module(wifi_demo).
-export([start/0]).

-include("logger.hrl").

start() ->
    case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

start_wifi() ->
    case network_fsm:wait_for_sta() of
        {ok, {Address, Netmask, Gateway}} ->
            ?LOG_INFO(
                "IP address: ~s Netmask: ~s Gateway: ~s", [
                    avm_util:address_to_string(Address),
                    avm_util:address_to_string(Netmask),
                    avm_util:address_to_string(Gateway)
                ]
            );
        Error ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Error])
    end.

run() ->
    avm_util:sleep_forever().

```

~~

### HTTPd Demo

```erlang
-module(httpd_demo).
-export([start/0, handle_api_request/4]).

-include("logger.hrl").

start() ->
    case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

start_wifi() ->
    case network_fsm:wait_for_sta([{sntp, "pool.ntp.org"}]) of
        {ok, {Address, Netmask, Gateway}} ->
            ?LOG_INFO(
                "IP address: ~s Netmask: ~s Gateway: ~s", [
                    avm_util:address_to_string(Address),
                    avm_util:address_to_string(Netmask),
                    avm_util:address_to_string(Gateway)
                ]
            );
        Error ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Error])
    end.

-record(opts, {dht, gpio, pin}).

run() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    {ok, DHT11} = dht:start(21, dht11),
    Opts = #opts{dht=DHT11, gpio=GPIO, pin=2},
    Config = [{["api"], api_handler, {?MODULE, Opts}}],
    ?LOG_INFO("Starting httpd on port 8080", []),
    case httpd:start(8080, Config) of
        {ok, _Pid} ->
            ?LOG_INFO("httpd started.", []),
            avm_util:sleep_forever();
        Error ->
            ?LOG_ERROR("An error occurred: ~p", [Error])
    end.

%%
%% API Handler implementation
%%

handle_api_request(get, ["temp"], HttpRequest, #opts{dht=DHT11}) ->
    Socket = proplists:get_value(socket, proplists:get_value(tcp, HttpRequest)),
    {ok, {Host, _Port}} = inet:peername(Socket),
    ?LOG_INFO("Temperature request from ~s", [avm_util:address_to_string(Host)]),
    {ok, {Temp, TempFractional, Hum, HumFractional}} = dht:measure(DHT11),
    {ok, [
        {temp, Temp},
        {temp_fractional, TempFractional},
        {hum, Hum},
        {hum_fractional, HumFractional}
    ]};
handle_api_request(post, ["led"], HttpRequest, #opts{gpio=GPIO, pin=Pin}) ->
    Socket = proplists:get_value(socket, proplists:get_value(tcp, HttpRequest)),
    {ok, {Host, _Port}} = inet:peername(Socket),
    QueryParams = proplists:get_value(query_params, HttpRequest),
    case proplists:get_value("led", QueryParams) of
        "on" ->
            ?LOG_INFO("Turning on LED from ~s", [avm_util:address_to_string(Host)]),
            {ok, gpio:set_level(GPIO, Pin, 1)};
        "off" ->
            ?LOG_INFO("Turning off LED from ~s", [avm_util:address_to_string(Host)]),
            {ok, gpio:set_level(GPIO, Pin, 0)};
        _ ->
            bad_request
    end;
handle_api_request(Method, Path, _HttpRequest, _HandlerOpts) ->
    ?LOG_ERROR("Unsupported method ~p and path ~p", [Method, Path]),
    not_found.
```

---

## Demo

~~

Laptop
```sh
shell$ curl -i -X GET "http://atomvm:8080/api/temp"
HTTP/1.1 200 OK
Server: atomvm-httpd
Content-Type: "application/json"

{"temp":22,"temp_fractional":9,"hum":43,"hum_fractional":0}
```

ESP32 Console
```sh
2020-05-03T23:24:21.000 [httpd_demo:handle_api_request/4:52] <0.8.0> info: Temperature request from 192.168.211.13
```

~~

Laptop
```sh
shell$ curl -i -X POST "http://atomvm:8080/api/led?led=on"
HTTP/1.1 200 OK
Server: atomvm-httpd
Content-Type: "application/json"

"ok"
```

ESP32 Console
```sh
2020-05-03T23:26:42.000 [httpd_demo:handle_api_request/4:66] <0.10.0> info: Turning on LED from 192.168.211.13
```

~~

Laptop
```sh
shell$ curl -i -X POST "http://atomvm:8080/api/led?led=off"
HTTP/1.1 200 OK
Server: atomvm-httpd
Content-Type: "application/json"

"ok"
```

ESP32 Console
```sh
2020-05-03T23:27:26.000 [httpd_demo:handle_api_request/4:69] <0.12.0> info: Turning off LED from 192.168.211.13
```

~~

Laptop
```sh
shell$ curl -i -X POST "http://atomvm:8080/api/led?foo=bar"
HTTP/1.1 400 BAD_REQUEST
Server: atomvm-httpd
Content-Type: "text/html"

Error: bad_request
```

ESP32 Console
```sh
2020-05-03T23:27:57.000 [httpd:handle_error/3:174] <0.14.0> error: error in httpd. StatusCode=400  Error=bad_request
```

~~

Laptop
```sh
shell$ curl -i -X GET "http://atomvm:8080/api/led"
HTTP/1.1 404 NOT_FOUND
Server: atomvm-httpd
Content-Type: "text/html"

Error: not_found
```

ESP32 Console
```sh
2020-05-03T23:28:29.000 [httpd_demo:handle_api_request/4:75] <0.16.0> error: Unsupported method get and path ["led"]
2020-05-03T23:28:29.000 [httpd:handle_error/3:174] <0.16.0> error: error in httpd. StatusCode=404  Error=not_found
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
