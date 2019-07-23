## TCP and UDP echo server in Common Lisp

This is a simple TCP and UDP echo server, inspired by [this](https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528), [this](https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc), [this](http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp), [this TCP server](https://gist.github.com/traut/6bf71d0da54493e6f22eb3d00671f2a), and [this UDP server](https://gist.github.com/traut/648dc0d7b22fdfeae6771a5a4a19f877).

## Install

1. Install ASDF and install this project into your ASDF system, then load: `(asdf:load-system :sockets-test)`

## Usage

#### Start the TCP server
```(sockets-test:tcp)```

#### Kill the TCP server
```(sockets-test:kill-tcp)```

#### Start the UDP server
```(sockets-test:udp)```

#### Kill the UDP server
```(sockets-test:kill-udp)```

#### Connect to TCP server:
  `$ telnet 0.0.0.0 8882`
  
#### Connect to UDP server:
  `$ nc -u localhost 8882`
