## examples/echo_server.av
## A simple echo server in aviatorscript
let io = require('io');

fn process_conn(socket) {
  let client_addr = getRemoteSocketAddress(socket);
  p("Accepted a connection from " + client_addr);
  let reader = nil;
  try{
    let out = getOutputStream(socket);
    reader = io.reader(getInputStream(socket));
    for line in io.line_seq(reader) {
      if line == "exit" {
        println(out, 'bye ^_^');
        break;
      }
      println(out, line);
    }
  }catch(e){
    pst(e);
  }finally{
    if reader != nil {
      close(reader);
    }
    close(socket);
    p("Closed a connection from " + client_addr);
  }
}

let port = count(ARGV) >= 1? long(ARGV[0]) : 8080;

let server =new java.net.ServerSocket(port);

p("Echo server listen at " + port);

let socket = nil;

while (socket =accept(server)) != nil {
  let t = new Thread(lambda () ->
                     process_conn(socket);
                     end);
  start(t);
}
