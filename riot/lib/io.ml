include Runtime.Net.Io

let ready fd mode fn = Runtime.syscall "custom" mode fd fn
