var N = null;var searchIndex = {};
searchIndex["ubend"]={"doc":"UBend","items":[[3,"Command","ubend","Configuration of a child process.",N,N],[12,"stdin","","",0,N],[12,"stdout","","",0,N],[12,"stderr","","",0,N],[12,"last","","",0,N],[12,"argv","","",0,N],[12,"envp","","",0,N],[3,"Child","","",N,N],[3,"Chain","","Represents a pipe chain.",N,N],[3,"Output","","Output and exit status of the last process in the chain.",N,N],[12,"status","","",1,N],[12,"stdout","","",1,N],[12,"stderr","","",1,N],[4,"Target","","Redirection target. Used with [PipeSetup].",N,N],[13,"Stdout","","",2,N],[13,"Stderr","","",2,N],[4,"Mode","","File open mode. Used with [PipeSetup].",N,N],[13,"Read","","",3,N],[13,"Write","","",3,N],[13,"Append","","",3,N],[4,"PipeSetup","","Setup a pipe.",N,N],[13,"Inherit","","Inherit the stream from the parent process. This is the default for stderr and for stdin of the first process in the pipe chain.",4,N],[13,"Pipe","","Open a pipe to the parent process or between processes. This is the default for stdout and for stdin of all but the first process in the pipe chain.",4,N],[13,"Null","","Open a pipe to `/dev/null`.",4,N],[13,"Redirect","","Redirect this stream to the specified target stream. Only stdout and stderr can be redirected this way and only to each other. If a stream is redirected to itself it is equivalent to [PipeSetup::Pipe].",4,N],[13,"Temp","","Connect the stream to a temp file. If supported this is done via the operating systems native temp file support using the `O_TMPFILE` flag. Such a file is written to the hard disk, but has no name and will be deleted after the file handle to it is closed.",4,N],[13,"FileDescr","","Connect the stream to the specified file descriptor. Note that error or not any passed file descriptor will be consumed (closed) by [Chain::new()].",4,N],[13,"FileName","","Connect the stream to the specified file using the specified mode.",4,N],[13,"File","","Connect the stream to the specified file. Note that error or not any passed file will be consumed (closed) by [Chain::new()].",4,N],[4,"Error","","",N,N],[13,"OS","","A libc function returned an error of specified `errno`.",5,N],[13,"NotEnoughArguments","","Any command needs to have at least one \"argument\" (the command name).",5,N],[13,"CannotRedirectStdinTo","","stdin cannot be redirected to stderr or stdout, since those are output streams and stdin is an input stream.",5,N],[13,"NotEnoughPipes","","A pipe chain has to have at least one command.",5,N],[4,"WaitError","","",N,N],[13,"Interrupted","","",6,N],[13,"ChildInvalid","","",6,N],[13,"ChildSignaled","","",6,N],[13,"ChildCoreDumped","","",6,N],[13,"ChildStopped","","",6,N],[13,"ChildContinued","","",6,N],[4,"OutputError","","",N,N],[13,"IO","","A Rust io function returned the specified error.",7,N],[13,"Pipe","","",7,N],[13,"Wait","","",7,N],[4,"KillError","","",N,N],[13,"InvalidSignal","","",8,N],[13,"NoPermissions","","",8,N],[13,"InvalidProcess","","",8,N],[6,"Result","","",N,N],[6,"WaitResult","","",N,N],[8,"IntoPipeSetup","","",N,N],[10,"into_pipe_setup","","",9,[[["self"],["mode"]],["pipesetup"]]],[11,"fmt","","",2,[[["self"],["formatter"]],["result"]]],[11,"clone","","",2,[[["self"]],["target"]]],[11,"eq","","",2,[[["self"],["target"]],["bool"]]],[11,"fmt","","",3,[[["self"],["formatter"]],["result"]]],[11,"clone","","",3,[[["self"]],["mode"]]],[11,"eq","","",3,[[["self"],["mode"]],["bool"]]],[11,"fmt","","",4,[[["self"],["formatter"]],["result"]]],[11,"is_redirect","","",4,[[["self"]],["bool"]]],[11,"is_redirect_to","","",4,[[["self"],["target"]],["bool"]]],[11,"is_inherit","","",4,[[["self"]],["bool"]]],[11,"is_pipe","","",4,[[["self"]],["bool"]]],[11,"is_null","","",4,[[["self"]],["bool"]]],[11,"is_temp","","",4,[[["self"]],["bool"]]],[11,"is_any_file","","",4,[[["self"]],["bool"]]],[11,"is_file","","",4,[[["self"]],["bool"]]],[11,"is_file_descr","","",4,[[["self"]],["bool"]]],[11,"is_file_name","","",4,[[["self"]],["bool"]]],[11,"fmt","","",0,[[["self"],["formatter"]],["result"]]],[11,"fmt","","",10,[[["self"],["formatter"]],["result"]]],[11,"fmt","","",11,[[["self"],["formatter"]],["result"]]],[11,"into_pipe_setup","","",4,[[["self"],["mode"]],["pipesetup"]]],[11,"into_pipe_setup","","",4,[[["self"],["mode"]],["pipesetup"]]],[11,"fmt","","",5,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",6,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",7,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",8,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",5,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",6,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",7,[[["self"],["formatter"]],["result",["error"]]]],[11,"fmt","","",8,[[["self"],["formatter"]],["result",["error"]]]],[11,"kill","","",10,[[["self"],["c_int"]],["result",["killerror"]]]],[11,"wait","","",10,[[["self"]],["waitresult"]]],[11,"pid","","",10,[[["self"]],["pid_t"]]],[11,"stdin","","Take ownership of stdin of the process.",10,[[["self"]],["option",["file"]]]],[11,"stdout","","Take ownership of stdout of the process.",10,[[["self"]],["option",["file"]]]],[11,"stderr","","Take ownership of stderr of the process.",10,[[["self"]],["option",["file"]]]],[11,"output","","Close stdin of the child process, read its stdout and stderr (if possible) and wait for the child process to finish.",10,[[["self"]],["result",["output","outputerror"]]]],[11,"drop","","",10,[[["self"]]]],[11,"empty","","",0,[[],["self"]]],[11,"new","","",0,[[["str"]],["self"]]],[11,"first","","",0,N],[11,"last","","",0,N],[11,"pass_through","","",0,N],[11,"pass_stdin","","",0,N],[11,"pass_stdout","","",0,N],[11,"pass_stderr","","",0,N],[11,"has_output","","",0,[[["self"]],["bool"]]],[11,"env","","",0,[[["self"],["str"],["str"]]]],[11,"arg","","",0,[[["self"],["str"]]]],[11,"stdin","","",0,[[["self"],["pipesetup"]]]],[11,"stdout","","",0,[[["self"],["pipesetup"]]]],[11,"stderr","","",0,[[["self"],["pipesetup"]]]],[11,"open","","",0,[[["self"]],["result",["child"]]]],[11,"kill_all","","Send signal `sig` to all child processes.",11,[[["self"],["c_int"]],["result",["killerror"]]]],[11,"new","","Create a new pipe chain. This function is called by the [ubend!] macro.",11,[[["vec",["command"]]],["result"]]],[11,"stdin","","Take ownership of stdin of the first process in the chain.",11,[[["self"]],["option",["file"]]]],[11,"stdout","","Take ownership of stdout of the last process in the chain.",11,[[["self"]],["option",["file"]]]],[11,"stderr","","Take ownership of stderr of the last process in the chain.",11,[[["self"]],["option",["file"]]]],[11,"stdin_at","","Take ownership of stdin of the process at index in the chain.",11,[[["self"],["usize"]],["option",["file"]]]],[11,"stdout_at","","Take ownership of stdout of the process at index in the chain.",11,[[["self"],["usize"]],["option",["file"]]]],[11,"stderr_at","","Take ownership of stderr of the process at index in the chain.",11,[[["self"],["usize"]],["option",["file"]]]],[11,"children","","",11,N],[11,"children_mut","","",11,N],[11,"wait_all","","Wait for all child processes to finish.",11,[[["self"]],["vec",["waitresult"]]]],[11,"wait_last","","Wait for the last child process to finish.",11,[[["self"]],["waitresult"]]],[11,"output","","Close stdin of the first child process and read stdout and stderr of the last child process (if possible) and wait for the last child process to finish.",11,[[["self"]],["result",["output","outputerror"]]]],[14,"ubend","","Create a pipe chain using a Unix shell like syntax.",N,N]],"paths":[[3,"Command"],[3,"Output"],[4,"Target"],[4,"Mode"],[4,"PipeSetup"],[4,"Error"],[4,"WaitError"],[4,"OutputError"],[4,"KillError"],[8,"IntoPipeSetup"],[3,"Child"],[3,"Chain"]]};
initSearch(searchIndex);