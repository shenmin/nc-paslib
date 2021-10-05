# nc-paslib
A base utilities library for Pascal (Delphi/Free Pascal/Lazarus)


## TncThread - a utility class for threading

    class procedure dispatch_to_main_thread(const proc : TProcedure); overload;
    class procedure dispatch_to_main_thread(const proc_method : TThreadMethod); overload;

Dispatches the execution of a method or a procedure call to the main thread queue.



    class procedure ensure_in_main_thread(const proc : TProcedure); overload;
    class procedure ensure_in_main_thread(const proc_method : TThreadMethod); overload;

Ensures the execution of a method or a procedure call within the main thread. If the current thread is the main thread, the method or the procedure will be executed directly without queuing.



## TncDelay - a utility class for delay running

    procedure run(const proc : TProcedure; const after_ms : UInt64); overload;
    procedure run(const proc_method : TThreadMethod; const after_ms : UInt64); overload;

Executes a method or a procedure **in the main thread** after a delay (milliseconds).



    procedure cancel(); overload;
    procedure cancel(const proc : TProcedure); overload;
    procedure cancel(const proc_method : TThreadMethod); overload;

Cancels the execution of a specified method/procedure or all methods/procedures in the delay queue.



    procedure begin_disable();
    procedure end_disable();

Temporarily not accepts any delay run request between `begin_disable()` and `end_disable()`.

