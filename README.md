# nc-paslib
A (non-GUI) base utilities library for Pascal (Delphi/Free Pascal/Lazarus).


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


## TncAnimateGroup - run animations in sequence for a single control

    constructor Create(const control : TControl); reintroduce;

The constructor's parameter specifies the control to perform the animation on.



    procedure add_size_animate(const w, h : Integer; const keep_center : Boolean; const duration : Cardinal);
    procedure add_expand_to_left_animate(const w : Integer; const duration : Cardinal);
    procedure add_move_animate(const x, y : Integer; const duration : Cardinal);
    procedure add_change_animate(const x, y, w, h : Integer; const duration : Cardinal);

Add animation effects for each step, supporting changes in size, expansion to the left, movement, and changes (in size and position).


    procedure run(); overload;
    procedure run(const progress_callback : TncAnimateProgressEvent); overload;

Start executing the animations.

## TncAnimateAsyncGroup - run animations asynchronously (simultaneously) for multiple controls

    procedure add_size_animate(const control : TControl; const w, h : Integer; const keep_center : Boolean; const duration : Cardinal);
    procedure add_expand_to_left_animate(const control : TControl; const w : Integer; const duration : Cardinal);
    procedure add_move_animate(const control : TControl; const x, y : Integer; const duration : Cardinal);
    procedure add_change_animate(const control : TControl; const x, y, w, h : Integer; const duration : Cardinal);

Add animation effects, supporting changes in size, expansion to the left, movement, and changes (in size and position).

    procedure add_alpha_animate(const form : TForm; const a : Integer; const duration : Cardinal);

Add animation effect for changing form's alpha value.

    procedure run(); overload;
    procedure run(const progress_callback : TncAnimateProgressEvent); overload;

Start executing the animations.
