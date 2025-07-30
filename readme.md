# HAC - HAC Ada Compiler

HAC is perhaps the first open-source (albeit very
partial) Ada compiler fully programmed in Ada itself.

**Complete description in: `doc/hac.txt`**

### Command-line flavor:

If you are impatient: in Alire (https://alire.ada.dev/),
do "alr get hac", then "alr run" from the hac* directory.

Alternatively: have GNAT installed (https://www.adacore.com/download),
then, on your preferred command-line interpreter:

```
  gnatmake -P hac

  cd exm
  ../hac gallery.adb
```

(if `gnatmake` doesn't work, try `gprbuild`;
 for Windows, '\\' is meant in place of '/' )

### Editor / pre-built flavor:

Or, if you don't want to touch the command-line at all for playing
with HAC, you can download and use LEA (http://l-e-a.sf.net/).

### Examples
You'll find hundreds of examples in the `exm` directory and
its subdirectories.

Here is the famous Hello World! File `hello.adb`:

```Ada
with HAT;

procedure Hello is
begin
  HAT.Put ("Hello world!");
end Hello;
```

Another classic example (file `fibo.adb`):

```Ada
with HAT;

procedure Fibo is

  function Fibonacci (P : Natural) return Positive is
  begin
    if P <= 2 then
      return 1;
    else
      return Fibonacci (P - 1) + Fibonacci (P - 2);
    end if;
  end Fibonacci;

  use HAT;

begin
  for i in 1 .. 22 loop
    Put_Line (Fibonacci (i));
  end loop;
end Fibo;
```

Enjoy!

### License

HAC is free, open-source and released under the MIT license.
