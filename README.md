# MiniVM

Bytecode compiler, interpreter and GC for a subset of Java.

BNF (Backus-Naur Form) is available [here](https://www.cambridge.org/resources/052182060X/MCIIJ2e/grammar.htm).

Test files in test were taken from [here]( https://www.cambridge.org/resources/052182060X/).

## Building

To build the project, run:

```
mkdir build
cd build
cmake ..
make -j
```

The executable should be in `build/src/interpreter`. You can also run the full test suite through the `test.sh` script:

```
bash test.sh
````

## Usage

```
Usage: <INTERPRETER_EXECUTABLE> <input file>
```

## Example of LLVM IR generation

This recursive implementation of factorial from the test files:

```java
class Factorial {

    public static void main(String[] a) {
        System.out.println(new Fac().ComputeFac(10));
    }
}

class Fac {

    public int ComputeFac(int num) {
        int num_aux;
        if (num < 1)
            num_aux = 1;
        else
            num_aux = num * (this.ComputeFac(num - 1));
        return num_aux;
    }

}
```

compiles down to this unoptimized LLVM IR, which can be later optimized by `opt` or `clang`:

```llvm
; ModuleID = 'MiniVM'
source_filename = "MiniVM"

%Fac_vtable = type { ptr }
%array_vtable = type {}

@Fac_vtable = constant %Fac_vtable { ptr @"Fac$ComputeFac" }
@array_vtable = constant %array_vtable zeroinitializer
@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i64 @printf(ptr, ...)

declare ptr @calloc(i64, i64)

declare void @free(ptr)

define void @main() {
entry:
  %0 = call ptr @calloc(i64 1, i64 16)
  %vtableptr = getelementptr ptr, ptr %0, i64 0
  store ptr @Fac_vtable, ptr %vtableptr, align 8
  %vtableptr1 = getelementptr ptr, ptr %0, i64 0
  %loadtmp = load ptr, ptr %vtableptr1, align 8
  %methodptr = getelementptr ptr, ptr %loadtmp, i64 0
  %loadtmp2 = load ptr, ptr %methodptr, align 8
  %calltmp = call i64 %loadtmp2(ptr %0, i64 10)
  %1 = call i64 (ptr, ...) @printf(ptr @0, i64 %calltmp)
  ret void
}

define i64 @"Fac$ComputeFac"(ptr %this, i64 %num) {
entry:
  %num1 = alloca i64, align 8
  store i64 %num, ptr %num1, align 4
  %num_aux = alloca i64, align 8
  store i64 0, ptr %num_aux, align 4
  %num2 = load i64, ptr %num1, align 4
  %cmptmp = icmp slt i64 %num2, 1
  br i1 %cmptmp, label %then, label %else

then:                                             ; preds = %entry
  store i64 1, ptr %num_aux, align 4
  br label %ifcont

else:                                             ; preds = %entry
  %num3 = load i64, ptr %num1, align 4
  %num4 = load i64, ptr %num1, align 4
  %subtmp = sub i64 %num4, 1
  %vtableptr = getelementptr ptr, ptr %this, i64 0
  %loadtmp = load ptr, ptr %vtableptr, align 8
  %methodptr = getelementptr ptr, ptr %loadtmp, i64 0
  %loadtmp5 = load ptr, ptr %methodptr, align 8
  %calltmp = call i64 %loadtmp5(ptr %this, i64 %subtmp)
  %multmp = mul i64 %num3, %calltmp
  store i64 %multmp, ptr %num_aux, align 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %num_aux6 = load i64, ptr %num_aux, align 4
  ret i64 %num_aux6
}
```

