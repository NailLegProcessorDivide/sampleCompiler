[section .text align=16]
global main

extern signal_error
extern input
extern output
extern allocate1
extern allocate2
extern allocate3
extern allocate4
extern allocate5
extern allocate6
extern allocate7

main:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  push rbx
  push r12
  push r13
  push r14
  push r15
.block0:
  push rcx
  push rdx
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  mov rdi, 42
  call f
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rdx
  pop rcx
  mov qword [r_Global], rax
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  leave
  ret
f:
  push rbp
  mov rbp, rsp
  sub rsp, 24
  push rbx
  push r12
  push r13
  push r14
  push r15
.block0:
  mov qword [rbp + -8], 486921
  mov qword [rbp + -16], 0
  push rcx
  push rdx
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  mov rdi, qword [rbp + -8]
  call output
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rdx
  pop rcx
  mov rax, qword [rbp + -16]
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  leave
  ret
bound_error:
  push rcx
  push rdx
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  mov rdi, 0
  call signal_error
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rdx
  pop rcx
null_error:
  push rcx
  push rdx
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  mov rdi, 1
  call signal_error
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rdx
  pop rcx
[section .bss align=16]
default rel
r_Global: resq 1
