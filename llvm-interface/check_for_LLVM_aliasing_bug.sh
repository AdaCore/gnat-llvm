#! /bin/bash
set -eu

cat << EOF > obj/c43204h.ll
; ModuleID = 'c43204h.adb'
source_filename = "c43204h.adb"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%c43204h__AREC1T = type <{ i64, i64 }>

define void @_ada_c43204h() {
entry:
  %AREC1 = alloca %c43204h__AREC1T, align 16
  %procgGP393__A8b = alloca [2 x i32], align 8
  %procgGP393__J9b = alloca i32, align 4
  %procgGP393__ga11 = alloca [2 x i32], align 8
  %0 = getelementptr inbounds [2 x i32], [2 x i32]* %procgGP393__A8b, i64 0, i32 0
  store i32 1, i32* %0, align 8, !tbaa !0
  store i32 1, i32* %procgGP393__J9b, align 4, !tbaa !4
  %attr-address = ptrtoint i32* %procgGP393__J9b to i64
  %1 = getelementptr inbounds %c43204h__AREC1T, %c43204h__AREC1T* %AREC1, i32 0, i32 0
  store i64 %attr-address, i64* %1, align 16, !tbaa !6
  br label %2

2:                                                ; preds = %loop-stmts, %entry
  %3 = load volatile i32, i32* %procgGP393__J9b, align 4, !tbaa !4
  %4 = icmp slt i32 %3, 2
  br i1 %4, label %loop-stmts, label %loop-exit

loop-stmts:                                       ; preds = %2
  %5 = load volatile i32, i32* %procgGP393__J9b, align 4, !tbaa !4
  %attr-succ = add nsw i32 %5, 1
  store volatile i32 %attr-succ, i32* %procgGP393__J9b, align 4, !tbaa !4
  %6 = load volatile i32, i32* %procgGP393__J9b, align 4, !tbaa !4
  %7 = sub nsw i32 %6, 1
  %8 = call i32 @report__ident_int(i32 2)
  %9 = getelementptr inbounds [2 x i32], [2 x i32]* %procgGP393__A8b, i64 0, i32 %7
  store i32 %8, i32* %9, align 4, !tbaa !0
  br label %2

loop-exit:                                        ; preds = %2
  %10 = load [2 x i32], [2 x i32]* %procgGP393__A8b, align 8, !tbaa !9
  store [2 x i32] %10, [2 x i32]* %procgGP393__ga11, align 8, !tbaa !14
  %attr-address1 = ptrtoint [2 x i32]* %procgGP393__ga11 to i64
  %11 = getelementptr inbounds %c43204h__AREC1T, %c43204h__AREC1T* %AREC1, i32 0, i32 1
  store i64 %attr-address1, i64* %11, align 8, !tbaa !18
  br label %13

12:                                               ; preds = %13
  call void @c43204h__procg(%c43204h__AREC1T* align 16 %AREC1)
  ret void

13:                                               ; preds = %loop-exit
  br label %12
}

define internal void @c43204h__procg(%c43204h__AREC1T* nest noalias nocapture readonly dereferenceable(16) %AREC2F) {
entry:
  %0 = getelementptr inbounds %c43204h__AREC1T, %c43204h__AREC1T* %AREC2F, i32 0, i32 0
  %1 = load i64, i64* %0, align 8, !tbaa !20
  %2 = inttoptr i64 %1 to i32*
  %3 = getelementptr inbounds %c43204h__AREC1T, %c43204h__AREC1T* %AREC2F, i32 0, i32 0
  %4 = load i64, i64* %3, align 8, !tbaa !20
  %5 = inttoptr i64 %4 to i32*
  %6 = getelementptr inbounds %c43204h__AREC1T, %c43204h__AREC1T* %AREC2F, i32 0, i32 1
  %7 = load i64, i64* %6, align 8, !tbaa !23
  %8 = inttoptr i64 %7 to [2 x i32]*
  %9 = getelementptr inbounds [2 x i32], [2 x i32]* %8, i64 0, i32 1
  %10 = load i32, i32* %9, align 4, !tbaa !24
  %11 = icmp ne i32 %10, 2
  br i1 %11, label %12, label %13

12:                                               ; preds = %entry
  call void @abort()
  br label %13

13:                                               ; preds = %12, %entry
  ret void
}

declare void @abort()

declare i32 @report__ident_int(i32)

!0 = !{!1, !1, i64 0, i64 4}
!1 = !{!2, i64 4, !"integer#TN"}
!2 = !{!3, i64 4, !"integerB#TN"}
!3 = !{!"Ada Root"}
!4 = !{!5, !5, i64 0, i64 4}
!5 = !{!2, i64 4, !"integerB#T10"}
!6 = !{!7, !7, i64 0, i64 8}
!7 = !{!8, i64 8, !"system__address#T0"}
!8 = !{!3, i64 8, !"system__address#TN"}
!9 = !{!10, !10, i64 0, i64 8}
!10 = !{!3, i64 8, !"c43204h__arr11#TN#AD", !11, i64 0, i64 4, !13, i64 4, i64 4}
!11 = !{!12, i64 4, !"integer#T11"}
!12 = !{!1, i64 4, !"integer#T2"}
!13 = !{!12, i64 4, !"integer#T12"}
!14 = !{!15, !15, i64 0, i64 8}
!15 = !{!3, i64 8, !"c43204h__procgGP393__T7b#TN#AD", !16, i64 0, i64 4, !17, i64 4, i64 4}
!16 = !{!12, i64 4, !"integer#T6"}
!17 = !{!12, i64 4, !"integer#T7"}
!18 = !{!19, !19, i64 0, i64 8}
!19 = !{!8, i64 8, !"system__address#T1"}
!20 = !{!21, !7, i64 0, i64 8}
!21 = !{!22, i64 16, !"c43204h__AREC1T", !7, i64 0, i64 8, !19, i64 8, i64 8}
!22 = !{!3, i64 16, !"c43204h__AREC1T#TN", !7, i64 0, i64 8, !19, i64 8, i64 8}
!23 = !{!21, !19, i64 8, i64 8}
!24 = !{!10, !13, i64 4, i64 4}

EOF
opt -O2 obj/c43204h.ll -o obj/c43204h_o.bc
llvm-dis obj/c43204h_o.bc
if [ "`wc -l obj/c43204h_o.ll | awk '{print $1}'` " -gt "40" ]; then
    BUG=False
    echo "OK: using LLVM without the aliasing bug"
else
    BUG=True
    echo "using LLVM with the aliasing bug, will pessimize slightly the optimized code"
fi
cat << EOF > obj/gnatllvm-aliasing-params.ads
package GNATLLVM.Aliasing.Params is
   LLVM_Struct_Tag_Bug : constant Boolean := $BUG;
end GNATLLVM.Aliasing.Params;
EOF
