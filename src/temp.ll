; ModuleID = 'Oscar'

@tmp = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp.5 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp, i32 0, i32 0), i1 true)
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.1, i32 0, i32 0), i1 false)
  %printf2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.2, i32 0, i32 0), i1 true)
  %printf3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.3, i32 0, i32 0), i1 true)
  %printf4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.4, i32 0, i32 0), i1 false)
  %printf5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.5, i32 0, i32 0), i1 true)
  ret void
}
