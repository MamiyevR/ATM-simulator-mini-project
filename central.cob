       identification division.
       program-id. central.
       author. RESUL MAMIYEV.
       
       environment division.
       input-output section.
       file-control.
           select master assign to 'master.txt'
               organization is line sequential.
           select trans711 assign to 'trans711.txt'
               organization is line sequential.
           select trans713 assign to 'trans713.txt'
               organization is line sequential.
           select transSorted711 assign to 'transSorted711.txt'
               organization is line sequential.
           select transSorted713 assign to 'transSorted713.txt'
               organization is line sequential.
           select transSorted assign to 'transSorted.txt'
               organization is line sequential.
           select updateMaster assign to 'updMaster.txt'
               organization is indexed
               access mode is random
               record key is upd-account-number
               file status is fs.
           select updMaster assign to 'updateMaster.txt'
               organization is line sequential.
           select negReport assign to 'negReport.txt'
               organization is line sequential.
           select trans-tmp assign to 'work.tmp'.
       data division.
           file section.
           fd  master.
           01  account-info.
               05 account-name     pic x(20).
               05 account-number   pic 9(16).
               05 account-password pic 9(6).
               05 account-balance  pic s9(15)
                   sign is leading separate character.

           fd trans711.
           01  tranfer-record1.
               05 account-num1     pic 9(16).
               05 operation1       pic A.
               05 amount1          pic 9(7).
               05 timestamp1       pic 9(5).

           fd trans713.
           01  tranfer-record2.
               05 account-num2     pic 9(16).
               05 operation2       pic A.
               05 amount2          pic 9(7).
               05 timestamp2       pic 9(5).

           fd transSorted711.
           01  srt-tranfer-record1.
               05 srt-account-num1 pic 9(16).
               05 srt-operation1   pic A.
               05 srt-amount1      pic 9(7).
               05 srt-timestamp1   pic 9(5).

           fd transSorted713.
           01  srt-tranfer-record2.
               05 srt-account-num2 pic 9(16).
               05 srt-operation2   pic A.
               05 srt-amount2      pic 9(7).
               05 srt-timestamp2   pic 9(5).

           fd transSorted.
           01  srt-tranfer-record.
               05 srt-account-num  pic 9(16).
               05 srt-operation    pic A.
               05 srt-amount       pic 9(7).
               05 srt-timestamp    pic 9(5).
           
           fd  updateMaster.
           01  upd-account-info.
               05 upd-account-name     pic x(20).
               05 upd-account-number   pic 9(16).
               05 upd-account-password pic 9(6).
               05 upd-account-balance  pic s9(15)
                   sign is leading separate character.
           
           fd  updMaster.
           01  final-account-info.
               05 final-account-name     pic x(20).
               05 final-account-number   pic 9(16).
               05 final-account-password pic 9(6).
               05 final-account-balance  pic s9(15)
                   sign is leading separate character.
           
           fd  negReport.
           01  neg-report.
               05 blank-name           pic x(6).
               05 rep-account-name     pic x(20).
               05 blank-number         pic x(17).
               05 rep-account-number   pic 9(16).
               05 blank-balance        pic x(10).
               05 rep-account-balance  pic s9(15)
                   sign is leading separate character.
           sd  trans-tmp.
           01  tmp-record.
               05 tmp-account-num      pic 9(16).
               05 tmp-operation        pic A.
               05 tmp-amount           pic 9(7).
               05 tmp-timestamp        pic 9(5). 
       working-storage section.
           01  ws-tranfer-record1.
               05 ws-account-num1      pic 9(16).
               05 ws-operation1        pic A.
               05 ws-amount1           pic 9(7).
               05 ws-timestamp1        pic 9(5).
       
           01  ws-tranfer-record2.
               05 ws-account-num2      pic 9(16).
               05 ws-operation2        pic A.
               05 ws-amount2           pic 9(7).
               05 ws-timestamp2        pic 9(5).
           
           01  ws-account-info.
               05 ws-account-name      pic x(20).
               05 ws-account-number    pic 9(16).
               05 ws-account-password  pic 9(6).
               05 ws-account-balance   pic s9(15)
                   sign is leading separate character.
           
           01 tmp-bal                  pic s9(15)
               sign is leading separate character.
           01 eof1                     pic A value "N".
           01 eof2                     pic A value  "N".
           01 first-read               pic 9 value 0.
           01 second-read              pic 9 value 0.
           01 fs                       pic 9(02).
       procedure division.
       trans-sort.
           sort trans-tmp
               on ascending key tmp-account-num
               on ascending key tmp-timestamp
               using trans711
               giving transSorted711.
           
           sort trans-tmp
               on ascending key tmp-account-num
               on ascending key tmp-timestamp
               using trans713
               giving transSorted713.
           
           open input transSorted711.
           open input transSorted713.
           open output transSorted.

       trans-merge.
           if first-read = 0 then 
               go to read-trans711
           end-if.
           if second-read = 0 then 
               go to read-trans713
           end-if.
           if ws-account-num1 < ws-account-num2 then 
               move ws-tranfer-record1 to srt-tranfer-record
               write srt-tranfer-record
               go to read-trans711
           end-if.
           if ws-account-num1 > ws-account-num2 then 
               move ws-tranfer-record2 to srt-tranfer-record
               write srt-tranfer-record
               go to read-trans713
           end-if.
           if ws-account-num1 = ws-account-num2 then 
               if ws-timestamp1 < ws-timestamp2 then 
                   move ws-tranfer-record1 to srt-tranfer-record
                   write srt-tranfer-record
                   go to read-trans711
               end-if
               if ws-timestamp1 > ws-timestamp2 then 
                   move ws-tranfer-record2 to srt-tranfer-record
                   write srt-tranfer-record
                   go to read-trans713
               end-if
           end-if.
       
       read-trans711.
           if eof1 = 'Y' then 
               if eof2 = 'Y' then 
                   go to pre-copy
               end-if
               move 9999999999999999 to ws-account-num1
               move 99999 to ws-timestamp1
               go to read-trans713
           end-if.
           move 9999999999999999 to ws-account-num1.
           move 99999 to ws-timestamp1.
           read transSorted711 into ws-tranfer-record1
           at end move 'Y' to eof1.
           if first-read = 0 then 
               set first-read to 1
           end-if.
           go to trans-merge.

       read-trans713.
           if eof2 = 'Y' then 
               if eof1 = 'Y' then 
                   go to pre-copy
               end-if 
               move 9999999999999999 to ws-account-num2
               move 99999 to ws-timestamp2
               go to read-trans711
           end-if.
           move 9999999999999999 to ws-account-num2.
           move 99999 to ws-timestamp2.
           read transSorted713 into ws-tranfer-record2
           at end move 'Y' to eof2.
           if second-read = 0 then 
               set second-read to 1
           end-if.
           go to trans-merge.
       
       pre-copy.
           close transSorted711.
           close transSorted713.
           close transSorted.
           open input master.
           open output updateMaster.
           move 'N' to eof1.

       copy-master.
           read master into ws-account-info
           at end move 'Y' to eof1.
           
           if eof1 = 'Y' then 
               close master
               close updateMaster
               move 'N' to eof1
               open input transSorted
               go to iterate-trans
           end-if.
           move ws-account-info to upd-account-info.
           write upd-account-info.
           go to copy-master.           

       iterate-trans.
           read transSorted into ws-tranfer-record1
           at end move 'Y' to eof1.

           if eof1 = 'Y' then 
               go to pre-copy-update
           end-if.
           open i-o updateMaster.
           move ws-account-num1 to upd-account-number.
           read updateMaster
               key is upd-account-number
               invalid key display 'Error!'
           end-read.
           if ws-operation1 = 'D' then
               compute upd-account-balance = upd-account-balance +
               ws-amount1
           end-if.
           if ws-operation1 = 'W' then
               compute upd-account-balance = upd-account-balance -
               ws-amount1
           end-if.
           rewrite upd-account-info
           end-rewrite.
           go to iterate-trans.
           
       pre-copy-update.
           close transSorted.
           close updateMaster.
           open input updateMaster.
           open output updMaster.
           open input master.
           open output negReport.
           move 'N' to eof1.

       copy-update.
           read master
           at end move 'Y' to eof1.
           
           if eof1 = 'Y' then 
               close updateMaster
               close updMaster
               close master
               close negReport
               move 'N' to eof1
               stop run
           end-if.
           move account-number to upd-account-number
           read updateMaster
               key is upd-account-number
               invalid key display 'Error!'
           end-read.
           if upd-account-balance < 0 then 
               move 'Name: ' to blank-name
               move upd-account-name to rep-account-name
               move ' Account Number: ' to blank-number
               move upd-account-number to rep-account-number
               move ' Balance: ' to blank-balance
               move upd-account-balance to rep-account-balance
               write neg-report
           end-if.
           move upd-account-info to final-account-info.
           write final-account-info.

           go to copy-update. 
