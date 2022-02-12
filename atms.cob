       identification division.
       program-id. atms.
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
       data division.
           file section.
           fd  master.
           01  account-info.
               05 account-name     pic x(20).
               05 account-number   pic 9(16).
               05 account-password pic 9(6).
               05 account-balance  pic s9(16)
                   sign is leading separate character.
           fd trans711.
           01  tranfer-record1.
               05 t-account-num1   pic 9(16).
               05 operation1       pic A.
               05 t-amount1        pic 9(7).
               05 t-timestamp1     pic 9(5).
           fd trans713.
           01  tranfer-record2.
               05 t-account-num2   pic 9(16).
               05 operation2       pic A.
               05 t-amount2        pic 9(7).
               05 t-timestamp2     pic 9(5).
       working-storage section.
       01  start-time              pic 9(5) value zeroes.
       01  cont                    pic x(10).
           88 isContinue           value "Y", "y".
           88 notContinue          value "N", "n".
       01  atmNum                  pic x(10).
           88 first-atm            value "1".
           88 second-atm           value "2".
       01  first-open              pic 9 value 0.
       01  second-open             pic 9 value 0.
       01  ws-account-info.
           05 ws-account-name      pic x(20).
           05 ws-account-number    pic 9(16).
           05 ws-account-password  pic 9(6).
           05 ws-account-balance   pic s9(16)
               sign is leading separate character.
       01  ws-t-account-info.
           05 ws-t-account-name     pic x(20).
           05 ws-t-account-number   pic 9(16).
           05 ws-t-account-password pic 9(6).
           05 ws-t-account-balance  pic s9(16)
               sign is leading separate character.
       01  accountNum              pic x(50).
       01  passwordNum             pic x(50).
       01  serviceSelect           pic x(10).
           88 deposit-option       value "D", "d".
           88 withdrawal-option    value "W", "w".
           88 transfer-option      value "T", "t".
       01  tempService             pic A.
       01  deposit                 pic s9(5)v9(3).
       01  withdrawal              pic s9(5)v9(3).
       01  transferAccount         pic x(50).
       01  transferAmount          pic s9(5)v9(3).
       01  record-amount           pic s9(7).
       01  eof                     pic A value "N".
       procedure division.
       welcome-display.
           display '##############################################'.
           display '##         Gringotts Wizarding Bank         ##'.
           display '##                 Welcome                  ##'.
           display '##############################################'.
       
       atm-selection.
           display '=> PLEASE CHOOSE THE ATM'.
           display '=> PRESS 1 FOR ATM 711'.
           display '=> PRESS 2 FOR ATM 713'.
           accept atmNum.
           if not first-atm and not second-atm then
               display '=> INVALID INPUT!'
               go to atm-selection
           end-if.           
           if first-atm and first-open = 0 then 
               open output trans711
               set first-open to 1
           end-if.
           if second-atm and second-open = 0 then 
               open output trans713
               set second-open to 1
           end-if.
           
       account-selection.
           display '=> ACCOUNT'.
           accept accountNum.
           display '=> PASSWORD'.
           accept passwordNum.
           open input master.
           move 'N' to eof.

       account-check.
           read master into ws-account-info
           at end move 'Y' to eof.
       
           if eof = 'Y' then
               if ws-account-number not = accountNum then
                   display "=> INCORRECT ACCOUNT/PASSWORD"
                   close master
                   go to account-selection
               end-if
              
               if ws-account-number = accountNum then
                   if ws-account-password not = passwordNum then
                       close master
                       display "=> INCORRECT ACCOUNT/PASSWORD"
                       close master
                       go to account-selection
                   end-if    
               end-if
           end-if.
           if eof = 'N' then
               if ws-account-number not = accountNum then
                   go to account-check
               end-if
              
               if ws-account-number = accountNum then
                   if ws-account-password not = passwordNum then
                       go to account-check
                  end-if
               end-if
           end-if.
           
       balance-check.
           close master.
           move 'N' to eof.
           if ws-account-balance < 0 then
               display '=> NEGATIVE REMAINS TRANSACTION ABORT'
               go to atm-selection
           end-if.
       
       service-selection.
           display '=> PLEASE CHOOSE YOUR SERVICE'.
           display '=> PRESS D FOR DEPOSIT'.
           display '=> PRESS W FOR WITHDRAWAL'.
           display '=> PRESS T FOR TRANSFER'.
           accept serviceSelect.
           move function upper-case(serviceSelect) to serviceSelect.
           if deposit-option then
               go to deposit-func
           end-if.
           if withdrawal-option then 
               go to withdrawal-func
           end-if.
           if transfer-option then 
               go to transfer-func
           end-if.
           if not deposit-option and not withdrawal-option and not
           transfer-option then
               display '=> INVALID INPUT'
               go to service-selection
           end-if.
       
       deposit-func.
           display '=> AMOUNT'.
           accept deposit.
           if deposit < 0 then 
               display 'INVALID INPUT'
               go to deposit-func
           end-if.
           compute record-amount = deposit*100.
           go to write-func.
       
       withdrawal-func.
           display '=> AMOUNT'.
           accept withdrawal.
           if withdrawal < 0 then 
               display 'INVALID INPUT'
               go to withdrawal-func
           end-if.
           if withdrawal > ws-account-balance then
               display '=> INSUFFICIENT BALANCE'
               go to withdrawal-func
           end-if.
           compute record-amount = withdrawal*100.
           go to write-func.

       transfer-func.
           display '=>TARGET ACCOUNT'.
           accept transferAccount.
           if transferAccount = ws-account-number then 
               display '=> YOU CANNOT TRANSFER TO YOURSELF'
               go to transfer-func
           end-if.
           open input master.
       
       find-account.
           read master into ws-t-account-info
           at end move 'Y' to eof.
       
           if eof = 'Y' then
               if ws-t-account-number not = transferAccount then
                   display "=> TARGET ACCOUNT DOES NOT EXIST"
                   close master
                   move 'N' to eof
                   go to transfer-func
               end-if
           end-if.
           if eof = 'N' then
               if ws-t-account-number not = transferAccount then
                   go to find-account
               end-if
           end-if.
           close master.
       
       get-t-amount.
           display '=> AMOUNT'
           accept transferAmount.
           if transferAmount < 0 then 
               display '=> INVALID INPUT'
               go to get-t-amount
           end-if.
           compute record-amount = transferAmount*100
           if record-amount > ws-account-balance then
               display '=> INSUFFICIENT BALANCE'
               go to get-t-amount
           end-if.

       write-func.
           move serviceSelect to tempService.
           if transfer-option then 
               move 'W' to tempService
           end-if.
           if first-atm then  
               move ws-account-number to t-account-num1
               move tempService to operation1
               move record-amount to t-amount1
               move start-time to t-timestamp1
               write tranfer-record1
           end-if.
           if second-atm then  
               move ws-account-number to t-account-num2
               move tempService to operation2
               move record-amount to t-amount2
               move start-time to t-timestamp2
               write tranfer-record2
           end-if.
           compute start-time = start-time + 1.
           if transfer-option then 
               move 'D' to tempService
               if first-atm then
                   move transferAccount to t-account-num1
                   move tempService to operation1
                   move record-amount to t-amount1
                   move start-time to t-timestamp1
                   write tranfer-record1
               end-if
               if second-atm then  
                   move ws-account-number to t-account-num2
                   move tempService to operation2
                   move record-amount to t-amount2
                   move start-time to t-timestamp2
                   write tranfer-record2
               end-if
               compute start-time = start-time + 1
           end-if.

       continue-option.
           display '=> CONTINUE?'.
           display '=> N FOR NO'.
           display '=> Y FOR YES'.
           accept cont.
           if not isContinue and not notContinue then
               display '=> INVALID INPUT'
               go to continue-option
           end-if.
           if isContinue then
               go to atm-selection
           end-if.
           if notContinue then
               if first-open = 1 then 
                   close trans711
               end-if
               if second-open = 1 then 
                   close trans713
               end-if
               stop run
           end-if.
