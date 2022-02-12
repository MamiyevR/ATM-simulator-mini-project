# ATM-simulator-mini-project
ATM simulator project implemented with C and COBOL  
GCC version requirement is: 6.3.0  
COBOL version requirement is: 3.1.0-2  
TO compile atms.c:  
>gcc -o atmc atms.c  
>get-content testcase.txt | atmc  

TO compile atms.cob:  

>cobc -x atmcob atms.cob  
>get-content testcase.txt | atmcob  

TO compile central.c:  

>gcc -o centralc central.c sort.c  
>./centralc  

TO compile central.cob:  

>cobc -x centralcob central.cob  
>./centralcob    
