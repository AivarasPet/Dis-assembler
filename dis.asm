                               
Printinimas MACRO p1, p2 
    mov dx, offset p1 ;input
    mov di, offset p2 ;ouput
    call Irasymas
ENDM Printinimas

Shiftai MACRO p1, l2
    mov al, byte ptr [si]
    shl al, p1
    shr al, l2    
ENDM Shiftai    

Pravalymas MACRO inp, clock
    mov di, offset inp
    mov cx, clock
    call pravalo
ENDM Pravalymas

.MODEL medium    ; Atminties modelis:  ; 64K kodui ir 64K duomenims 
 
.STACK 100h     ; Stekas 
; ---------------Duomenys------------------------------------------- 
.DATA 
  
enteris db 13,10,'$' 
neatpazinta db "NEATPAZINTA",'$'
textKablelis db ", $"
textMov db "mov $" 
textAdd db "add $"
textSub db "sub $"
textCmp db "cmp $"
textJmp db "jmp $"
textCall db "call $"
textLoop db "loop $"
textInc  db "inc $"
textDec  db "dec $"
textInt  db "int $" 
textRet  db "ret $" 
textMul  db "mul $"
textDiv  db "div $" 
textXor  db "xor $"

registrai db "al$cl$dl$bl$ah$ch$dh$bh$ax$cx$dx$bx$sp$bp$si$di$es$cs$ss$ds$"
sudetiniai db "[bx+si]$[bx+di]$[bp+si]$[bp+di]$[si]$   [di]$   [bp]$   [bx]$   "
patogusZodziai db "inc $ dec $ call $call $jmp $ jmp $ push $" 
pushaipopai db "inc $ dec $ push $pop $ "   
salyginiai db "JO $ JNO $JB $ JAE $JE $ JNE $JBE $JA $ JS $ JNS $JP $ JNP $JL $ JGE $JLE $JG $ "
returnai db "ret $ ret $ int 3$int $ into $iret $"
skaiciai db "0123456789ABCDEF$"
  
kursorius dw 30
pradinisSi dw 0 
dabartinisIP dw 100h
RegIndexPosl db 0

kryptis db ?  ;d
wbitas  dw ?  ;w 
sbitas  db ?  ;s
RnM     db ?
Reg     db ?
Mood     db ?

stringas db "                                                      $$"
op1 db "                  $"
op2 db "                  $"
 
duom db 20 dup(0);"PVZ0COM.com"
rez  db 20 dup(0);"rez.txt",0 
kiekis dw 30
skBuf DB 5000 DUP(?)   
dFail	dw ?			;vieta, skirta saugoti duomenÅ³ failo deskriptoriaus numerÄÆ ("handle")
rFail	dw ?	                                                                                                                           

help db "Sveiki, sia programa sukure Aivaras Petrikas, VU 2019. I komandine eilute irasykite PVZCOM.com ir rez.txt",10,13,'$' 
klaidaAtidarymoS db "klaida atidarant skaitymo faila...$"
klaidaAtidarymoR  db "klaida atidarant rasymo faila... $"
klaidaUzdarymoS db "klaida uzddarant skaitymo faila... $"
klaidaUzdarymoR db "klaida uzddarant rasymo faila... $"        
; ---------------Kodas----------------------------------------------- 
.CODE                                                                                                      

PROCESAS:  
    
    mov ax,@data             ; ds - duomenu segmentas  
    mov ds,ax              
	
	MOV	ch, 0			
	MOV	cl, es:0080h		
	CMP	cx, 0			
	JE	zinute			;nereikia nieko tikrinti.
	MOV	bx, 0081h		
  Ieskok:
	CMP	es:bx, '?/'		;atmintyje jaunesnysis baitas saugomas pirmiau.
	JE	zinute			;jeigu radau '/?', vadinasi reikia spausdinti praneÅimÄ…
	INC	bx			
	LOOP	Ieskok			
    
    mov di, 82h
    mov si, offset duom 
	call SkKomEil   
	mov si, offset rez 
	call SkKomEil
    jmp Tesinys                           

   zinute:
    mov dx, offset help
	call Write		
	jmp skiperis2
      
   Tesinys:   
	
    mov al, 0
    MOV	ah, 03Dh				
	MOV	dx, offset duom			;vieta, kur nurodomas failo pavadinimas, pasibaigiantis nuliniu simboliu
	INT	21h				;failas atidaromas skaitymui
	JC	klaidaAtidarantSkaitymui	;jei atidarant fail? skaitymui ?vyksta klaida, nustatomas carry flag
	MOV	dFail, ax	
	
	MOV	ah, 3Ch				;21h pertraukimo failo suk?rimo funkcijos numeris
	MOV	cx, 0				;kuriamo failo atributai
	MOV	dx, offset rez			;vieta, kur nurodomas failo pavadinimas, pasibaigiantis nuliniu simboliu
	INT	21h				;sukuriamas failas; jei failas jau egzistuoja, visa jo informacija i?trinama
	JC	klaidaAtidarantRasymui		;jei kuriant fail? skaitymui ?vyksta klaida, nustatomas carry flag
	MOV	rFail, ax
	   jmp Skaityk
	
	klaidaAtidarantSkaitymui:           
	mov dx, offset klaidaAtidarymoS
	call Write
	JMP	skiperis2
    klaidaAtidarantRasymui:
	mov dx, offset klaidaAtidarymoR
	call Write
	JMP	skiperis2
	
	
	Skaityk:
	MOV	bx, dFail			;duomenu failo deskriptoriaus nr
	CALL	SkaitykBuf			;iskvieciame skaitymo  failo procedura
	CMP	ax, 0				;kiek baitu buvo nuskaityta, jeigu 0 - pasiekta failo pabaiga
	JNE	uzdaryti
	jmp uzdarytiRasymui
	uzdaryti:
	mov kiekis, ax  ;baitu kiekis


    mov si, offset skBuf
    add kiekis, si    
    mov pradinisSI, si    
    KitaKomanda:
	
    cmp kiekis, si
    ja galimSkaityt
    jmp uzdarytiRasymui  
    galimSkaityt:
            mov kursorius, 0  
                       
                mov ax, si
                sub ax, pradinisSi
                add dabartinisIP, ax 
                    
                mov dx, offset stringas
                mov ax, dabartinisIp   
                call transform
                mov bx, offset stringas
                mov byte ptr[bx+4], 58  ;:
                                                 
    mov pradinisSi, si        
    mov kursorius, 26  
	mov al, byte ptr [si]
    Shr al, 4
    
        cmp al, 0 
        jne Toliau4b2
        jmp t0000   
        
        Toliau4b2:
        cmp al, 2 
        jne Toliau4b3
        jmp t0010
        
        Toliau4b3:
        cmp al, 3 
        jne Toliau4b4
        jmp t0011
        
        Toliau4b4:
        cmp al, 4 
        jne Toliau4b5
        jmp t0100
        
        Toliau4b5:
        cmp al, 5 
        jne Toliau4b7
        jmp t0101
        
        Toliau4b7:
        cmp al, 7 
        jne Toliau4b8
        jmp t0111
        
        Toliau4b8:
        cmp al, 8 
        jne Toliau4b9
        jmp t1000
        
        Toliau4b9:
        cmp al, 9 
        jne Toliau4b10
        jmp t1001
                                                                              
        Toliau4b10:
        cmp al, 10 
        jne Toliau4b11
        jmp t1010
        
        Toliau4b11:
        cmp al, 11 
        jne Toliau4b12
        jmp t1011
        
        Toliau4b12:
        cmp al, 12 
        jne Toliau4b14
        jmp t1100
        
        Toliau4b14:
        cmp al, 14 
        jne Toliau4b15
        jmp t1110
        
        Toliau4b15:
        cmp al, 15 
        jne toliau
        jmp t1111
         
    toliau: 
    mov kursorius, 26 
    Printinimas neatpazinta stringas
    inc si
    jmp endOfOps                                                  
        
        t0000:
            Printinimas textAdd stringas
            jeiAtimtis:
            call getDWbits
            call AntroBaitoInit
            Shiftai 4, 6
                             
            cmp al, 0
            jne t0000skip1
            jmp sameOld       ;add reg ir reg
                
            t0000skip1:
                cmp al, 1
                jne iseinamIsCia
                    cmp wbitas, 1 ;add accumulator ir  betOp
                    je nustatom1
                    mov Mood, 0  
                    jmp skipppp
                    nustatom1:
                    mov Mood, 1
                    skipppp:
                    mov Reg, 0
                    jmp akumuliatorius
                IseinamIsCia:
                jmp toliau     
        t0010:
            Shiftai 5, 5
            cmp al, 110b
            je prefiksas
            
            Shiftai 4 6
            cmp al, 3     
            jne atmintisSub
            Printinimas textSub stringas
            jmp t0000skip1 ;betarpiskas sub  
                
            atmintisSub: 
            cmp al, 2
            jne garbage
            Printinimas textSub stringas
            irComparui:
            call getDWbits
            call AntroBaitoInit  
            jmp sameOld ;eis i sameOld 
            
            garbage:
            jmp toliau
            
            prefiksas:   
            Shiftai 3, 6
            mov Reg, al
            dec si
            mov wbitas, 1
            jmp segmentini
        t0011: ;cmp'arai 
            Shiftai 5, 5
            cmp al, 110b
            je prefiksas
            Shiftai 4, 6
            cmp al, 0
            jne tai2
            Printinimas textXor stringas
            jmp irComparui
            tai2:
            cmp al, 2 ;reg - reg/rnm
            jne tai3
            Printinimas textCmp stringas
            jmp irComparui
            tai3:
            cmp al, 3
            jne blogaiTikrai 
            Printinimas textCmp stringas
            jmp t0000skip1
            blogaiTikrai:
            jmp toliau           
        t0100: ;dec ir inc reg               
        t0101: ;push pop reg
            Shiftai 3, 6       
            mov wbitas, 1
            xor ah, ah
            mov dx, offset pushaipopai
            mov di, offset stringas
            mov bx, 6
            call PrintKiturShort
            mov byte ptr [di], 32 ;tarpas
            
            Shiftai 5, 5 
            mov Reg, al
            mov dx, offset op1
            VienoSpausdinimas:
            call PrintReg
            inc si
            jmp galutinis
            
        t0111: ;sa;yginiai, nereik jmp toliau
            Shiftai 4, 4
            mov bx, 5               
            mov di, offset stringas
            mov dx, offset salyginiai
            call PrintKiturShort
            mov bl, byte ptr [si+1]   
            xor cx, cx
            jmp ViengubasPoslinkis
        t1000:   
            call getDWbits
            call AntroBaitoInit
            
            Shiftai 4, 6 ;returnina i al
                
            cmp al, 0
            je Toliau6b00 
            jmp kitasTikrinimas
            
            Toliau6b00:             ;! add ir sub mem/reg betOP
                  mov al, kryptis
                  mov sbitas, al
                  cmp Reg, 5
                  jne ArSudetis 
                  Printinimas textSub stringas
                  jmp jmpMemBetOp
                  ArSudetis:
                  cmp Reg, 7
                  jne taiSudetis 
                  Printinimas textCmp stringas  
                  jmp jmpMemBetOp
                  taiSudetis:
                  cmp Reg, 0
                  jne niekas
                  Printinimas textAdd stringas
                  jmp jmpMemBetOp                      
                  niekas:
                  mov sbitas, 0
                  jmp toliau
            kitasTikrinimas:                             
            cmp al, 2
            je Toliau6b10
            jmp Toliau6b11
        
            Toliau6b10:   ; MOV mem <-> reg/mem  
                  Printinimas textMov stringas                  
                  sameOld: 
                  add si, 2
                  mov al, Mood
                  cmp al, 3 ; Mood = 11
                  je  mod11 
                  jmp neMod11
                  mod11:
                      mov al, Reg
                      mov dx, offset op1
                      call PrintReg
                      mov al, RnM
                      mov dx, offset op2
                      call PrintReg
                      jmp galutinis
                  
                  neMod11:
                      sudetinis:
                      mov al, Reg  
                      mov dx, offset op1
                      call PrintReg     
                      
                      mov dx, offset op2
                      mov al, RnM
                      call PrintSudet
                      
                      jmp galutinis
                                    
             Toliau6b11:         ; MOV reg <-> segm reg                 
                 cmp  al, 3
                 je  segmMov
                 jmp toliau  
                 segmMOV:
                 mov dx, offset textMov
                 Printinimas textMov stringas
                 mov wbitas, 1
                 mov al, RnM
                 mov dx, offset op2
                 call PrintReg    
                 segmentini:
                 mov RegIndexPosl, 24  
                 mov al, Reg
                 mov dx, offset op1
                 call PrintReg
                 mov RegIndexPosl, 0
                 add si, 2         
                 jmp galutinis            
              
        t1001:  ; tik vienas callas toks
                mov al, byte ptr [si]
                cmp al, 10011010b
                jne soktiINezinoma
                    Printinimas textCall stringas 
                    jmp isorinisPoslinkis
                soktiINezinoma:
                jmp toliau 
        t1010:  ;akumuliatorius
                Shiftai 4, 5
                cmp al, 1
                jne arNuliukas
                mov kryptis, 0
                jmp galimDirbt
                arNuliukas:
                cmp al, 0
                jne negalimaDirbt
                mov kryptis, 1
                jmp galimDirbt
                negalimaDirbt:
                jmp toliau
 
                galimDirbt:
                Shiftai 7, 7
                xor ah, ah
                mov wbitas, ax
                mov Mood, 0
                mov RnM, 6
                mov Reg, 0 
                dec si ;nera adresavimo baito
                jmp sameOld
        t1011:  ;1011 wreg
                Printinimas textMov stringas
                Shiftai 4, 7
                xor ah, ah
                mov wbitas, ax
                Shiftai 5, 5
                mov Reg, al  
                
                mov Mood, 2 ;kad 2 baitus skaitytu
                akumuliatorius: 
                mov kryptis, 0
                add si, 1 
                mov dx, offset op1
                call BetarpiskasOp
                mov al, Reg
                mov dx, offset op2
                call PrintReg  
                
                jmp galutinis
        t1100:                       ; reg/mem <-> betarpiskasOp         ir returnai
                call getDWbits
                call AntroBaitoInit
                Shiftai 4, 5
                
                cmp al, 3 ;mov'as
                    jne ziurimArReturnas
                    Printinimas textMov stringas  
                    
                    jmpMeMBetOp:        
                        mov dx, offset op2
                        mov kryptis, 0     
                        add si, 2
                        cmp Mood, 3 ;MOD = 11
                        je regOnly 
                        call PrintSudet
                        jmp dooone
                        regOnly:
                        mov al, RnM
                        call PrintReg
                        dooone:
                        mov dx, offset op1
                        call BetarpiskasOp 
                        jmp galutinis      
                        
                ziurimArReturnas: 
                Shiftai 4, 4
                 
                cmp al, 2
                jne tryyys
                    ce:
                    Printinimas textRet stringas
                    inc si
                    mov dx, offset op1
                    mov wbitas, 1
                    call BetarpiskasOp
                    jmp galutinis
                tryyys:
                cmp al, 3
                jne kitii
                    Printinimas textRet stringas
                    inc si
                    jmp endOfOps     
             
                kitii:
                cmp al, 1010b
                jb neee   
                    Shiftai 5, 5
                    mov bx, 6
                    mov dx, offset returnai 
                    sub al, 2
                    mov di, offset stringas
                    call PrintKiturShort
                    Shiftai 5, 5
                    cmp al, 010b
                    je ce       
                    cmp al, 101b
                    je vientisas 
                    jmp endOfOps 
                    vientisas:
                    inc si
                    mov wbitas, 0
                    mov dx, offset op1
                    call BetarpiskasOp
                    jmp galutinis
                neee:
                jmp toliau       
        t1110:    ;jumpai call loop 
            xor cx, cx
            mov bh, byte ptr [si+2]
            mov bl, byte ptr [si+1]
     
            Shiftai 4, 4
            cmp al, 1000b ;vidinis tiesioginis Call
                jne neCall
                Printinimas textCall stringas
                jmp dvigubasPoslinkis
                                
            neCall:                       
            cmp al, 1011b ;vidinis artimas  
            jne arTiesioginis
                Printinimas textJmp stringas
                ViengubasPoslinkis: 
                add si, 2
                mov ax, 2                 
                call pletimas ;bx plecia
                jmp baigiam1110
            arTiesioginis:
            cmp al, 1001b ;vidinis tiesioginis
            jne isoriniai
                Printinimas textJmp stringas              
                dvigubasPoslinkis:
                add si, 3
                mov ax, 3
                jmp baigiam1110                
            isoriniai:
            cmp al, 1010b ;isorinis
            jne loopas 
                Printinimas textJmp stringas 
                isorinisPoslinkis:
                mov dx, offset op1  
                mov al, byte ptr [si+3]
                mov ah, byte ptr [si+4]
                call transform   
                mov bh, byte ptr [si+2]
                mov bl, byte ptr [si+1]
                mov di, dx
                xor ax, ax  
                xor cx, cx
                mov byte ptr[di+4], ':'
                add si, 5   
                add cx, 5
                jmp ignoruojamIP
            loopas:
            cmp al, 0010b
            jne neatpazinta1110 
                Printinimas textLoop stringas
                jmp  ViengubasPoslinkis
            neatpazinta1110:
            jmp toliau    
            
            baigiam1110:
            add ax, dabartinisIp         ;artimas jmp
            mov dx, offset op1   
            ignoruojamIP:
            add ax, bx
            add dx, cx
            call transform 
            jmp galutinis
        t1111:
            call AntroBaitoInit
            cmp Mood, 11
            jne allGood
            jmp toliau ;neatpazinta
            allGood:
            Shiftai 4, 4
            cmp al, 1111b
            je allGood2      
            jmp divmul    
            allGood2:
            
            mov bx, 6
            xor ah, ah
            mov al, Reg
            mov dx, offset patogusZodziai
            mov di, offset stringas            
            
            mul bl       
            add dx, ax
            call Irasymas
            
            jmp pabaiga1111
            divmul: 
                call getDwBits
                Shiftai 4 5 
                cmp al, 3
                je good
                jmp toliau
                good:
                    cmp Reg, 4
                    jne arDalyba
                        Printinimas textMul, stringas
                        jmp pabaiga1111
                    arDalyba:
                    cmp Reg, 6
                    je taiDalyba
                    jmp toliau
                    taiDalyba:
                        Printinimas textDiv stringas
            pabaiga1111:
            mov dx, offset op1 
            cmp Mood, 11b
            jne sudet
            inc si
            mov al, RnM 
            jmp VienoSpausdinimas
            sudet:
            add si, 2
            call PrintSudet
            jmp galutinis

    galutinis:       
        
        mov di, offset op2
        cmp byte ptr[di], 20h ;jei tuscias op1
        je when0
        
            cmp kryptis, 0   
            je  case0
                           
            Printinimas op1 stringas          
            jmp continue          
                                                      
            case0:
            Printinimas op2 stringas    
        
            continue:        
            Printinimas textKablelis stringas
                  
            cmp kryptis, 0
            je  when0
            Printinimas op2 stringas 
            jmp endOfOps         
            when0:
            Printinimas op1 stringas
            
        endOfOps:
        mov kursorius, 53
        Printinimas enteris stringas 
        
        Pravalymas op1 19
        Pravalymas op2 19
        
            HEXA:  
            mov di, PradinisSi
            mov dx, offset op2         
            mov kursorius, 8
            kartojimas:
            mov al, byte ptr [di]
            push di
            call transform  
            Printinimas op2 stringas
            pop di 
            inc kursorius      
            inc di
            cmp di, si
            jne kartojimas 
            baigta: 
        Pravalymas op2 19
        mov dx, offset stringas
        call Write
        call RasykBuf
        
        Pravalymas stringas 53
        jmp kitaKomanda  
   
      uzdarytiRasymui:
	MOV	ah, 3Eh				
	MOV	bx, rFail			;deskrpt nr
	INT	21h				
	JC	klaidaUzdarantRasymui		;jei uzdarant - klaida, nustatomas carry flag 
    
	jmp skiperis
	
	klaidaUzdarantRasymui:
	mov dx, offset klaidaUzdarymoR
	call Write
	JMP	skiperis2
	skiperis:
    
    xor ax, ax           ; isvalome nuo nereikalingu bitu                 
    mov al, bl
	
	MOV	ah, 3Eh				
	MOV	bx, dFail			;deskr nr
	INT	21h				
	JC	klaidaUzdarantSkaitymui		;jei uÅ¾darant failÄ… ÄÆvyksta klaida, nustatomas carry flag
  
  jmp skiperis2 
  klaidaUzdarantSkaitymui:
	mov dx, offset klaidaUzdarymoS
	call Write   
	           
  skiperis2:    ;------------------------------------END-----------------
    mov ax,04C00h  
    int 21h    ; int 21,4C - programos pabaiga 

SkKomEil PROC      
    skaitomToliau:
    mov al, byte ptr[es:di]
    cmp al, 13
    je BaigiamSkaityt 
    mov [si], al
    inc si
    inc di
    cmp byte ptr[es:di], 20h ;space byte ptr[es:di]
    je BaigiamSkaityt
    cmp byte ptr[es:di], 0h ;byte ptr[es:di]
    je BaigiamSkaityt
    jmp skaitomToliau
    BaigiamSkaityt:
    inc di
    ret        
SkKomEil ENDP 

  
PROC Write   
    push ax                     ;issaugo ax registra steke
    mov ah,09h  
    int 21h                     ; int 21,09 - isvedimas i ekrana  
    pop ax                      ;ji atsiima is steko
    ret 
ENDP Write  

Pletimas PROC
    xor bh, bh
    cmp bl, 128
    jae pleciam
    ret
    pleciam:
    mov bh, 0FFh
    ret
Pletimas ENDP
            
dvigubinisBetOp PROC
    ret
dvigubinisBetOp ENDP          
            
Pravalo PROC
    valom:
    mov byte ptr [di], 32
    inc di     
    loop valom    
    ret
Pravalo ENDP

PROC getDWbits
    Shiftai 6, 7
    mov kryptis, al
    Shiftai 7, 7  
    xor ah,ah
    mov wbitas, ax                        
    ret    
ENDP getDWbits    


AntroBaitoInit PROC
    inc si
    Shiftai  5, 5 
    mov RnM, al   
    
    Shiftai  2, 5 
    mov Reg, al
    
    mov al, byte ptr [si]
	shr al, 6    
    mov Mood, al
    dec si
	ret    
AntroBaitoInit ENDP

PROC BetarpiskasOp
                
    xor ah,ah
    xor cx, cx
    xor bh, bh
    
    cmp wbitas, 0
    je dvizenklis
        cmp sbitas, 1 
        jne iprastasOp 
        mov sbitas, 0
        mov bl, byte ptr [si]
        call pletimas
        jmp dvizenklis    
    iprastasOp:    
    push dx             
    mov al, byte ptr [si+1]
    call transform
    inc cl
    pop dx
    add dx, 2
    dvizenklis:            
    mov al, byte ptr [si]
    mov ah, bh
    call transform
    inc cl 
    
    add si, cx
    ret
ENDP BetarpiskasOp    

PROC PrintKiturLong  ;visur kur ner stringo
    push kursorius
    mov kursorius, 0
    call PrintKiturShort
    pop kursorius
    mov byte ptr[di], 36
    ret
ENDP PrintKiturLong

PROC PrintKiturShort ;kai i stringa
    push dx 
    mul bl       
    add dx, ax
    call Irasymas
    pop dx
    ret
ENDP PrintKiturShort

PrintSudet PROC
                        
    mov di, dx  ;i kur irasys   
    mov dx, offset sudetiniai
    mov bx, 8
    xor ah, ah
    mov al, RnM
    call PrintKiturLong  
    
    cmp Mood, 0     
        jne nelygus0
         cmp RnM, 6
            jne iseinaIsCia
            sub di, 3; ateiti prie [
            mov dx, di               
            push wbitas
            mov wbitas, 1 
            call BetarpiskasOp
            pop wbitas
            mov byte ptr [di], ']' 
            inc di
            jmp iseinaIsCia
    
    iseinaIsCia:
    mov byte ptr [di], 36      
    ret   
    
    nelygus0:
    mov byte ptr [di], ' ' 
    mov byte ptr [di+1], '+'
    add di, 3
    mov dx, di
      
    cmp Mood, 2
    jne Lygus1  
    mov ah, byte ptr [si+1]
    mov al, byte ptr [si] 
    add si, 2 ;komanda current per 2 
    jmp transformink      
    
    Lygus1:      
    mov al, byte ptr [si]
    inc si    ;current komanda per 1
    transformink:
    call transform
    jmp  iseinaIsCia ;ten ret bus 
PrintSudet ENDP     

PrintReg PROC
    push dx
    
    mov di, dx ;outputas
    mov dx, offset registrai 
    
    cmp wbitas, 1
    jne PrintRegSkip1
    add dx, 24
    PrintRegSkip1:                 
    add dl, RegIndexPosl  
    
    mov bx, 3  
    call PrintKiturLong
       
    pop dx 
    ret
PrintReg ENDP
    
PROC Irasymas
    push si
    push ax
    push cx 
    
    mov si, dx ;saltinis 
    add di, kursorius
     
    ciklas:              
    cmp byte ptr [si], 36
    je doleris
    mov al, byte ptr [si]
    mov byte ptr [di], al
    inc di
    inc si
    inc kursorius
    jmp ciklas       
    doleris:
    
    pop cx
    pop ax
    pop si
    ret
ENDP Irasymas
; ---------------Procedura-2----------------------------------------  
    
transform PROC           ; procedura spausdina 10-aini skaiciu
         
    push bx                  ; Issaugo registrus steke
    push cx                                                   
    push dx
           
    mov di, dx               ;i ka irasyt

    mov bx, 16               ; nustatome dalikli
    mov cx, 0                ; nustatome laikroduka
    xor dx, dx               ; isvalome dx
       
    Divide:                         
                             
        div bx               ; Dalina AX is BX, AX bus dalmuo, DX - liekana
        push dx              ; liekana issaugoma steke

        inc cx               ; pridedam viena prie skaitliuko 
        xor dx, dx           ; isvaloma liekana
        cmp ax, 0            ; tikrina ar dalmuo 0
        jne Divide           ; jei dalmuo ne nulis, kartojam
    
    cmp cx, 3 
    je AddZero
    cmp cx, 1
    je AddZero
        jmp Print
        AddZero:
        mov byte ptr [di], 48; 0is
        inc di
    Print:     
        pop ax               ; dx pasiema steko virsu
        mov bx, offset skaiciai
        add bl, al
        
        mov al, byte ptr[bx]    
        mov byte ptr[di], al
        inc di               ; simbolio atspausdinimo komanda  
        loop print           ; testi cikla, kol cx != 0
                       
        mov byte ptr [di],36 ;$ 
        pop dx
        pop cx               ; atsiima registrus
        pop bx
               
        ret
transform ENDP       ; proceduros pabaigos eilute        

SkaitykBuf PROC 
	MOV	ah, 3Fh			
	MOV	cx, 5000		
	MOV	dx, offset skBuf	
	INT	21h			
	JC	klaidaSkaitant		
  SkaitykBufPabaiga:
	RET
  klaidaSkaitant:
	MOV ax, 0			
	JMP	SkaitykBufPabaiga
SkaitykBuf  ENDP 

RasykBuf PROC 
	mov bx, rFail    
	mov cx, 55 
	MOV	ah, 40h			
	MOV	dx, offset stringas
	INT	21h			
	JC	klaidaRasant
	CMP	cx, ax			;jei cx nelygus ax, vadinasi buvo irasyta tik dalis informacijos
	JNE	dalinisIrasymas
  RasykBufPabaiga:
	RET
  dalinisIrasymas:
	JMP	RasykBufPabaiga
  klaidaRasant:
	MOV	ax, 0			;Parasysime registre ax, kad nebuvo irasyttas nei vienas simbolis
	JMP	RasykBufPabaiga
RasykBuf ENDP  	

END PROCESAS