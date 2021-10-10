
main:     file format elf32-littlearm


Disassembly of section .interp:

00010154 <.interp> (File Offset: 0x154):
   10154:	6c2f      	ldr	r7, [r5, #64]	; 0x40
   10156:	6269      	str	r1, [r5, #36]	; 0x24
   10158:	6c2f      	ldr	r7, [r5, #64]	; 0x40
   1015a:	2d64      	cmp	r5, #100	; 0x64
   1015c:	696c      	ldr	r4, [r5, #20]
   1015e:	756e      	strb	r6, [r5, #21]
   10160:	2e78      	cmp	r6, #120	; 0x78
   10162:	6f73      	ldr	r3, [r6, #116]	; 0x74
   10164:	332e      	adds	r3, #46	; 0x2e
	...

Disassembly of section .note.ABI-tag:

00010168 <.note.ABI-tag> (File Offset: 0x168):
   10168:	0004      	movs	r4, r0
   1016a:	0000      	movs	r0, r0
   1016c:	0010      	movs	r0, r2
   1016e:	0000      	movs	r0, r0
   10170:	0001      	movs	r1, r0
   10172:	0000      	movs	r0, r0
   10174:	4e47      	ldr	r6, [pc, #284]	; (10294 <__libc_start_main@plt-0x20> (File Offset: 0x294))
   10176:	0055      	lsls	r5, r2, #1
   10178:	0000      	movs	r0, r0
   1017a:	0000      	movs	r0, r0
   1017c:	0003      	movs	r3, r0
   1017e:	0000      	movs	r0, r0
   10180:	0002      	movs	r2, r0
   10182:	0000      	movs	r0, r0
   10184:	0000      	movs	r0, r0
	...

Disassembly of section .note.gnu.build-id:

00010188 <.note.gnu.build-id> (File Offset: 0x188):
   10188:	0004      	movs	r4, r0
   1018a:	0000      	movs	r0, r0
   1018c:	0014      	movs	r4, r2
   1018e:	0000      	movs	r0, r0
   10190:	0003      	movs	r3, r0
   10192:	0000      	movs	r0, r0
   10194:	4e47      	ldr	r6, [pc, #284]	; (102b4 <__libc_start_main@plt> (File Offset: 0x2b4))
   10196:	0055      	lsls	r5, r2, #1
   10198:	678b      	str	r3, [r1, #120]	; 0x78
   1019a:	6641      	str	r1, [r0, #100]	; 0x64
   1019c:	23fb      	movs	r3, #251	; 0xfb
   1019e:	ef30 ab8a 	vqdmulh.s<illegal width 64>	d10, d16, d10
   101a2:	d481      	bmi.n	100a8 <__libc_start_main@plt-0x20c> (File Offset: 0xa8)
   101a4:	c6e1      	stmia	r6!, {r0, r5, r6, r7}
   101a6:	3e8d      	subs	r6, #141	; 0x8d
   101a8:	abe3      	add	r3, sp, #908	; 0x38c
   101aa:	1201      	asrs	r1, r0, #8

Disassembly of section .gnu.hash:

000101ac <.gnu.hash> (File Offset: 0x1ac):
   101ac:	0002      	movs	r2, r0
   101ae:	0000      	movs	r0, r0
   101b0:	0002      	movs	r2, r0
   101b2:	0000      	movs	r0, r0
   101b4:	0001      	movs	r1, r0
   101b6:	0000      	movs	r0, r0
   101b8:	0005      	movs	r5, r0
   101ba:	0000      	movs	r0, r0
   101bc:	4800      	ldr	r0, [pc, #0]	; (101c0 <__libc_start_main@plt-0xf4> (File Offset: 0x1c0))
   101be:	2002      	movs	r0, #2
   101c0:	0002      	movs	r2, r0
   101c2:	0000      	movs	r0, r0
   101c4:	0003      	movs	r3, r0
   101c6:	0000      	movs	r0, r0
   101c8:	4e2f      	ldr	r6, [pc, #188]	; (10288 <__libc_start_main@plt-0x2c> (File Offset: 0x288))
   101ca:	f63d ed7d 			; <UNDEFINED> instruction: 0xf63ded7d
   101ce:	0f11      	lsrs	r1, r2, #28

Disassembly of section .dynsym:

000101d0 <.dynsym> (File Offset: 0x1d0):
	... (skipping 16 zeroes, resuming at file offset: 0x1e0)
   101e0:	002d      	movs	r5, r5
	... (skipping 8 zeroes, resuming at file offset: 0x1ea)
   101ea:	0000      	movs	r0, r0
   101ec:	0020      	movs	r0, r4
   101ee:	0000      	movs	r0, r0
   101f0:	0011      	movs	r1, r2
	... (skipping 8 zeroes, resuming at file offset: 0x1fa)
   101fa:	0000      	movs	r0, r0
   101fc:	0012      	movs	r2, r2
   101fe:	0000      	movs	r0, r0
   10200:	000b      	movs	r3, r1
	... (skipping 8 zeroes, resuming at file offset: 0x20a)
   1020a:	0000      	movs	r0, r0
   1020c:	0012      	movs	r2, r2
	...

Disassembly of section .dynstr:

00010210 <.dynstr> (File Offset: 0x210):
   10210:	6c00      	ldr	r0, [r0, #64]	; 0x40
   10212:	6269      	str	r1, [r5, #36]	; 0x24
   10214:	2e63      	cmp	r6, #99	; 0x63
   10216:	6f73      	ldr	r3, [r6, #116]	; 0x74
   10218:	362e      	adds	r6, #46	; 0x2e
   1021a:	6100      	str	r0, [r0, #16]
   1021c:	6f62      	ldr	r2, [r4, #116]	; 0x74
   1021e:	7472      	strb	r2, [r6, #17]
   10220:	5f00      	ldrsh	r0, [r0, r4]
   10222:	6c5f      	ldr	r7, [r3, #68]	; 0x44
   10224:	6269      	str	r1, [r5, #36]	; 0x24
   10226:	5f63      	ldrsh	r3, [r4, r5]
   10228:	7473      	strb	r3, [r6, #17]
   1022a:	7261      	strb	r1, [r4, #9]
   1022c:	5f74      	ldrsh	r4, [r6, r5]
   1022e:	616d      	str	r5, [r5, #20]
   10230:	6e69      	ldr	r1, [r5, #100]	; 0x64
   10232:	4700      	bx	r0
   10234:	494c      	ldr	r1, [pc, #304]	; (10368 <abort@plt+0x9c> (File Offset: 0x368))
   10236:	4342      	muls	r2, r0
   10238:	325f      	adds	r2, #95	; 0x5f
   1023a:	342e      	adds	r4, #46	; 0x2e
   1023c:	5f00      	ldrsh	r0, [r0, r4]
   1023e:	675f      	str	r7, [r3, #116]	; 0x74
   10240:	6f6d      	ldr	r5, [r5, #116]	; 0x74
   10242:	5f6e      	ldrsh	r6, [r5, r5]
   10244:	7473      	strb	r3, [r6, #17]
   10246:	7261      	strb	r1, [r4, #9]
   10248:	5f74      	ldrsh	r4, [r6, r5]
   1024a:	005f      	lsls	r7, r3, #1

Disassembly of section .gnu.version:

0001024c <.gnu.version> (File Offset: 0x24c):
   1024c:	0000      	movs	r0, r0
   1024e:	0000      	movs	r0, r0
   10250:	0002      	movs	r2, r0
   10252:	0002      	movs	r2, r0

Disassembly of section .gnu.version_r:

00010254 <.gnu.version_r> (File Offset: 0x254):
   10254:	0001      	movs	r1, r0
   10256:	0001      	movs	r1, r0
   10258:	0001      	movs	r1, r0
   1025a:	0000      	movs	r0, r0
   1025c:	0010      	movs	r0, r2
   1025e:	0000      	movs	r0, r0
   10260:	0000      	movs	r0, r0
   10262:	0000      	movs	r0, r0
   10264:	6914      	ldr	r4, [r2, #16]
   10266:	0d69      	lsrs	r1, r5, #21
   10268:	0000      	movs	r0, r0
   1026a:	0002      	movs	r2, r0
   1026c:	0023      	movs	r3, r4
   1026e:	0000      	movs	r0, r0
   10270:	0000      	movs	r0, r0
	...

Disassembly of section .rel.dyn:

00010274 <.rel.dyn> (File Offset: 0x274):
   10274:	1018      	asrs	r0, r3, #32
   10276:	0002      	movs	r2, r0
   10278:	0115      	lsls	r5, r2, #4
	...

Disassembly of section .rel.plt:

0001027c <.rel.plt> (File Offset: 0x27c):
   1027c:	100c      	asrs	r4, r1, #32
   1027e:	0002      	movs	r2, r0
   10280:	0216      	lsls	r6, r2, #8
   10282:	0000      	movs	r0, r0
   10284:	1010      	asrs	r0, r2, #32
   10286:	0002      	movs	r2, r0
   10288:	0116      	lsls	r6, r2, #4
   1028a:	0000      	movs	r0, r0
   1028c:	1014      	asrs	r4, r2, #32
   1028e:	0002      	movs	r2, r0
   10290:	0316      	lsls	r6, r2, #12
	...

Disassembly of section .init:

00010294 <.init> (File Offset: 0x294):
   10294:	4008      	ands	r0, r1
   10296:	e92d 001d 	stmdb	sp!, {r0, r2, r3, r4}
   1029a:	eb00 8008 			; <UNDEFINED> instruction: 0xeb008008
   1029e:	Address 0x000000000001029e is out of bounds.


Disassembly of section .plt:

000102a0 <__libc_start_main@plt-0x14> (File Offset: 0x2a0):
   102a0:	e004      	b.n	102ac <__libc_start_main@plt-0x8> (File Offset: 0x2ac)
   102a2:	e52d      	b.n	fd00 <__libc_start_main@plt-0x5b4> (File Offset: 0xfffffffffffffd00)
   102a4:	e004      	b.n	102b0 <__libc_start_main@plt-0x4> (File Offset: 0x2b0)
   102a6:	e59f      	b.n	fde8 <__libc_start_main@plt-0x4cc> (File Offset: 0xfffffffffffffde8)
   102a8:	e00e      	b.n	102c8 <__gmon_start__@plt+0x8> (File Offset: 0x2c8)
   102aa:	e08f      	b.n	103cc <abort@plt+0x100> (File Offset: 0x3cc)
   102ac:	f008 e5be 	blx	418e2c <abort@plt+0x408b60> (File Offset: 0x408e2c)
   102b0:	0d50      	lsrs	r0, r2, #21
   102b2:	0001      	movs	r1, r0

000102b4 <__libc_start_main@plt> (File Offset: 0x2b4):
   102b4:	c600      	stmia	r6!, {}
   102b6:	e28f      	b.n	107d8 <abort@plt+0x50c> (File Offset: 0x7d8)
   102b8:	ca10      	ldmia	r2!, {r4}
   102ba:	e28c      	b.n	107d6 <abort@plt+0x50a> (File Offset: 0x7d6)
   102bc:	fd50 e5bc 	ldc2l	5, cr14, [r0, #-752]	; 0xfffffd10

000102c0 <__gmon_start__@plt> (File Offset: 0x2c0):
   102c0:	c600      	stmia	r6!, {}
   102c2:	e28f      	b.n	107e4 <abort@plt+0x518> (File Offset: 0x7e4)
   102c4:	ca10      	ldmia	r2!, {r4}
   102c6:	e28c      	b.n	107e2 <abort@plt+0x516> (File Offset: 0x7e2)
   102c8:	fd48 e5bc 	stc2l	5, cr14, [r8, #-752]	; 0xfffffd10

000102cc <abort@plt> (File Offset: 0x2cc):
   102cc:	c600      	stmia	r6!, {}
   102ce:	e28f      	b.n	107f0 <abort@plt+0x524> (File Offset: 0x7f0)
   102d0:	ca10      	ldmia	r2!, {r4}
   102d2:	e28c      	b.n	107ee <abort@plt+0x522> (File Offset: 0x7ee)
   102d4:	fd40 e5bc 	stc2l	5, cr14, [r0, #-752]	; 0xfffffd10

Disassembly of section .text:

000102d8 <.text> (File Offset: 0x2d8):
   102d8:	b000      	add	sp, #0
   102da:	e3a0      	b.n	10a1e <abort@plt+0x752> (File Offset: 0xa1e)
   102dc:	e000      	b.n	102e0 <abort@plt+0x14> (File Offset: 0x2e0)
   102de:	e3a0      	b.n	10a22 <abort@plt+0x756> (File Offset: 0xa22)
   102e0:	1004      	asrs	r4, r0, #32
   102e2:	e49d      	b.n	fc20 <__libc_start_main@plt-0x694> (File Offset: 0xfffffffffffffc20)
   102e4:	200d      	movs	r0, #13
   102e6:	e1a0      	b.n	1062a <abort@plt+0x35e> (File Offset: 0x62a)
   102e8:	2004      	movs	r0, #4
   102ea:	e52d      	b.n	fd48 <__libc_start_main@plt-0x56c> (File Offset: 0xfffffffffffffd48)
   102ec:	0004      	movs	r4, r0
   102ee:	e52d      	b.n	fd4c <__libc_start_main@plt-0x568> (File Offset: 0xfffffffffffffd4c)
   102f0:	c010      	stmia	r0!, {r4}
   102f2:	e59f      	b.n	fe34 <__libc_start_main@plt-0x480> (File Offset: 0xfffffffffffffe34)
   102f4:	c004      	stmia	r0!, {r2}
   102f6:	e52d      	b.n	fd54 <__libc_start_main@plt-0x560> (File Offset: 0xfffffffffffffd54)
   102f8:	000c      	movs	r4, r1
   102fa:	e59f      	b.n	fe3c <__libc_start_main@plt-0x478> (File Offset: 0xfffffffffffffe3c)
   102fc:	300c      	adds	r0, #12
   102fe:	e59f      	b.n	fe40 <__libc_start_main@plt-0x474> (File Offset: 0xfffffffffffffe40)
   10300:	ffeb ebff 			; <UNDEFINED> instruction: 0xffebebff
   10304:	fff0 ebff 			; <UNDEFINED> instruction: 0xfff0ebff
   10308:	0488      	lsls	r0, r1, #18
   1030a:	0001      	movs	r1, r0
   1030c:	03f9      	lsls	r1, r7, #15
   1030e:	0001      	movs	r1, r0
   10310:	0428      	lsls	r0, r5, #16
   10312:	0001      	movs	r1, r0
   10314:	3014      	adds	r0, #20
   10316:	e59f      	b.n	fe58 <__libc_start_main@plt-0x45c> (File Offset: 0xfffffffffffffe58)
   10318:	2014      	movs	r0, #20
   1031a:	e59f      	b.n	fe5c <__libc_start_main@plt-0x458> (File Offset: 0xfffffffffffffe5c)
   1031c:	3003      	adds	r0, #3
   1031e:	e08f      	b.n	10440 <abort@plt+0x174> (File Offset: 0x440)
   10320:	2002      	movs	r0, #2
   10322:	e793      	b.n	1024c <__libc_start_main@plt-0x68> (File Offset: 0x24c)
   10324:	0000      	movs	r0, r0
   10326:	e352      	b.n	109ce <abort@plt+0x702> (File Offset: 0x9ce)
   10328:	ff1e 012f 	vrhadd.u16	d0, d14, d31
   1032c:	ffe3 eaff 			; <UNDEFINED> instruction: 0xffe3eaff
   10330:	0cdc      	lsrs	r4, r3, #19
   10332:	0001      	movs	r1, r0
   10334:	0018      	movs	r0, r3
   10336:	0000      	movs	r0, r0
   10338:	0018      	movs	r0, r3
   1033a:	e59f      	b.n	fe7c <__libc_start_main@plt-0x438> (File Offset: 0xfffffffffffffe7c)
   1033c:	3018      	adds	r0, #24
   1033e:	e59f      	b.n	fe80 <__libc_start_main@plt-0x434> (File Offset: 0xfffffffffffffe80)
   10340:	0000      	movs	r0, r0
   10342:	e153      	b.n	105ec <abort@plt+0x320> (File Offset: 0x5ec)
   10344:	ff1e 012f 	vrhadd.u16	d0, d14, d31
   10348:	3010      	adds	r0, #16
   1034a:	e59f      	b.n	fe8c <__libc_start_main@plt-0x428> (File Offset: 0xfffffffffffffe8c)
   1034c:	0000      	movs	r0, r0
   1034e:	e353      	b.n	109f8 <abort@plt+0x72c> (File Offset: 0x9f8)
   10350:	ff1e 012f 	vrhadd.u16	d0, d14, d31
   10354:	ff13 e12f 	vrhadd.u16	d14, d3, d31
   10358:	1024      	asrs	r4, r4, #32
   1035a:	0002      	movs	r2, r0
   1035c:	1024      	asrs	r4, r4, #32
   1035e:	0002      	movs	r2, r0
   10360:	0000      	movs	r0, r0
   10362:	0000      	movs	r0, r0
   10364:	0024      	movs	r4, r4
   10366:	e59f      	b.n	fea8 <__libc_start_main@plt-0x40c> (File Offset: 0xfffffffffffffea8)
   10368:	1024      	asrs	r4, r4, #32
   1036a:	e59f      	b.n	feac <__libc_start_main@plt-0x408> (File Offset: 0xfffffffffffffeac)
   1036c:	1000      	asrs	r0, r0, #32
   1036e:	e041      	b.n	103f4 <abort@plt+0x128> (File Offset: 0x3f4)
   10370:	1141      	asrs	r1, r0, #5
   10372:	e1a0      	b.n	106b6 <abort@plt+0x3ea> (File Offset: 0x6b6)
   10374:	1fa1      	subs	r1, r4, #6
   10376:	e081      	b.n	1047c <abort@plt+0x1b0> (File Offset: 0x47c)
   10378:	10c1      	asrs	r1, r0, #3
   1037a:	e1b0      	b.n	106de <abort@plt+0x412> (File Offset: 0x6de)
   1037c:	ff1e 012f 	vrhadd.u16	d0, d14, d31
   10380:	3010      	adds	r0, #16
   10382:	e59f      	b.n	fec4 <__libc_start_main@plt-0x3f0> (File Offset: 0xfffffffffffffec4)
   10384:	0000      	movs	r0, r0
   10386:	e353      	b.n	10a30 <abort@plt+0x764> (File Offset: 0xa30)
   10388:	ff1e 012f 	vrhadd.u16	d0, d14, d31
   1038c:	ff13 e12f 	vrhadd.u16	d14, d3, d31
   10390:	1024      	asrs	r4, r4, #32
   10392:	0002      	movs	r2, r0
   10394:	1024      	asrs	r4, r4, #32
   10396:	0002      	movs	r2, r0
   10398:	0000      	movs	r0, r0
   1039a:	0000      	movs	r0, r0
   1039c:	4010      	ands	r0, r2
   1039e:	e92d 4018 	stmdb	sp!, {r3, r4, lr}
   103a2:	e59f      	b.n	fee4 <__libc_start_main@plt-0x3d0> (File Offset: 0xfffffffffffffee4)
   103a4:	3000      	adds	r0, #0
   103a6:	e5d4      	b.n	ff52 <__libc_start_main@plt-0x362> (File Offset: 0xffffffffffffff52)
   103a8:	0000      	movs	r0, r0
   103aa:	e353      	b.n	10a54 <abort@plt+0x788> (File Offset: 0xa54)
   103ac:	8010      	strh	r0, [r2, #0]
   103ae:	18bd      	adds	r5, r7, r2
   103b0:	ffe0 ebff 			; <UNDEFINED> instruction: 0xffe0ebff
   103b4:	3001      	adds	r0, #1
   103b6:	e3a0      	b.n	10afa <abort@plt+0x82e> (File Offset: 0xafa)
   103b8:	3000      	adds	r0, #0
   103ba:	e5c4      	b.n	ff46 <__libc_start_main@plt-0x36e> (File Offset: 0xffffffffffffff46)
   103bc:	8010      	strh	r0, [r2, #0]
   103be:	e8bd 1024 	ldmia.w	sp!, {r2, r5, ip}
   103c2:	0002      	movs	r2, r0
   103c4:	ffe6 eaff 			; <UNDEFINED> instruction: 0xffe6eaff
   103c8:	b580      	push	{r7, lr}
   103ca:	af00      	add	r7, sp, #0
   103cc:	2301      	movs	r3, #1
   103ce:	0018      	movs	r0, r3
   103d0:	46bd      	mov	sp, r7
   103d2:	bd80      	pop	{r7, pc}
   103d4:	b580      	push	{r7, lr}
   103d6:	af00      	add	r7, sp, #0
   103d8:	2302      	movs	r3, #2
   103da:	0018      	movs	r0, r3
   103dc:	46bd      	mov	sp, r7
   103de:	bd80      	pop	{r7, pc}
   103e0:	b580      	push	{r7, lr}
   103e2:	af00      	add	r7, sp, #0
   103e4:	2303      	movs	r3, #3
   103e6:	0018      	movs	r0, r3
   103e8:	46bd      	mov	sp, r7
   103ea:	bd80      	pop	{r7, pc}
   103ec:	b580      	push	{r7, lr}
   103ee:	af00      	add	r7, sp, #0
   103f0:	2304      	movs	r3, #4
   103f2:	0018      	movs	r0, r3
   103f4:	46bd      	mov	sp, r7
   103f6:	bd80      	pop	{r7, pc}
   103f8:	b590      	push	{r4, r7, lr}
   103fa:	b083      	sub	sp, #12
   103fc:	af00      	add	r7, sp, #0
   103fe:	f7ff ffe3 	bl	103c8 <abort@plt+0xfc> (File Offset: 0x3c8)
   10402:	0004      	movs	r4, r0
   10404:	f7ff ffe6 	bl	103d4 <abort@plt+0x108> (File Offset: 0x3d4)
   10408:	0003      	movs	r3, r0
   1040a:	18e4      	adds	r4, r4, r3
   1040c:	f7ff ffe8 	bl	103e0 <abort@plt+0x114> (File Offset: 0x3e0)
   10410:	0003      	movs	r3, r0
   10412:	18e4      	adds	r4, r4, r3
   10414:	f7ff ffea 	bl	103ec <abort@plt+0x120> (File Offset: 0x3ec)
   10418:	0003      	movs	r3, r0
   1041a:	18e3      	adds	r3, r4, r3
   1041c:	607b      	str	r3, [r7, #4]
   1041e:	687b      	ldr	r3, [r7, #4]
   10420:	0018      	movs	r0, r3
   10422:	46bd      	mov	sp, r7
   10424:	b003      	add	sp, #12
   10426:	bd90      	pop	{r4, r7, pc}
   10428:	47f0      	blx	lr
   1042a:	e92d 604c 	stmdb	sp!, {r2, r3, r6, sp, lr}
   1042e:	e59f      	b.n	ff70 <__libc_start_main@plt-0x344> (File Offset: 0xffffffffffffff70)
   10430:	504c      	str	r4, [r1, r1]
   10432:	e59f      	b.n	ff74 <__libc_start_main@plt-0x340> (File Offset: 0xffffffffffffff74)
   10434:	6006      	str	r6, [r0, #0]
   10436:	e08f      	b.n	10558 <abort@plt+0x28c> (File Offset: 0x558)
   10438:	5005      	str	r5, [r0, r0]
   1043a:	e08f      	b.n	1055c <abort@plt+0x290> (File Offset: 0x55c)
   1043c:	6005      	str	r5, [r0, #0]
   1043e:	e046      	b.n	104ce <abort@plt+0x202> (File Offset: 0x4ce)
   10440:	7000      	strb	r0, [r0, #0]
   10442:	e1a0      	b.n	10786 <abort@plt+0x4ba> (File Offset: 0x786)
   10444:	8001      	strh	r1, [r0, #0]
   10446:	e1a0      	b.n	1078a <abort@plt+0x4be> (File Offset: 0x78a)
   10448:	9002      	str	r0, [sp, #8]
   1044a:	e1a0      	b.n	1078e <abort@plt+0x4c2> (File Offset: 0x78e)
   1044c:	ff90 ebff 			; <UNDEFINED> instruction: 0xff90ebff
   10450:	6146      	str	r6, [r0, #20]
   10452:	e1b0      	b.n	107b6 <abort@plt+0x4ea> (File Offset: 0x7b6)
   10454:	87f0      	strh	r0, [r6, #62]	; 0x3e
   10456:	08bd      	lsrs	r5, r7, #2
   10458:	4000      	ands	r0, r0
   1045a:	e3a0      	b.n	10b9e <abort@plt+0x8d2> (File Offset: 0xb9e)
   1045c:	4001      	ands	r1, r0
   1045e:	e284      	b.n	1096a <abort@plt+0x69e> (File Offset: 0x96a)
   10460:	3004      	adds	r0, #4
   10462:	e495      	b.n	fd90 <__libc_start_main@plt-0x524> (File Offset: 0xfffffffffffffd90)
   10464:	2009      	movs	r0, #9
   10466:	e1a0      	b.n	107aa <abort@plt+0x4de> (File Offset: 0x7aa)
   10468:	1008      	asrs	r0, r1, #32
   1046a:	e1a0      	b.n	107ae <abort@plt+0x4e2> (File Offset: 0x7ae)
   1046c:	0007      	movs	r7, r0
   1046e:	e1a0      	b.n	107b2 <abort@plt+0x4e6> (File Offset: 0x7b2)
   10470:	ff33 e12f 	vrhadd.u<illegal width 64>	d14, d3, d31
   10474:	0004      	movs	r4, r0
   10476:	e156      	b.n	10726 <abort@plt+0x45a> (File Offset: 0x726)
   10478:	fff7 1aff 			; <UNDEFINED> instruction: 0xfff71aff
   1047c:	87f0      	strh	r0, [r6, #62]	; 0x3e
   1047e:	e8bd 0ad8 	ldmia.w	sp!, {r3, r4, r6, r7, r9, fp}
   10482:	0001      	movs	r1, r0
   10484:	0ad0      	lsrs	r0, r2, #11
   10486:	0001      	movs	r1, r0
   10488:	ff1e e12f 	vrhadd.u16	d14, d14, d31

Disassembly of section .fini:

0001048c <.fini> (File Offset: 0x48c):
   1048c:	4008      	ands	r0, r1
   1048e:	e92d 8008 	stmdb	sp!, {r3, pc}
   10492:	Address 0x0000000000010492 is out of bounds.


Disassembly of section .rodata:

00010494 <.rodata> (File Offset: 0x494):
   10494:	0001      	movs	r1, r0
   10496:	0002      	movs	r2, r0

Disassembly of section .ARM.exidx:

00010498 <.ARM.exidx> (File Offset: 0x498):
   10498:	fe40 7fff 	mcr2	15, 2, r7, cr0, cr15, {7}
   1049c:	0001      	movs	r1, r0
	...

Disassembly of section .eh_frame:

000104a0 <.eh_frame> (File Offset: 0x4a0):
   104a0:	0000      	movs	r0, r0
	...

Disassembly of section .init_array:

00020f10 <.init_array> (File Offset: 0xf10):
   20f10:	03c4      	lsls	r4, r0, #15
   20f12:	0001      	movs	r1, r0

Disassembly of section .fini_array:

00020f14 <.fini_array> (File Offset: 0xf14):
   20f14:	039c      	lsls	r4, r3, #14
   20f16:	0001      	movs	r1, r0

Disassembly of section .dynamic:

00020f18 <.dynamic> (File Offset: 0xf18):
   20f18:	0001      	movs	r1, r0
   20f1a:	0000      	movs	r0, r0
   20f1c:	0001      	movs	r1, r0
   20f1e:	0000      	movs	r0, r0
   20f20:	000c      	movs	r4, r1
   20f22:	0000      	movs	r0, r0
   20f24:	0294      	lsls	r4, r2, #10
   20f26:	0001      	movs	r1, r0
   20f28:	000d      	movs	r5, r1
   20f2a:	0000      	movs	r0, r0
   20f2c:	048c      	lsls	r4, r1, #18
   20f2e:	0001      	movs	r1, r0
   20f30:	0019      	movs	r1, r3
   20f32:	0000      	movs	r0, r0
   20f34:	0f10      	lsrs	r0, r2, #28
   20f36:	0002      	movs	r2, r0
   20f38:	001b      	movs	r3, r3
   20f3a:	0000      	movs	r0, r0
   20f3c:	0004      	movs	r4, r0
   20f3e:	0000      	movs	r0, r0
   20f40:	001a      	movs	r2, r3
   20f42:	0000      	movs	r0, r0
   20f44:	0f14      	lsrs	r4, r2, #28
   20f46:	0002      	movs	r2, r0
   20f48:	001c      	movs	r4, r3
   20f4a:	0000      	movs	r0, r0
   20f4c:	0004      	movs	r4, r0
   20f4e:	0000      	movs	r0, r0
   20f50:	fef5 6fff 	mrc2	15, 7, r6, cr5, cr15, {7}
   20f54:	01ac      	lsls	r4, r5, #6
   20f56:	0001      	movs	r1, r0
   20f58:	0005      	movs	r5, r0
   20f5a:	0000      	movs	r0, r0
   20f5c:	0210      	lsls	r0, r2, #8
   20f5e:	0001      	movs	r1, r0
   20f60:	0006      	movs	r6, r0
   20f62:	0000      	movs	r0, r0
   20f64:	01d0      	lsls	r0, r2, #7
   20f66:	0001      	movs	r1, r0
   20f68:	000a      	movs	r2, r1
   20f6a:	0000      	movs	r0, r0
   20f6c:	003c      	movs	r4, r7
   20f6e:	0000      	movs	r0, r0
   20f70:	000b      	movs	r3, r1
   20f72:	0000      	movs	r0, r0
   20f74:	0010      	movs	r0, r2
   20f76:	0000      	movs	r0, r0
   20f78:	0015      	movs	r5, r2
   20f7a:	0000      	movs	r0, r0
   20f7c:	0000      	movs	r0, r0
   20f7e:	0000      	movs	r0, r0
   20f80:	0003      	movs	r3, r0
   20f82:	0000      	movs	r0, r0
   20f84:	1000      	asrs	r0, r0, #32
   20f86:	0002      	movs	r2, r0
   20f88:	0002      	movs	r2, r0
   20f8a:	0000      	movs	r0, r0
   20f8c:	0018      	movs	r0, r3
   20f8e:	0000      	movs	r0, r0
   20f90:	0014      	movs	r4, r2
   20f92:	0000      	movs	r0, r0
   20f94:	0011      	movs	r1, r2
   20f96:	0000      	movs	r0, r0
   20f98:	0017      	movs	r7, r2
   20f9a:	0000      	movs	r0, r0
   20f9c:	027c      	lsls	r4, r7, #9
   20f9e:	0001      	movs	r1, r0
   20fa0:	0011      	movs	r1, r2
   20fa2:	0000      	movs	r0, r0
   20fa4:	0274      	lsls	r4, r6, #9
   20fa6:	0001      	movs	r1, r0
   20fa8:	0012      	movs	r2, r2
   20faa:	0000      	movs	r0, r0
   20fac:	0008      	movs	r0, r1
   20fae:	0000      	movs	r0, r0
   20fb0:	0013      	movs	r3, r2
   20fb2:	0000      	movs	r0, r0
   20fb4:	0008      	movs	r0, r1
   20fb6:	0000      	movs	r0, r0
   20fb8:	fffe 6fff 			; <UNDEFINED> instruction: 0xfffe6fff
   20fbc:	0254      	lsls	r4, r2, #9
   20fbe:	0001      	movs	r1, r0
   20fc0:	ffff 6fff 			; <UNDEFINED> instruction: 0xffff6fff
   20fc4:	0001      	movs	r1, r0
   20fc6:	0000      	movs	r0, r0
   20fc8:	fff0 6fff 			; <UNDEFINED> instruction: 0xfff06fff
   20fcc:	024c      	lsls	r4, r1, #9
   20fce:	0001      	movs	r1, r0
	...

Disassembly of section .got:

00021000 <.got> (File Offset: 0x1000):
   21000:	0f18      	lsrs	r0, r3, #28
   21002:	0002      	movs	r2, r0
	... (skipping 8 zeroes, resuming at file offset: 0x100c)
   2100c:	02a0      	lsls	r0, r4, #10
   2100e:	0001      	movs	r1, r0
   21010:	02a0      	lsls	r0, r4, #10
   21012:	0001      	movs	r1, r0
   21014:	02a0      	lsls	r0, r4, #10
   21016:	0001      	movs	r1, r0
   21018:	0000      	movs	r0, r0
	...

Disassembly of section .data:

0002101c <.data> (File Offset: 0x101c):
	...

Disassembly of section .bss:

00021024 <.bss> (File Offset: 0x1024):
   21024:	0000      	movs	r0, r0
	...

Disassembly of section .comment:

00000000 <.comment> (File Offset: 0x1024):
   0:	4347      	muls	r7, r0
   2:	3a43      	subs	r2, #67	; 0x43
   4:	2820      	cmp	r0, #32
   6:	6255      	str	r5, [r2, #36]	; 0x24
   8:	6e75      	ldr	r5, [r6, #100]	; 0x64
   a:	7574      	strb	r4, [r6, #21]
   c:	4c2f      	ldr	r4, [pc, #188]	; (cc <__libc_start_main@plt-0x101e8> (File Offset: 0x10f0))
   e:	6e69      	ldr	r1, [r5, #100]	; 0x64
  10:	7261      	strb	r1, [r4, #9]
  12:	206f      	movs	r0, #111	; 0x6f
  14:	2e37      	cmp	r6, #55	; 0x37
  16:	2e35      	cmp	r6, #53	; 0x35
  18:	2d30      	cmp	r5, #48	; 0x30
  1a:	7533      	strb	r3, [r6, #20]
  1c:	7562      	strb	r2, [r4, #21]
  1e:	746e      	strb	r6, [r5, #17]
  20:	3175      	adds	r1, #117	; 0x75
  22:	317e      	adds	r1, #126	; 0x7e
  24:	2e38      	cmp	r6, #56	; 0x38
  26:	3430      	adds	r4, #48	; 0x30
  28:	2029      	movs	r0, #41	; 0x29
  2a:	2e37      	cmp	r6, #55	; 0x37
  2c:	2e35      	cmp	r6, #53	; 0x35
  2e:	0030      	movs	r0, r6

Disassembly of section .ARM.attributes:

00000000 <.ARM.attributes> (File Offset: 0x1054):
   0:	2941      	cmp	r1, #65	; 0x41
   2:	0000      	movs	r0, r0
   4:	6100      	str	r0, [r0, #16]
   6:	6165      	str	r5, [r4, #20]
   8:	6962      	ldr	r2, [r4, #20]
   a:	0100      	lsls	r0, r0, #4
   c:	001f      	movs	r7, r3
   e:	0000      	movs	r0, r0
  10:	3505      	adds	r5, #5
  12:	0054      	lsls	r4, r2, #1
  14:	0306      	lsls	r6, r0, #12
  16:	0108      	lsls	r0, r1, #4
  18:	0109      	lsls	r1, r1, #4
  1a:	0412      	lsls	r2, r2, #16
  1c:	0113      	lsls	r3, r2, #4
  1e:	0114      	lsls	r4, r2, #4
  20:	0115      	lsls	r5, r2, #4
  22:	0317      	lsls	r7, r2, #12
  24:	0118      	lsls	r0, r3, #4
  26:	0119      	lsls	r1, r3, #4
  28:	021a      	lsls	r2, r3, #8
